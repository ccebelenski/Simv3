package com.sim.pdp11

import com.sim.cpu.{BasicMMU, CompositeRegister32, Register16}
import com.sim.unsigned.UInt

import scala.annotation.switch
import com.sim.unsigned.ushort2uint
import com.sim.unsigned.ubyte2uint
import com.sim.unsigned.uint2int
import com.sim.unsigned.ushort2Int

import scala.language.implicitConversions

class PDP11MMU(cpu: PDP11) extends BasicMMU(cpu) {


  // Registers
  val MMR0 = new Register16("MMR0") // MMR0 - Status
  val MMR1 = new Register16("MMR1") // MMR1 - R+/-R
  val MMR2 = new Register16("MMR2") // MMR2 - saved PC
  val MMR3 = new Register16("MMR3") // MMR3 - 22b status

  val KIPAR0 = new Register16("KIPARO")
  val KIPDR0 = new Register16("KIPDRO")
  val KIP0 = new CompositeRegister32("KIP0", KIPAR0, KIPDR0)


  /* MMR0 */
  val MMR0_MME = 0x1 // mem mgt enable
  val MMR0_V_PAGE = 1 // offset to pageno
  val MMR0_M_PAGE = 0x3f // mask for pageno
  val MMR0_PAGE: Int = MMR0_M_PAGE << MMR0_V_PAGE
  val MMR0_IC = 0x80 // instr complete
  val MMR0_MAINT = 0x100 // maintenance
  val MMR0_TENB = 0x200 // trap enable
  val MMR0_TRAP = 0x1000 // mem mgt trap
  val MMR0_RO = 0x2000 // read only error
  val MMR0_PL = 0x4000 // page lnt error
  val MMR0_NR = 0x8000 // no access error
  val MMR0_FREEZE = 0xe000 // if set, no update
  val MMR0_WR = 0xf301 // writeable bits

  /* MMR3 */
  val MMR3_UDS = 0x1 // user dspace enbl
  val MMR3_SDS = 0x2 // super dspace enbl
  val MMR3_KDS = 0x4 // krnl dspace enbl
  val MMR3_CSM = 0x8 // CSM enable
  val MMR3_M22E = 0x10 // 22b mem mgt enbl
  val MMR3_BME: UInt = UInt(0x20) // DMA bus map enbl

  /* if set, no update */
  @inline def update_MM: Boolean = (MMR0 & MMR0_FREEZE) == 0

  MMR0(0)
  MMR1(0)
  MMR2(0)
  MMR3(0)
  KIP0(0)

  /* PARs/PDRs */
  val APRFILE: Array[UInt] = new Array[UInt](64)

  var isenable: Int = 0
  var dsenable: Int = 0
  /* i, d space flags */
  var reg_mods: Int = 0
  /* reg deltas */
  var last_pa: UInt = _ /* pa from ReadMW/ReadMB */

  def calc_MMR1(val1: Int): Int = if (reg_mods != 0) (val1 << 8) | reg_mods else val1

  val dsmask: Array[Int] = Array(MMR3_KDS, MMR3_SDS, 0, MMR3_UDS) /* dspace enables */

  /* Virtual address */

  val VA_DF = 0x1fff // displacement
  val VA_BN = 0x1fc0 // block number
  val VA_V_APF = 13 // offset to APF
  val VA_V_DS = 16 // offset to space
  val VA_V_MODE = 17 // offset to mode
  val VA_DS: UInt = UInt(1) << VA_V_DS // data space flag

  def calc_is(md: Int): Int = md << VA_V_MODE

  def calc_ds(md: Int): Int = if ((calc_is(md) | (MMR3.get16 & dsmask(md))) != 0) VA_DS.intValue else 0

  /* I/O access modes */

  val READ = 0
  val READC = 1 // read console
  val WRITE = 2
  val WRITEC = 3 // write console
  val WRITEB = 4

  /* PDR */
  val PDR_ACF = 0x7 // access control
  val PDR_ACS = 0x6 // 2b access control
  val PDR_ED = 0x8 // expansion dir
  val PDR_W = 0x40 // written flag
  val PDR_A: UInt = UInt(0x80) // access flag
  val PDR_PLF = 0x7f00 // page lnt field
  val PDR_NOC = 0x8000 // don't cache
  val PDR_PRD = 0x3 // page readable if 2


  // TODO Explore how this gets set and what it represents - is this the best way to handle this?
  var uc15_memsize: UInt = _

  @inline def ADDR_IS_MEM(x: UInt): Boolean = x < uc15_memsize

  /* Effective address calculations
     Inputs:
          spec    =       specifier <5:0>
     Outputs:
          ea      =       effective address
                          <15:0> =  virtual address
                          <16> =    instruction/data data space
                          <18:17> = mode
     Data space calculation: the PDP-11 features both instruction and data
     spaces.  Instruction space contains the instruction and any sequential
     add ons (eg, immediates, absolute addresses).  Data space contains all
     data operands and indirect addresses.  If data space is enabled, then
     memory references are directed according to these rules:
          Mode    Index ref       Indirect ref            Direct ref
          10..16  na              na                      data
          17      na              na                      instruction
          20..26  na              na                      data
          27      na              na                      instruction
          30..36  na              data                    data
          37      na              instruction (absolute)  data
          40..46  na              na                      data
          47      na              na                      instruction
          50..56  na              data                    data
          57      na              instruction             data
          60..67  instruction     na                      data
          70..77  instruction     data                    data
     According to the PDP-11 Architecture Handbook, MMR1 records all
     autoincrement and autodecrement operations, including those which
     explicitly reference the PC.  For the J-11, this is only true for
     autodecrement operands, autodecrement deferred operands, and
     autoincrement destination operands that involve a write to memory.
     The simulator follows the Handbook, for simplicity.
     Notes:
     - dsenable will direct a reference to data space if data space is enabled
     - ds will direct a reference to data space if data space is enabled AND if
          the specifier register is not PC; this is used for 17, 27, 37, 47, 57
     - Modes 2x, 3x, 4x, and 5x must update MMR1 if updating enabled
     - Modes 46 and 56 must check for stack overflow if kernel mode
  */

  /* Effective address calculation for words */
  def GeteaW(spec: Int): UInt = {
    val reg = spec & 0x7
    /* register number */
    val ds = if (reg == 7) isenable else dsenable /* dspace if not PC */

    ((spec >> 3).intValue: @switch) match {
      /* decode spec<5:3> */

      case 1 => /* (R) */
        UInt(cpu.R(reg) | ds)

      case 2 => /* (R)+ */
        val adr: UInt = UInt((cpu.R(reg) + UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)

        reg_mods = calc_MMR1(0x10 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        adr | UInt(ds)

      case 3 => /* @(R)+ */
        var adr = UInt(cpu.R(reg) + UInt(2) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0x10 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        adr = ReadW(adr | UInt(ds))
        adr | UInt(dsenable)

      case 4 => /* -(R) */
        //adr = R[reg] = (R[reg] - 2) & 0xffff
        val adr = UInt((cpu.R(reg) - UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0xf0 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr.intValue < (cpu.STKLIM + PDP11.STKL_Y).intValue))
          cpu.set_stack_trap(adr)
        UInt(adr | ds)

      case 5 => /* @-(R) */
        //adr = R[reg] = (R[reg] - 2) & 0xffff
        var adr = UInt((cpu.R(reg) - UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0xf0 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr.intValue < (cpu.STKLIM + PDP11.STKL_Y).intValue))
          cpu.set_stack_trap(adr)
        adr = ReadW(UInt(adr | ds))
        UInt(adr | dsenable)

      case 6 => /* d(r) */
        val adr = ReadW(UInt(cpu.PC | isenable))
        cpu.PC((cpu.PC + 2) & 0xffff)
        UInt(((cpu.R(reg) + adr) & 0xffff) | dsenable)

      case 7 => /* @d(R) */
        var adr = ReadW(UInt(cpu.PC | isenable))
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        adr = ReadW(UInt(((cpu.R(reg) + adr) & 0xffff) | dsenable))
        UInt(adr | dsenable)
    } /* end switch */
  }

  /* Effective address calculation for bytes */

  def GeteaB(spec: UInt): UInt = {
    val reg = spec & 0x7
    /* reg number */
    val ds = if (reg == 7) isenable else dsenable /* dspace if not PC */
    ((spec >> 3).intValue: @switch) match {
      /* decode spec<5:3> */

      case 1 => /* (R) */
        UInt(cpu.R(reg) | ds)

      case 2 => /* (R)+ */
        val delta = 1 + {
          if (reg >= 6) -1 else 0
        }
        /* 2 if R6, PC */
        val adr = UInt((cpu.R(reg) + delta) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1((delta << 3) | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        UInt(adr | ds)

      /* @(R)+ */
      case 3 =>
        var adr: UInt = UInt((cpu.R(reg) + UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0x10 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        adr = ReadW(UInt(adr | ds))
        UInt(adr | dsenable)

      case 4 => /* -(R) */
        val delta = UInt(1) + (if (reg >= 6) -1 else 0) /* 2 if R6, PC */
        cpu.R(reg).set16((cpu.R(reg) - delta) & 0xffff)

        val adr = cpu.R(reg).get16
        reg_mods = calc_MMR1((((-delta) & 0x1f) << 3) | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr)
        UInt(adr | ds)

      case 5 => /* @-(R) */
        cpu.R(reg).set16((cpu.R(reg) - 2) & 0xffff)
        var adr = cpu.R(reg).get16
        reg_mods = calc_MMR1(0xf0 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr)
        adr = ReadW(UInt(adr | ds)).toUShort
        UInt(adr | dsenable)

      case 6 => /* d(r) */
        val adr = ReadW(UInt(cpu.PC.get16 | isenable))
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        UInt(((cpu.R(reg) + adr) & 0xffff) | dsenable)

      case 7 => /* @d(R) */
        var adr = ReadW(UInt(cpu.PC.get16 | isenable))
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        adr = ReadW(UInt(((cpu.R(reg) + adr) & 0xffff) | dsenable))
        UInt(adr | dsenable)
    } /* end switch */
  }

  /* Read byte and word routines, read only and read-modify-write versions
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          data    =       data read from memory or I/O space
  */

  // Solve the data write
  def ReadE(va: UInt): UInt = {

    if (((va & UInt(1)) != 0) && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)
    }
    val pa = relocR(va) /* relocate */
    //if (BPT_SUMM_RD &&
    //  (sim_brk_test(va & 0xffff, BPT_RDVIR) ||
    //    sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    if (ADDR_IS_MEM(pa)) /* memory address? */
      return get16(pa.intValue)
    //      return RdMemW(pa)
    if ((pa < PDP11.IOPAGEBASE) || /* not I/O address */
      (cpu.CPUT(CPUOPT.CPUT_J) && (pa.intValue >= PDP11.IOBA_CPU))) {
      /* or J11 int reg? */
      cpu.setCPUERR(PDP11.CPUE_NXM)
      throw AbortException(PDP11.TRAP_NXM)
    }
    iopageR(pa, READ) match {
      case None =>
        /* invalid I/O addr? */
        cpu.setCPUERR(PDP11.CPUE_TMO)
        throw AbortException(PDP11.TRAP_NXM)
      case Some(x) => x
    }

  }

  def ReadW(va: UInt): UInt = {

    if (((va & 1) != 0) && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)
    }
    val pa = relocR(va) /* relocate */
    //if (BPT_SUMM_RD &&
    //  (cpu.sim_brk_test(va & 0xffff, BPT_RDVIR) ||
    //    cpu.sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    PReadW(pa)
  }

  def ReadB(va: UInt): UInt = {

    val pa = relocR(va) /* relocate */
    //if (BPT_SUMM_RD &&
    //  (cpu.sim_brk_test(va & 0xffff, BPT_RDVIR) ||
    //    cpu.sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    PReadB(pa)
  }

  /* Read word with breakpoint check: if a data breakpoint is encountered,
     set reason accordingly but don't do an ABORT.  This is used when we want
     to break after doing the operation, used for interrupt processing.  */
  def ReadCW(va: UInt): UInt = {
    if (((va & 1) != 0) && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)
    }
    val pa = relocR(va) /* relocate */
    //if (BPT_SUMM_RD &&
    //  (sim_brk_test(va & 0xffff, BPT_RDVIR) ||
    //    sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
    //  reason = STOP_IBKPT /* report that */
    PReadW(pa)
  }

  def ReadMW(va: UInt): UInt = {
    if (((va & 1) != 0) && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)
    }
    last_pa = relocW(va) /* reloc, wrt chk */
    //if (BPT_SUMM_RW &&
    //  (sim_brk_test(va & 0xffff, BPT_RWVIR) ||
    //    sim_brk_test(last_pa, BPT_RWPHY))) /* read or write breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    PReadW(last_pa)
  }

  def ReadMB(va: UInt): UInt = {
    last_pa = relocW(va) /* reloc, wrt chk */
    // if (BPT_SUMM_RW &&
    //   (sim_brk_test(va & 0xffff, BPT_RWVIR) ||
    //     sim_brk_test(last_pa, BPT_RWPHY))) /* read or write breakpoint? */
    //   ABORT(ABRT_BKPT) /* stop simulation */
    PReadB(last_pa)
  }

  val int_internal: Array[Int] = Array(
    UInt(0), PDP11.INT_INTERNAL1, PDP11.INT_INTERNAL2, PDP11.INT_INTERNAL3,
    PDP11.INT_INTERNAL4, PDP11.INT_INTERNAL5, PDP11.INT_INTERNAL6, PDP11.INT_INTERNAL7
  )

  /* Calculate interrupt outstanding
   In a Qbus system, all device interrupts are treated as BR4 */

  def calc_ints(nipl: Int, trq: Int): Int = {
    val all_int: Boolean = if ((cpu.UNIBUS.intValue != 0) || (nipl < PDP11.IPL_HMIN.intValue)) true else false
    var i = PDP11.IPL_HLVL - 1
    while (i > nipl.intValue) {
      val t = if (all_int) cpu.int_req(i)
      else cpu.int_req(i) & int_internal(i)
      if (t != 0) return trq | PDP11.TRAP_INT.intValue

      i = i - 1
    }
    trq & ~PDP11.TRAP_INT
  }

  // IO Dispatch jump tables
  val iodispR = new Array[(UInt, Int) => Option[UInt]](PDP11.IOPAGESIZE.intValue >> 1)
  val iodispW = new Array[(UInt, UInt, Int) => Option[UInt]](PDP11.IOPAGESIZE.intValue >> 1)

  //t_stat (*iodispR[IOPAGESIZE >> 1])(int32 *dat, int32 ad, int32 md);
  //t_stat (*iodispW[IOPAGESIZE >> 1])(int32 dat, int32 ad, int32 md);
  /* I/O page lookup and linkage routines
     Inputs:
          *data   =       pointer to data to read, if READ
          data    =       data to store, if WRITE or WRITEB
          pa      =       address
          access  =       READ, WRITE, or WRITEB
     Outputs:
          status  =       SCPE_OK or SCPE_NXM
  */

  def iopageR(pa: UInt, access: Int): Option[UInt] = {
    var dataret: Option[UInt] = null

    val idx = (pa & PDP11.IOPAGEMASK) >> 1
    if (iodispR(idx) != null) {
      dataret = iodispR(idx)(pa, access)
      cpu.trap_req = calc_ints(cpu.ipl, cpu.trap_req)
      return dataret
    }
    None
  }

  def iopageW(data: UInt, pa: UInt, access: Int): Option[UInt] = {
    val idx = (pa & PDP11.IOPAGEMASK) >> 1
    if (iodispW(idx) != null) {
      val dataret = iodispW(idx)(data, pa, access)
      cpu.trap_req = calc_ints(cpu.ipl, cpu.trap_req)
      return dataret
    }
    None
  }

  // Handle data write
  def PReadW(pa: UInt): UInt = {
    if (ADDR_IS_MEM(pa)) /* memory address? */
      return get16(pa.intValue)
    //return RdMemW(pa)
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      cpu.setCPUERR(PDP11.CPUE_NXM)
      throw AbortException(PDP11.TRAP_NXM)
    }
    val dataret = iopageR(pa, READ)
    dataret match {
      case None =>
        /* invalid I/O addr? */
        cpu.setCPUERR(PDP11.CPUE_TMO)
        throw AbortException(PDP11.TRAP_NXM)
      case Some(x) => x
    }

  }

  // Handle data write
  def PReadB(pa: UInt): UInt = {

    if (ADDR_IS_MEM(pa)) /* memory address? */
      return get8(pa.intValue)
    //return RdMemB(pa)
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      cpu.setCPUERR(PDP11.CPUE_NXM)
      throw AbortException(PDP11.TRAP_NXM)

    }
    val dataret = iopageR(pa, READ)
    dataret match {
      case None =>
        /* invalid I/O addr? */
        cpu.setCPUERR(PDP11.CPUE_TMO)
        throw AbortException(PDP11.TRAP_NXM)
      case Some(x) =>
        UInt((if ((pa & 1) != 0) x >> 8 else x) & 0xff)
    }

  }

  /* Write byte and word routines
     Inputs:
          data    =       data to be written
          va      =       virtual address, <18:16> = mode, I/D space, or
          pa      =       physical address
     Outputs: none
  */

  def WriteW(data: UInt, va: UInt): Unit = {
    if ((va & 1) == 0 && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)

    }
    val pa = relocW(va) /* relocate */
    //if (BPT_SUMM_WR &&
    //  (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
    //    sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    PWriteW(data, pa)
  }

  def WriteB(data: UInt, va: UInt): Unit = {
    val pa = relocW(va) /* relocate */
    //if (BPT_SUMM_WR &&
    //  (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
    //    sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
    //  ABORT(ABRT_BKPT) /* stop simulation */
    PWriteB(data, pa)
  }

  /* Write word with breakpoint check: if a data breakpoint is encountered,
     set reason accordingly but don't do an ABORT.  This is used when we want
     to break after doing the operation, used for interrupt processing.  */
  def WriteCW(data: UInt, va: UInt): Unit = {
    if (((va & UInt(1)) != 0) && cpu.CPUT(CPUOPT.HAS_ODD)) {
      /* odd address? */
      cpu.setCPUERR(PDP11.CPUE_ODD)
      throw AbortException(PDP11.TRAP_ODD)
    }
    val pa = relocW(va) /* relocate */
    //if (BPT_SUMM_WR &&
    //  (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
    //    sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
    //  reason = STOP_IBKPT /* report that */
    PWriteW(data, pa)
  }


  def PWriteW(data: UInt, pa: UInt): Unit = {
    if (ADDR_IS_MEM(pa)) {
      /* memory address? */
      put16(pa.intValue, data.toUShort)
      //      WrMemW(pa, data)
      return
    }
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      cpu.setCPUERR(PDP11.CPUE_NXM)
      throw AbortException(PDP11.TRAP_NXM)
    }
    iopageW(data, pa, WRITE) match {
      case None =>
        /* invalid I/O addr? */
        cpu.setCPUERR(PDP11.CPUE_TMO)
        throw AbortException(PDP11.TRAP_NXM)
      case Some(x) => // Nothing
    }
  }

  def PWriteB(data: UInt, pa: UInt): Unit = {
    if (ADDR_IS_MEM(pa)) {
      /* memory address? */
      put8(pa.intValue, data.toUByte)
      //WrMemB(pa, data)
      return
    }
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      cpu.setCPUERR(PDP11.CPUE_NXM)
      throw AbortException(PDP11.TRAP_NXM)
    }
    iopageW(data, pa, WRITEB) match {
      case None =>
        /* invalid I/O addr? */
        cpu.setCPUERR(PDP11.CPUE_TMO)
        throw AbortException(PDP11.TRAP_NXM)
      case Some(_) => // Nothing
    }
  }

  /* Relocate virtual address, read access
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          pa      =       physical address
     On aborts, this routine aborts back to the top level simulator
     with an appropriate trap code.
     Notes:
     - The 'normal' read codes (010, 110) are done in-line; all
       others in a subroutine
     - APRFILE[UNUSED] is all zeroes, forcing non-resident abort
     - Aborts must update MMR0<15:13,6:1> if updating is enabled
  */

  def relocR(va: UInt): UInt = {
    var pa: UInt = UInt(0)

    if ((MMR0.get16 & MMR0_MME) != 0) {
      /* if mmgt */
      val apridx = (va >> VA_V_APF) & 0x3f
      /* index into APR */
      val apr = APRFILE(apridx) /* with va<18:13> */
      if ((apr & PDR_PRD) != 2) /* not 2, 6? */
        relocR_test(va, apridx) /* long test */
      if (PLF_test(va, apr)) /* pg lnt error? */
        reloc_abort(MMR0_PL, apridx)
      pa = UInt(((va & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK)
      if ((MMR3.get16 & MMR3_M22E) == 0) {
        pa = pa & UInt(0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000) | pa
      }
    }
    else {
      pa = va & UInt(0xffff) /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000) | pa
    }
    pa
  }

  /* Read relocation, access control field != read only or read/write
     ACF value            11/45,11/70             all others
     0                    abort NR                abort NR
     1                    trap                    -
     2                    ok                      ok
     3                    abort NR                -
     4                    trap                    abort NR
     5                    ok                      -
     6                    ok                      ok
     7                    abort NR                -
  */

  def relocR_test(va: UInt, apridx: Int): Unit = {
    var err = 0
    /* init status */
    val apr = APRFILE(apridx) /* get APR */
    (apr & PDR_ACF: @switch) match {
      /* case on ACF */

      case 1 | 4 => /* trap read */
        if (cpu.CPUT(CPUOPT.HAS_MMTR)) {
          /* traps implemented? */
          APRFILE(apridx) = APRFILE(apridx) | PDR_A /* set A */
          if ((MMR0 & MMR0_TENB) != 0) {
            /* traps enabled? */
            if (update_MM) /* update MMR0 */
              MMR0.set16((MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE))
            MMR0.set16(MMR0 | MMR0_TRAP) /* set trap flag */
            cpu.setTRAP(PDP11.TRAP_MME.intValue) /* set trap */
          }
          return /* continue op */
        } /* not impl, abort NR */
      case 0 | 3 | 7 => /* non-resident */
        err = MMR0_NR /* set MMR0 */
      /* go test PLF, abort */

      case 2 | 5 | 6 => /* readable */
        return /* continue */
    } /* end switch */

    if (PLF_test(va, apr)) /* pg lnt error? */
      err = err | MMR0_PL
    reloc_abort(err, apridx)
  }

  def PLF_test(va: UInt, apr: UInt): Boolean = {
    val dbn = va & VA_BN
    /* extr block num */
    val plf = (apr & PDR_PLF) >> 2 /* extr page length */

    if ((apr.intValue & PDR_ED) != 0) dbn < plf else dbn > plf /* pg lnt error? */
  }

  def reloc_abort(err: Int, apridx: Int): Unit = {
    /* update MMR0 */
    if (update_MM) MMR0.set16((MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE))
    APRFILE(apridx) = APRFILE(apridx) | PDR_A /* set A */
    MMR0.set16(MMR0.get16 | err) /* set aborts */
    throw AbortException(PDP11.TRAP_MME)
    //ABORT(TRAP_MME) /* abort ref */
  }

  /* Relocate virtual address, write access
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          pa      =       physical address
     On aborts, this routine aborts back to the top level simulator
     with an appropriate trap code.
     Notes:
     - The 'normal' write code (110) is done in-line all others
       in a subroutine
     - APRFILE[UNUSED] is all zeroes, forcing non-resident abort
     - Aborts must update MMR0<15:13,6:1> if updating is enabled
  */

  def relocW(va: UInt): UInt = {
    var pa = UInt(0)
    if ((MMR0 & MMR0_MME) != 0) {
      /* if mmgt */
      val apridx = (va >> VA_V_APF) & 0x3f
      /* index into APR */
      val apr = APRFILE(apridx) /* with va<18:13> */
      if ((apr & PDR_ACF) != 6) /* not writeable? */
        relocW_test(va, apridx) /* long test */
      if (PLF_test(va, apr)) /* pg lnt error? */
        reloc_abort(MMR0_PL, apridx)
      APRFILE(apridx) = UInt(apr.intValue | PDR_W) /* set W */
      pa = UInt(((va & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK)
      if ((MMR3 & MMR3_M22E) == 0) {
        pa = UInt(pa & 0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000 | pa)
      }
    }
    else {
      pa = UInt(va & 0xffff) /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000 | pa)
    }
    pa
  }

  /* Write relocation, access control field != read/write
     ACF value            11/45,11/70             all others
     0                    abort NR                abort NR
     1                    abort RO                -
     2                    abort RO                abort RO
     3                    abort NR                -
     4                    trap                    abort NR
     5                    trap                    -
     6                    ok                      ok
     7                    abort NR                -
  */

  def relocW_test(va: UInt, apridx: Int): Unit = {

    var err = 0
    /* init status */
    val apr = APRFILE(apridx) /* get APR */
    (apr & PDR_ACF: @switch) match {
      /* case on ACF */

      case 4 | 5 => /* trap write */
        if (cpu.CPUT(CPUOPT.HAS_MMTR)) {
          /* traps implemented? */
          APRFILE(apridx) = APRFILE(apridx) | PDR_A /* set A */
          if ((MMR0.get16 & MMR0_TENB) != 0) {
            /* traps enabled? */
            if (update_MM) /* update MMR0 */
              MMR0.set16((MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE))
            MMR0.set16(MMR0 | MMR0_TRAP) /* set trap flag */
            cpu.setTRAP(PDP11.TRAP_MME.intValue) /* set trap */
          }
          return /* continue op */
        } /* not impl, abort NR */
      case 0 | 3 | 7 => /* non-resident */
        err = MMR0_NR /* MMR0 status */

      case 1 | 2 => /* read only */
        err = MMR0_RO /* MMR0 status */


      case 6 => /* read/write */
        return /* continue */
    } /* end switch */
    if (PLF_test(va, apr)) /* pg lnt error? */
      err = err | MMR0_PL
    reloc_abort(err, apridx)
  }

  /* Relocate virtual address, console access
     Inputs:
          va      =       virtual address
          sw      =       switches
     Outputs:
          pa      =       physical address
     On aborts, this routine returns MAXMEMSIZE
  */

  def relocC(va: UInt, sw: UInt): UInt = {
    var mode: Int = 0
    var pa: UInt = UInt(0)
    var va2 = va

    if ((MMR0.get16 & MMR0_MME) != 0) {
      /* if mmgt */
      if ((sw & cpu.SWMASK('K')) != 0)
        mode = cpu.MD_KER
      else if ((sw & cpu.SWMASK('S')) != 0)
        mode = cpu.MD_SUP
      else if ((sw & cpu.SWMASK('U')) != 0)
        mode = cpu.MD_USR
      else if ((sw & cpu.SWMASK('P')) != 0)
        mode = (cpu.PSW >> PDP11.PSW_V_PM) & 0x3
      else mode = (cpu.PSW >> PDP11.PSW_V_CM) & 0x3
      va2 = if ((va | (sw & cpu.SWMASK('T'))) != 0)
        va | UInt(calc_ds(mode))
      else va | UInt(calc_is(mode))

      val apridx = (va2 >> VA_V_APF) & 0x3f
      /* index into APR */
      val apr = APRFILE(apridx)
      /* with va<18:13> */
      val dbn = va2 & VA_BN
      /* extr block num */
      val plf = (apr & PDR_PLF) >> 2 /* extr page length */
      if ((apr & PDR_PRD) == 0) /* not readable? */
        return PDP11.MAXMEMSIZE
      if (if ((apr & PDR_ED) != 0) dbn < plf else dbn > plf)
        return PDP11.MAXMEMSIZE
      pa = UInt(((va2 & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK)
      if ((MMR3.get16 & MMR3_M22E) == 0) {
        pa = pa & UInt(0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000 | pa)
      }
    }
    else {
      pa = va2 & UInt(0xffff) /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000 | pa)
    }
    pa
  }

  /* Memory management registers
     MMR0 17777572        read/write, certain bits unimplemented or read only
     MMR1 17777574        read only
     MMR2 17777576        read only
     MMR3 17772516        read/write, certain bits unimplemented
  */

  // Solve data return
  def MMR012_rd(pa: UInt, access: UInt): Option[UInt] = {
    (pa >> 1) & 3 match {
      /* decode pa<2:1> */

      case 0 => /* SR */
        None

      case 1 => /* MMR0 */
        //data(MMR0 & cpu_tab[cpu_model].mm0)
        Some(MMR0.get16 & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm0)

      case 2 =>
        /* MMR1 */
        Some(MMR1.get16)

      case 3 => /* MMR2 */
        Some(MMR2.get16)
    } /* end switch pa */
  }

  // data return instead of pointer mod
  def MMR012_wr(data: UInt, pa: UInt, access: Int): UInt = {
    var dataret = data
    ((pa >> 1) & 3: @switch) match {
      /* decode pa<2:1> */
      case 0 => /* DR */
      case 1 => /* MMR0 */
        if (access == WRITEB)
          dataret = if ((pa & 1) != 0) UInt((MMR0.get16 & 0xff) | (data.intValue << 8))
          else UInt((MMR0 & ~0xff) | data)
        dataret = UInt(dataret.intValue & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm0)
        MMR0.set16((MMR0 & ~MMR0_WR) | (data & MMR0_WR))
      case _ => /* MMR1, MMR2 */
    }
    /* end switch pa */
    dataret
  }

  def MMR3_rd(): UInt = /* MMR3 */ {
    MMR3.get16 & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm3
  }

  def MMR3_wr(data: UInt, pa: UInt, access: UInt): Unit = {
    if ((pa & 1) == 0) {
      MMR3.set16(data.intValue & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm3)
      cpu.cpu_bme = if ((MMR3.get16 & MMR3_BME.intValue) != 0 && ((cpu.cpu_opt.intValue & CPUOPT.OPT_UBM.intValue) != 0)) true else false
      dsenable = calc_ds(cpu.cm)
    }
  }

  /* PARs and PDRs.  These are grouped in I/O space as follows:
          17772200 - 17772276     supervisor block
          17772300 - 17772376     kernel block
          17777600 - 17777676     user block
     Within each block, the subblocks are I PDR's, D PDR's, I PAR's, D PAR's
     Thus, the algorithm for converting between I/O space addresses and
     APRFILE indices is as follows:
          idx<3:0> =      dspace'page     =       pa<4:1>
          par     =       PDR vs PAR      =       pa<5>
          idx<5:4> =      ker/sup/user    =       pa<8>'~pa<6>
     Note: the A,W bits are read only; they are cleared by any write to an APR
  */

  def APR_rd(pa: UInt, access: UInt): UInt = {
    var idx = (pa >> 1) & 0xf
    /* dspace'page */
    val left = (pa >> 5) & 1 /* PDR vs PAR */
    if ((pa & 0x40) == 0) /* 1 for super, user */
      idx = idx | 0x10
    if ((pa & 0x100) != 0) /* 1 for user only */
      idx = idx | 0x20
    if (left != 0)
      (APRFILE(idx) >> 16) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par
    else APRFILE(idx) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr
  }

  // return value rather than pointer
  def APR_wr(data: UInt, pa: UInt, access: Int): UInt = {
    var dataret = data

    var idx = (pa >> 1) & 0xf
    /* dspace'page */
    val left = (pa >> 5) & 1 /* PDR vs PAR */
    if ((pa & 0x40) == 0) /* 1 for super, user */
      idx = idx | 0x10
    if ((pa & 0x100) != 0) /* 1 for user only */
      idx = idx | 0x20
    val curr = {
      if (left != 0)
        (APRFILE(idx) >> 16) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par
      else APRFILE(idx) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr
    }
    if (access == WRITEB)
      dataret = if ((pa & 1) != 0) UInt((curr & 0xff) | (data << 8)) else UInt((curr & ~0xff) | data)
    APRFILE(idx) = {
      if (left != 0)
        UInt(((APRFILE(idx) & 0xffff) | ((data & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par) << 16)) & ~(PDR_A | PDR_W))
      else UInt(((APRFILE(idx) & ~0xffff) | (data & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr)) & ~(PDR_A | PDR_W))
    }
    dataret
  }
}
