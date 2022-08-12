package com.sim.pdp11

import com.sim.device.{Bootable, MappedDeviceAction, MappedDeviceAction16, MemoryMappedDevice, PortMappedDiskDevice, SupportsOptions}
import com.sim.machine.AbstractMachine
import com.sim.s100.S100Machine
import com.sim.unsigned.{UInt, UShort, uint2int}

import java.util.Random

/*
  The RK11 is an eight drive cartridge disk subsystem.  An RK05 drive
   consists of 203 cylinders, each with 2 surfaces containing 12 sectors
   of 512 bytes.
   The most complicated part of the RK11 controller is the concept of
   interrupt "polling".  While only one read or write can occur at a
   time, the controller supports multiple seeks.  When a seek completes,
   if done is set the drive attempts to interrupt.  If an interrupt is
   already pending, the interrupt is "queued" until it can be processed.
   When an interrupt occurs, RKDS<15:13> is loaded with the number of the
   interrupting drive.
   To implement this structure, and to assure that read/write interrupts
   take priority over seek interrupts, the controller contains an
   interrupt queue, rkintq, with a bit for a controller interrupt and
   then one for each drive.  In addition, the drive number of the last
   non-seeking drive is recorded in last_drv.

 */
class RK11D(machine: AbstractMachine, lowAddress: UInt, highAddress: UInt) extends MemoryMappedDevice(machine, lowAddress, highAddress)
  with SupportsOptions with Bootable {
  override def optionChanged(sb: StringBuilder): Unit = ???

  override val description: String = "RK11 eight drive cartridge disk subsystem"
  override val name = "RK"
  override val supportsBoot: Boolean = true

  override def init(): Unit = {
    // map out actions for the addresses
    addAction(new RKDS(RK11D.RKDS_ADDRESS, this))
    addAction(new RKER(RK11D.RKER_ADDRESS, this))
    addAction(new RKCS(RK11D.RKCS_ADDRESS, this))
    addAction(new RKWC(RK11D.RKWC_ADDRESS, this))
    addAction(new RKBA(RK11D.RKBA_ADDRESS, this))
    addAction(new RKDA(RK11D.RKDA_ADDRESS, this))
    addAction(new RKMR(RK11D.RKMR_ADDRESS, this))
    addAction(new RKDB(RK11D.RKDB_ADDRESS, this))
  }

  def get8(address: Int): com.sim.unsigned.UByte = ???

  def load8(address: Int, value: com.sim.unsigned.UByte): Unit = ???

  def put8(address: Int, value: com.sim.unsigned.UByte): Unit = ???

  override def put16(address: Int, value: UShort): Unit = ???

  override def get16(address: Int): UShort = ???

  override def createUnitOptions(): Unit = ???

  var rkcs = 0 // control/status
  var rkds = 0 //drive status
  var rkba = 0 // memory address
  var rkda = 0 // disk address
  var rker = 0 // error status
  var rkwc = 0 // word count
  var rkintq = 0 // interrupt queue
  var last_drv = 0 //last r/w drive
  var rk_stopioe = 1 // stop on error
  var rk_swait = 10 //seek time
  var rk_rwait = 10 // rotate time

  /* I/O dispatch routine, I/O addresses 17777400 - 17777416
     17777400     RKDS    read only, constructed from "id'd drive"
                          plus current drive status flags
     17777402     RKER    read only, set as operations progress,
                          cleared by INIT or CONTROL RESET
     17777404     RKCS    read/write
     17777406     RKWC    read/write
     17777410     RKBA    read/write
     17777412     RKDA    read/write
     17777414     RKMR    read/write, unimplemented
     17777416     RKDB    read only, unimplemented
  */


  private val RKDA_V_DRIVE = 13 // drive
  private val RKDA_M_DRIVE = 07
  private val RKDA_V_SECT = 0 // sector
  private val RKDA_M_SECT = 0x0f
  private val RKDA_V_TRACK = 4 // track
  private val RKDA_M_TRACK = 0x1ff

  private val RKDA_V_CYL = 5 // cylinder
  private val RKDA_M_CYL = 0xff

  private val RKDA_DRIVE = (RKDA_M_DRIVE << RKDA_V_DRIVE)

  inline def GET_DRIVE(x: Int): RK11DUnit = {
    findUnitByNumber((x >> RKDA_V_DRIVE) & RKDA_M_DRIVE).orNull.asInstanceOf[RK11DUnit]
  }

  // TODO
  def rk_go(): Unit = {

  }
}

object RKDS {
  val rand = new Random()
}

class RKDS(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (isWrite) 0 // READ ONLY
    else {

      /* RKDS: read only */
      device.rkds = device.rkds & RK11D.RKDS_ID
      /* identified unit */
      val unit: RK11DUnit = device.GET_DRIVE(device.rkda)
      /* selected unit */
      if (unit.isEnabled) {
        /* not disabled? */
        device.rkds = device.rkds | RK11D.RKDS_RK05 | RK11D.RKDS_SC_OK | /* random sector */
          (RKDS.rand.nextInt() % RK11D.RK_NUMSC)
        if (unit.isAttached) /* attached? */
          device.rkds = device.rkds | RK11D.RKDS_RDY
        if (unit.isActive) /* idle? */
          device.rkds = device.rkds | RK11D.RKDS_RWS
        if (unit.isLocked) /* write locked? */
          device.rkds = device.rkds | RK11D.RKDS_WLK
        if (RK11D.GET_SECT(device.rkda) == (device.rkds & RK11D.RKDS_SC))
          device.rkds = device.rkds | RK11D.RKDS_ON_SC
      }
      device.rkds
    }
  }

}

class RKER(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (!isWrite) device.rker & RK11D.RKER_IMP else 0
  }
}

class RKCS(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (isWrite) {
      var data = value
      var cpu: PDP11 = device.machine.getCPU.asInstanceOf[PDP11]
      device.rkcs = device.rkcs & RK11D.RKCS_REAL
      if (isByte) data = {
        if (oddAddress) (device.rkcs & 0xFF) | (data << 8) else (device.rkcs & ~0xFF) | data
      }
      if ((data & PDP11.CSR_IE) == 0) {
        /* int disable? */
        device.rkintq = 0 /* clr int queue */
        //sim_debug(RKDEB_INT, & rk_dev, "rk_wr(CLR_INT)\n");
        cpu.CLR_INT(PDP11.IPL_RK, PDP11.INT_RK) /* clr int request */
      }
      else if ((device.rkcs & (PDP11.CSR_DONE + PDP11.CSR_IE)) == PDP11.CSR_DONE) {
        device.rkintq = device.rkintq | RK11D.RK_CTLI
        /* queue ctrl int */
        //sim_debug(RKDEB_INT, &rk_dev, "rk_wr(SET_INT)\n");
        cpu.SET_INT(PDP11.IPL_RK, PDP11.INT_RK) // set int request
      }
      device.rkcs = (device.rkcs & ~RK11D.RKCS_RW) | (data & RK11D.RKCS_RW)
      if (((device.rkcs & PDP11.CSR_DONE) != 0) && ((data & PDP11.CSR_GO) != 0)) device.rk_go()
      0
    } else {
      device.rkcs = device.rkcs & RK11D.RKCS_REAL
      if (device.rker != 0) /* update err flags */
        device.rkcs = device.rkcs | RK11D.RKCS_ERR
      if ((device.rker & RK11D.RKER_HARD) != 0)
        device.rkcs = device.rkcs | RK11D.RKCS_HERR
      device.rkcs
    }
  }
}

class RKWC(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (isWrite) {
      var data = value
      if (isByte) data = {
        if (oddAddress) (device.rkwc & 0xFF) | (data << 8) else (device.rkwc & ~0xFF) | data
      }
      device.rkwc = data
      0
    } else device.rkwc
  }
}

class RKBA(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    var data = value
    if (isWrite) {
      if (isByte) data = {
        if (oddAddress) (device.rkba & 0xFF) | (data << 8) else (device.rkba & ~0xFF) | data
      }
      device.rkba = data & RK11D.RKBA_IMP
      0
    } else device.rkba & RK11D.RKBA_IMP
  }
}

class RKDA(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    var data = value
    if (isWrite) {
      if ((device.rkcs & PDP11.CSR_DONE) == 0)
        return 0
      if (isByte) data = {
        if (oddAddress) (device.rkda & 0xFF) | (data << 8) else (device.rkda & ~0xFF) | data
      }
      device.rkda = data
      0
    } else device.rkda
  }
}

class RKMR(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (!isWrite) device.rker & RK11D.RKER_IMP else 0
  }
}

class RKDB(address: Int, device: RK11D) extends MappedDeviceAction16(address, device) {
  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address, isByte, value, isWrite)

    if (!isWrite) device.rker & RK11D.RKER_IMP else 0
  }
}

object RK11D {
  val RK_NUMWD = 256 // words/sector
  val RK_NUMSC = 12 // sectors/surface
  val RK_NUMSF = 2 // surfaces/cylinder
  val RK_NUMCY = 203 // cylinders/drive
  val RK_NUMTR: Int = RK_NUMCY * RK_NUMSF // tracks/drive
  val RK_NUMDR = 8 // drives/controller
  val RK_M_NUMDR = 0x7
  val RK_SIZE: Int = RK_NUMCY * RK_NUMSF * RK_NUMSC * RK_NUMWD // words/drive
  val RK_CTLI = 1 // controller int

  def RK_SCPI(x: UInt): Int = 2 << x // drive int

  val RK_MAXFR: Int = 1 << 16 // max transfer

  val BOOT_START = 0x400 // start
  val BOOT_ENTRY: Int = BOOT_START + 2 // entry
  val BOOT_UNIT: Int = BOOT_START + 0x8 // unit number
  val BOOT_CSR: Int = BOOT_START + 0x1a // CSR

  val RKDS_ADDRESS: Int = 0x3fff00 // 17777400
  val RKER_ADDRESS: Int = 0x3fff02 //17777402
  val RKCS_ADDRESS: Int = 0x3fff04 //17777404 RKCS read / write
  val RKWC_ADDRESS: Int = 0x3fff06 //17777406 RKWC read / write
  val RKBA_ADDRESS: Int = 0x3fff08 //17777410 RKBA read / write
  val RKDA_ADDRESS: Int = 0x3fff0a //17777412 RKDA read / write
  val RKMR_ADDRESS: Int = 0x3fff0c // 17777414 RKMR read / write, unimplemented
  val RKDB_ADDRESS: Int = 0x3fff0e // 17777416 RKDB read only, unimplemented

  val boot_rom: Array[Int] = Array(
    0x444b, // "KD"
    0x15c6, BOOT_START, // MOV #boot_start, SP
    0x15c0, 0x0, // MOV #unit, R0        ; unit number
    0x1003, // MOV R0, R3
    0xc3, // SWAB R3
    0xcc3, // ASL R3
    0xcc3, // ASL R3
    0xcc3, // ASL R3
    0xcc3, // ASL R3
    0xcc3, // ASL R3
    0x15c1, 0xff0a, // MOV #RKDA, R1        ; csr
    0x10c9, // MOV R3, (R1)         ; load da
    0xa21, // CLR -(R1)            ; clear ba
    0x15e1, 0xfe00, // MOV #-256.*2, -(R1)  ; load wc
    0x15e1, 0x5, // MOV #READ+GO, -(R1)  ; read & go
    0xa02, // CLR R2
    0xa03, // CLR R3
    0x15c4, BOOT_START + 0x10, // MOV #START+20, R4
    0xa05, // CLR R5
    0x8bc9, // TSTB (R1)
    0x80fe, // BPL .-2
    0x8a09, // CLRB (R1)
    0xa07 // CLR PC
  )
  val BOOT_LEN: Int = boot_rom.length

  // RKDS Bits
  val RKDS_SC = 0x0f // sector counter */
  val RKDS_ON_SC = 0x10 // on sector */
  val RKDS_WLK = 0x20 // write locked */
  val RKDS_RWS = 0x40 // rd/wr/seek ready */
  val RKDS_RDY = 0x80 // drive ready */
  val RKDS_SC_OK = 0x100 // SC valid */
  val RKDS_INC = 0x200 // seek incomplete */
  val RKDS_UNSAFE = 0x400 // unsafe */
  val RKDS_RK05 = 0x800 // RK05 */
  val RKDS_PWR = 0x1000 // power low */
  val RKDS_ID = 0xe000 // drive ID */
  val RKDS_V_ID = 13

  // RKRE Bits
  val RKER_WCE = 1 // write check
  val RKER_CSE = 2 // checksum
  val RKER_NXS = 0x20 // nx sector
  val RKER_NXC = 0x40 // nx cylinder
  val RKER_NXD = 0x80 // nx drive
  val RKER_TE = 0x100 // timing error
  val RKER_DLT = 0x200 // data late
  val RKER_NXM = 0x400 // nx memory
  val RKER_PGE = 0x800 // programming error
  val RKER_SKE = 0x1000 // seek error
  val RKER_WLK = 0x2000 // write lock
  val RKER_OVR = 0x4000 // overrun
  val RKER_DRE = 0x8000 // drive error
  val RKER_IMP = 0xffe3 // implemented
  val RKER_SOFT = (RKER_WCE + RKER_CSE) // soft errors
  val RKER_HARD = 0xffe0 // hard errors

  // RKCS Bits
  val RKCS_M_FUNC = 7 // function
  val RKCS_CTLRESET = 0
  val RKCS_WRITE = 1
  val RKCS_READ = 2
  val RKCS_WCHK = 3
  val RKCS_SEEK = 4
  val RKCS_RCHK = 5
  val RKCS_DRVRESET = 6
  val RKCS_WLK = 7
  val RKCS_V_FUNC = 1
  val RKCS_MEX = 0x30 // memory extension
  val RKCS_V_MEX = 4
  val RKCS_SSE = 0x100 // stop on soft err
  val RKCS_FMT = 0x400 // format
  val RKCS_INH = 0x800 // inhibit increment
  val RKCS_SCP = 0x2000 // search complete
  val RKCS_HERR = 0x4000 // hard error
  val RKCS_ERR = 0x8000 // error
  val RKCS_REAL = 0x2dfe // kept here
  val RKCS_RW = 0xd7e // read/write

  def GET_FUNC(x: Int): Int = {
    (((x) >> RKCS_V_FUNC) & RKCS_M_FUNC)
  }

  // RKDA Bits 
  val RKDA_V_SECT = 0 // sector
  val RKDA_M_SECT = 0x0f
  val RKDA_V_TRACK = 4 // track
  val RKDA_M_TRACK = 0x1ff
  val RKDA_V_CYL = 5 // cylinder
  val RKDA_M_CYL = 0xff
  val RKDA_V_DRIVE = 13 // drive
  val RKDA_M_DRIVE = 07

  // RKBA Bits
  val RKBA_IMP = 0xfffe

  def GET_SECT(x: Int) = {
    (((x) >> RKDA_V_SECT) & RKDA_M_SECT)
  }

  def GET_CYL(x: Int) = {
    (((x) >> RKDA_V_CYL) & RKDA_M_CYL)
  }

  def GET_TRACK(x: Int) = {
    (((x) >> RKDA_V_TRACK) & RKDA_M_TRACK)
  }

  def GET_DRIVE(x: Int) = {
    (((x) >> RKDA_V_DRIVE) & RKDA_M_DRIVE)
  }

  def GET_DA(x: Int) = {
    ((GET_TRACK(x) * RK_NUMSC) + GET_SECT(x))
  }

}