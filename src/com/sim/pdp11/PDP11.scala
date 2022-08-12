package com.sim.pdp11

import com.sim.SimTimer
import com.sim.cpu.{BasicCPU, Register, Register16}
import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.*
import com.sim.unsigned.ushort2uint
import com.sim.unsigned.ushort2Int
import com.sim.unsigned.uint2int

import scala.collection.mutable
import scala.language.implicitConversions

abstract class PDP11(isBanked: Boolean = false, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "PDP11"
  override val MMU: PDP11MMU = new PDP11MMU(this)
  override val description: String = "PDP11 CPU"

  override def createUnitOptions(): Unit = {
    // Set up CPU common options.
    super.createUnitOptions()

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Break on HALT instruction.", value = false))
    unitOptions.append(ValueUnitOption("MEMORY", "Set the RAM size.", value = 0xFFFF))

  }

  override def showCommand(stringBuilder: mutable.StringBuilder): Unit = {
    super.showCommand(stringBuilder)
  }

  // PSW
  var PSW: UInt = UInt(0)
  var cm: Int = 0
  /*   current mode */
  var pm: Int = 0
  /*   previous mode */
  var rs: Int = 0
  /*   register set */
  var fpd: Int = 0
  /*   first part done */
  var ipl: Int = 0
  /*   int pri level */
  var tbit: Int = 0
  /*   trace flag */
  /*   condition codes */
  var N: Boolean = false
  var Z: Boolean = false
  var V: Boolean = false
  var C: Boolean = false

  // orig from cputab
  val CPU_MOD_MFPT: Int

  override def runcpu(singleStep: Boolean): Unit = {

    var IR = UInt(0)
    var srcspec: Int = 0
    var srcreg = true
    var dstspec: Int = 0
    var dstreg = true
    var src = UInt(0)
    var src2 = UInt(0)
    var dst = UInt(0)
    var ea = UInt(0)
    var i = 0
    var t = 0
    var sign: Int = 0
    var oldrs = 0

    // TODO Bunch of housekeeping

    IR = MMU.ReadE(PC.get16 | UInt(MMU.isenable)) /* fetch instruction */
    SimTimer.sim_interval = SimTimer.sim_interval - 1
    srcspec = (IR >> 6) & 0x3f /* src, dst specs */
    dstspec = IR & 0x3f
    srcreg = srcspec <= 0x7 /* src, dst = rmode? */
    dstreg = dstspec <= 0x7
    PC(PC + 2 & 0xffff) /* incr PC, mod 65k */

    // Start of decode ***********************************************************************************
    /* decode IR<15:12> */
    (IR >> 12) & 0xf match {

      /* Opcode 0: no operands, specials, branches, JSR, SOPs */
      case 0 =>
        /* decode IR<11:6> */
        (IR >> 6) & 0x3f match {
          case 0 =>
            /* no operand */
            if (IR.intValue >= 0x8) {
              /* 000010 - 000077 */
              setTRAP(PDP11.TRAP_ILL.intValue) /* illegal */
              // break
            }
            else IR.intValue match {
              case 0 => /* HALT */
              // TODO if ((cm == MD_KER) && (!CPUT(CPUOPT.CPUT_J) || ((MAINT & MAINT_HTRAP) == 0)))
              //  reason = STOP_HALT
              //else if (CPUT(CPUOPT.HAS_HALT4)) {
              /* priv trap? */
              //  setTRAP(PDP11.TRAP_PRV.intValue)
              //  setCPUERR(PDP11.CPUE_HALT)
              //}
              //else setTRAP(PDP11.TRAP_ILL.intValue) /* no, ill inst */

              case 1 => /* WAIT */
                wait_state = 1

              case 3 => /* BPT */
                setTRAP(PDP11.TRAP_BPT.intValue)

              case 4 => /* IOT */
                setTRAP(PDP11.TRAP_IOT.intValue)

              case 5 => /* RESET */
                if (cm == MD_KER) {
                  // TODO reset_all(2) /* skip CPU, sys reg */
                  PIRQ(0) /* clear PIRQ */
                  STKLIM(0) /* clear STKLIM */
                  MMU.MMR0(0) /* clear MMR0 */
                  MMU.MMR3(0) /* clear MMR3 */
                  cpu_bme = false /* (also clear bme) */
                  for (i <- 0 to PDP11.IPL_HLVL) int_req(i) = 0
                  trap_req = trap_req & ~PDP11.TRAP_INT
                  MMU.dsenable = MMU.calc_ds(cm)
                }

              case 6 => /* RTT */
                if (!CPUT(CPUOPT.HAS_RTT)) {
                  setTRAP(PDP11.TRAP_ILL.intValue)

                }
              case 2 => /* RTI */
                src = MMU.ReadW(UInt(SP.get16 | MMU.dsenable))
                src2 = MMU.ReadW(UInt(((SP.get16 + 2) & 0xffff) | MMU.dsenable))
                STACKFILE(cm)(
                  {
                    SP((SP + 4) & 0xffff)
                    SP.get16
                  }
                )
                oldrs = rs
                put_PSW(src2.intValue, cm != MD_KER) /* store PSW, prot */
                if (rs != oldrs) {
                  for (i <- 0 to 6) {
                    REGFILE(i)(oldrs) = R(i)
                    R(i) = REGFILE(i)(rs)
                  }
                }
                SP(STACKFILE(cm))
                MMU.isenable = MMU.calc_is(cm)
                MMU.dsenable = MMU.calc_ds(cm)
                trap_req = MMU.calc_ints(ipl, trap_req)
                JMP_PC(src)
              // TODO if (CPUT(CPUOPT.HAS_RTT) && tbit && /* RTT impl? */
              //   (IR == 0x2))
              //   setTRAP(PDP11.TRAP_TRC.intValue) /* RTI immed trap */

              case 7 => /* MFPT */
                if (CPUT(CPUOPT.HAS_MFPT)) /* implemented? */
                  R(0)(CPU_MOD_MFPT) /* get type */
                else setTRAP(PDP11.TRAP_ILL.intValue)

            } /* end switch no ops */
          /* end case no ops */

          case 1 => /* JMP */
            if (dstreg) {
              setTRAP(if (CPUT(CPUOPT.HAS_JREG4)) PDP11.TRAP_PRV.intValue else PDP11.TRAP_ILL.intValue)
            } else {
              dst = UInt(MMU.GeteaW(dstspec) & 0xffff) /* get eff addr */
              if (CPUT(CPUOPT.CPUT_05 | CPUOPT.CPUT_20) && /* 11/05, 11/20 */
                ((dstspec & 0x38) == 0x10)) /* JMP (R)+? */
                dst = R(dstspec & 0x7).get16 /* use post incr */
              JMP_PC(dst)
            }
          /* end JMP */

          case 2 => /* RTS et al*/
            if (IR.intValue < 0x88) {
              /* RTS */
              dstspec = dstspec & 0x7
              JMP_PC(R(dstspec))
              R(dstspec)(MMU.ReadW(UInt(SP | MMU.dsenable)).intValue)
              if (dstspec != 6) SP((SP + 2) & 0xffff)

            } else {
              /* end if RTS */
              if (IR.intValue < 0x98) {
                setTRAP(PDP11.TRAP_ILL.intValue)

              } else {
                if (IR.intValue < 0xa0) {
                  /* SPL */
                  if (CPUT(CPUOPT.HAS_SPL)) {
                    if (cm == MD_KER)
                      ipl = IR & 0x7
                    trap_req = MMU.calc_ints(ipl, trap_req)
                  }
                  else setTRAP(PDP11.TRAP_ILL.intValue)

                } else {
                  /* end if SPL */
                  if (IR.intValue < 0xb0) {
                    /* clear CC */
                    if ((IR & 0x8) != 0) N = false
                    if ((IR & 0x4) != 0) Z = false
                    if ((IR & 2) != 0) V = false
                    if ((IR & 1) != 0) C = false

                  } else {
                    /* end if clear CCs */
                    /* set CC */
                    if ((IR & 0x8) != 0) N = true
                    if ((IR & 4) != 0) Z = true
                    if ((IR & 2) != 0) V = true
                    if ((IR & 1) != 0) C = true
                    /* end case RTS et al */
                  }
                }
              }
            }

          case 3 => /* SWAB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt(((dst & 0xff) << 8) | ((dst >> 8) & 0xff))
            // TODO N = GET_SIGN_B(dst & 0xff)
            Z = GET_Z(UInt(dst & 0xff))
            if (!CPUT(CPUOPT.CPUT_20)) V = false
            C = false
            if (dstreg) R(dstspec)(dst.intValue)
            else MMU.PWriteW(dst, MMU.last_pa)
          /* end SWAB */

          case 4 | 5 => /* BR */
            BRANCH_F(IR)

          case 0x6 | 0x7 => /* BR */
            BRANCH_B(IR)

          case 0x8 | 0x9 => /* BNE */
            if (!Z) BRANCH_F(IR)

          case 0xa | 0xb => /* BNE */
            if (!Z) BRANCH_B(IR)

          case 0xc | 0xd => /* BEQ */
            if (Z) BRANCH_F(IR)

          case 0xe | 0xf => /* BEQ */
            if (Z) BRANCH_B(IR)

          case 0x10 | 0x11 => /* BGE */
            if (!(N ^ V)) BRANCH_F(IR)

          case 0x12 | 0x13 => /* BGE */
            if (!(N ^ V)) BRANCH_B(IR)

          case 0x14 | 0x15 => /* BLT */
            if (N ^ V) BRANCH_F(IR)

          case 0x16 | 0x17 => /* BLT */
            if (N ^ V) BRANCH_B(IR)

          case 0x18 | 0x19 => /* BGT */
            if (!(Z | (N ^ V))) BRANCH_F(IR)

          case 0x1a | 0x1b => /* BGT */
            if (!(Z | (N ^ V))) BRANCH_B(IR)

          case 0x1c | 0x1d => /* BLE */
            if (Z | (N ^ V)) BRANCH_F(IR)

          case 0x1e | 0x1f => /* BLE */
            if (Z | (N ^ V)) BRANCH_B(IR)

          case 0x20 | 0x21 | 0x22 | 0x23 | /* JSR */
               0x24 | 0x25 | 0x26 | 0x27 =>
            if (dstreg) {
              setTRAP(if (CPUT(CPUOPT.HAS_JREG4)) PDP11.TRAP_PRV.intValue else PDP11.TRAP_ILL.intValue)
            } else {
              srcspec = srcspec & 0x7
              dst = MMU.GeteaW(dstspec)
              if (CPUT(CPUOPT.CPUT_05 | CPUOPT.CPUT_20) && /* 11/05, 11/20 */
                ((dstspec & 0x38) == 0x10)) /* JSR (R)+? */
                dst = R(dstspec & 0x7).get16 /* use post incr */
              SP((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6)
              if (MMU.update_MM) MMU.MMR1(MMU.reg_mods)
              MMU.WriteW(R(srcspec).get16, UInt(SP | MMU.dsenable))
              if ((cm == MD_KER) && (SP.get16 < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP.get16)
              R(srcspec)(PC)
              JMP_PC(UInt(dst & 0xffff))
            }
          /* end JSR */

          case 0x28 => /* CLR */
            N = false
            V = false
            C = false
            Z = true
            if (dstreg) R(dstspec)(0) else MMU.WriteW(UInt(0), MMU.GeteaW(dstspec))

          case 0x29 => /* COM */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt(dst ^ 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = false
            C = true
            if (dstreg) R(dstspec) else MMU.PWriteW(dst, MMU.last_pa)

          case 0x2a => /* INC */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt((dst + 1) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = if (dst == 0x8000) true else false
            if (dstreg) R(dstspec)(dst.intValue)
            else MMU.PWriteW(dst, MMU.last_pa)

          case 0x2b => /* DEC */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt((dst - 1) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = if (dst == 0x7fff) true else false
            if (dstreg) {
              R(dstspec)(dst.intValue)
            } else {
              MMU.PWriteW(dst, MMU.last_pa)
            }

          case 0x2c => /* NEG */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt((-dst) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = if (dst == 0x8000) true else false
            C = !Z
            if (dstreg)
              R(dstspec)(dst.intValue)
            else MMU.PWriteW(dst, MMU.last_pa)

          case 0x2d => /* ADC */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt(dst + (if (C) 1 else 0) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = C && (if (dst == 0x8000) true else false)
            C = C & Z
            if (dstreg) R(dstspec)(dst.intValue)
            else MMU.PWriteW(dst, MMU.last_pa)

          case 0x2e => /* SBC */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt(dst - (if (C) 1 else 0) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = C && (dst == 0x7fff)
            C = C && (dst == 0xffff)
            if (dstreg) R(dstspec)(dst.intValue)
            else MMU.PWriteW(dst, MMU.last_pa)

          case 0x2f => /* TST */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            V = false
            C = false

          case 0x30 => /* ROR */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            // TODO CHeck logic
            // dst = (src >> 1) | (C << 15)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            C = CHECK_C(src & 1)
            V = N ^ C
            if (dstreg) R(dstspec)(dst.intValue) else MMU.PWriteW(dst, MMU.last_pa)

          case 0x31 => /* ROL */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt(((src << 1) | {
              if (C) 1 else 0
            }) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            C = CHECK_C(GET_SIGN_W(src))
            V = N ^ C
            if (dstreg) R(dstspec)(dst.intValue) else MMU.PWriteW(dst, MMU.last_pa)
          //break

          case 0x32 => /* ASR */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt((src >> 1) | (src & 0x8000))
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            C = CHECK_C(src & 1)
            V = N ^ C
            if (dstreg) R(dstspec)(dst.intValue) else MMU.PWriteW(dst, MMU.last_pa)

          case 0x33 => /* ASL */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = UInt((src << 1) & 0xffff)
            N = CHECK_C(GET_SIGN_W(dst))
            Z = GET_Z(dst)
            C = CHECK_C(GET_SIGN_W(src))
            V = N ^ C
            if (dstreg) R(dstspec)(dst.intValue) else MMU.PWriteW(dst, MMU.last_pa)

          /* Notes:
         - MxPI must mask GeteaW returned address to force ispace
         - MxPI must set MMR1 for SP recovery in case of fault
      */

          case 0x34 => /* MARK */
            if (CPUT(CPUOPT.HAS_MARK)) {
              i = (PC + dstspec + dstspec) & 0xffff
              JMP_PC(R(5))
              R(5)(MMU.ReadW(UInt(i | MMU.dsenable)).intValue)
              SP((i + 2) & 0xffff)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x35 => /* MFPI */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm)) dst = STACKFILE(pm).get16
                else dst = R(dstspec).get16
              }
              else {
                i = if ((cm == pm) && (cm == MD_USR)) MMU.calc_ds(pm) else MMU.calc_is(pm)
                dst = MMU.ReadW(UInt((MMU.GeteaW(dstspec) & 0xffff) | i))
              }
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              SP((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6)
              if (MMU.update_MM) MMU.MMR1(MMU.reg_mods)
              MMU.WriteW(dst, UInt(SP.get16 | MMU.dsenable))
              if ((cm == MD_KER) && (SP.get16 < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP.get16)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x36 => /* MTPI */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              dst = MMU.ReadW(UInt(SP | MMU.dsenable))
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              SP((SP + 2) & 0xffff)
              MMU.reg_mods = 0x16
              if (MMU.update_MM) MMU.MMR1(MMU.reg_mods)
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  STACKFILE(pm)(dst.intValue)
                else R(dstspec)(dst.intValue)
              }
              else MMU.WriteW(dst, UInt((MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_is(pm)))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x37 => /* SXT */
            if (CPUT(CPUOPT.HAS_SXS)) {
              dst = if (N) UInt(0xffff) else UInt(0)
              Z = N
              V = false
              if (dstreg)
                R(dstspec)(dst.intValue)
              else MMU.WriteW(dst, MMU.GeteaW(dstspec))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x38 => /* CSM */
            if (CPUT(CPUOPT.HAS_CSM) && ((MMU.MMR3.get16 & MMU.MMR3_CSM)
              != 0) && (cm != MD_KER)) {
              dst = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
              PSW = UInt(get_PSW & ~PDP11.PSW_CC) /* PSW, cc = 0 */
              STACKFILE(cm)(SP)
              MMU.WriteW(PSW, UInt(((SP.get16 - 2) & 0xffff) | MMU.calc_ds(MD_SUP)))
              MMU.WriteW(PC.get16, UInt(((SP.get16 - 4) & 0xffff) | MMU.calc_ds(MD_SUP)))
              MMU.WriteW(dst, UInt(((SP.get16 - 6) & 0xffff) | MMU.calc_ds(MD_SUP)))
              SP((SP.get16 - 6) & 0xffff)
              pm = cm
              cm = MD_SUP
              tbit = 0
              MMU.isenable = MMU.calc_is(cm)
              MMU.dsenable = MMU.calc_ds(cm)
              PC(MMU.ReadW(UInt(0x8 | MMU.isenable)).intValue)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)


          case 0x3a => /* TSTSET */
            if (CPUT(CPUOPT.HAS_TSWLK) && !dstreg) {
              dst = MMU.ReadMW(MMU.GeteaW(dstspec))
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              C = CHECK_C(dst & 1)
              R(0)(dst.intValue) /* R[0] <- dst */
              MMU.PWriteW(UInt(R(0) | 1), MMU.last_pa) /* dst <- R[0] | 1 */
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x3b => /* WRTLCK */
            if (CPUT(CPUOPT.HAS_TSWLK) && !dstreg) {
              N = CHECK_C(GET_SIGN_W(R(0).get16))
              Z = GET_Z(R(0).get16)
              V = false
              MMU.WriteW(R(0).get16, MMU.GeteaW(dstspec))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case _ =>
            setTRAP(PDP11.TRAP_ILL.intValue)

        } /* end switch SOPs */
      /* end case 000 */

      /* Opcodes 01 - 06: double operand word instructions
       J-11 (and F-11) optimize away register source operand decoding.
       As a result, dop R,+/-(R) use the modified version of R as source.
       Most (but not all) other PDP-11's fetch the source operand before
       any destination operand decoding.
       Add: v = [sign (src) = sign (src2)] and [sign (src) != sign (result)]
       Cmp: v = [sign (src) != sign (src2)] and [sign (src2) = sign (result)]
    */

      case 1 => /* MOV */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          ea = MMU.GeteaW(dstspec)
          dst = R(srcspec).get16
        } else {
          dst = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          if (!dstreg)
            ea = MMU.GeteaW(dstspec)
        }
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = false
        if (dstreg) R(dstspec)(dst.intValue)
        else MMU.WriteW(dst, ea)

      case 2 => /* CMP */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
        }
        dst = UInt((src - src2) & 0xffff)
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = CHECK_C(GET_SIGN_W((src ^ src2) & (~src2 ^ dst)))
        C = src < src2

      case 3 => /* BIT */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
        }
        dst = src2 & src
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = false

      case 4 => /* BIC */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = src2 & ~src
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = false
        if (dstreg)
          R(dstspec)(dst.intValue)
        else MMU.PWriteW(dst, MMU.last_pa)

      case 5 => /* BIS */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = src2 | src
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = false
        if (dstreg) R(dstspec)(dst.intValue)
        else MMU.PWriteW(dst, MMU.last_pa)


      case 6 => /* ADD */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = UInt((src2 + src) & 0xffff)
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = CHECK_C(GET_SIGN_W((~src ^ src2) & (src ^ dst)))
        C = dst < src
        if (dstreg) R(dstspec)(dst.intValue)
        else MMU.PWriteW(dst, MMU.last_pa)


      /* Opcode 07: EIS, FIS, CIS
       Notes:
       - The code assumes that the host int length is at least 32 bits.
       - MUL carry: C is set if the (signed) result doesn't fit in 16 bits.
       - Divide has three error cases:
            1. Divide by zero.
            2. Divide largest negative number by -1.
            3. (Signed) quotient doesn't fit in 16 bits.
         Cases 1 and 2 must be tested in advance, to avoid C runtime errors.
       - ASHx left: overflow if the bits shifted out do not equal the sign
         of the result (convert shift out to 1/0, xor against sign).
       - ASHx right: if right shift sign extends, then the shift and
         conditional or of shifted -1 is redundant.  If right shift zero
         extends, then the shift and conditional or does sign extension.
    */

      case 0x7 =>
        srcspec = srcspec & 0x7
        (IR >> 9) & 0x7 match {
          /* decode IR<11:9> */

          case 0 => /* MUL */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue)

            } else {
              src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
              src = R(srcspec).get16
              if (CHECK_C(GET_SIGN_W(src2))) src2 = UInt(src2 | ~0x7fff)
              if (CHECK_C(GET_SIGN_W(src))) src = UInt(src | ~0x7fff)
              dst = src * src2
              R(srcspec)((dst >> 16) & 0xffff)
              R(srcspec | 1)(dst & 0xffff)
              // TODO This is a bug - won't go negative
              N = if (dst.intValue < 0) true else false
              Z = GET_Z(dst)
              V = false
              C = (dst.intValue > 0x7fff) || (dst.intValue < -0x8000)
            }


          case 1 => /* DIV */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue)

            } else {
              src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
              src = UInt((R(srcspec) << 16) | R(srcspec | 1))
              if (src2 == 0) {
                N = false /* J11,11/70 compat */
                Z = true
                V = true
                C = true

              } else {
                if ((src == 0x80000000) && (src2 == 0xffff)) {
                  V = true /* J11,11/70 compat */
                  N = false
                  Z = false
                  C = false /* N =Z = false */

                } else {
                  if (CHECK_C(GET_SIGN_W(src2))) src2 = UInt(src2 | ~0x7fff)
                  if (CHECK_C(GET_SIGN_W(R(srcspec).get16))) src = UInt(src | ~0x7fffffff)
                  dst = src / src2
                  N = if (dst.intValue < 0) true else false /* N set on 32b result */
                  if ((dst.intValue > 0x7fff) || (dst.intValue < -0x8000)) {
                    V = true /* J11,11/70 compat */
                    Z = false
                    C = false /* Z =C = false */

                  } else {
                    R(srcspec)(dst & 0xffff)
                    R(srcspec | 1)((src - (src2 * dst)) & 0xffff)
                    Z = GET_Z(dst)
                    V = false
                    C = false

                  }
                }
              }
            }

          case 2 => /* ASH */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue)

            } else {
              src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
              src2 = UInt(src2 & 0x3f)
              sign = GET_SIGN_W(R(srcspec).get16)
              src = if (sign != 0) UInt(R(srcspec).get16 | ~0x7fff) else R(srcspec).get16
              if (src2 == 0) {
                /* [0] */
                dst = src
                V = false
                C = false
              }
              else if (src2.intValue <= 15) {
                /* [1,15] */
                dst = src << src2
                i = (src >> (16 - src2)) & 0xffff
                V = if (i != (dst & 0x8000)) true else false
                C = CHECK_C(i & 1)
              }
              else if (src2.intValue <= 31) {
                /* [16,31] */
                dst = UInt(0)
                V = if (src != 0) true else false
                C = CHECK_C((src << (src2 - 16)) & 1)
              }
              else if (src2 == 32) {
                /* [32] = -32 */
                // TODO Check logic
                // dst = -sign
                V = false
                C = CHECK_C(sign)
              }
              else {
                /* [33,63] = -31,-1 */
                // TODO Check logic
                dst = UInt((src >> (64 - src2)) | (-sign << (src2 - 32)))
                V = false
                C = CHECK_C((src >> (63 - src2)) & 1)
              }
              dst = {
                R(srcspec)(dst & 0xffff)
                R(srcspec).get16
              }
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)

            }
          case 3 => /* ASHC */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue)

            } else {
              src2 = if (dstreg) R(dstspec).get16 else MMU.ReadW(MMU.GeteaW(dstspec))
              src2 = UInt(src2 & 0x3f)
              sign = GET_SIGN_W(R(srcspec).get16)
              src = UInt((R(srcspec) << 16) | R(srcspec | 1))
              if (src2 == 0) {
                /* [0] */
                dst = src
                V = false
                C = false
              }
              else if (src2.intValue <= 31) {
                /* [1,31] */
                dst = src << src2
                // TODO Check logic
                // i = (src >> (32 - src2)) | (-sign << src2)
                V = if (i != (dst & 0x80000000)) true else false
                C = CHECK_C(i & 1)
              }
              else if (src2 == 32) {
                /* [32] = -32 */
                // TODO Check Logic
                // dst = -sign
                V = false
                C = CHECK_C(sign)
              }
              else {
                /* [33,63] = -31,-1 */
                dst = UInt((src >> (64 - src2)) | (-sign << (src2 - 32)))
                V = false
                C = if (((src >> (63 - src2)) & 1) != 0) true else false
              }
              i = {
                R(srcspec)((dst >> 16) & 0xffff)
                R(srcspec)
              }
              dst = {
                R(srcspec | 1)(dst & 0xffff)
                R(srcspec | 1).get16
              }
              N = CHECK_C(GET_SIGN_W(UInt(i)))
              Z = GET_Z(UInt(dst | i))

            }
          case 4 => /* XOR */
            if (CPUT(CPUOPT.HAS_SXS)) {
              if (CPUT(CPUOPT.IS_SDSD) && !dstreg) {
                /* R,not R */
                src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
                src = R(srcspec).get16
              }
              else {
                src = R(srcspec).get16
                src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
              }
              dst = src ^ src2
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              if (dstreg) R(dstspec)(dst.intValue)
              else MMU.PWriteW(dst, MMU.last_pa)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 5 => /* FIS */
          // TODO if (CPUO(CPUOPT.OPT_FIS))
          //  fis11(IR)
          // else setTRAP(PDP11.TRAP_ILL.intValue)


          case 6 => /* CIS */
            if (CPUT(CPUOPT.CPUT_60) && (cm == MD_KER) && /* 11/60 MED? */
              (IR == 0x7d80)) {
              // Check Logic
              MMU.ReadE(UInt(PC.get16 | MMU.isenable)) /* read immediate */
              PC((PC + 2) & 0xffff)
            }
          // TODO else if (CPUO(CPUOPT.OPT_CIS)) /* CIS option? */
          //  reason = cis11(IR)
          // else setTRAP(PDP11.TRAP_ILL.intValue)


          case 7 => /* SOB */
            if (CPUT(CPUOPT.HAS_SXS)) {
              R(srcspec)((R(srcspec) - 1) & 0xffff)
              if (R(srcspec).get16 != 0) {
                JMP_PC(UInt((PC.get16 - dstspec - dstspec) & 0xffff))
              }
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

        } /* end switch EIS */
      /* end case 007 */

      /* Opcode 10: branches, traps, SOPs */

      case 0x8 =>
        (IR >> 6) & 0x3f match {
          /* decode IR<11:6> */

          case 0x0 | 0x1 => /* BPL */
            if (!N) BRANCH_F(IR)

          case 0x2 | 0x3 => /* BPL */
            if (!N) BRANCH_B(IR)

          case 0x4 | 0x5 => /* BMI */
            if (N) BRANCH_F(IR)

          case 0x6 | 0x7 => /* BMI */
            if (N) BRANCH_B(IR)

          case 0x08 | 0x09 => /* BHI */
            if (!(C | Z)) BRANCH_F(IR)

          case 0xa | 0xb => /* BHI */
            if (!(C | Z)) BRANCH_B(IR)

          case 0xc | 0xd => /* BLOS */
            if (C | Z) BRANCH_F(IR)

          case 0xe | 0xf => /* BLOS */
            if (C | Z) BRANCH_B(IR)

          case 0x10 | 0x11 => /* BVC */
            if (!V) BRANCH_F(IR)

          case 0x12 | 0x13 => /* BVC */
            if (!V) BRANCH_B(IR)

          case 0x14 | 0x15 => /* BVS */
            if (V) BRANCH_F(IR)

          case 0x16 | 0x17 => /* BVS */
            if (V) BRANCH_B(IR)

          case 0x18 | 0x19 => /* BCC */
            if (!C) BRANCH_F(IR)

          case 0x1a | 0x1b => /* BCC */
            if (!C) BRANCH_B(IR)

          case 0x1c | 0x1d => /* BCS */
            if (C) BRANCH_F(IR)

          case 0x1e | 0x1f => /* BCS */
            if (C) BRANCH_B(IR)

          case 0x20 | 0x21 | 0x22 | 0x23 => /* EMT */
            setTRAP(PDP11.TRAP_EMT.intValue)

          case 0x24 | 0x25 | 0x26 | 0x27 => /* TRAP */
            setTRAP(PDP11.TRAP_TRAP.intValue)

          case 0x28 => /* CLRB */
            N = false
            V = false
            C = false
            Z = true
            if (dstreg) R(dstspec)(R(dstspec) & 0xff00)
            else MMU.WriteB(UInt(0), MMU.GeteaB(UInt(dstspec)))

          case 0x29 => /* COMB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((dst ^ 0xff) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.toUByte))
            Z = GET_Z(dst)
            V = false
            C = true
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)

          case 0x2a => /* INCB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((dst + 1) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = dst == 0x80
            if (dstreg)
              R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x2b => /* DECB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((dst - 1) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = dst == 0x7f
            if (dstreg)
              R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x2c => /* NEGB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((-dst) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = dst == 0x80
            C = !Z
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x2d => /* ADCB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((dst + {
              if (C) 1 else 0
            }) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = C && (if (dst == 0x80) true else false)
            C = C & Z
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x2e => /* SBCB */
            dst = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((dst - {
              if (C) 1 else 0
            }) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = C && (dst == 0x7f)
            C = C && (dst == 0xff)
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x2f => /* TSTB */
            dst = if (dstreg) UInt(R(dstspec).get16 & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            V = false
            C = false


          case 0x30 => /* RORB */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            // TODO Check logic
            // dst = ((src & 0xff) >> 1) | (C << 7)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            C = CHECK_C(src & 1)
            V = N ^ C
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x31 => /* ROLB */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt(((src << 1) | {
              if (C) 1 else 0
            }) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            C = CHECK_C(GET_SIGN_B(src & 0xff))
            V = N ^ C
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)


          case 0x32 => /* ASRB */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt(((src & 0xff) >> 1) | (src & 0x80))
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            C = CHECK_C(src & 1)
            V = N ^ C
            if (dstreg)
              R(dstspec)((R(dstspec) & 0xff00) | dst.intValue)
            else MMU.PWriteB(dst, MMU.last_pa)

          case 0x33 => /* ASLB */
            src = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
            dst = UInt((src << 1) & 0xff)
            N = CHECK_C(GET_SIGN_B(dst.intValue))
            Z = GET_Z(dst)
            C = CHECK_C(GET_SIGN_B(src & 0xff))
            V = N ^ C
            if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)

          /* Notes:
         - MTPS cannot alter the T bit
         - MxPD must mask GeteaW returned address, dspace is from cm not pm
         - MxPD must set MMR1 for SP recovery in case of fault
      */
          case 0x34 => /* MTPS */
            if (CPUT(CPUOPT.HAS_MXPS)) {
              dst = if (dstreg) R(dstspec).get16 else MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
              if (cm == MD_KER) {
                ipl = (dst >> PDP11.PSW_V_IPL) & 0x7
                trap_req = MMU.calc_ints(ipl, trap_req)
              }
              N = CHECK_C((dst >> PDP11.PSW_V_N) & 1)
              Z = CHECK_C((dst >> PDP11.PSW_V_Z) & 1)
              V = CHECK_C((dst >> PDP11.PSW_V_V) & 1)
              C = CHECK_C((dst >> PDP11.PSW_V_C) & 1)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)


          case 0x35 => /* MFPD */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  dst = STACKFILE(pm).get16
                else dst = R(dstspec).get16
              }
              else dst = MMU.ReadW(UInt((MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_ds(pm)))
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              SP((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6)
              if (MMU.update_MM) MMU.MMR1(MMU.reg_mods)
              MMU.WriteW(dst, UInt(SP.get16 | MMU.dsenable))
              if ((cm == MD_KER) && (SP.get16 < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP.get16)
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case 0x36 => /* MTPD */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              dst = MMU.ReadW(UInt(SP | MMU.dsenable))
              N = CHECK_C(GET_SIGN_W(dst))
              Z = GET_Z(dst)
              V = false
              SP((SP + 2) & 0xffff)
              MMU.reg_mods = 0x16
              if (MMU.update_MM)
                MMU.MMR1(MMU.reg_mods)
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  STACKFILE(pm)(dst.intValue)
                else R(dstspec)(dst.intValue)
              }
              else MMU.WriteW(dst, UInt((MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_ds(pm)))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)


          case 0x37 => /* MFPS */
            if (CPUT(CPUOPT.HAS_MXPS)) {
              dst = UInt(get_PSW & 0xff)
              N = CHECK_C(GET_SIGN_B(dst.intValue))
              Z = GET_Z(dst)
              V = false
              // TODO if (dstreg) R(dstspec)(if (dst & 0x80) 0xff00 | dst.intValue else dst.intValue)
              //else MMU.WriteB(dst, MMU.GeteaB(dstspec))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)

          case _ =>
            setTRAP(PDP11.TRAP_ILL.intValue)

        } /* end switch SOPs */
      /* end case 010 */

      /* Opcodes 11 - 16: double operand byte instructions
       Cmp: v = [sign (src) != sign (src2)] and [sign (src2) = sign (result)]
       Sub: v = [sign (src) != sign (src2)] and [sign (src) = sign (result)]
    */

      case 0x9 => /* MOVB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          ea = MMU.GeteaB(UInt(dstspec))
          dst = UInt(R(srcspec) & 0xff)
        }
        else {
          dst = if (srcreg) UInt(R(srcspec).get16 & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(srcspec)))
          if (!dstreg) ea = MMU.GeteaB(UInt(dstspec))
        }
        N = CHECK_C(GET_SIGN_B(dst.intValue))
        Z = GET_Z(dst)
        V = false
        if (dstreg)
          R(dstspec)(if ((dst & 0x80) != 0) 0xff00 | dst.intValue else dst.intValue)
        else MMU.WriteB(dst, ea)

      case 0xa => /* CMPB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
          src = UInt(R(srcspec) & 0xff)
        }
        else {
          src = if (srcreg) UInt(R(srcspec) & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(srcspec)))
          src2 = if (dstreg) UInt(R(dstspec) & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
        }
        dst = UInt((src - src2) & 0xff)
        N = CHECK_C(GET_SIGN_B(dst.intValue))
        Z = GET_Z(dst)
        V = CHECK_C(GET_SIGN_B((src ^ src2) & (~src2 ^ dst.intValue)))
        C = src < src2

      case 0xb => /* BITB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
          src = UInt(R(srcspec) & 0xff)
        }
        else {
          src = if (srcreg) UInt(R(srcspec).get16 & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(srcspec)))
          src2 = if (dstreg) UInt(R(dstspec).get16 & 0xff) else MMU.ReadB(MMU.GeteaB(UInt(dstspec)))
        }
        dst = UInt((src2 & src) & 0xff)
        N = CHECK_C(GET_SIGN_B(dst.intValue))
        Z = GET_Z(dst)
        V = false


      case 0xc => /* BICB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadB(MMU.GeteaB(UInt(srcspec)))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
        }
        dst = UInt((src2 & ~src) & 0xff)
        N = CHECK_C(GET_SIGN_B(dst.intValue))
        Z = GET_Z(dst)
        V = false
        if (dstreg)
          R(dstspec)((R(dstspec) & 0xff00) | dst)
        else MMU.PWriteB(dst, MMU.last_pa)


      case 0xd => /* BISB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadB(MMU.GeteaB(UInt(srcspec)))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMB(MMU.GeteaB(UInt(dstspec)))
        }
        dst = UInt((src2 | src) & 0xff)
        N = CHECK_C(GET_SIGN_B(dst.intValue))
        Z = GET_Z(dst)
        V = false
        if (dstreg) R(dstspec)((R(dstspec) & 0xff00) | dst)
        else MMU.PWriteB(dst, MMU.last_pa)


      case 0xe => /* SUB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec).get16
        }
        else {
          src = if (srcreg) R(srcspec).get16 else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec).get16 else MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = UInt((src2 - src) & 0xffff)
        N = CHECK_C(GET_SIGN_W(dst))
        Z = GET_Z(dst)
        V = CHECK_C(GET_SIGN_W((src ^ src2) & (~src ^ dst)))
        C = src2 < src
        if (dstreg)
          R(dstspec)(dst.intValue)
        else MMU.PWriteW(dst, MMU.last_pa)


      /* Opcode 17: floating point */

      case 0xf =>
      // TODO if (CPUO(CPUOPT.OPT_FPP))
      // TODO fp11(IR) /* call fpp */
      // else setTRAP(PDP11.TRAP_ILL.intValue)
      /* end case 017 */
    } /* end switch op */
  } /* end main loop */


  override def runcpu(singleStep: Boolean, startAddr: UInt): Unit = {
    // Force the PC
    PC(startAddr.intValue)
    runcpu(singleStep)
  }

  // cpu_type - what type of CPU - determines some features and functions
  // override this for specific cpu implementation
  val cpu_type: UInt // Model as bit mask
  val cpu_opt: UInt // cpu options
  var cpu_bme: Boolean
  /* bus map enable */
  val cpu_model: UInt

  /* CPU model */
  @inline def CPUT(x: UInt): Boolean = (cpu_type & x) != 0

  @inline def CPUO(x: UInt): Boolean = (cpu_opt & x) != 0

  @inline def UNIBUS: UInt = cpu_opt & CPUOPT.BUS_U

  /*
  Traps and interrupts.  Variable trap_req bit-encodes all possible
  traps.  In addition, an interrupt pending bit is encoded as the
  lowest priority trap.  Traps are processed by trap_vec and trap_clear,
  which provide the vector and subordinate traps to clear, respectively.
    Array int_req[0:7] bit encodes all possible interrupts.  It is masked
  under the interrupt priority level, ipl.  If any interrupt request
    is not masked, the interrupt bit is set in trap_req.  While most
    interrupts are handled centrally, a device can supply an interrupt
    acknowledge routine.
    */

  val int_req = new Array[Int](PDP11.IPL_HLVL)

  val PCQ_SIZE: Int = 64
  /* must be 2**n */
  val pcq: Array[Register16] = new Array[Register16](PCQ_SIZE)
  /* PC queue */
  var pcq_p: Int = 0
  val PCQ_MASK: Int = {
    PCQ_SIZE - 1
  }

  def PCQ_ENTRY(): Unit = {
    pcq_p = (pcq_p - 1) & PCQ_MASK
    pcq(pcq_p) = PC
  }

  /* Protection modes */
  val MD_KER = 0
  val MD_SUP = 1
  val MD_UND = 2
  val MD_USR = 3

  val STKLIM_RW: UInt = UInt(0xff00)

  /* Register change tracking actually goes into variable reg_mods; from there
     it is copied into MMR1 if that register is not currently locked.  */

  @inline
  def CHECK_C(c: Int): Boolean = {
    if (c != 0) true else false
  }

  def GET_SIGN_W(v: UInt): Int = {
    (v >> 15) & 1
  }

  def GET_SIGN_B(v: UByte): Int = {
    (v >> 7) & 1
  }

  def GET_SIGN_B(v: Int): Int = GET_SIGN_B(UByte(v.toByte))


  def GET_Z(v: UInt): Boolean = {
    v.intValue == 0
  }

  def JMP_PC(x: Register16): Unit = {
    PCQ_ENTRY()
    PC.set16(x)
  }

  def JMP_PC(x: UInt): Unit = {
    PCQ_ENTRY()
    PC.set16(x.intValue & 0xffff)
  }

  def BRANCH_F(x: UInt): Unit = {
    PCQ_ENTRY()
    PC.set16(PC.get16 + ((x + x) & UInt(0xff)) & 0xffff)
  }

  def BRANCH_B(x: UInt): Unit = {
    PCQ_ENTRY()
    PC.set16(PC.get16 + ((x + x) | UInt(0xff00)) & 0xffff)
  }

  var wait_state: Int = 0
  var trap_req: Int = 0

  // Registers

  val R0 = new Register16("R0")
  val R1 = new Register16("R1")
  val R2 = new Register16("R2")
  val R3 = new Register16("R3")
  val R4 = new Register16("R4")
  val R5 = new Register16("R5")

  val R00 = new Register16("R00")
  val R01 = new Register16("R01")
  val R02 = new Register16("R02")
  val R03 = new Register16("R03")
  val R04 = new Register16("R04")
  val R05 = new Register16("R05")
  val R10 = new Register16("R10")
  val R11 = new Register16("R11")
  val R12 = new Register16("R12")
  val R13 = new Register16("R13")
  val R14 = new Register16("R14")
  val R15 = new Register16("R15")
  val KSP = new Register16("KSP") // Kernel Stack Pointer
  val SSP = new Register16("SSP") // Supervisor Stack Pointer
  val USP = new Register16("USP") // User Stack Pointer
  //  val PSW = new Register16("PSW") // Processor status word - See bits above
  val PIRQ = new Register16("PIRQ") // Programmed Interrupt Request
  val STKLIM = new Register16("STKLIM") // Stack Limit
  // TODO FP

  // Register file - Working registers
  val R: Array[Register16] = new Array[Register16](8)

  // Aliases for PC and SP
  @inline def PC: Register16 = R(6)

  def SP: Register16 = R(7)

  val REGFILE: Array[Array[Register16]] = Array.ofDim[Register16](6, 2)
  /* R0-R5, two sets */
  val STACKFILE = new Array[Register16](4) // SP, 4 modes

  // TODO set up REGIFILE and STACKFILE


  override def onHalt(singleStepped: Boolean): Unit = ???

  //override val registers: Map[String, Register] = _
  override def resetCPU(): Unit = {

    PIRQ.set16(0)
    STKLIM.set16(0)
    if (CPUT(CPUOPT.CPUT_T)) /* T11? */
      PSW = UInt(0xe0) /* start at IPL 7 */
    else
      PSW = UInt(0) /* else at IPL 0 */
    MMU.MMR0.set16(0)
    MMU.MMR1.set16(0)
    MMU.MMR2.set16(0)
    MMU.MMR3.set16(0)

    trap_req = 0
    wait_state = 0
    //if (M == NULL) {                    /* First time init */
    //  M = (uint16 *) calloc (MEMSIZE >> 1, sizeof (uint16));
    //  if (M == NULL)
    //    return SCPE_MEM;
    //sim_set_pchar (0, "01000023640"); /* ESC, CR, LF, TAB, BS, BEL, ENQ */
    //sim_brk_dflt = SWMASK ('E');
    //sim_brk_types = sim_brk_dflt|SWMASK ('P')|
    //  SWMASK ('R')|SWMASK ('S')|
    //  SWMASK ('W')|SWMASK ('X');
    //sim_brk_type_desc = cpu_breakpoints;
    //sim_vm_is_subroutine_call = &cpu_is_pc_a_subroutine_call;
    //sim_clock_precalibrate_commands = pdp11_clock_precalibrate_commands;
    //auto_config(NULL, 0);           /* do an initial auto configure */

    //pcq_r = find_reg ("PCQ", NULL, dptr);
    //if (pcq_r)
    //  pcq_r->qptr = 0;
    //else
    //  return SCPE_IERR;
    //set_r_display (0, MD_KER);
    //return build_dib_tab ();            /* build, chk dib_tab */
  }


  //**************************** CIS11
  /* Opcode bits */

  val INLINE = 0x40 // inline
  val PACKED = 0x10 // packed
  val NUMERIC = 0x0 // numeric

  // Interrupt test latency

  val INT_TEST = 100

  // Operand type definitions

  val R0_DESC = 1 // descr in R0:R1
  val R2_DESC = 2 // descr in R2:R3
  val R4_DESC = 3 // descr in R4:R5
  val R4_ARG = 4 // argument in R4
  val IN_DESC = 5 // inline descriptor
  val IN_ARG = 6 // inline argument
  val MAXOPN = 4 // max # operands

  // Decimal data type definitions

  val XZ = 0 // signed zoned
  val UZ = 1 // unsigned zoned
  val TO = 2 // trailing overpunch
  val LO = 3 // leading overpunch
  val TS = 4 // trailing separate
  val LS = 5 // leading separate
  val XP = 6 // signed packed
  val UP = 7 // unsigned packed

  // Decimal descriptor definitions

  val DTYP_M = 0x7 // type mask
  val DTYP_V = 12 // type position
  val DLNT_M = 0x1f // length mask
  val DLNT_V = 0 // length position

  def GET_DTYP(x: Int): Int = {
    (x >> DTYP_V) & DTYP_M
  }

  def GET_DLNT(x: Int): Int = {
    (x >> DLNT_V) & DLNT_M
  }

  // Shift operand definitions

  private val ASHRND_M: Int = 0xf // round digit mask
  private val ASHRND_V = 8 // round digit pos
  private val ASHLNT_M = 0xff // shift count mask
  private val ASHLNT_V = 0 // shift length pos
  private val ASHSGN = 0x80 // shift sign

  private def GET_ASHRND(x: Int): Int = {
    (x >> ASHRND_V) & ASHRND_M
  }

  private def GET_ASHLNT(x: Int): Int = {
    (x >> ASHLNT_V) & ASHLNT_M
  }

  //val A1       =       &arg(0)
  //val A2       =       &arg(2)
  //val A3       =       &arg(4)

  /* Decimal string structure */

  private val DSTRLNT = 4
  private val DSTRMAX = DSTRLNT - 1
  private val MAXDVAL = 429496730 // 2^32 / 10

  class DSTR {
    var sign: Int = 0
    var val0: Array[Int] = Array(0, 0, 0, 0)
  }

  private val Dstr0: DSTR = new DSTR
  /* Table of instruction operands */

  private val opntab: Array[Array[Int]] = Array(
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 000 - 007
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 010 - 017
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // LD2R
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), // MOVC
    Array(0, 0, 0, 0), // MOVRC
    Array(0, 0, 0, 0), // MOVTC
    Array(0, 0, 0, 0), // 033
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 034 - 037
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), // LOCC
    Array(0, 0, 0, 0), // SKPC
    Array(0, 0, 0, 0), // SCANC
    Array(0, 0, 0, 0), // SPANC
    Array(0, 0, 0, 0), // CMPC
    Array(0, 0, 0, 0), // MATC
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 046 - 047
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // ADDN
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // SUBN
    Array(R0_DESC, R2_DESC, 0, 0), // CMPN
    Array(R0_DESC, 0, 0, 0), // CVTNL
    Array(R0_DESC, R2_DESC, 0, 0), // CVTPN
    Array(R0_DESC, R2_DESC, 0, 0), // CVTNP
    Array(R0_DESC, R2_DESC, R4_ARG, 0), // ASHN
    Array(R0_DESC, 0, 0, 0), // CVTLN
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // LD3R
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // ADDP
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // SUBP
    Array(R0_DESC, R2_DESC, 0, 0), // CMPP
    Array(R0_DESC, 0, 0, 0), // CVTPL
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // MULP
    Array(R0_DESC, R2_DESC, R4_DESC, 0), // DIVP
    Array(R0_DESC, R2_DESC, R4_ARG, 0), // ASHP
    Array(R0_DESC, 0, 0, 0), // CVTLP
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 100 - 107
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 110 - 117
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 120 - 127
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(IN_DESC, IN_DESC, IN_ARG, 0), // MOVCI
    Array(IN_DESC, IN_DESC, IN_ARG, 0), // MOVRCI
    Array(IN_DESC, IN_DESC, IN_ARG, IN_ARG), // MOVTCI
    Array(0, 0, 0, 0), // 133
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 134 - 137
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(IN_DESC, IN_ARG, 0, 0), // LOCCI
    Array(IN_DESC, IN_ARG, 0, 0), // SKPCI
    Array(IN_DESC, IN_DESC, 0, 0), // SCANCI
    Array(IN_DESC, IN_DESC, 0, 0), // SPANCI
    Array(IN_DESC, IN_DESC, IN_ARG, 0), // CMPCI
    Array(IN_DESC, IN_DESC, 0, 0), // MATCI
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 146 - 147
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // ADDNI
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // SUBNI
    Array(IN_DESC, IN_DESC, 0, 0), // CMPNI
    Array(IN_DESC, IN_ARG, 0, 0), // CVTNLI
    Array(IN_DESC, IN_DESC, 0, 0), // CVTPNI
    Array(IN_DESC, IN_DESC, 0, 0), // CVTNPI
    Array(IN_DESC, IN_DESC, IN_ARG, 0), // ASHNI
    Array(IN_DESC, IN_DESC, 0, 0), // CVTLNI
    Array(0, 0, 0, 0), Array(0, 0, 0, 0), // 160 - 167
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(0, 0, 0, 0), Array(0, 0, 0, 0),
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // ADDPI
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // SUBPI
    Array(IN_DESC, IN_DESC, 0, 0), // CMPPI
    Array(IN_DESC, IN_ARG, 0, 0), // CVTPLI
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // MULPI
    Array(IN_DESC, IN_DESC, IN_DESC, 0), // DIVPI
    Array(IN_DESC, IN_DESC, IN_ARG, 0), // ASHPI
    Array(IN_DESC, IN_DESC, 0, 0) // CVTLPI
  )
  /* ASCII to overpunch table: sign is <7>, digit is <4:0> */

  private val overbin: Array[Int] = Array(
    0, 0, 0, 0, 0, 0, 0, 0, // 000 - 037
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0x80, 0, 0, 0, 0, 0, 0, // 040 - 077
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 2, 3, 4, 5, 6, 7,
    8, 9, 0x80, 0, 0, 0, 0, 0,
    0, 1, 2, 3, 4, 5, 6, 7, // 100 - 137
    8, 9, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86,
    0x87, 0x88, 0x89, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0x80, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, // 140 - 177
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0x80, 0, 0
  )

  /* Overpunch to ASCII table: indexed by sign and digit */

  private val binover: Array[Array[Char]] = Array(
    Array('{', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
      '0', '0', '0', '0', '0', '0'),
    Array('}', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
      '0', '0', '0', '0', '0', '0')
  )

  private def cis11(IR: UInt): Unit = {
    var c: Int = 0
    var i: Int = 0
    var j: Int = 0
    var t: Int = 0
    var op: Int = IR & 0x7f
    var rn: Int = 0
    var addr: Int = 0
    var match0: Int = 0
    var limit: Int = 0
    var mvlnt: Int = 0
    var shift: Int = 0
    var spc: Int = 0
    var ldivd: Int = 0
    var ldivr: Int = 0
    var arg: Array[Int] = new Array(6)
    /* operands */
    var old_PC: UInt = UInt((PC.get16 - 2) & 0xffff)
    var nc: Int = 0
    var digit: Int = 0
    var result: Int = 0
    var accum: DSTR = new DSTR
    var src1: DSTR = new DSTR
    var src2: DSTR = new DSTR
    var dst: DSTR = new DSTR
    var mptable: Array[DSTR] = new Array(10)
    val Dstr1: DSTR = new DSTR //{ 0, {0x10, 0, 0, 0} }
    Dstr1.val0 = Array(0x10, 0, 0, 0)


  }
  //**************************** CIS11 end
  //**************************** FP11
  //**************************** FP11 end

  override def showRegisters(): String = ???

  override def showFlags(): String = ???

  override def DAsm(addr: Int, sb: mutable.StringBuilder): Int = ???

  override def init(): Unit = {} // TODO

  override def handles(value: UInt): Boolean = ???

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override val registers: Map[String, Register] = ???

  def set_stack_trap(adr: UInt): Unit = {
    if (CPUT(CPUOPT.HAS_STKLF)) {
      /* fixed stack? */
      setTRAP(PDP11.TRAP_YEL.intValue) /* always yellow trap */
      setCPUERR(PDP11.CPUE_YEL)
    }
    else if (CPUT(CPUOPT.HAS_STKLR)) /* register limit? */
      if (adr.intValue >= (STKLIM.get16 + PDP11.STKL_R).intValue) {
        /* yellow zone? */
        setTRAP(PDP11.TRAP_YEL.intValue) /* still yellow trap */
        setCPUERR(PDP11.CPUE_YEL)
      }
      else {
        /* red zone abort */
        setCPUERR(PDP11.CPUE_RED)
        STACKFILE(MD_KER).set16(4)
        SP.set16(4)
        throw AbortException(PDP11.TRAP_RED)
      }
    /* no stack limit */
  }


  @inline def setTRAP(name: Int): Unit = {
    trap_req = trap_req | name
  }

  @inline def setCPUERR(name: UInt): Unit = CPUERR = CPUERR | name

  /* Assemble PSW from pieces */

  def get_PSW: Int = {
    (cm << PDP11.PSW_V_CM) | (pm << PDP11.PSW_V_PM) |
      (rs << PDP11.PSW_V_RS) | (fpd << PDP11.PSW_V_FPD) |
      (ipl << PDP11.PSW_V_IPL) | (tbit << PDP11.PSW_V_TBIT) |
      (if (N) 1 else 0 << PDP11.PSW_V_N) | (if (Z) 1 else 0 << PDP11.PSW_V_Z) |
      (if (V) 1 else 0 << PDP11.PSW_V_V) | (if (C) 1 else 0 << PDP11.PSW_V_C)
  }

  /* Explicit PSW write - T-bit may be protected */

  def PSW_wr(data: Int, pa: Int, access: Int): Unit = {

    var d = data
    if (access == MMU.WRITEC) {
      /* console access? */
      // TODO PSW = d & cpu_tab[cpu_model].psw;
      return
    }
    val curr = get_PSW
    /* get current */
    val oldrs = rs /* save reg set */
    STACKFILE(cm)(SP.get16) /* save curr SP */
    if (access == MMU.WRITEB) d = {
      if ((pa & 1) != 0) (curr & 0xff) | (d << 8) else (curr & ~0xff) | d
    }
    if (!CPUT(CPUOPT.HAS_EXPT)) /* expl T writes? */
      d = (d & ~PDP11.PSW_TBIT) | (curr & PDP11.PSW_TBIT) /* no, use old T */
    put_PSW(d, false) /* call calc_is,ds */
    if (rs != oldrs) {
      /* switch reg set */
      for (i <- 0 to 6) {
        REGFILE(i)(oldrs)(R(i).get16)
        R(i)(REGFILE(i)(rs).get16)
      }
    }
    // changing what SP points to
    R(7) = STACKFILE(cm) /* switch SP */
    MMU.isenable = MMU.calc_is(cm)
    MMU.dsenable = MMU.calc_ds(cm)
  }

  /* Store pieces of new PSW - implements RTI/RTT protection */
  val CPU_PSW_MASK: Int

  def put_PSW(val1: Int, prot: Boolean): Unit = {
    val val2 = val1 & CPU_PSW_MASK /* mask off invalid bits */
    if (prot) {
      /* protected? */
      cm = cm | ((val2 >> PDP11.PSW_V_CM) & 0x3) /* or to cm,pm,rs */
      pm = pm | ((val2 >> PDP11.PSW_V_PM) & 0x3) /* can't change ipl */
      rs = rs | ((val2 >> PDP11.PSW_V_RS) & 0x1)
    }
    else {
      cm = (val2 >> PDP11.PSW_V_CM) & 0x3 /* write cm,pm,rs,ipl */
      pm = (val2 >> PDP11.PSW_V_PM) & 0x3
      rs = (val2 >> PDP11.PSW_V_RS) & 0x1
      ipl = (val2 >> PDP11.PSW_V_IPL) & 0x7
    }
    fpd = (val2 >> PDP11.PSW_V_FPD) & 0x1 /* always writeable */
    tbit = (val2 >> PDP11.PSW_V_TBIT) & 0x1
    N = CHECK_C((val2 >> PDP11.PSW_V_N) & 0x1)
    Z = CHECK_C((val2 >> PDP11.PSW_V_Z) & 0x1)
    V = CHECK_C((val2 >> PDP11.PSW_V_V) & 0x1)
    C = CHECK_C((val2 >> PDP11.PSW_V_C) & 0x1)

  }

  /* Get decimal string
   Arguments:
        dscr    =       decimal string descriptor
        src     =       decimal string structure
        flag    =       numeric/packed flag
   The routine returns the length in int32's of the non-zero part of
   the string.
   This routine plays fast and loose with operand checking, as did the
   original 11/23 microcode (half of which I wrote).  In particular,
   - If the flag specifies packed, the type is not checked at all.
     The sign of an unsigned string is assumed to be 0xF (an
     alternative for +).
   - If the flag specifies numeric, packed types will be treated
     as unsigned zoned.
   - For separate, only the '-' sign is checked, not the '+'.
   However, to simplify the code elsewhere, digits are range checked,
   and bad digits are replaced with 0's.
*/

  def ReadDstr(dscr: Array[Int], flag: Int): Int = {
    val src = Dstr0
    /* clear result */
    val type0 = GET_DTYP(dscr(0))
    /* get type */
    val lnt = GET_DLNT(dscr(0))
    /* get string length */
    var t = 0
    if ((flag & PACKED) != 0) {
      /* packed? */
      val end = lnt / 2 /* last byte */
      for (i <- 0 to end) {
        /* loop thru string */
        var c: Int = MMU.ReadB(UInt(((dscr(1) + end - i) & 0xffff) | MMU.dsenable)).intValue
        if (i == 0) /* save sign */
          t = c & 0xF;
        if ((i == end) && ((lnt & 1) == 0))
          c = c & 0xF
        if (c >= 0xA0) /* check hi digit */
          c = c & 0xF
        if ((c & 0xF) >= 0xA) /* check lo digit */
          c = c & 0xF0;
        src.val0(i / 4) = src.val0(i / 4) | (c << ((i % 4) * 8))
      } /* end for */
      if ((t == 0xB) || (t == 0xD)) /* if -, set sign */
        src.sign = 1
      src.val0(0) = src.val0(0) & ~0xF /* clear sign */
    } /* end packed */
    else {
      /* numeric */
      // TODO if (type0 >= TS) src.sign = (
      //  MMU.ReadB(UInt((({if(type0 == TS)
      // dscr(1) + lnt else  dscr(1) - 1}) & 0xffff) | MMU.dsenable)) == '-')
      for (i <- 1 to lnt) {
        /* loop thru string */
        var c = MMU.ReadB(UInt(((dscr(1) + lnt - i) & 0xffff) | MMU.dsenable)).intValue
        if ((i == 1) && (type0 == XZ) && ((c & 0xF0) == 0x70))
          src.sign = 1 /* signed zoned */
        else if (((i == 1) && (type0 == TO)) ||
          ((i == lnt) && (type0 == LO))) {
          c = overbin(c & 0x7f) /* get sign and digit */
          src.sign = c >> 7 /* set sign */
        }
        c = c & 0xF /* get digit */
        if (c > 9) /* range check */
          c = 0;
        src.val0(i / 8) = src.val0(i / 8) | (c << ((i % 8) * 4))
      } /* end for */
    } /* end numeric */
    TestDstr(src) /* clean -0 */
  }

  /* Store decimal string
     Arguments:
          dsrc    =       decimal string descriptor
          src     =       decimal string structure
          flag    =       numeric/packed flag
     PSW.NZ are also set to their proper values
     PSW.V will be set on overflow; it must be initialized elsewhere
     (to allow for external overflow calculations)
     The rules for the stored sign and the PSW sign are:
     - Stored sign is negative if input is negative, string type
       is signed, and the result is non-zero or there was overflow
     - PSW sign is negative if input is negative, string type is
       signed, and the result is non-zero
     Thus, the stored sign and the PSW sign will differ in one case:
     a negative zero generated by overflow is stored with a negative
     sign, but PSW.N is clear
  */
  val masktab: Array[Int] = Array(
    0xFFFFFFF0, 0xFFFFFF00, 0xFFFFF000, 0xFFFF0000,
    0xFFF00000, 0xFF000000, 0xF0000000, 0x00000000
  )
  val unsignedtab: Array[Int] = Array(0, 1, 0, 0, 0, 0, 0, 1)

  def WriteDstr(dscr: Array[Int], dst: DSTR, flag: Int): Unit = {
    val type0 = GET_DTYP(dscr(0))
    /* get type */
    val lnt = GET_DLNT(dscr(0))
    /* get string length */
    var mask = UInt(0) /* can't ovflo */
    Z = true
    /* assume all 0's */
    val limit = lnt / 8 /* limit for test */
    for (i <- 0 until DSTRLNT) {
      /* loop thru value */
      if (i == limit) /* at limit, get mask */
        mask = UInt(masktab(lnt % 8))
      else if (i > limit) /* beyond, all ovflo */
        mask = UInt(0xFFFFFFFF)
      if ((dst.val0(i) & mask) != 0) /* test for ovflo */
        V = true
      dst.val0(i) = dst.val0(i) & ~mask
      if (dst.val0(i) != 0) /* test nz */
        Z = false
    }
    dst.sign = dst.sign & ~unsignedtab(type0) & (if (!(Z & !V)) 1 else 0)
    N = CHECK_C(dst.sign & (if (!Z) 1 else 0)) /* N = sign, if ~zero */

    if ((flag & PACKED) != 0) {
      /* packed? */
      val end = lnt / 2 /* end of string */
      if (type0 == UP)
        dst.val0(0) = dst.val0(0) | 0xF
      else dst.val0(0) = dst.val0(0) | 0xC | dst.sign
      for (i <- 0 to end) {
        /* store string */
        val c = (dst.val0(i / 4) >> ((i % 4) * 8)) & 0xFF
        MMU.WriteB(UInt(c), UInt(((dscr(1) + end - i) & 0xffff) | MMU.dsenable))
      } /* end for */
    } /* end packed */
    else {
      if (type0 >= TS) MMU.WriteB(if (dst.sign != 0) UInt('-') else UInt('+'),
        if (type0 == TS) UInt(dscr(1) + lnt) else UInt(((dscr(1) - 1) & 0xffff) | MMU.dsenable))
      for (i <- 1 to lnt) {
        /* store string */
        var c = (dst.val0(i / 8) >> ((i % 8) * 4)) & 0xF /* get digit */
        if ((i == 1) && (type0 == XZ) && (dst.sign != 0))
          c = c | 0x70 /* signed zoned */
        else if (((i == 1) && (type0 == TO)) || ((i == lnt) && (type0 == LO)))
          c = binover(dst.sign)(c) /* get sign and digit */
        else c = c | 0x30 /* default */
        MMU.WriteB(UInt(c), UInt(((dscr(1) + lnt - i) & 0xffff) | MMU.dsenable))
      } /* end for */
    } /* end numeric */

  }

  /* Add decimal string magnitudes
     Arguments:
          s1      =       source1 decimal string
          s2      =       source2 decimal string
          ds      =       destination decimal string
          cy      =       carry in
     Output       =       1 if carry, 0 if no carry
     This algorithm courtesy Anton Chernoff, circa 1992 or even earlier.
     We trace the history of a pair of adjacent digits to see how the
     carry is fixed; each parenthesized item is a 4b digit.
     Assume we are adding:
          (a)(b)  I
     +    (x)(y)  J
     First compute I^J:
          (a^x)(b^y)      TMP
     Note that the low bit of each digit is the same as the low bit of
     the sum of the digits, ignoring the carry, since the low bit of the
     sum is the xor of the bits.
     Now compute I+J+66 to get decimal addition with carry forced left
     one digit:
          (a+x+6+carry mod 16)(b+y+6 mod 16)      SUM
     Note that if there was a carry from b+y+6, then the low bit of the
     left digit is different from the expected low bit from the xor.
     If we xor this SUM into TMP, then the low bit of each digit is 1
     if there was a carry, and 0 if not.  We need to subtract 6 from each
     digit that did not have a carry, so take ~(SUM ^ TMP) & 0x11, shift
     it right 4 to the digits that are affected, and subtract 6*adjustment
     (actually, shift it right 3 and subtract 3*adjustment).
  */

  def AddDstr(s1: DSTR, s2: DSTR, ds: DSTR, cy: Int): Int = {
    var cy2 = cy
    for (i <- 0 until DSTRLNT) {
      /* loop low to high */
      val tm1: Int = s1.val0(i) ^ (s2.val0(i) + cy2)
      /* xor operands */
      val sm1 = s1.val0(i) + (s2.val0(i) + cy2)
      /* sum operands */
      val sm2 = sm1 + 0x66666666 /* force carry out */
      cy2 = if (sm1 < s1.val0(i) || (sm2 < sm1)) 1 else 0
      /* check for overflow */
      val tm2 = tm1 ^ sm2
      /* get carry flags */
      val tm3 = (tm2 >> 3) | (cy2 << 29)
      /* compute adjustment */
      val tm4 = 0x22222222 & ~tm3 /* clear where carry */
      ds.val0(i) = sm2 - (3 * tm4) /* final result */
    }
    cy
  }

  /* Subtract decimal string magnitudes
     Arguments:
          s1      =       source1 decimal string
          s2      =       source2 decimal string
          ds      =       destination decimal string
     Outputs:             s2 - s1 in ds
     Note: the routine assumes that s1 <= s2
  */

  def SubDstr(s1: DSTR, s2: DSTR, ds: DSTR): Unit = {

    val complX: DSTR = new DSTR

    for (i <- 0 to DSTRLNT) complX.val0(i) = 0x99999999 - s1.val0(i)
    AddDstr(complX, s2, ds, 1)
    /* s1 + ~s2 + 1 */
  }

  /* Compare decimal string magnitudes
     Arguments:
          s1      =       source1 decimal string
          s2      =       source2 decimal string
     Output       =       1 if >, 0 if =, -1 if <
  */

  def CmpDstr(s1: DSTR, s2: DSTR): Int = {
    for (i <- DSTRMAX to 0 by -1) {
      if (s1.val0(i) > s2.val0(i)) return 1
      if (s1.val0(i) < s2.val0(i)) return -1
    }
    0
  }

  /* Test decimal string for zero
     Arguments:
          dsrc    =       decimal string structure
     Returns the non-zero length of the string, in int32 units
     If the string is zero, the sign is cleared
  */

  def TestDstr(dsrc: DSTR): Int = {
    for (i <- 0 to DSTRMAX) {
      if (dsrc.val0(i) != 0) return i + 1
    }
    dsrc.sign = 0
    0
  }

  /* Get exact length of decimal string
     Arguments:
          dsrc    =       decimal string structure
          nz      =       result from TestDstr
  */

  def LntDstr(dsrc: DSTR, nz: Int): Int = {
    if (nz == 0) return 0
    for (i <- 7 to 0 by -1) {
      if (((dsrc.val0(nz - 1) >> (i * 4)) & 0xF) != 0)
        return ((nz - 1) * 8) + i
    }
    (nz - 1) * 8
  }

  /* Create table of multiples
     Arguments:
          dsrc    =       base decimal string structure
          mtable[10] =    array of decimal string structures
     Note that dsrc has a high order zero nibble; this
     guarantees that the largest multiple won't overflow.
     Also note that mtable[0] is not filled in.
  */

  def CreateTable(dsrc: DSTR, mtable: Array[DSTR]): Unit = {
    mtable(1) = dsrc
    for (i <- 2 until 10)
      AddDstr(mtable(1), mtable(i - 1), mtable(i), 0)
  }

  /* Word shift right
     Arguments:
          dsrc    =       decimal string structure
          sc      =       shift count
  */

  def WordRshift(dsrc: DSTR, sc: Int): Unit = {

    if (sc != 0) {
      for (i <- 0 until DSTRLNT) {
        if ((i + sc) < DSTRLNT)
          dsrc.val0(i) = dsrc.val0(i + sc)
        else dsrc.val0(i) = 0
      }
    }
  }

  /* Word shift left
     Arguments:
          dsrc    =       decimal string structure
          sc      =       shift count
  */

  def WordLshift(dsrc: DSTR, sc: Int): Int = {
    var c = 0
    if (sc != 0) {
      for (i <- DSTRMAX to 0 by -1) {
        if (i >= sc)
          dsrc.val0(i) = dsrc.val0(i - sc)
        else {
          c |= dsrc.val0(i)
          dsrc.val0(i) = 0
        }
      }
    }
    c
  }

  /* Nibble shift decimal string right
     Arguments:
          dsrc    =       decimal string structure
          sc      =       shift count
          cin     =       carry in
  */

  def NibbleRshift(dsrc: DSTR, sc: Int, cin: Int): Int = {
    val s: Int = sc * 4
    var cin2: Int = cin

    if (s != 0) {
      for (i <- DSTRMAX to 0 by -1) {
        val nc = (dsrc.val0(i) << (32 - s)) & 0xFFFFFFFF
        dsrc.val0(i) = ((dsrc.val0(i) >> s) | cin) & 0xFFFFFFFF
        cin2 = nc
      }
      return cin2
    }
    0
  }

  /* Nibble shift decimal string left
     Arguments:
          dsrc    =       decimal string structure
          sc      =       shift count
  */

  def NibbleLshift(dsrc: DSTR, sc: Int): Int = {
    //int32 i, s;
    //uint32 nc, cin;
    val s: Int = sc * 4

    var cin = 0
    if (s != 0) {
      for (i <- 0 until DSTRLNT) {
        val nc = dsrc.val0(i) >> (32 - s)
        dsrc.val0(i) = ((dsrc.val0(i) << s) | cin) & 0xFFFFFFFF
        cin = nc
      }
      return cin
    }
    0
  }

  /* Common setup routine for MOVC class instructions */

  def movx_setup(op: Int, arg: Array[Int]): UInt = {
    //int32 mvlnt, t;
    var mvlnt: UInt = UInt(0)

    if (CPUT(CPUOPT.CPUT_44)) {
      /* 11/44? */
      MMU.ReadMB(UInt(((SP - 0x80) & 0xffff) | MMU.dsenable)) /* probe both blocks */
      MMU.ReadMB(UInt(((SP - 0x40) & 0xffff) | MMU.dsenable)) /* in 64W stack area */
    }
    if ((op & INLINE) != 0) {
      /* inline */
      mvlnt = if (arg(0) < arg(2)) UInt(arg(0)) else UInt(arg(2))
      MMU.WriteW(mvlnt, UInt(((SP - 14) & 0xffff) | MMU.dsenable)) /* push move length */
      MMU.WriteW(R(0).get16, UInt(((SP - 12) & 0xffff) | MMU.dsenable)) /* push R0 - R5 */
      MMU.WriteW(R(1).get16, UInt(((SP - 10) & 0xffff) | MMU.dsenable))
      MMU.WriteW(R(2).get16, UInt(((SP - 8) & 0xffff) | MMU.dsenable))
      MMU.WriteW(R(3).get16, UInt(((SP - 6) & 0xffff) | MMU.dsenable))
      MMU.WriteW(R(4).get16, UInt(((SP - 4) & 0xffff) | MMU.dsenable))
      MMU.WriteW(R(5).get16, UInt(((SP - 2) & 0xffff) | MMU.dsenable))
      SP((SP - 14) & 0xffff)
      R(0)(arg(0)) /* args to registers */
      R(1)(arg(1))
      R(2)(arg(2))
      R(3)(arg(3))
      R(4)(arg(4))
      R(5)(arg(5) & 0xffff)
    }
    else {
      /* register */
      mvlnt = if (R(0).get16 < R(2).get16) R(0).get16 else R(2).get16
      MMU.WriteW(mvlnt, UInt(((SP - 2) & 0xffff) | MMU.dsenable)) /* push move length */
      SP((SP - 2) & 0xffff)
    }
    fpd = 1
    val t = R(0) - R(2) /* src.lnt - dst.lnt */
    // TODO Check these are correct versions
    N = CHECK_C(GET_SIGN_W(UInt(t))) /* set cc's from diff */
    Z = GET_Z(UInt(t))
    V = CHECK_C(GET_SIGN_W(UInt((R(0).get16 ^ R(2).get16) & (~R(2).get16 ^ t))))
    C = R(0).get16 < R(2).get16
    mvlnt
  }

  /* Common cleanup routine for MOVC class instructions */

  def movx_cleanup(op: Int): Unit = {
    SP((SP + 2) & 0xffff) /* discard mvlnt */
    if ((op & INLINE) != 0) {
      /* inline? */
      R(0).set16(MMU.ReadW(UInt(SP | MMU.dsenable)).intValue) /* restore R0 - R5 */
      R(1).set16(MMU.ReadW(UInt(((SP + 2) & 0xffff) | MMU.dsenable)).intValue)
      R(2).set16(MMU.ReadW(UInt(((SP + 4) & 0xffff) | MMU.dsenable)).intValue)
      R(3).set16(MMU.ReadW(UInt(((SP + 6) & 0xffff) | MMU.dsenable)).intValue)
      R(4).set16(MMU.ReadW(UInt(((SP + 8) & 0xffff) | MMU.dsenable)).intValue)
      R(5).set16(MMU.ReadW(UInt(((SP + 10) & 0xffff) | MMU.dsenable)).intValue)

      SP((SP + 12) & 0xffff)
    }
    else {
      /* reg, clear R1 - R3 */
      R(1).set16(0)
      R(2).set16(0)
      R(3).set16(0)
    }
    fpd = 0 /* instr done */
  }

  def SET_INT(ipl_dv:Int,int_dv:Int ) = {int_req(ipl_dv) = int_req(ipl_dv) |(int_dv) }
  def CLR_INT(ipl_dv:Int, int_dv:Int) = { int_req(ipl_dv) = int_req(ipl_dv) & ~(int_dv) }
  
  /* TODO Test for CIS mid-instruction interrupt */
  //  def cis_int_test (cycles:Int, oldpc:Int):Boolean =
  //  {
  //    var cycles2 = cycles
  //    while (cycles2 >= 0) {                                   /* until delay done */
  //      if (SimTimer.sim_interval > cycles2) {                        /* event > delay */
  //        SimTimer.sim_interval = SimTimer.sim_interval - cycles2
  //        cycles2 = -1
  //      }
  //      else {                                              /* event <= delay */
  //        cycles2 = cycles2 - SimTimer.sim_interval                 /* decr delay */
  //        SimTimer.sim_interval = 0                               /* process event */
  //        *st = machine.eventQueue.processEvent()
  //        trap_req = calc_ints (ipl, trap_req)           /* recalc int req */
  //        if ((*st != SCPE_OK) ||                         /* bad status or */
  //          trap_req & PDP11.TRAP_INT) {                      /* interrupt? */
  //          PC(oldpc)                                 /* back out */
  //          return true
  //        }                                           /* end if stop */
  //      }                                               /* end else event */
  //    }                                                   /* end while delay */
  //    return false
  //  }
}

object PDP11 {
  /* Architectural constants */

  val STKL_R: UInt = UInt(0xe0)
  /* stack limit */
  val STKL_Y: UInt = UInt(0x100)
  val VASIZE: UInt = UInt(0x10000) // 2**16
  val VAMASK: Int = VASIZE - 1 // 2**16 - 1
  val MEMSIZE64K: UInt = UInt(0x10000) //  2**16
  val UNIMEMSIZE: UInt = UInt(0x40000) // 2**18
  val UNIMASK: Int = UNIMEMSIZE - 1 // 2**18 - 1
  val IOPAGEBASE: UInt = UInt(0x3fe000) // 2**22 - 2**13
  val IOPAGESIZE: UInt = UInt(0x2000) // 2**13
  val IOPAGEMASK: Int = IOPAGESIZE - 1 // 2**13 - 1
  val MAXMEMSIZE: UInt = UInt(0x400000) // 2**22
  val PAMASK: Int = MAXMEMSIZE - 1 // 2**22 - 1
  val DMASK: UInt = UInt(0xffff)
  val BMASK: UInt = UInt(0xff)

  /* PSW */

  val PSW_V_C = 0
  /* condition codes */
  val PSW_V_V = 1
  val PSW_V_Z = 2
  val PSW_V_N = 3
  val PSW_V_TBIT = 4 // trace trap
  val PSW_V_IPL = 5 // int priority
  val PSW_V_FPD = 8 // first part done
  val PSW_V_RS = 11 // register set
  val PSW_V_PM = 12 // previous mode
  val PSW_V_CM = 14 // current mode
  val PSW_CC = 0xf
  val PSW_TBIT: Int = 1 << PSW_V_TBIT
  val PSW_PM: Int = 3 << PSW_V_PM

  // PSW Bits
  val BIT_C: Int = 2 ^ 0 // Carry
  val BIT_V: Int = 2 ^ 1 // Overflow
  val BIT_Z: Int = 2 ^ 2 // Zero
  val BIT_N: Int = 2 ^ 3 // Negative
  val BIT_TBIT: Int = 2 ^ 4 // Trace Trap
  val BIT_IPL: Int = 2 ^ 5 // IPL - 3 bits
  val BIT_FPD: Int = 2 ^ 8 // First Part Done
  val BIT_NCF: Int = 2 ^ 9 // Must Be Zero - 2 bits
  val BIT_RS: Int = 2 ^ 11 // Register Set
  val BIT_PM: Int = 2 ^ 12 // Previous access mode, 2 bits
  val BIT_CM: Int = 2 ^ 14 // Current access mode, 2 bits

  /* Trap data structures */
  val VEC_RED: UInt = UInt(0x4)
  /* trap vectors */
  val VEC_ODD: UInt = UInt(0x4)
  val VEC_MME: UInt = UInt(0xa8)
  val VEC_NXM: UInt = UInt(0x4)
  val VEC_PAR: UInt = UInt(0x4c)
  val VEC_PRV: UInt = UInt(0x4)
  val VEC_ILL: UInt = UInt(0x8)
  val VEC_BPT: UInt = UInt(0xc)
  val VEC_IOT: UInt = UInt(0x10)
  val VEC_EMT: UInt = UInt(0x18)
  val VEC_TRAP: UInt = UInt(0x1c)
  val VEC_TRC: UInt = UInt(0xc)
  val VEC_YEL: UInt = UInt(0x4)
  val VEC_PWRFL: UInt = UInt(0x14)
  val VEC_FPE: UInt = UInt(0xa4)

  val trap_vec: Array[UInt] = Array(/* trap req to vector */
    VEC_RED, VEC_ODD, VEC_MME, VEC_NXM,
    VEC_PAR, VEC_PRV, VEC_ILL, VEC_BPT,
    VEC_IOT, VEC_EMT, VEC_TRAP, VEC_TRC,
    VEC_YEL, VEC_PWRFL, VEC_FPE
  )

  /* Trap masks, descending priority order, following J-11
   An interrupt summary bit is kept with traps, to minimize overhead
*/

  val TRAP_V_RED = 0 //red stk abort  4
  val TRAP_V_ODD = 1 //odd address    4
  val TRAP_V_MME = 2 //mem mgt      250
  val TRAP_V_NXM = 3 //nx memory      4
  val TRAP_V_PAR = 4 //parity err   114
  val TRAP_V_PRV = 5 //priv inst      4
  val TRAP_V_ILL = 6 //illegal inst  10
  val TRAP_V_BPT = 7 //BPT           14
  val TRAP_V_IOT = 8 //IOT           20
  val TRAP_V_EMT = 9 //EMT           30
  val TRAP_V_TRAP = 10 // TRAP          34
  val TRAP_V_TRC = 11 //T bit         14
  val TRAP_V_YEL = 12 //stack          4
  val TRAP_V_PWRFL = 13 // power fail    24
  val TRAP_V_FPE = 14 // fpe          244
  val TRAP_V_MAX = 15 // intr = max trp #
  val ABRT_V_BKPT = 16 // stop due to breakpt
  val TRAP_RED: UInt = UInt(1) << TRAP_V_RED
  val TRAP_ODD: UInt = UInt(1) << TRAP_V_ODD
  val TRAP_MME: UInt = UInt(1) << TRAP_V_MME
  val TRAP_NXM: UInt = UInt(1) << TRAP_V_NXM
  val TRAP_PAR: UInt = UInt(1) << TRAP_V_PAR
  val TRAP_PRV: UInt = UInt(1) << TRAP_V_PRV
  val TRAP_ILL: UInt = UInt(1) << TRAP_V_ILL
  val TRAP_BPT: UInt = UInt(1) << TRAP_V_BPT
  val TRAP_IOT: UInt = UInt(1) << TRAP_V_IOT
  val TRAP_EMT: UInt = UInt(1) << TRAP_V_EMT
  val TRAP_TRAP: UInt = UInt(1) << TRAP_V_TRAP
  val TRAP_TRC: UInt = UInt(1) << TRAP_V_TRC
  val TRAP_YEL: UInt = UInt(1) << TRAP_V_YEL
  val TRAP_PWRFL: UInt = UInt(1) << TRAP_V_PWRFL
  val TRAP_FPE: UInt = UInt(1) << TRAP_V_FPE
  val TRAP_INT: UInt = UInt(1) << TRAP_V_MAX
  val TRAP_ALL: UInt = UInt((UInt(1) << TRAP_V_MAX) - 1)
  /* all traps */
  val ABRT_BKPT: UInt = UInt(1) << ABRT_V_BKPT


  val trap_clear: Array[UInt] = Array(/* trap clears */
    TRAP_RED + TRAP_PAR + TRAP_YEL + TRAP_TRC + TRAP_ODD + TRAP_NXM,
    TRAP_ODD + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_MME + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_NXM + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_PAR + TRAP_TRC, TRAP_PRV + TRAP_TRC,
    TRAP_ILL + TRAP_TRC, TRAP_BPT + TRAP_TRC,
    TRAP_IOT + TRAP_TRC, TRAP_EMT + TRAP_TRC,
    TRAP_TRAP + TRAP_TRC, TRAP_TRC,
    TRAP_YEL, TRAP_PWRFL, TRAP_FPE
  )
  /* CPUERR */

  val CPUE_RED: UInt = UInt(0x4) // red stack
  val CPUE_YEL: UInt = UInt(0x8) // yellow stack
  val CPUE_TMO: UInt = UInt(0x10) // IO page nxm
  val CPUE_NXM: UInt = UInt(0x20) // memory nxm
  val CPUE_ODD: UInt = UInt(0x40) // odd address
  val CPUE_HALT: UInt = UInt(0x80) // HALT not kernel
  val CPUE_IMP: UInt = UInt(0xfc) // implemented bits

  /* Unibus I/O page layout - see pdp11_io_lib.c for address layout details
    Massbus devices (RP, TU) do not appear in the Unibus IO page */

  val IOBA_AUTO = 0 /* Assigned by Auto Configure */

  /* Processor registers which have I/O page addresses
   */

  val IOBA_CTL: Int = IOPAGEBASE + 0x1f50
  /* board ctrl */
  val IOLN_CTL = 0x8

  val IOBA_UCA: Int = IOPAGEBASE + 0xff8
  /* UC15 DR11 #1 */
  val IOLN_UCA = 0x6
  val IOBA_UCB: Int = IOPAGEBASE + 0xff0
  /* UC15 DR11 #2 */
  val IOLN_UCB = 0x6
  val IOBA_UBM: Int = IOPAGEBASE + 0x1080
  /* Unibus map */
  //val IOLN_UBM     =   (UBM_LNT_LW * sizeof (int32))
  val IOBA_MMR3: Int = IOPAGEBASE + 0x154e
  /* MMR3 */
  val IOLN_MMR3 = 0x2
  val IOBA_TTI: Int = IOPAGEBASE + 0x1f70
  /* DL11 rcv */
  val IOLN_TTI = 0x4
  val IOBA_TTO: Int = IOPAGEBASE + 0x1f74
  /* DL11 xmt */
  val IOLN_TTO = 0x4
  val IOBA_SR: Int = IOPAGEBASE + 0x1f78
  /* SR */
  val IOLN_SR = 0x2
  val IOBA_MMR012: Int = IOPAGEBASE + 0x1f7a
  /* MMR0-2 */
  val IOLN_MMR012 = 0x6
  val IOBA_GPR: Int = IOPAGEBASE + 0x1fc0
  /* GPR's */
  val IOLN_GPR = 0x8
  val IOBA_UCTL: Int = IOPAGEBASE + 0x1fd8
  /* UBA ctrl */
  val IOLN_UCTL = 0x8
  val IOBA_CPU: Int = IOPAGEBASE + 0x1fe0
  /* CPU reg */
  val IOLN_CPU = 0x1e
  val IOBA_PSW: Int = IOPAGEBASE + 0x1ffe
  /* PSW */
  val IOLN_PSW = 0x2
  val IOBA_UIPDR: Int = IOPAGEBASE + 0x1f80
  /* user APR's */
  val IOLN_UIPDR = 0x10
  val IOBA_UDPDR: Int = IOPAGEBASE + 0x1f90
  val IOLN_UDPDR = 0x10
  val IOBA_UIPAR: Int = IOPAGEBASE + 0x1fa0
  val IOLN_UIPAR = 0x10
  val IOBA_UDPAR: Int = IOPAGEBASE + 0x1fb0
  val IOLN_UDPAR = 0x10
  val IOBA_SUP: Int = IOPAGEBASE + 0x1480
  /* supervisor APR's */
  val IOLN_SUP = 0x40
  val IOBA_KIPDR: Int = IOPAGEBASE + 0x14c0
  /* kernel APR's */
  val IOLN_KIPDR = 0x10
  val IOBA_KDPDR: Int = IOPAGEBASE + 0x14d0
  val IOLN_KDPDR = 0x10
  val IOBA_KIPAR: Int = IOPAGEBASE + 0x14e0
  val IOLN_KIPAR = 0x10
  val IOBA_KDPAR: Int = IOPAGEBASE + 0x14f0
  val IOLN_KDPAR = 0x10

  /* Interrupt assignments; within each level, priority is right to left
     PIRQn has the highest priority with a level and is always bit <0>
     On level 6, the clock is second highest priority */

  val IPL_HLVL = 8
  /* # int levels */
  val IPL_HMIN = 4 /* lowest IO int level */

  val INT_V_PIR7 = 0
  /* BR7 */
  val INT_V_UCA = 1

  val INT_V_PIR6 = 0
  /* BR6 */
  val INT_V_CLK = 1
  val INT_V_PCLK = 2
  val INT_V_DTA = 3
  val INT_V_TA = 4
  val INT_V_CR = 5 /* CR11 */

  val INT_V_PIR5 = 0
  /* BR5 */
  val INT_V_RK = 1
  val INT_V_RL = 2
  val INT_V_RX = 3
  val INT_V_TM = 4
  val INT_V_RP = 5
  val INT_V_TS = 6
  val INT_V_HK = 7
  val INT_V_RQ = 8
  val INT_V_DZRX = 9
  val INT_V_DZTX = 10
  val INT_V_TQ = 11
  val INT_V_RY = 12
  val INT_V_XQ = 13
  val INT_V_XU = 14
  val INT_V_TU = 15
  val INT_V_RF = 16
  val INT_V_RC = 17
  val INT_V_RS = 18
  val INT_V_DMCRX = 19
  val INT_V_DMCTX = 20
  val INT_V_DUPRX = 21
  val INT_V_DUPTX = 22
  val INT_V_KMCA = 23
  val INT_V_KMCB = 24
  val INT_V_UCB = 25
  val INT_V_CH = 26
  val INT_V_NG = 27

  val INT_V_PIR4 = 0
  /* BR4 */
  val INT_V_TTI = 1
  val INT_V_TTO = 2
  val INT_V_PTR = 3
  val INT_V_PTP = 4
  val INT_V_LPT = 5
  val INT_V_VHRX = 6
  val INT_V_VHTX = 7
  val INT_V_CD = 8
  /* CD11 */
  val INT_V_DLI = 9
  val INT_V_DLO = 10
  val INT_V_DCI = 11
  val INT_V_DCO = 12
  /* VT simulation is sequential, so only
     one interrupt is posted at a time. */
  val INT_V_VTST = 13
  val INT_V_VTLP = 14
  val INT_V_VTCH = 15
  val INT_V_VTNM = 16
  val INT_V_LK = 17
  val INT_V_TDRX = 18
  val INT_V_TDTX = 19

  val INT_V_PIR3 = 0
  /* BR3 */
  val INT_V_PIR2 = 0
  /* BR2 */
  val INT_V_PIR1 = 0 /* BR1 */

  val INT_PIR7: UInt = UInt(1) << INT_V_PIR7
  val INT_UCB: UInt = UInt(1) << INT_V_UCB
  val INT_PIR6: UInt = UInt(1) << INT_V_PIR6
  val INT_CLK: UInt = UInt(1) << INT_V_CLK
  val INT_PCLK: UInt = UInt(1) << INT_V_PCLK
  val INT_DTA: UInt = UInt(1) << INT_V_DTA
  val INT_TA: UInt = UInt(1) << INT_V_TA
  val INT_CR: UInt = UInt(1) << INT_V_CR
  val INT_PIR5: UInt = UInt(1) << INT_V_PIR5
  val INT_RK: UInt = UInt(1) << INT_V_RK
  val INT_RL: UInt = UInt(1) << INT_V_RL
  val INT_RX: UInt = UInt(1) << INT_V_RX
  val INT_TM: UInt = UInt(1) << INT_V_TM
  val INT_RP: UInt = UInt(1) << INT_V_RP
  val INT_TS: UInt = UInt(1) << INT_V_TS
  val INT_HK: UInt = UInt(1) << INT_V_HK
  val INT_RQ: UInt = UInt(1) << INT_V_RQ
  val INT_DZRX: UInt = UInt(1) << INT_V_DZRX
  val INT_DZTX: UInt = UInt(1) << INT_V_DZTX
  val INT_TQ: UInt = UInt(1) << INT_V_TQ
  val INT_RY: UInt = UInt(1) << INT_V_RY
  val INT_XQ: UInt = UInt(1) << INT_V_XQ
  val INT_XU: UInt = UInt(1) << INT_V_XU
  val INT_TU: UInt = UInt(1) << INT_V_TU
  val INT_RF: UInt = UInt(1) << INT_V_RF
  val INT_RC: UInt = UInt(1) << INT_V_RC
  val INT_RS: UInt = UInt(1) << INT_V_RS
  val INT_DMCRX: UInt = UInt(1) << INT_V_DMCRX
  val INT_DMCTX: UInt = UInt(1) << INT_V_DMCTX
  val INT_KMCA: UInt = UInt(1) << INT_V_KMCA
  val INT_KMCB: UInt = UInt(1) << INT_V_KMCB
  val INT_DUPRX: UInt = UInt(1) << INT_V_DUPRX
  val INT_DUPTX: UInt = UInt(1) << INT_V_DUPTX
  val INT_UCA: UInt = UInt(1) << INT_V_UCA
  val INT_PIR4: UInt = UInt(1) << INT_V_PIR4
  val INT_TTI: UInt = UInt(1) << INT_V_TTI
  val INT_TTO: UInt = UInt(1) << INT_V_TTO
  val INT_PTR: UInt = UInt(1) << INT_V_PTR
  val INT_PTP: UInt = UInt(1) << INT_V_PTP
  val INT_LPT: UInt = UInt(1) << INT_V_LPT
  val INT_VHRX: UInt = UInt(1) << INT_V_VHRX
  val INT_VHTX: UInt = UInt(1) << INT_V_VHTX
  val INT_CD: UInt = UInt(1) << INT_V_CD
  val INT_DLI: UInt = UInt(1) << INT_V_DLI
  val INT_DLO: UInt = UInt(1) << INT_V_DLO
  val INT_DCI: UInt = UInt(1) << INT_V_DCI
  val INT_DCO: UInt = UInt(1) << INT_V_DCO
  val INT_VTLP: UInt = UInt(1) << INT_V_VTLP
  val INT_VTST: UInt = UInt(1) << INT_V_VTST
  val INT_VTCH: UInt = UInt(1) << INT_V_VTCH
  val INT_VTNM: UInt = UInt(1) << INT_V_VTNM
  val INT_LK: UInt = UInt(1) << INT_V_LK
  val INT_PIR3: UInt = UInt(1) << INT_V_PIR3
  val INT_PIR2: UInt = UInt(1) << INT_V_PIR2
  val INT_PIR1: UInt = UInt(1) << INT_V_PIR1
  val INT_TDRX: UInt = UInt(1) << INT_V_TDRX
  val INT_TDTX: UInt = UInt(1) << INT_V_TDTX
  val INT_CH: UInt = UInt(1) << INT_V_CH
  val INT_NG: UInt = UInt(1) << INT_V_NG

  val INT_INTERNAL7: UInt = INT_PIR7
  val INT_INTERNAL6: UInt = INT_PIR6 | INT_CLK
  val INT_INTERNAL5: UInt = INT_PIR5
  val INT_INTERNAL4: UInt = INT_PIR4
  val INT_INTERNAL3: UInt = INT_PIR3
  val INT_INTERNAL2: UInt = INT_PIR2
  val INT_INTERNAL1: UInt = INT_PIR1

  val IPL_UCB = 7
  /* int pri levels */
  val IPL_CLK = 6
  val IPL_PCLK = 6
  val IPL_DTA = 6
  val IPL_TA = 6
  val IPL_CR = 6
  val IPL_RK = 5
  val IPL_RL = 5
  val IPL_RX = 5
  val IPL_TM = 5
  val IPL_RP = 5
  val IPL_TS = 5
  val IPL_HK = 5
  val IPL_RQ = 5
  val IPL_DZRX = 5
  val IPL_DZTX = 5
  val IPL_TQ = 5
  val IPL_RY = 5
  val IPL_XQ = 5
  val IPL_XU = 5
  val IPL_CH = 5
  val IPL_TU = 5
  val IPL_RF = 5
  val IPL_RC = 5
  val IPL_RS = 5
  val IPL_DMCRX = 5
  val IPL_DMCTX = 5
  val IPL_KMCA = 5
  val IPL_KMCB = 5
  val IPL_DUPRX = 5
  val IPL_DUPTX = 5
  val IPL_UCA = 5
  val IPL_NG = 5
  val IPL_PTR = 4
  val IPL_PTP = 4
  val IPL_TTI = 4
  val IPL_TTO = 4
  val IPL_LPT = 4
  val IPL_VHRX = 4
  val IPL_VHTX = 4
  val IPL_CD = 4
  val IPL_DLI = 4
  val IPL_DLO = 4
  val IPL_DCI = 4
  val IPL_DCO = 4
  val IPL_VTLP = 4
  val IPL_VTST = 4
  val IPL_VTCH = 4
  val IPL_VTNM = 4
  val IPL_LK = 4
  /* XXX just a guess */
  val IPL_TDRX = 4
  val IPL_TDTX = 4

  val IPL_PIR7 = 7
  val IPL_PIR6 = 6
  val IPL_PIR5 = 5
  val IPL_PIR4 = 4
  val IPL_PIR3 = 3
  val IPL_PIR2 = 2
  val IPL_PIR1 = 1

  /* Device vectors */

  val VEC_AUTO = 0
  /* Assigned by Auto Configure */
  val VEC_FLOAT = 0 /* Assigned by Auto Configure */

  /* Processor specific internal fixed vectors */
  val VEC_PIRQ = 0xa0
  val VEC_TTI = 0x30
  val VEC_TTO = 0x34
  val VEC_UCA = 0xc0
  val VEC_UCB = 0xc8

  // Device CSRs */
  val CSR_V_GO =0 // go */
  val CSR_V_IE =6 // interrupt enable */
  val CSR_V_DONE =7 // done */
  val CSR_V_BUSY =11 // busy */
  val CSR_V_ERR =15 // error */
  val CSR_GO =(1 << CSR_V_GO)
  val CSR_IE =(1 << CSR_V_IE)
  val CSR_DONE =(1 << CSR_V_DONE)
  val CSR_BUSY =(1 << CSR_V_BUSY)
  val CSR_ERR =(1 << CSR_V_ERR)


}
