package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}
import com.sim.{Console, SimTimer, Utils}

import scala.annotation.switch
import com.sim.unsigned.ushort2Int
import com.sim.unsigned.ubyte2Int
import com.sim.unsigned.ushort2uint
import com.sim.unsigned.ubyte2uint
import com.sim.unsigned.uint2int

import scala.collection.mutable
import scala.language.implicitConversions

//noinspection ScalaUnusedSymbol
class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "Z80"
  override val MMU: Z80MMU = new Z80MMU(this)
  override val description: String = "Z80 CPU"

  override def init(): Unit = {} // TODO

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override def handles(value: UInt): Boolean = ???

  var tStates: Long = 0L

  // Default timer interrupt handler routine
  val timerInterruptHandler: Int = 0xFC00

  // Default keyboard interrupt handler routine
  val keyboardInterruptHandler: Int = 0x0038

  override def createUnitOptions(): Unit = {
    // Set up CPU common options.
    super.createUnitOptions()

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("ALTAIRROM", "Enable Altair ROM.", value = false))
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Break on HALT instruction.", value = false))
    unitOptions.append(ValueUnitOption("MEMORY", "Set the RAM size.", value = 0xFFFF))

  }

  override def resetCPU(): Unit = {

    tStates = 0L
    PC(0x0000)
    HL(0x0000)
    BC(0x0000)
    AF(0x0000)
    DE(0x0000)
    IX(0x0000)
    IY(0x0000)
    SP(0x0000)
    AF(0x0000)
    R(0x0000)
    I(0x00) // Interrupts disabled
    IFF(0x00)
    HLP(0x0000)
    BCP(0x0000)
    AFP(0x0000)
    DEP(0x0000)
    AFP(0x0000)

  }

  override def showCommand(stringBuilder: mutable.StringBuilder): Unit = {
    super.showCommand(stringBuilder)
  }

  val H = new Register8("H")
  val L = new Register8("L")
  val A = new Register8("A")
  val F = new Register8("F")
  val B = new Register8("B")
  val C = new Register8("C")
  val D = new Register8("D")
  val E = new Register8("E")
  val I = new Register8("I")
  val R = new Register8("R")
  val IR = new CompositeRegister16("IR", I, R) // Interrupt/Refresh
  val IFF = new Register8("IFF") // Interrupt ena/disable
  val HP = new Register8("H'")
  val LP = new Register8("L'")
  val AP = new Register8("A'")
  val FP = new Register8("F'")
  val BP = new Register8("B'")
  val CP = new Register8("C'")
  val DP = new Register8("D'")
  val EP = new Register8("E'")
  val HL = new CompositeRegister16("HL", H, L)
  val AF = new CompositeRegister16("AF", A, F)
  val BC = new CompositeRegister16("BC", B, C)
  val DE = new CompositeRegister16("DE", D, E)
  val HLP = new CompositeRegister16("HL'", HP, LP)
  val AFP = new CompositeRegister16("AF'", AP, FP)
  val BCP = new CompositeRegister16("BC'", BP, CP)
  val DEP = new CompositeRegister16("DE'", DP, EP)
  val IXH = new Register8("IXH") // Not real registers, but helpful to address as high/low
  val IXL = new Register8("IXL")
  val IYH = new Register8("IYH")
  val IYL = new Register8("IYL")
  val IX = new CompositeRegister16("IX", IXH, IXL)
  val IY = new CompositeRegister16("IY", IYH, IYL)
  val SP = new Register16("SP")
  val PC = new Register16("PC")

  override val registers: Map[String, Register] = Map("H" -> H, "L" -> L, "HL" -> HL,
    "B" -> B, "C" -> C, "BC" -> BC,
    "D" -> D, "E" -> E, "DE" -> DE,
    "A" -> A, "F" -> F, "AF" -> AF,
    "I" -> I, "IX" -> IX, "IY" -> IY,
    "SP" -> SP, "PC" -> PC, "R" -> R,
    "HP" -> HP, "LP" -> LP, "HLP" -> HLP,
    "BP" -> BP, "CP" -> CP, "BCP" -> BCP,
    "DP" -> DP, "EP" -> EP, "DEP" -> DEP,
    "AP" -> AP, "FP" -> FP, "AFP" -> AFP, "IFF" -> IFF, "IR" -> IR
  )

  resetCPU()

  override def showRegisters(): String = {
    s"$PC  $SP  $AF\nr\r$BC  $DE  $HL\n\r$IX  $IY  $R  $I\n\r$AFP $BCP $DEP $HLP\n\r$IFF"
  }

  // Z80 Flags
  final val FLAG_C = 1
  final val FLAG_N = 2
  final val FLAG_P = 4
  final val FLAG_H = 16
  final val FLAG_Z = 64
  final val FLAG_S = 128
  final val FLAG_3 = 0x0008
  final val FLAG_5 = 0x0020

  override def showFlags(): String = {
    val carry = if ((F & FLAG_C) != 0) true else false
    val addsub = if ((AF & FLAG_N) != 0) true else false
    val pv = if ((AF & FLAG_P) != 0) true else false
    val bit3 = if ((AF & 8) != 0) true else false
    val h = if ((AF & FLAG_H) != 0) true else false
    val bit5 = if ((AF & 32) != 0) true else false
    val z = if ((AF & FLAG_Z) != 0) true else false
    val s = if ((AF & FLAG_S) != 0) true else false
    val str = F.toBinaryString.replaceAll(" ", "0")
    s"\n\rF=$str  :  S=$s  Z=$z  H=$h  P/V=$pv  N=$addsub  C=$carry"

  }

  override def runcpu(singleStep: Boolean, startAddr: UInt): Unit = {
    // Force the PC
    PC(startAddr.intValue)
    runcpu(singleStep)
  }

  /** First level instruction jump table */
  private val execMap: Map[Int, Function[Int, Unit]] = Map[Int, Int => Unit](
    0x00 -> fn0x00, 0x01 -> fn0x01, 0x02 -> fn0x02, 0x03 -> fn0x03, 0x04 -> fn0x04, 0x05 -> fn0x05,
    0x06 -> fn0x06, 0x07 -> fn0x07, 0x08 -> fn0x08, 0x09 -> fn0x09, 0x0a -> fn0x0a, 0x0b -> fn0x0b,
    0x0c -> fn0x0c, 0x0d -> fn0x0d, 0x0e -> fn0x0e, 0x0f -> fn0x0f,

    0x10 -> fn0x10, 0x11 -> fn0x11, 0x12 -> fn0x12, 0x13 -> fn0x13, 0x14 -> fn0x14, 0x15 -> fn0x15,
    0x16 -> fn0x16, 0x17 -> fn0x17, 0x18 -> fn0x18, 0x19 -> fn0x19, 0x1a -> fn0x1a, 0x1b -> fn0x1b,
    0x1c -> fn0x1c, 0x1d -> fn0x1d, 0x1e -> fn0x1e, 0x1f -> fn0x1f,

    0x20 -> fn0x20, 0x21 -> fn0x21, 0x22 -> fn0x22, 0x23 -> fn0x23, 0x24 -> fn0x24, 0x25 -> fn0x25,
    0x26 -> fn0x26, 0x27 -> fn0x27, 0x28 -> fn0x28, 0x29 -> fn0x29, 0x2a -> fn0x2a, 0x2b -> fn0x2b,
    0x2c -> fn0x2c, 0x2d -> fn0x2d, 0x2e -> fn0x2e, 0x2f -> fn0x2f,

    0x30 -> fn0x30, 0x31 -> fn0x31, 0x32 -> fn0x32, 0x33 -> fn0x33, 0x34 -> fn0x34, 0x35 -> fn0x35,
    0x36 -> fn0x36, 0x37 -> fn0x37, 0x38 -> fn0x38, 0x39 -> fn0x39, 0x3a -> fn0x3a, 0x3b -> fn0x3b,
    0x3c -> fn0x3c, 0x3d -> fn0x3d, 0x3e -> fn0x3e, 0x3f -> fn0x3f,

    0x40 -> fn0x40, 0x41 -> fn0x41, 0x42 -> fn0x42, 0x43 -> fn0x43, 0x44 -> fn0x44, 0x45 -> fn0x45,
    0x46 -> fn0x46, 0x47 -> fn0x47, 0x48 -> fn0x48, 0x49 -> fn0x49, 0x4a -> fn0x4a, 0x4b -> fn0x4b,
    0x4c -> fn0x4c, 0x4d -> fn0x4d, 0x4e -> fn0x4e, 0x4f -> fn0x4f,

    0x50 -> fn0x50, 0x51 -> fn0x51, 0x52 -> fn0x52, 0x53 -> fn0x53, 0x54 -> fn0x54, 0x55 -> fn0x55,
    0x56 -> fn0x56, 0x57 -> fn0x57, 0x58 -> fn0x58, 0x59 -> fn0x59, 0x5a -> fn0x5a, 0x5b -> fn0x5b,
    0x5c -> fn0x5c, 0x5d -> fn0x5d, 0x5e -> fn0x5e, 0x5f -> fn0x5f,

    0x60 -> fn0x60, 0x61 -> fn0x61, 0x62 -> fn0x62, 0x63 -> fn0x63, 0x64 -> fn0x64, 0x65 -> fn0x65,
    0x66 -> fn0x66, 0x67 -> fn0x67, 0x68 -> fn0x68, 0x69 -> fn0x69, 0x6a -> fn0x6a, 0x6b -> fn0x6b,
    0x6c -> fn0x6c, 0x6d -> fn0x6d, 0x6e -> fn0x6e, 0x6f -> fn0x6f,

    0x70 -> fn0x70, 0x71 -> fn0x71, 0x72 -> fn0x72, 0x73 -> fn0x73, 0x74 -> fn0x74, 0x75 -> fn0x75,
    0x76 -> fn0x76, 0x77 -> fn0x77, 0x78 -> fn0x78, 0x79 -> fn0x79, 0x7a -> fn0x7a, 0x7b -> fn0x7b,
    0x7c -> fn0x7c, 0x7d -> fn0x7d, 0x7e -> fn0x7e, 0x7f -> fn0x7f,

    0x80 -> fn0x80, 0x81 -> fn0x81, 0x82 -> fn0x82, 0x83 -> fn0x83, 0x84 -> fn0x84, 0x85 -> fn0x85,
    0x86 -> fn0x86, 0x87 -> fn0x87, 0x88 -> fn0x88, 0x89 -> fn0x89, 0x8a -> fn0x8a, 0x8b -> fn0x8b,
    0x8c -> fn0x8c, 0x8d -> fn0x8d, 0x8e -> fn0x8e, 0x8f -> fn0x8f,

    0x90 -> fn0x90, 0x91 -> fn0x91, 0x92 -> fn0x92, 0x93 -> fn0x93, 0x94 -> fn0x94, 0x95 -> fn0x95,
    0x96 -> fn0x96, 0x97 -> fn0x97, 0x98 -> fn0x98, 0x99 -> fn0x99, 0x9a -> fn0x9a, 0x9b -> fn0x9b,
    0x9c -> fn0x9c, 0x9d -> fn0x9d, 0x9e -> fn0x9e, 0x9f -> fn0x9f,

    0xa0 -> fn0xa0, 0xa1 -> fn0xa1, 0xa2 -> fn0xa2, 0xa3 -> fn0xa3, 0xa4 -> fn0xa4, 0xa5 -> fn0xa5,
    0xa6 -> fn0xa6, 0xa7 -> fn0xa7, 0xa8 -> fn0xa8, 0xa9 -> fn0xa9, 0xaa -> fn0xaa, 0xab -> fn0xab,
    0xac -> fn0xac, 0xad -> fn0xad, 0xae -> fn0xae, 0xaf -> fn0xaf,

    0xb0 -> fn0xb0, 0xb1 -> fn0xb1, 0xb2 -> fn0xb2, 0xb3 -> fn0xb3, 0xb4 -> fn0xb4, 0xb5 -> fn0xb5,
    0xb6 -> fn0xb6, 0xb7 -> fn0xb7, 0xb8 -> fn0xb8, 0xb9 -> fn0xb9, 0xba -> fn0xba, 0xbb -> fn0xbb,
    0xbc -> fn0xbc, 0xbd -> fn0xbd, 0xbe -> fn0xbe, 0xbf -> fn0xbf,

    0xc0 -> fn0xc0, 0xc1 -> fn0xc1, 0xc2 -> fn0xc2, 0xc3 -> fn0xc3, 0xc4 -> fn0xc4, 0xc5 -> fn0xc5,
    0xc6 -> fn0xc6, 0xc7 -> fn0xc7, 0xc8 -> fn0xc8, 0xc9 -> fn0xc9, 0xca -> fn0xca, 0xcb -> fn0xcb,
    0xcc -> fn0xcc, 0xcd -> fn0xcd, 0xce -> fn0xce, 0xcf -> fn0xcf,

    0xd0 -> fn0xd0, 0xd1 -> fn0xd1, 0xd2 -> fn0xd2, 0xd3 -> fn0xd3, 0xd4 -> fn0xd4, 0xd5 -> fn0xd5,
    0xd6 -> fn0xd6, 0xd7 -> fn0xd7, 0xd8 -> fn0xd8, 0xd9 -> fn0xd9, 0xda -> fn0xda, 0xdb -> fn0xdb,
    0xdc -> fn0xdc, 0xdd -> fn0xdd, 0xde -> fn0xde, 0xdf -> fn0xdf,

    0xe0 -> fn0xe0, 0xe1 -> fn0xe1, 0xe2 -> fn0xe2, 0xe3 -> fn0xe3, 0xe4 -> fn0xe4, 0xe5 -> fn0xe5,
    0xe6 -> fn0xe6, 0xe7 -> fn0xe7, 0xe8 -> fn0xe8, 0xe9 -> fn0xe9, 0xea -> fn0xea, 0xeb -> fn0xeb,
    0xec -> fn0xec, 0xed -> fn0xed, 0xee -> fn0xee, 0xef -> fn0xef,

    0xf0 -> fn0xf0, 0xf1 -> fn0xf1, 0xf2 -> fn0xf2, 0xf3 -> fn0xf3, 0xf4 -> fn0xf4, 0xf5 -> fn0xf5,
    0xf6 -> fn0xf6, 0xf7 -> fn0xf7, 0xf8 -> fn0xf8, 0xf9 -> fn0xf9, 0xfa -> fn0xfa, 0xfb -> fn0xfb,
    0xfc -> fn0xfc, 0xfd -> fn0xfd, 0xfe -> fn0xfe, 0xff -> fn0xff
  )

  // NOP
  private final def fn0x00(x: Int): Unit = {
    addTStates(4)
  }

  // LD BC, nnnn
  private final def fn0x01(x: Int): Unit = {
    addTStates(10)
    BC(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (BC),A
  private final def fn0x02(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(BC)
    MMU.put8(BC, A)
  }

  // INC BC
  private final def fn0x03(x: Int): Unit = {
    addTStates(6)
    BC.increment()
  }

  // INC B
  private final def fn0x04(x: Int): Unit = {
    addTStates(4)
    INC(B)
  }

  // DEC B
  private final def fn0x05(x: Int): Unit = {
    addTStates(4)
    DEC(B)
  }

  // LD B,nn
  private final def fn0x06(x: Int): Unit = {
    addTStates(7)
    B(MMU.get8PC(PC))
  }

  // RLCA
  private final def fn0x07(x: Int): Unit = {
    addTStates(4)
    AF(((AF.get16 >> 7) & 0x0128) | ((AF.get16 << 1) & ~0x1ff) | (AF.get16 & 0xc4) | ((AF.get16 >> 15) & 1))
  }

  // EX AF, AF'
  private final def fn0x08(x: Int): Unit = {
    addTStates(4)
    AF.swap(AFP)
  }

  // ADD HL, BC
  private final def fn0x09(x: Int): Unit = {
    addTStates(11)
    ADD(HL, BC)
  }

  // LD A, (BC)
  private final def fn0x0a(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(BC)
    A(MMU.get8(BC))
  }

  // DEC BC
  private final def fn0x0b(x: Int): Unit = {
    addTStates(6)
    BC.decrement()
  }

  // INC C
  private final def fn0x0c(x: Int): Unit = {
    addTStates(4)
    INC(C)
  }

  // DEC C
  private final def fn0x0d(x: Int): Unit = {
    addTStates(4)
    DEC(C)
  }

  // LD C,nn
  private final def fn0x0e(x: Int): Unit = {
    addTStates(7)
    C(MMU.get8PC(PC))
  }

  // RRCA
  private final def fn0x0f(x: Int): Unit = {
    addTStates(4)
    AF((AF & 0xc4) | rrcaTable(A))
  }

  // DJNZ dd
  private final def fn0x10(x: Int): Unit = {
    B.decrement()
    if (B.get8() != 0) {
      // Jump
      addTStates(13)
      PC(PC.get16 + MMU.get8(PC).byteValue + 1)
    } else {
      PC.increment()
      addTStates(8)
    }
  }

  // LD DE, nnnn
  private final def fn0x11(x: Int): Unit = {
    addTStates(10)
    DE(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (DE),A
  private final def fn0x12(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(DE)
    MMU.put8(DE, A)
  }

  // INC DE
  private final def fn0x13(x: Int): Unit = {
    addTStates(6)
    DE.increment()
  }

  // INC D
  private final def fn0x14(x: Int): Unit = {
    addTStates(4)
    INC(D)
  }

  // DEC D
  private final def fn0x15(x: Int): Unit = {
    addTStates(4)
    DEC(D)
  }

  // LD D,nn
  private final def fn0x16(x: Int): Unit = {
    addTStates(7)
    D(MMU.get8PC(PC))
  }

  // RLA
  private final def fn0x17(x: Int): Unit = {
    addTStates(4)
    AF(((AF << 8) & 0x0100) | ((AF >> 7) & 0x28) | ((AF << 1) & ~0x01ff) |
      (AF & 0xc4) | ((AF >> 15) & 1))

  }

  // JR dd
  private final def fn0x18(x: Int): Unit = {
    addTStates(12)
    PC(PC + MMU.get8(PC).byteValue + 1)
  }

  // ADD HL, DE
  private final def fn0x19(x: Int): Unit = {
    addTStates(11)
    ADD(HL,DE)
  }

  // LD A, (DE)
  private final def fn0x1a(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(DE)
    A(MMU.get8(DE))
  }

  // DEC DE
  private final def fn0x1b(x: Int): Unit = {
    addTStates(6)
    DE.decrement()
  }

  // INC E
  private final def fn0x1c(x: Int): Unit = {
    addTStates(4)
    INC(E)
  }

  // DEC E
  private final def fn0x1d(x: Int): Unit = {
    addTStates(4)
    DEC(E)
  }

  // LD E,nn
  private final def fn0x1e(x: Int): Unit = {
    addTStates(7)
    E(MMU.get8PC(PC))
  }

  // RRA
  private final def fn0x1f(x: Int): Unit = {
    addTStates(4)
    AF(((AF & 1) << 15) | (AF & 0xc4) | rraTable(A))
  }

  // JR NZ,dd
  private final def fn0x20(x: Int): Unit = {

    if (testFlag(F, FLAG_Z)) { // Z flag set?
      addTStates(7)
      PC.increment()
    } else {
      PC(PC + MMU.get8(PC).byteValue + 1)
      addTStates(12)
    }
  }

  // LD HL, nnnn
  private final def fn0x21(x: Int): Unit = {
    addTStates(10)
    HL(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (nnnn), HL
  private final def fn0x22(x: Int): Unit = {
    addTStates(16)
    CHECK_LOG_WORD(PC)
    val dest: Int = MMU.get16(PC)
    MMU.put16(dest, HL)
    PC(PC + 2)
  }

  // INC HL
  private final def fn0x23(x: Int): Unit = {
    addTStates(6)
    HL.increment()
  }

  // INC H
  private final def fn0x24(x: Int): Unit = {
    addTStates(4)
    INC(H)
  }

  // DEC H
  private final def fn0x25(x: Int): Unit = {
    addTStates(4)
    DEC(H)
  }

  // LD H,nn
  private final def fn0x26(x: Int): Unit = {
    addTStates(7)
    H(MMU.get8PC(PC))
  }

  // DAA
  private final def fn0x27(x: Int): Unit = {
    addTStates(4)
    val ans:Int = A.get8().intValue()
    var incr: Int = 0
    var carry :Boolean = testFlag(F, FLAG_C)
    if((testFlag(F, FLAG_H)) || ((ans & 0x0f) > 0x09)) {incr = 0x06}
    if (carry || (ans > 0x9f) || ((ans > 0x8f) && ((ans & 0x0f) > 0x09))) { incr |= 0x60}
    if(ans > 0x99) carry = true
    if(testFlag(F,FLAG_N)) SUB(UByte(incr.byteValue())) else ADD(A, UByte(incr.byteValue()))
    if(carry) setFlag(F,FLAG_C, false) else setFlag(F,FLAG_C,true)
    setPV(PARITY_TABLE(A.get8().intValue()))

    /*
    addTStates(4)
    val tmp1: UByte = A
    var tmp2 = UByte(0)
    val tmp3 = F & 1
    var tmp = tmp3
    if (((F & 0x10) != 0) || ((tmp1 & 0x0f) > 0x09)) tmp2 = UByte((tmp2.byteValue | 0x06).toByte)
    if ((tmp3 == 1) || (tmp1 > 0x9f) || ((tmp1 > 0x8f) && ((tmp1 & 0x0f) > 0x09))) {
      tmp2 = UByte((tmp2.byteValue | 0x60).toByte)
      tmp = 1
    }
    if (tmp1 > 0x99) tmp = 1
    if ((F & 0x02) != 0) {
      //cycle -= 4
      SUB(tmp2)
    }
    else {
      //cycle -= 4
      ADD(A, tmp2)
    }
    F((F & 0xfe) | tmp)
    if (parity(A)) F((F & 0xfb) | 4)
    F(F & 0xfb)*/
  }

  // JR Z,dd
  private final def fn0x28(x: Int): Unit = {
    if (testFlag(F, FLAG_Z)) {
      addTStates(12)
      PC(PC + MMU.get8(PC).byteValue + 1)
    } else {
      PC.increment()
      addTStates(7)
    }
  }

  // ADD HL,HL
  private final def fn0x29(x: Int): Unit = {
    addTStates(11)
    ADD(HL, HL)
  }

  // LD HL,(nnnn)
  private final def fn0x2a(x: Int): Unit = {
    addTStates(16)
    CHECK_LOG_WORD(PC)
    val fetch: Int = MMU.get16(PC)
    CHECK_LOG_WORD(fetch)
    HL(MMU.get16(fetch))
    PC(PC + 2)
  }

  // DEC HL
  private final def fn0x2b(x: Int): Unit = {
    addTStates(6)
    HL.decrement()
  }

  // INC L
  private final def fn0x2c(x: Int): Unit = {
    addTStates(4)
    INC(L)
  }

  // DEC L
  private final def fn0x2d(x: Int): Unit = {
    addTStates(4)
    DEC(L)
  }

  // LD L,nn
  private final def fn0x2e(x: Int): Unit = {
    addTStates(7)
    L(MMU.get8PC(PC))
  }

  // CPL
  private final def fn0x2f(x: Int): Unit = {
    addTStates(4)
    A(A.get8().intValue() ^ 0x00ff)
    setFlag(F,FLAG_H,false)
    setFlag(F,FLAG_N,false)
    setUnusedFlags(A.get8().intValue())
//    AF((~AF.get16 & ~0xff) | (AF & 0xc5) | ((~AF.get16 >> 8) & 0x28) | 0x12)
  }

  // JR NC,dd
  private final def fn0x30(x: Int): Unit = {
    if (!testFlag(F, FLAG_C)) {
      addTStates(12)
      PC(PC + MMU.get8(PC).byteValue + 1)
    } else {
      PC.increment()
      addTStates(7)
    }
  }

  // LD SP, nnnn
  private final def fn0x31(x: Int): Unit = {
    addTStates(10)
    SP(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (nnnn), A
  private final def fn0x32(x: Int): Unit = {
    addTStates(13)
    CHECK_LOG_WORD(PC)
    val fetch: Int = MMU.get16(PC)
    CHECK_LOG_WORD(fetch)
    MMU.put8(fetch, A)
    PC(PC + 2)
  }

  // INC SP
  private final def fn0x33(x: Int): Unit = {
    addTStates(6)
    INC(SP)
    //SP.increment()
  }

  // INC (HL)
  private final def fn0x34(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_BYTE(HL)
    //val temp: UByte = UByte((MMU.get8(HL) + UByte(1)).byteValue())
    var value:Int = MMU.get8(HL).intValue()
    setHalfCarryFlagAdd(value, 1)
    setPV(value == 0x7F)
    value = value + 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,true)
    setUnusedFlags(value)
    //r1(value)
    MMU.put8(HL, value)
//    AF((AF & ~0xfe) | incTable(temp) | SET_PV2(0x80, temp.intValue()))
  }

  // DEC (HL)
  private final def fn0x35(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_BYTE(HL)
    var value:Int = MMU.get8(HL).intValue()
    setHalfCarryFlagSub(value, 1)
    setPV(value == 0x80)
    value = value - 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,false)
    setUnusedFlags(value)
    MMU.put8(HL, value)
//    DEC(MMU.get8(HL))
    /* val temp: UByte = UByte((MMU.get8(HL) - UByte(1)).byteValue())
    MMU.put8(HL, temp.intValue())
    AF((AF & ~0xfe) | decTable(temp) | SET_PV2(0x7f, temp.intValue())) */
  }

  // LD (HL),nn
  private final def fn0x36(x: Int): Unit = {
    addTStates(10)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, MMU.get8PC(PC))
  }

  // SCF
  private final def fn0x37(x: Int): Unit = {
    addTStates(4)
    setFlag(F,FLAG_C,false)
    setFlag(F,FLAG_N,true)
    setFlag(F,FLAG_H,true)
    setUnusedFlags(A.get8().intValue())
  }

  // JR C,dd
  private final def fn0x38(x: Int): Unit = {
    if (testFlag(F, FLAG_C)) {
      addTStates(12)
      PC(PC + MMU.get8(PC).byteValue + 1)
    } else {
      PC.increment()
      addTStates(7)
    }
  }

  // ADD HL, SP
  private final def fn0x39(x: Int): Unit = {
    addTStates(11)
    ADD(HL, SP)
  }

  // LD A,(nnnn)
  private final def fn0x3a(x: Int): Unit = {
    addTStates(13)
    val tmp: Int = MMU.get16(PC)
    CHECK_LOG_BYTE(tmp)
    A(MMU.get8(tmp))
    PC(PC + 2)
  }

  // DEC SP
  private final def fn0x3b(x: Int): Unit = {
    addTStates(6)
    SP.decrement()
  }

  // INC A
  private final def fn0x3c(x: Int): Unit = {
    addTStates(4)
    INC(A)
  }

  // DEC A
  private final def fn0x3d(x: Int): Unit = {
    addTStates(4)
    DEC(A)
  }

  // LD A,nn
  private final def fn0x3e(x: Int): Unit = {
    addTStates(7)
    A(MMU.get8PC(PC))
  }

  // CCF - Complement Carry Flag
  private final def fn0x3f(x: Int): Unit = {
    addTStates(4)
    if(testFlag(F,FLAG_C)) setH(true) else setH(false)
    F(F.get8().intValue() ^ FLAG_C)
    setFlag(F,FLAG_N,true)
    setUnusedFlags(A.get8().intValue())

  }

  // LD B,B
  private final def fn0x40(x: Int): Unit = {
    addTStates(4)
  }

  // LD B,C
  private final def fn0x41(x: Int): Unit = {
    addTStates(4)
    B(C)
  }

  // LD B,D
  private final def fn0x42(x: Int): Unit = {
    addTStates(4)
    B(D)
  }

  // LD B,E
  private final def fn0x43(x: Int): Unit = {
    addTStates(4)
    B(E)
  }

  // LD B,H
  private final def fn0x44(x: Int): Unit = {
    addTStates(4)
    B(H)
  }

  // LD B,L
  private final def fn0x45(x: Int): Unit = {
    addTStates(4)
    B(L)
  }

  // LD B,(HL)
  private final def fn0x46(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    B(MMU.get8(HL))
  }

  // LD B,A
  private final def fn0x47(x: Int): Unit = {
    addTStates(4)
    B(A)
  }

  // LD C,B
  private final def fn0x48(x: Int): Unit = {
    addTStates(4)
    C(B)
  }

  // LD C,C
  private final def fn0x49(x: Int): Unit = {
    addTStates(4)
  }

  // LD C,D
  private final def fn0x4a(x: Int): Unit = {
    addTStates(4)
    C(D)
  }

  // LD C,E
  private final def fn0x4b(x: Int): Unit = {
    addTStates(4)
    C(E)
  }

  // LD C,H
  private final def fn0x4c(x: Int): Unit = {
    addTStates(4)
    C(H)
  }

  // LD C,L
  private final def fn0x4d(x: Int): Unit = {
    addTStates(4)
    C(L)
  }

  // LD C,(HL)
  private final def fn0x4e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    C(MMU.get8(HL))
  }

  // LD C,A
  private final def fn0x4f(x: Int): Unit = {
    addTStates(4)
    C(A)
  }

  // LD D,B
  private final def fn0x50(x: Int): Unit = {
    addTStates(4)
    D(B)
  }

  // LD D,C
  private final def fn0x51(x: Int): Unit = {
    addTStates(4)
    D(C)
  }

  // LD D,D
  private final def fn0x52(x: Int): Unit = {
    addTStates(4)
  }

  // LD D,E
  private final def fn0x53(x: Int): Unit = {
    addTStates(4)
    D(E)
  }

  // LD D,H
  private final def fn0x54(x: Int): Unit = {
    addTStates(4)
    D(H)
  }

  // LD D,L
  private final def fn0x55(x: Int): Unit = {
    addTStates(4)
    D(L)
  }

  // LD D,(HL)
  private final def fn0x56(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    D(MMU.get8(HL))
  }

  // LD D,A
  private final def fn0x57(x: Int): Unit = {
    addTStates(4)
    D(A)
  }

  // LD E,B
  private final def fn0x58(x: Int): Unit = {
    addTStates(4)
    E(B)
  }

  // LD E,C
  private final def fn0x59(x: Int): Unit = {
    addTStates(4)
    E(C)
  }

  // LD E,D
  private final def fn0x5a(x: Int): Unit = {
    addTStates(4)
    E(D)
  }

  // LD E,E
  private final def fn0x5b(x: Int): Unit = {
    addTStates(4)
  }

  // LD E,H
  private final def fn0x5c(x: Int): Unit = {
    addTStates(4)
    E(H)
  }

  // LD E,L
  private final def fn0x5d(x: Int): Unit = {
    addTStates(4)
    E(L)
  }

  // LD E,(HL)
  private final def fn0x5e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    E(MMU.get8(HL))
  }

  // LD E,A
  private final def fn0x5f(x: Int): Unit = {
    addTStates(4)
    E(A)
  }

  // LD H,B
  private final def fn0x60(x: Int): Unit = {
    addTStates(4)
    H(B)
  }

  // LD H,C
  private final def fn0x61(x: Int): Unit = {
    addTStates(4)
    H(C)
  }

  // LD H,D
  private final def fn0x62(x: Int): Unit = {
    addTStates(4)
    H(D)
  }

  // LD H,E
  private final def fn0x63(x: Int): Unit = {
    addTStates(4)
    H(E)
  }

  // LD H,H
  private final def fn0x64(x: Int): Unit = {
    addTStates(4)
  }

  // LD H,L
  private final def fn0x65(x: Int): Unit = {
    addTStates(4)
    H(L)
  }

  // LD H,(HL)
  private final def fn0x66(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    H(MMU.get8(HL))
  }

  // LD H,A
  private final def fn0x67(x: Int): Unit = {
    addTStates(4)
    H(A)
  }

  // LD L,B
  private final def fn0x68(x: Int): Unit = {
    addTStates(4)
    L(B)
  }

  // LD L,C
  private final def fn0x69(x: Int): Unit = {
    addTStates(4)
    L(C)
  }

  // LD L,D
  private final def fn0x6a(x: Int): Unit = {
    addTStates(4)
    L(D)
  }

  // LD L,E
  private final def fn0x6b(x: Int): Unit = {
    addTStates(4)
    L(E)
  }

  // LD L,H
  private final def fn0x6c(x: Int): Unit = {
    addTStates(4)
    L(H)
  }

  // LD L,L
  private final def fn0x6d(x: Int): Unit = {
    addTStates(4)
  }

  // LD L,(HL)
  private final def fn0x6e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    L(MMU.get8(HL))
  }

  // LD L,A
  private final def fn0x6f(x: Int): Unit = {
    addTStates(4)
    L(A)
  }

  // LD (HL),B
  private final def fn0x70(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, B)
  }

  // LD (HL),C
  private final def fn0x71(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, C)
  }

  // LD (HL),D
  private final def fn0x72(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, D)
  }

  // LD (HL),E
  private final def fn0x73(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, E)
  }

  // LD (HL),H
  private final def fn0x74(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, H)
  }

  // LD (HL),L
  private final def fn0x75(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, L)
  }

  // HALT
  private final def fn0x76(x: Int): Unit = {
    addTStates(4)
    PC(PC - 1)
    // Check stop on halt, otherwise sim_sleep
    if (stopOnHALT) execute = false
    else {
      SimTimer.sim_interval = 0
      // Only sleep if there are no interrupts pending
      //if (!keyboardInterrupt && !timerInterrupt) Thread.sleep(0, 100) // 100 uSecs
    }
  }

  // LD (HL),A
  private final def fn0x77(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    MMU.put8(HL, A)
  }

  // LD A,B
  private final def fn0x78(x: Int): Unit = {
    addTStates(4)
    A(B)
  }

  // LD A,C
  private final def fn0x79(x: Int): Unit = {
    addTStates(4)
    A(C)
  }

  // LD A,D
  private final def fn0x7a(x: Int): Unit = {
    addTStates(4)
    A(D)
  }

  // LD A,E
  private final def fn0x7b(x: Int): Unit = {
    addTStates(4)
    A(E)
  }

  // LD A,H
  private final def fn0x7c(x: Int): Unit = {
    addTStates(4)
    A(H)
  }

  // LD A,L
  private final def fn0x7d(x: Int): Unit = {
    addTStates(4)
    A(L)
  }

  // LD A,(HL)
  private final def fn0x7e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    A(MMU.get8(HL))
  }

  // LD A,A
  private final def fn0x7f(x: Int): Unit = {
    addTStates(4)
  }

  // ADD A,B
  private final def fn0x80(x: Int): Unit = {
    addTStates(4)
    ADD(A, B)
  }

  // ADD A,C
  private final def fn0x81(x: Int): Unit = {
    addTStates(4)
    ADD(A, C)
  }

  // ADD A,D
  private final def fn0x82(x: Int): Unit = {
    addTStates(4)
    ADD(A, D)
  }

  // ADD A,E
  private final def fn0x83(x: Int): Unit = {
    addTStates(4)
    ADD(A, E)
  }

  // ADD A,H
  private final def fn0x84(x: Int): Unit = {
    addTStates(4)
    ADD(A, H)
  }

  // ADD A,L
  private final def fn0x85(x: Int): Unit = {
    addTStates(4)
    ADD(A, L)
  }

  // ADD A,(HL)
  private final def fn0x86(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    ADD(A, MMU.get8(HL))
  }

  // ADD A,A
  private final def fn0x87(x: Int): Unit = {
    addTStates(4)
    ADD(A,A)

  }

  // ADC A,B
  private final def fn0x88(x: Int): Unit = {
    addTStates(4)
    ADC(A, B)
  }

  // ADC A,C
  private final def fn0x89(x: Int): Unit = {
    addTStates(4)
    ADC(A, C)
  }

  // ADC A,D
  private final def fn0x8a(x: Int): Unit = {
    addTStates(4)
    ADC(A, D)
  }

  // ADC A,E
  private final def fn0x8b(x: Int): Unit = {
    addTStates(4)
    ADC(A, E)
  }

  // ADC A,H
  private final def fn0x8c(x: Int): Unit = {
    addTStates(4)
    ADC(A, H)
  }

  // ADC A,L
  private final def fn0x8d(x: Int): Unit = {
    addTStates(4)
    ADC(A, L)
  }

  // ADC A,(HL)
  private final def fn0x8e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    ADC(A, MMU.get8(HL))
  }

  // ADC A,A
  private final def fn0x8f(x: Int): Unit = {
    addTStates(4)
    ADC(A,A)
/*    val cbits: UInt = UInt(2) * A.get8() + {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    AF(cbitsDup8Table(cbits & 0x1ff) | SET_PV(cbits)) */
  }

  // SUB B
  private final def fn0x90(x: Int): Unit = {
    addTStates(4)
    SUB(B)
  }

  // SUB C
  private final def fn0x91(x: Int): Unit = {
    addTStates(4)
    SUB(C)
  }

  // SUB D
  private final def fn0x92(x: Int): Unit = {
    addTStates(4)
    SUB(D)
  }

  // SUB E
  private final def fn0x93(x: Int): Unit = {
    addTStates(4)
    SUB(E)
  }

  // SUB H
  private final def fn0x94(x: Int): Unit = {
    addTStates(4)
    SUB(H)
  }

  // SUB L
  private final def fn0x95(x: Int): Unit = {
    addTStates(4)
    SUB(L)
  }

  // SUB (HL)
  private final def fn0x96(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    SUB(MMU.get8(HL))
    /*
    val temp: UByte = MMU.get8(HL)
    val acu: UByte = A.get8()
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits)) */
  }

  // SUB A
  private final def fn0x97(x: Int): Unit = {
    addTStates(4)
    AF(0x0042)
  }

  // SBC A,B
  private final def fn0x98(x: Int): Unit = {
    addTStates(4)
    SBC(A, B)
  }

  // SBC A,C
  private final def fn0x99(x: Int): Unit = {
    addTStates(4)
    SBC(A, C)
  }

  // SBC A,D
  private final def fn0x9a(x: Int): Unit = {
    addTStates(4)
    SBC(A, D)
  }

  // SBC A,E
  private final def fn0x9b(x: Int): Unit = {
    addTStates(4)
    SBC(A, E)
  }

  // SBC A,H
  private final def fn0x9c(x: Int): Unit = {
    addTStates(4)
    SBC(A, H)
  }

  // SBC A,L
  private final def fn0x9d(x: Int): Unit = {
    addTStates(4)
    SBC(A, L)
  }

  // SBC A,(HL)
  private final def fn0x9e(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    SBC(A, MMU.get8(HL))
  }

  // SBC A,A
  private final def fn0x9f(x: Int): Unit = {
    addTStates(4)
    SBC(A,A)
  }

  // AND B
  private final def fn0xa0(x: Int): Unit = {
    addTStates(4)
    AND(B)
    //AF(andTable(A.get8() & B.get8()))
  }

  // AND C
  private final def fn0xa1(x: Int): Unit = {
    addTStates(4)
    AND(C)
    //AF(andTable(A.get8() & C.get8()))
  }

  // AND D
  private final def fn0xa2(x: Int): Unit = {

    addTStates(4)
    AND(D)
    //AF(andTable(A.get8() & D.get8()))
  }

  // AND E
  private final def fn0xa3(x: Int): Unit = {

    addTStates(4)
    AND(E)
    //AF(andTable(A.get8() & E.get8()))
  }

  // AND H
  private final def fn0xa4(x: Int): Unit = {
    addTStates(4)
    AND(H)
    //AF(andTable(A.get8() & H.get8()))
  }

  // AND L
  private final def fn0xa5(x: Int): Unit = {

    addTStates(4)
    AND(L)
    //AF(andTable(A.get8() & L.get8()))
  }

  // AND (HL)
  private final def fn0xa6(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    AND(MMU.get8(HL))
    //AF(andTable(A.get8() & MMU.get8(HL)))
  }

  // AND A
  private final def fn0xa7(x: Int): Unit = {
    addTStates(4)
    AND(A)
    //AF(andTable(A.get8()))
  }

  // XOR B
  private final def fn0xa8(x: Int): Unit = {
    addTStates(4)
    XOR(B)
    //AF(xororTable(A.get8() ^ B.get8()))
  }

  // XOR C
  private final def fn0xa9(x: Int): Unit = {
    addTStates(4)
    XOR(C)
//    AF(xororTable(A.get8() ^ C.get8()))
  }

  // XOR D
  private final def fn0xaa(x: Int): Unit = {

    addTStates(4)
    XOR(D)
    //AF(xororTable(A.get8() ^ D.get8()))
  }

  // XOR E
  private final def fn0xab(x: Int): Unit = {

    addTStates(4)
    XOR(E)
    //AF(xororTable(A.get8() ^ E.get8()))
  }

  // XOR H
  private final def fn0xac(x: Int): Unit = {

    addTStates(4)
    XOR(H)
//    AF(xororTable(A.get8() ^ H.get8()))

  }

  // XOR L
  private final def fn0xad(x: Int): Unit = {

    addTStates(4)
    XOR(L)
//    AF(xororTable(A.get8() ^ L.get8()))
  }

  // XOR (HL)
  private final def fn0xae(x: Int): Unit = {
    addTStates(7)
    CHECK_LOG_BYTE(HL)
    XOR(MMU.get8(HL))
//    AF(xororTable(A.get8() ^ MMU.get8(HL)))
  }

  // XOR A
  private final def fn0xaf(x: Int): Unit = {
    addTStates(4)
    AF(0x0044)
  }

  // OR B
  private final def fn0xb0(x: Int): Unit = {
    addTStates(4)
    OR(B)
//    AF(xororTable(A.get8() | B.get8()))
  }

  // OR C
  private final def fn0xb1(x: Int): Unit = {

    addTStates(4)
    OR(C)
    //AF(xororTable(A.get8() | C.get8()))
  }

  // OR D
  private final def fn0xb2(x: Int): Unit = {

    addTStates(4)
    OR(D)
    //AF(xororTable(A.get8() | D.get8()))
  }

  // OR E
  private final def fn0xb3(x: Int): Unit = {

    addTStates(4)
    OR(E)
//    AF(xororTable(A.get8() | E.get8()))
  }

  // OR H
  private final def fn0xb4(x: Int): Unit = {

    addTStates(4)
    OR(H)
    //AF(xororTable(A.get8() | H.get8()))
  }

  // OR L
  private final def fn0xb5(x: Int): Unit = {

    addTStates(4)
    OR(L)
    //AF(xororTable(A.get8() | L.get8()))
  }

  // OR (HL)
  private final def fn0xb6(x: Int): Unit = {

    addTStates(7)
    CHECK_LOG_BYTE(HL)
    OR(MMU.get8(HL))
    //AF(xororTable((AF >> 8) | MMU.get8(HL) & 0xff))
  }

  // OR A
  private final def fn0xb7(x: Int): Unit = {

    addTStates(4)
    OR(A)
//    AF(xororTable((AF >> 8) & 0xff))

  }

  // CP B
  private final def fn0xb8(x: Int): Unit = {
    addTStates(4)
    ICP(B)
  }

  // CP C
  private final def fn0xb9(x: Int): Unit = {
    addTStates(4)
    ICP(C)
  }

  // CP D
  private final def fn0xba(x: Int): Unit = {

    addTStates(4)
    ICP(D)
  }

  // CP E
  private final def fn0xbb(x: Int): Unit = {

    addTStates(4)
    ICP(E)
  }

  // CP H
  private final def fn0xbc(x: Int): Unit = {

    addTStates(4)
    ICP(H)
  }

  // CP L
  private final def fn0xbd(x: Int): Unit = {
    addTStates(4)
    ICP(L)
  }

  // CP (HL)
  private final def fn0xbe(x: Int): Unit = {

    addTStates(7)
    CHECK_LOG_BYTE(HL)
    ICP(MMU.get8(HL))
  }

  // CP A
  private final def fn0xbf(x: Int): Unit = {

    addTStates(4)
    F((A & 0x28) | 0x42)
  }

  // RET NZ
  private final def fn0xc0(x: Int): Unit = {

    if (testFlag(F, FLAG_Z)) addTStates(5)
    else {
      CHECK_LOG_WORD(SP)
      addTStates(11)
      POP(PC)
    }
  }

  // POP BC
  private final def fn0xc1(x: Int): Unit = {
    addTStates(10)
    CHECK_LOG_WORD(SP)
    POP(BC)
  }

  // JP NZ,nnnn
  private final def fn0xc2(x: Int): Unit = {
    JPC(!testFlag(F, FLAG_Z))
  }

  // JP nnnn
  private final def fn0xc3(x: Int): Unit = {
    JPC(true)
  }

  // CALL NZ,nnnn
  private final def fn0xc4(x: Int): Unit = {
    CALLC(!testFlag(F, FLAG_Z))
  }

  // PUSH BC
  private final def fn0xc5(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(BC)
  }

  // ADD A,nn
  private final def fn0xc6(x: Int): Unit = {
    addTStates(7)
    ADD(A, MMU.get8PC(PC))
  }

  // RST 0
  private final def fn0xc7(x: Int): Unit = {

    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x0000)
  }

  // RET Z
  private final def fn0xc8(x: Int): Unit = {
    if (testFlag(F, FLAG_Z)) {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    } else {
      addTStates(5)
    }
  }

  // RET
  private final def fn0xc9(x: Int): Unit = {

    addTStates(10)
    CHECK_LOG_WORD(SP)
    POP(PC)
  }

  // JP Z,nnnn
  private final def fn0xca(x: Int): Unit = {

    JPC(testFlag(F, FLAG_Z))
  }

  // CB prefix
  private final def fn0xcb(x: Int): Unit = {
    // ******************************************************************************** CB
    INCR(1)
    val adr: Int = HL.get16
    val op: Int = MMU.get8(PC)
    cbprefix(op, adr)
  }

  // CALL Z,nnnn
  private final def fn0xcc(x: Int): Unit = {
    CALLC(testFlag(F, FLAG_Z))
  }

  // CALL nnnn
  private final def fn0xcd(x: Int): Unit = {
    CALLC(true)
  }

  // ADC A,nn
  private final def fn0xce(x: Int): Unit = {
    addTStates(7)
    ADC(A, MMU.get8PC(PC))
  }

  // RST 8
  private final def fn0xcf(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x0008)
  }

  // RET NC
  private final def fn0xd0(x: Int): Unit = {
    if (testFlag(F, FLAG_C)) addTStates(5)
    else {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    }
  }

  // POP DE
  private final def fn0xd1(x: Int): Unit = {
    addTStates(10)
    CHECK_LOG_WORD(SP)
    POP(DE)
  }

  // JP NC,nnnn
  private final def fn0xd2(x: Int): Unit = {
    JPC(!testFlag(F, FLAG_C))
  }

  // OUT (nn),A
  private final def fn0xd3(x: Int): Unit = {
    addTStates(11)
    MMU.out8(MMU.get8PC(PC), A)
  }

  // CALL NC, nnnn
  private final def fn0xd4(x: Int): Unit = {
    CALLC(!testFlag(F, FLAG_C))
  }

  // PUSH DE
  private final def fn0xd5(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(DE)
  }

  // SUB nn
  private final def fn0xd6(x: Int): Unit = {
    addTStates(7)
    SUB(MMU.get8PC(PC))
  }

  // RST 10H
  private final def fn0xd7(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x0010)
  }

  // RET C
  private final def fn0xd8(x: Int): Unit = {
    if (testFlag(F, FLAG_C)) {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    } else addTStates(5)
  }

  // EXX
  private final def fn0xd9(x: Int): Unit = {
    addTStates(4)
    var temp = BC.get16
    BC(BCP.get16)
    BCP(temp)
    temp = DE.get16
    DE(DEP.get16)
    DEP(temp)
    temp = HL.get16
    HL(HLP.get16)
    HLP(temp)
  }

  // JP C,nnnn
  private final def fn0xda(x: Int): Unit = {
    JPC(testFlag(F, FLAG_C))
  }

  // IN A,(nn)
  private final def fn0xdb(x: Int): Unit = {
    addTStates(11)
    A(MMU.in8(MMU.get8PC(PC)))
  }

  // CALL C,nnnn
  private final def fn0xdc(x: Int): Unit = {

    CALLC(testFlag(F, FLAG_C))
  }

  // DD Prefix
  private final def fn0xdd(x: Int): Unit = {
    // ************************************************************************************** DD
    INCR(1)
    val op: Int = MMU.get8PC(PC)
    ddprefix(op)
  }

  // SBC A,nn
  private final def fn0xde(x: Int): Unit = {
    addTStates(7)
    SBC(A, MMU.get8PC(PC))
  }

  // RST 18H
  private final def fn0xdf(x: Int): Unit = {
    addTStates(18)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x18)
  }

  // RET PO
  private final def fn0xe0(x: Int): Unit = {
    if (testFlag(F, FLAG_P)) addTStates(5)
    else {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    }
  }

  // POP HL
  private final def fn0xe1(x: Int): Unit = {

    addTStates(10)
    CHECK_LOG_WORD(SP)
    POP(HL)
  }

  // JP PO,nnnn
  private final def fn0xe2(x: Int): Unit = {

    JPC(!testFlag(F, FLAG_P))
  }

  //  EX (SP),HL
  private final def fn0xe3(x: Int): Unit = {

    addTStates(19)
    CHECK_LOG_WORD(SP)
    val temp: Int = HL.get16
    POP(HL)
    PUSH(temp)
  }

  // CALL PO,nnnn
  private final def fn0xe4(x: Int): Unit = {

    CALLC(!testFlag(F, FLAG_P))
  }

  // PUSH HL
  private final def fn0xe5(x: Int): Unit = {

    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(HL)
  }

  // AND nn
  private final def fn0xe6(x: Int): Unit = {

    addTStates(7)
    AND(MMU.get8PC(PC))
//    AF(andTable(((AF >> 8) & MMU.get8PC(PC)) & 0xff))
  }

  // RST 20H
  private final def fn0xe7(x: Int): Unit = {

    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x20)
  }

  // RET PE
  private final def fn0xe8(x: Int): Unit = {
    if (testFlag(F, FLAG_P)) {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    } else {
      addTStates(5)
    }
  }

  // JP (HL)
  private final def fn0xe9(x: Int): Unit = {
    addTStates(4)
    PC(HL)
  }

  // JP PE,nnnn
  private final def fn0xea(x: Int): Unit = {
    JPC(testFlag(F, FLAG_P))
  }

  // EX DE,HL
  private final def fn0xeb(x: Int): Unit = {
    addTStates(4)
    val temp: Int = HL
    HL(DE)
    DE(temp)
  }

  // CALL PE,nnnn
  private final def fn0xec(x: Int): Unit = {
    CALLC(testFlag(F, FLAG_P))
  }

  // ED PREFIX
  private final def fn0xed(x: Int): Unit = {
    // *************************************************************************************** ED
    INCR(1)
    val op: Int = MMU.get8PC(PC)
    edprefix(op)
  }

  // XOR nn
  private final def fn0xee(x: Int): Unit = {
    addTStates(7)
    XOR(MMU.get8PC(PC))
//    AF(xororTable(((AF >> 8) ^ MMU.get8PC(PC)) & 0xff))
  }

  // RST 28H
  private final def fn0xef(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x28)
  }

  // RET P - sign is not set
  private final def fn0xf0(x: Int): Unit = {
    if (testFlag(F, FLAG_S)) {
      addTStates(5)
    } else {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    }
  }

  // POP AF
  private final def fn0xf1(x: Int): Unit = {

    addTStates(10)
    CHECK_LOG_WORD(SP)
    POP(AF)
  }

  // JP P,nnnn
  private final def fn0xf2(x: Int): Unit = {
    JPC(!testFlag(F, FLAG_S))
  }

  // DI
  private final def fn0xf3(x: Int): Unit = {
    addTStates(4)
    IFF(0)
  }

  // CALL P,nnnn
  private final def fn0xf4(x: Int): Unit = {
    CALLC(!testFlag(F, FLAG_S))
  }

  // PUSH AF
  private final def fn0xf5(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(AF)
  }

  // OR nn
  private final def fn0xf6(x: Int): Unit = {
    addTStates(7)
    OR(MMU.get8PC(PC))
//    AF(xororTable(((AF >> 8) | MMU.get8PC(PC)) & 0xff))
  }

  // RST 30H
  private final def fn0xf7(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x30)
  }

  // RET M
  private final def fn0xf8(x: Int): Unit = {
    if (testFlag(F, FLAG_S)) {
      CHECK_LOG_WORD(SP)
      POP(PC)
      addTStates(11)
    } else {
      addTStates(5)
    }
  }

  // LD SP,HL
  private final def fn0xf9(x: Int): Unit = {
    addTStates(6)
    SP(HL)
  }

  // JP M,nnnn
  private final def fn0xfa(x: Int): Unit = {
    JPC(testFlag(F, FLAG_S))
  }

  // EI
  private final def fn0xfb(x: Int): Unit = {
    addTStates(4)
    IFF(3)
  }

  // CALL M, nnnn
  private final def fn0xfc(x: Int): Unit = {
    CALLC(testFlag(F, FLAG_S))
  }

  // FD Prefix
  private final def fn0xfd(x: Int): Unit = {

    // ************************************************************************************ FD

    INCR(1)
    val op: Int = MMU.get8PC(PC)
    fdprefix(op)
  }

  // CP nn
  private final def fn0xfe(x: Int): Unit = {
    addTStates(7)
    ICP(MMU.get8PC(PC))
  }

  // RST 38H
  private final def fn0xff(x: Int): Unit = {
    addTStates(11)
    CHECK_LOG_WORD(SP - 2)
    PUSH(PC)
    PC(0x38)
  }

  /** ***********************************************************************************************************
   *
   * Main  CPU running here...
   *
   */
  private var execute: Boolean = true

  override def runcpu(singleStep: Boolean = false): Unit = {

    execute = true
    try {
      // tStates contains the number of t-states executed.  1 t-state is executed in 1 microsecond
      // on a 1Mhz CPU.  tStates is used for real-time simulations.
      val sliceLength = 10L
      var tStatesInSlice: Long = sliceLength * clockFrequency // Number of t-states in 10mSec time-slice
      var startTime: Long = System.currentTimeMillis()
      var now: Long = 0L

      clockHasChanged = true
      tStates = 0L

      while (execute) {

        // If we are single stepping, the only do this one instruction.
        if (singleStep) execute = false

        if (machine.checkBreak(PC.toUInt) && lastBreak != PC.toUInt && !singleStep) {
          execute = false
          lastBreak = PC.toUInt
          Utils.outln("\n\r" + f"Z80: Break at: ${PC.intValue()}%05X")

        } else {
          lastBreak = UInt(0)

          if (SimTimer.sim_interval <= 0) { // Check clock queue
            // sim_process_event()
            machine.eventQueue.processEvent()

            if (clockHasChanged) {
              clockHasChanged = false
              tStates = 0L
              startTime = System.currentTimeMillis()
              tStatesInSlice = sliceLength * clockFrequency
            }
          }

          // Quick check for special processing
          if (clockFrequency != 0 || timerInterrupt || keyboardInterrupt) {
            if (clockFrequency != 0 && (tStates >= tStatesInSlice)) {
              startTime += sliceLength
              tStates -= tStatesInSlice
              now = System.currentTimeMillis()
              if (startTime > now) Thread.sleep(0, (1000 * (startTime - now)).intValue())
            }

            if (timerInterrupt && (IFF & 1) != 0) {
              timerInterrupt = false
              IFF(0) // Disable Interrupts
              val currentOp = MMU.get8(PC)
              if ((currentOp == 0x76) && !stopOnHALT) {
                PUSH(PC + 1)
              } else {
                PUSH(PC)
              }
              PC(timerInterruptHandler)
            }
            if (keyboardInterrupt && (IFF & 1) != 0) {
              keyboardInterrupt = false
              IFF(0) // Disable Interrupts
              val currentOp = MMU.get8(PC)
              if ((currentOp == 0x76) && !stopOnHALT) {
                PUSH(PC + 1)
              } else {
                PUSH(PC)
              }
              PC(keyboardInterruptHandler)
            }
          }
          // Interrupted the sim
          if (Console.userInterrupt) execute = false

          // Instruction execution

          INCR(1)
          val instr: Int = MMU.get8PC(PC)

          execMap.get(instr) match {
            case Some(x) => x(instr)
            case None =>

          }

          SimTimer.sim_interval = SimTimer.sim_interval - 1

        } // End Break check
      } // end SwitchCPUNow

      // simulation halted
      onHalt(singleStep | lastBreak != 0)
    } catch {
      case t: Throwable =>
        Utils.outln(s"SIM: Exception: ${t.getMessage}\n\r")
        t.getStackTrace.foreach(x => Utils.outln(msg = s"${x.toString}"))
        onHalt(singleStep | lastBreak != 0)
        throw t
    }
  }

  override def onHalt(singleStepped: Boolean): Unit = {
    if (!singleStepped) {
      Utils.outln(s"\n\r$getName: Halted.")

      Utils.outln(showRegisters())
      Utils.outln(showFlags())
    } else {
      // Single stepped or break
      val sb = new mutable.StringBuilder
      sb.append(f"${PC.intValue()}%05X : ")
      DAsm(PC.intValue(), sb)
      Utils.outln(sb.toString())

    }
  }

  private inline final def addTStates(x: Long): Unit = {
    tStates = tStates + x
  }

  private inline def INCR(count: Int): Unit = {
    R.set8(UByte(((R.get8() & ~0x7f) | ((R.get8() + count) & 0x7f)).toByte)) // Increment R
  }


  private inline final def HIGH_DIGIT(x: UByte): Int = {
    (x.intValue >> 4) & 0xf
  }

  private inline final def LOW_DIGIT(x: UByte): Int = {
    x.intValue & 0xf
  }

  private final val parityTable: Array[UByte] = {
    for (i <- 0 to 255) yield {
      if ((((i & 1) + ((i & 2) >> 1) + ((i & 4) >> 2) + ((i & 8) >> 3) +
        ((i & 16) >> 4) + ((i & 32) >> 5) + ((i & 64) >> 6) + ((i & 128) >> 7)) % 2) != 0)
        UByte(0) else UByte(4)
    }
  }.toArray

  private final val rrcaTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort((((temp & 1) << 15) | (sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private final val rraTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort(((sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private final val PARITY_TABLE : Array[Boolean] = new Array(256)
  PARITY_TABLE(0) = true
  var ppp:Int  = 1
  for(bit <- 0 until 8) {
    for(fill <- 0 until ppp) {
      PARITY_TABLE(ppp + fill) = !PARITY_TABLE(fill)
    }
    ppp = ppp * 2
  }

  private inline final def PARITY(value: Int): UByte = parityTable(value & 0xff)

  private final val rotateShiftTable: Array[UByte] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UByte(((sum & 0xa8) | (t1 << 6) | PARITY(sum)).toByte)
    }
  }.toArray

  private final val rrdrldTable: Array[UShort] = {
    for (acu <- 0 to 255) yield {
      val t1 = if ((acu & 0xff) == 0) 1 else 0
      UShort(((acu << 8) | (acu & 0xa8) | (t1 << 6) | parityTable(acu)).toShort)
    }
  }.toArray

  private inline final def POP(x: Register16): Unit = {
    val y = MMU.get8(SP)
    SP.increment()
    x(y + (MMU.get8(SP) << 8).shortValue)
    SP.increment()
  }

  private inline final def PUSH(x: Register16): Unit = {
    PUSH(x.get16)
  }

  private inline final def PUSH(x: Int): Unit = {
    SP.decrement()
    MMU.put8(SP, UByte((x >> 8).byteValue))
    SP.decrement()
    MMU.put8(SP, UByte((x & 0xff).byteValue))
  }

  private inline final def JPC(cond: => Boolean): Unit = {
    addTStates(10)
    if (cond) {
      PC(MMU.get16(PC))
    } else PC(PC + 2)
  }

  private inline final def CALLC(cond: => Boolean): Unit = {
    if (cond) {
      val addr = MMU.get16(PC)
      CHECK_LOG_WORD(SP - 2)
      PUSH(PC + 2)
      PC(addr)
      addTStates(17)
    } else {
      PC(PC + 2)
      addTStates(10)
    }
  }

  //**** Utility helpers *****

  // 16 bit ADD
  private inline final def ADD(r1: Register16, r2: Register16): Unit = {
    val hl:Int = r1.get16.intValue()
    val value:Int = r2.get16.intValue()
    val result:Int = hl + value
    setFlag(F, FLAG_N, true) // Clear N
    val temp:Int = (hl & 0x0FFF) + (value & 0x0FFF)
    if((temp & 0xF000) != 0)  setFlag(F,FLAG_H, false) else setFlag(F,FLAG_H,true)
    if((result & 0x0800) != 0) setFlag(F,FLAG_3,false) else setFlag(F,FLAG_3,true)
    if((result & 0x2000) != 0) setFlag(F,FLAG_5,false) else setFlag(F,FLAG_5,true)
    if(result > 0x0000FFFF) {
      setC(true)
      r1(result & 0x0000FFFF)
    } else {
      setC(false)
      r1(result)
    }

  }

  private inline final def ADD(r1: Register8, r2: Register8): Unit = {
    ADD(r1.get8(), r2.get8())
  }

  private inline final def setHalfCarryFlagAdd(left: Int, right: Int) : Unit = {
    val left1 = left & 0x000F
    val right1 = right & 0x000F
    setH((right1 + left1) > 0x0f)
  }
  private inline final def setHalfCarryFlagAdd(left: Int, right: Int, carry:Int) : Unit = {
    val left1 = left & 0x000F
    val right1 = right & 0x000F
    setH((right1 + left1 + carry) > 0x0f)
  }
  private inline final def setOverflowFlagAdd(left:Int, right:Int, carry:Int) : Unit = {
    var left1 :Int = left
    var right1:Int = right
    if(left1 > 127) left1 = left1 - 256
    if(right1 > 127) right1 = right1 -256
    left1 = left1 + right1 + carry
    setPV((left1 < -128) || (left1 > 127))
  }

  private inline final def setOverflowFlagAdd(left:Int, right:Int) : Unit = {
    setOverflowFlagAdd(left,right,0)
  }

  private inline final def AND(value: UByte) : Unit = {
    val v:Int  = value.intValue()
    var a:Int = A.get8().intValue()
    F(0x10)
    A(a & v)
    a = A.get8().intValue()
    setS((a & 0x0080) != 0)
    setZ(a == 0)
    setPV(PARITY_TABLE(a))
    setUnusedFlags(a)
  }

  private inline final def OR(value: UByte) : Unit = {
    val v:Int  = value.intValue()
    var a:Int = A.get8().intValue()
    F(0)
    A(a | v)
    a = A.get8().intValue()
    setS((a & 0x0080) != 0)
    setZ(a == 0)
    setPV(PARITY_TABLE(a))
    setUnusedFlags(a)
  }

  private inline final def XOR(value: UByte) : Unit = {
    val v:Int  = value.intValue()
    var a:Int = A.get8().intValue()
    F(0)
    A(a ^ v)
    a = A.get8().intValue()
    setS((a & 0x0080) != 0)
    setZ(a == 0)
    setPV(PARITY_TABLE(a))
    setUnusedFlags(a)
  }


  // 8 bit ADD
  private inline final def ADD(t1: UByte, t2: UByte) : Unit = {
    var v1:Int = t1.intValue()
    val v2:Int = t2.intValue()
    setHalfCarryFlagAdd(v1,v2)
    setOverflowFlagAdd(v1,v2)
    v1 = v1 + v2
    setS((v1 & 0x0080) != 0)
    setC((v1 & 0xFF00) != 0)
    v1 = v1 & 0x00FF
    setZ(v1 == 0)
    setFlag(F,FLAG_N,true)
    A(v1)
    setUnusedFlags(v1)
  }

  private inline final def ADD(r1: Register8, temp: UByte) : Unit = {
    ADD(r1.get8(), temp)
  }

  private inline final def ADC(r1: Register8, r2: Register8): Unit = {
    ADC(r1.get8(), r2.get8())
  }

  private inline final def ADC(r1: Register8, temp: UByte): Unit = {
    ADC(r1.get8(), temp)

  }
// 8 bit ADC
  private inline final def ADC(t1: UByte, t2: UByte) : Unit = {
    var v1:Int  = t1.intValue()
    val v2:Int  = t2.intValue()
    val carry:Int  = if(testFlag(F, FLAG_C)) 1 else 0
    setHalfCarryFlagAdd(v1,v2,carry)
    setOverflowFlagAdd(v1,v2,carry)
    v1 = v1 + v2 + carry
    setS((v1 & 0x0080) != 0)
    setC((v1 & 0xff00) != 0)
    v1 = v1 & 0x00ff
    setZ(v1 == 0)
    setFlag(F,FLAG_N,true)
    A(v1)
    setUnusedFlags(v1)
 }

  private inline final def setOverflowFlagAdd16(left:Int, right:Int, carry:Int) : Unit = {
    var left1 = left
    var right1 = right
    if(left1 > 32767) left1 = left1 - 65536
    if(right1 > 32767) right1 = right1 - 65536
    left1 = left1 + right1 + carry
    setPV((left1 < -32768) || (left1 > 32767))
  }
  private inline final def setOverflowFlagSub16(left:Int, right:Int, carry:Int) : Unit = {
    var left1 = left
    var right1 = right
    if(left1 > 32767) left1 = left1 - 65536
    if(right1 > 32767) right1 = right1 - 65536
    left1 = left1 - right1 - carry
    setPV((left1 < -32768) || (left1 > 32767))
  }

// 16 bit ADC
  private inline final def ADC( r2: Register16): Unit = {
    var a: Int = HL.get16.intValue()
    var b: Int = r2.get16.intValue()
    val c: Int = if (testFlag(F, FLAG_C)) 1 else 0
    val lans: Int = a + b + c
    val ans = lans & 0xffff
    setS((ans & (FLAG_S << 8)) != 0)
    set3(((ans & (FLAG_3 << 8)) != 0))
    set5((ans & (FLAG_5 << 8)) != 0)
    setZ(ans == 0)
    setC(lans > 0xFFFF)
    setOverflowFlagAdd16(a, b, c)
    if ((((a & 0x0FFF) + (b & 0x0FFF) + c) & 0x1000) != 0) setH(true)
    else setH(false)
    setFlag(F, FLAG_N, true)
    HL(ans)
  }
  private inline final def setHalfCarryFlagSub(left:Int, right:Int) : Unit = {
    val left1:Int  = left & 0x000f
    val right1:Int  = right & 0x000f
    setH(left1 < right1)
  }
  private inline final def setHalfCarryFlagSub(left:Int, right : Int, carry:Int) : Unit = {
    val left1:Int  = left & 0x000f
    val right1:Int  = right & 0x000f
    setH(left1 < (right1 + carry))
  }
  private inline final def setOverflowFlagSub(left:Int, right:Int, carry:Int) : Unit = {
   var left1:Int  = left
   var right1:Int  = right
   if(left > 127) left1 = left1 - 256
   if(right1 > 127) right1 = right1 - 256
   left1 = left1 - right1 - carry
   setPV((left1 < -128) || (left1 > 127))
 }
  private inline final def setOverflowFlagSub(left:Int, right:Int) : Unit = {
   setOverflowFlagSub(left,right,0)
 }
  private inline final def setPV(b:Boolean) : Unit = {
    if(b) setFlag(FLAG_P, false)
    else setFlag(FLAG_P,true)
  }
  private inline final def setH(b:Boolean) : Unit = {
   if(b) setFlag(FLAG_H,false)
   else setFlag(FLAG_H,true)
 }
  private inline final def setS(b:Boolean):Unit = {
  if(b) setFlag(FLAG_S,false)
  else setFlag(FLAG_S,true)
}
  private final def setC(b:Boolean):Unit = {
  if(b) setFlag(FLAG_C,false)
  else setFlag(FLAG_C,true)
}
  private inline final def setZ(b:Boolean) : Unit = {
  if(b) setFlag(FLAG_Z,false)
  else setFlag(FLAG_Z,true)
}
  private inline final def setN():Unit = {
  setFlag(FLAG_N,false)

}
  private inline final def setUnusedFlags(value:Int):Unit = {
  val v:Int  = value & 0x28
  F(F.intValue() & 0xd7)
  F(F.intValue() | v)
}
// 8 bit SUB
  private inline final def SUB(temp: UByte): Unit = {
    var local_reg_A:Int  = A.get8().intValue()
    val value:Int = temp.intValue()
    setHalfCarryFlagSub(local_reg_A, value)
    setOverflowFlagSub(local_reg_A, value)
    local_reg_A = local_reg_A - value
    setS((local_reg_A & 0x0080) != 0)
    setC((local_reg_A & 0xff00) != 0)
    local_reg_A = local_reg_A & 0x00ff
    setZ(local_reg_A == 0)
    setN()
    A(local_reg_A)
    setUnusedFlags(local_reg_A)


  }

  private inline final def SUB(r1: Register8): Unit = {
    SUB(r1.get8())
  }

  private inline final def SBC(r1: Register8, r2: Register8): Unit = {
    SBC(r1.get8(), r2.get8())
  }

  private inline final def SBC(r1: Register8, temp: UByte): Unit = {
    SBC(r1.get8(), temp)
  }

  // 8 bit SBC
  private inline final def SBC(t1: UByte, t2:UByte) : Unit = {
    var v1:Int  = t1.intValue()
    val v2:Int  = t2.intValue()
    val carry:Int  = if(testFlag(F,FLAG_C)) 1 else 0
    setHalfCarryFlagSub(v1,v2,carry)
    setOverflowFlagSub(v1,v2,carry)
    v1 = v1 - v2 - carry
    setS((v1 & 0x0080) != 0)
    setC((v1 & 0xff00) != 0)
    v1 = v1 & 0x00ff
    setZ(v1 == 0)
    setN()
    A(v1)
    setUnusedFlags(v1)

  }

  // 16 bit SBC
   private inline final def SBC(r1: Register16, r2: Register16): Unit = {
    val a: Int = r1.get16.intValue()
    val b: Int = r2.get16.intValue()
    val c: Int = if (testFlag(F, FLAG_C)) 1 else 0
    val lans: Int = a - b - c
    val ans: Int = lans & 0xffff
    setS((ans & (FLAG_S << 8)) != 0)
    set3((ans & (FLAG_3 << 8)) != 0)
    set5((ans & (FLAG_5 << 8)) != 0)
    setZ(ans == 0)
    setC(lans < 0)
    setOverflowFlagSub16(a, b, c)
    if ((((a & 0x0FFF) - (b & 0x0FFF) - c) & 0x1000) != 0) setH(true) else setH(false)
    setN()
    r1(ans)
  }

  private inline final def ICP(r1: Register8): Unit = {
    ICP(r1.get8())
  }

  private inline final def set3(b:Boolean) : Unit = {
    if(b) setFlag(F,FLAG_3,false)
    else setFlag(F,FLAG_3,true)
  }
  private inline final def set3(): Unit = {
    set3(true)
  }

  private inline final def set5(b:Boolean) : Unit = {
    if(b) setFlag(F,FLAG_5,false)
    else setFlag(F,FLAG_5,true)
  }
  private inline final def set5(): Unit = {
    set5(true)
  }
  // 8 bit CP
  private inline final def ICP(temp: UByte): Unit = {
    val a:Int  = A.get8().intValue()
    val b:Int  = temp.intValue()
    val wans:Int  = a - b
    val ans:Int  = wans & 0xff
    F(0x02)
    setS((ans & FLAG_S) != 0)
    set3((b & FLAG_3) != 0)
    set5((b & FLAG_5) != 0)
    setZ(ans == 0)
    setC((wans & 0x100) != 0)
    setH((((a & 0x0f) - (b & 0x0f)) & FLAG_H) != 0)
    setPV(((a ^ b) & (a ^ ans) & 0x80) != 0)

  }

  private inline final def LDIDXdd(r1: Register8, ridx: Register16): Unit = {
    val adr: Int = ridx.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    r1(MMU.get8(adr))
  }


  private inline final def LDIDXdd(ridx: Register16, r1: Register8): Unit = {
    val adr = ridx.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    MMU.put8(adr, r1)
  }

  // 8 bit increment
  private inline final def INC(r1: Register8): Unit = {
    var value:Int = r1.get8().intValue()
    setHalfCarryFlagAdd(value, 1)
    setPV(value == 0x7F)
    value = value + 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,true)
    setUnusedFlags(value)
    r1(value)

    //AF((AF & ~0xfe) | incTable(r1) | SET_PV2(0x80, r1))
  }


  private inline final def DEC(r1: Register8): Unit = {
    var value:Int = r1.get8().intValue()
    setHalfCarryFlagSub(value, 1)
    setPV(value == 0x80)
    value = value - 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,false)
    setUnusedFlags(value)
    r1(value)
  }

  private inline final def INC(r1: Register16) : Unit = {
    val value:Int = r1.intValue()
    r1(value + 1)
  }

  private inline final def DEC(r1: Register16) : Unit = {
    val value:Int = r1.intValue()
    r1(value - 1)
  }

  private inline final def INCIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    var value:Int = MMU.get8(adr).intValue()
    setHalfCarryFlagAdd(value, 1)
    setPV(value == 0x7F)
    value = value + 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,true)
    setUnusedFlags(value)
    MMU.put8(adr, UByte(value.toByte))
  }

  private inline final def DECIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16 + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    var value:Int = MMU.get8(adr).intValue()
    setHalfCarryFlagSub(value, 1)
    setPV(value == 0x80)
    value = value - 1
    setS((value & 0x0080) !=  0)
    value = value & 0x00FF
    setZ(value == 0)
    setFlag(F,FLAG_N,false)
    setUnusedFlags(value)
    MMU.put8(adr, UByte(value.toByte))
  }


  private inline final def ANDIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    AND(MMU.get8(adr))
  }

  private inline final def ORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    OR(MMU.get8(adr))
  }


  private inline final def XORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    XOR(MMU.get8(adr))
  }


  private inline final def CPIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    ICP(MMU.get8(adr))
  }

  private inline final def NEG(): Unit = {
    addTStates(8)
    var value :Int = A.get8().intValue()
    setHalfCarryFlagSub(0, value, 0)
    setOverflowFlagSub(0, value,0)
    value = (- value)
    if((value & 0xFF00) != 0) setFlag(F,FLAG_C,false) else setFlag(F,FLAG_C,true)
    setN()
    A(value & 0x00FF)
    if(A.get8().intValue() == 0) setZ(true) else setZ(false)
    if((A.get8().intValue() & 0x0080) != 0) setS(true) else setS(false)
    setUnusedFlags(A.get8().intValue())

  }

  private inline final def RETN(): Unit = {
    addTStates(14)
    IFF(IFF | (IFF >> 1))
    CHECK_LOG_WORD(SP)
    POP(PC)
  }

  private inline final def setFlag(flag: Int, clear: Boolean): Unit = {
    AF(if (!clear) AF | flag else AF & ~flag)

  }


  /*
  Macros for the IN/OUT instructions INI/INIR/IND/INDR/OUTI/OTIR/OUTD/OTDR
    Pre condition
        temp == value of register B at entry of the instruction
        acu == value of transferred byte (IN or OUT)
    Post condition
        F is set correctly
    Use INOUTFLAGS_ZERO(x) for INIR/INDR/OTIR/OTDR where
        x == (C + 1) & 0xff for INIR
        x == L              for OTIR and OTDR
        x == (C - 1) & 0xff for INDR
    Use INOUTFLAGS_NONZERO(x) for INI/IND/OUTI/OUTD where
        x == (C + 1) & 0xff for INI
        x == L              for OUTI and OUTD
        x == (C - 1) & 0xff for IND
*/

  private inline final def INOUTFLAGS_NONZERO(x: Int, acu: Int, temp: Int): Unit = {
    INOUTFLAGS((C.get8() & 0xa8) | ({
      if (C.get8() == 0) 1 else 0
    } << 6), x, acu, temp)
  }

  private inline final def INOUTFLAGS_ZERO(x: Int, acu: Int, temp: Int): Unit = {
    INOUTFLAGS(FLAG_Z, x, acu, temp)
  }


  private inline final def INOUTFLAGS(syzx: Int, x: Int, acu: Int, temp: Int): Unit = {
    AF((AF & 0xff00) | syzx |
      ((acu & 0x80) >> 6) | {
      if ((acu + x) > 0xff) FLAG_C | FLAG_H else 0
    } |
      parityTable(((acu + x) & 7) ^ temp))
  }


  override def DAsm(addr: Int, sb: mutable.StringBuilder): Int = {
    var pc = addr
    var T: String = null
    var J = false
    var Offset = 0
    var C: Char = 0x00
    val op = MMU.get8(pc).intValue()

    op match {
      case 0xcb =>
        pc += 1
        T = Z80.MnemonicsCB(MMU.get8(pc).intValue())
        pc += 1

      case 0xed =>
        pc += 1
        T = Z80.MnemonicsED(MMU.get8(pc).intValue())

      case 0xdd | 0xfd =>

        C = {
          val x = MMU.get8(pc).intValue()
          pc += 1
          if (x == 0xdd) 'X' else 'Y'

        }
        if (MMU.get8(pc).intValue == 0xcb) {
          pc += 1
          Offset = MMU.get8(pc)
          pc += 1
          J = true
          T = Z80.MnemonicsXCB(MMU.get8(pc).intValue())
          pc += 1
        }
        else {
          T = Z80.MnemonicsXX(MMU.get8(pc).intValue())
          pc += 1
        }

      case _ =>
        T = Z80.MnemonicsZ80(MMU.get8(pc).intValue())
        pc += 1
    }

    var R: String = if (T.contains("^")) {
      val x = MMU.get8(pc).intValue()
      pc += 1
      T.replace("^", f"$x%02x")


    } else T

    R = R.replace('%', C)

    R = if (R.contains("*")) {
      val x = MMU.get8(pc).intValue()
      pc += 1
      R.replace("*", f"$x%02x")
    } else if (R.contains("@")) {
      if (J) {
        Offset = MMU.get8(pc).intValue()
        pc += 1

      }
      val S = {
        if ((Offset & 0x80) != 0) "-" else "+"
      }
      val j = {
        if ((Offset & 0x80) != 0) 256 - Offset else Offset
      }
      R.replace("@", f"$S$j%02x")
    } else if (R.contains("$")) {
      Offset = MMU.get8(pc).intValue()
      pc += 1
      val x = pc + 2 + {
        if ((Offset & 0x80) != 0) Offset - 256 else Offset
      } & 0xFFFF
      pc += 2
      R.replace("$", f"$x%04x")

    } else if (R.contains("#")) {
      val x = MMU.get8(pc).intValue + 256 * MMU.get8(pc + 1).intValue
      pc += 2
      R.replace("#", f"$x%04x")

    } else R

    sb.append(R)
    pc
  }

  /**
   * CB Prefix
   *
   * @param op opcode
   * @param adr parameters
   */
  private def cbprefix(op: Int, adr: Int): Unit = {
    var acu: UByte = UByte(0)
    var cbits: UInt = UInt(0)
    var temp: UByte = UByte(0)
    var tStateModifier = false

    val op1 = op & 7
    (op1 : @switch) match {
      case 0 =>
        PC.increment()
        acu = B.get8()
        addTStates(8)
      case 1 =>
        PC.increment()
        acu = C.get8()
        addTStates(8)
      case 2 =>
        PC.increment()
        acu = D.get8()
        addTStates(8)
      case 3 =>
        PC.increment()
        acu = E.get8()
        addTStates(8)
      case 4 =>
        PC.increment()
        acu = H.get8()
      case 5 =>
        PC.increment()
        acu = L.get8()
        addTStates(8)
      case 6 =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)
        addTStates(15)
        tStateModifier = true
      case 7 =>
        PC.increment()
        acu = A.get8()
        addTStates(8)
      case _ =>

    }
    val op2 = op & 0xc0
    (op2 : @switch) match {
      case 0x00 => // shift/rotate
        op & 0x38 match {
          case 0x00 => //RLC
            temp = UByte(((acu << 1) | (acu >> 7)).byteValue())
            cbits = UInt(temp & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x08 => // RRC
            temp = UByte(((acu >> 1) | (acu << 7)).byteValue())
            cbits = UInt(temp & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x10 => // RL
            temp = UByte(((acu << 1) | {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }).byteValue())
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x18 => // RR
            temp = UByte(((acu >> 1) | ({
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            } << 7)).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x20 => // SLA
            temp = UByte((acu << 1).byteValue())
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x28 => // SRA
            temp = UByte(((acu >> 1) | (acu & 0x80)).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x30 => // SLIA
            temp = UByte(((acu << 1) | UByte(1)).byteValue())
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case 0x38 => // SRL
            temp = UByte((acu >> 1).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })
          case _ =>
        }
      case 0x40 => // BIT
        if (tStateModifier) addTStates(-3)
        if ((acu & (1 << ((op >> 3) & 7))) != 0)
          AF((AF & ~0xfe) | 0x10 | ({
            if ((op & 0x38) == 0x38)
              1
            else 0
          } << 7))
        else AF((AF & ~0xfe) | 0x54)
        if ((op & 7) != 6) AF(AF | (acu & 0x28))
        temp = acu
      case 0x80 => // RES
        temp = UByte((acu & ~(1 << ((op >> 3) & 7))).byteValue())
      case 0xc0 => // SET
        temp = UByte((acu | (1 << ((op >> 3) & 7))).byteValue())
      case _ =>
    }
    op & 7 match {
      case 0 =>
        B(temp)
      case 1 =>
        C(temp)
      case 2 =>
        D(temp)
      case 3 =>
        E(temp)
      case 4 =>
        H(temp)
      case 5 =>
        L(temp)
      case 6 =>
        MMU.put8(adr, temp)
      case 7 =>
        A(temp)
      case _ =>
    }
  }

  private val ddexecMap: Map[Int, Function[Int, Unit]] = Map[Int, Int => Unit](
    0x09 -> fn0xdd0x09, 0x19 -> fn0xdd0x19, 0x21 -> fn0xdd0x21,
    0x22 -> fn0xdd0x22, 0x23 -> fn0xdd0x23, 0x24 -> fn0xdd0x24, 0x25 -> fn0xdd0x25, 0x26 -> fn0xdd0x26,
    0x29 -> fn0xdd0x29, 0x2a -> fn0xdd0x2a, 0x2b -> fn0xdd0x2b, 0x2c -> fn0xdd0x2c, 0x2d -> fn0xdd0x2d,
    0x2e -> fn0xdd0x2e, 0x34 -> fn0xdd0x34, 0x35 -> fn0xdd0x35, 0x36 -> fn0xdd0x36, 0x39 -> fn0xdd0x39,
    0x44 -> fn0xdd0x44, 0x45 -> fn0xdd0x45, 0x46 -> fn0xdd0x46, 0x4c -> fn0xdd0x4c, 0x4d -> fn0xdd0x4d,
    0x4e -> fn0xdd0x4e, 0x54 -> fn0xdd0x54, 0x55 -> fn0xdd0x55, 0x56 -> fn0xdd0x56, 0x5c -> fn0xdd0x5c,
    0x5d -> fn0xdd0x5d, 0x5e -> fn0xdd0x5e, 0x60 -> fn0xdd0x60, 0x61 -> fn0xdd0x61, 0x62 -> fn0xdd0x62,
    0x63 -> fn0xdd0x63, 0x64 -> fn0xdd0x64, 0x65 -> fn0xdd0x65, 0x66 -> fn0xdd0x66, 0x67 -> fn0xdd0x67,
    0x68 -> fn0xdd0x68, 0x69 -> fn0xdd0x69, 0x6a -> fn0xdd0x6a, 0x6b -> fn0xdd0x6b, 0x6c -> fn0xdd0x6c,
    0x6d -> fn0xdd0x6d, 0x6e -> fn0xdd0x6e, 0x6f -> fn0xdd0x6f, 0x70 -> fn0xdd0x70, 0x71 -> fn0xdd0x71,
    0x72 -> fn0xdd0x72, 0x73 -> fn0xdd0x73, 0x74 -> fn0xdd0x74, 0x75 -> fn0xdd0x75, 0x77 -> fn0xdd0x77,
    0x7c -> fn0xdd0x7c, 0x7d -> fn0xdd0x7d, 0x7e -> fn0xdd0x7e, 0x84 -> fn0xdd0x84, 0x85 -> fn0xdd0x85,
    0x86 -> fn0xdd0x86, 0x8c -> fn0xdd0x8c, 0x8d -> fn0xdd0x8d, 0x8e -> fn0xdd0x8e, 0x94 -> fn0xdd0x94,
    0x95 -> fn0xdd0x95, 0x96 -> fn0xdd0x96, 0x9c -> fn0xdd0x9c, 0x9d -> fn0xdd0x9d, 0x9e -> fn0xdd0x9e,
    0xa4 -> fn0xdd0xa4, 0xa5 -> fn0xdd0xa5, 0xa6 -> fn0xdd0xa6, 0xac -> fn0xdd0xac, 0xad -> fn0xdd0xad,
    0xae -> fn0xdd0xae, 0xb4 -> fn0xdd0xb4, 0xb5 -> fn0xdd0xb5, 0xb6 -> fn0xdd0xb6, 0xbc -> fn0xdd0xbc,
    0xbd -> fn0xdd0xbd, 0xbe -> fn0xdd0xbe, 0xcb -> fn0xdd0xcb, 0xe1 -> fn0xdd0xe1, 0xe3 -> fn0xdd0xe3,
    0xe5 -> fn0xdd0xe5, 0xe9 -> fn0xdd0xe9, 0xf9 -> fn0xdd0xf9
  )

  // ADD IX,BC
  private final def fn0xdd0x09(x: Int): Unit = {
    addTStates(15)
    ADD(IX, BC)
  }

  // ADD IX,DE
  private final def fn0xdd0x19(x: Int): Unit = {
    addTStates(15)
    ADD(IX, DE)
  }

  // LD IX,nnnn
  private final def fn0xdd0x21(x: Int): Unit = {
    addTStates(14)
    IX(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (nnnn),IX
  private final def fn0xdd0x22(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, IX.get16)
    PC(PC + 2)
  }

  // INC IX
  private final def fn0xdd0x23(x: Int): Unit = {
    addTStates(10)
    INC(IX)
    //IX.increment()
  }

  // INC IXH
  private final def fn0xdd0x24(x: Int): Unit = {
    addTStates(9)
    INC(IXH)
    //IXH.increment()
    //AF((AF & ~0xfe) | incZ80Table(IXH.get8()))
  }

  // DEC IXH
  private final def fn0xdd0x25(x: Int): Unit = {
    addTStates(9)
    DEC(IXH)
/*    IXH.decrement()
    AF((AF & ~0xfe) | decZ80Table(IXH)) */
  }

  // LD IXH,nn
  private final def fn0xdd0x26(x: Int): Unit = {
    addTStates(9)
    IXH(MMU.get8PC(PC))
  }

  // ADD IX,IX
  private final def fn0xdd0x29(x: Int): Unit = {
    addTStates(15)
    ADD(IX, IX)
  }

  // LD IX,(nnnn)
  private final def fn0xdd0x2a(x: Int): Unit = {
    addTStates(15)
    val tmp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(tmp)
    IX(MMU.get16(tmp))
    PC(PC + 2)
  }

  // DEC IX
  private final def fn0xdd0x2b(x: Int): Unit = {
    addTStates(9)
    IX.decrement()
  }

  // INC IXL
  private final def fn0xdd0x2c(x: Int): Unit = {
    addTStates(9)
    INC(IXL)
    //IXL.increment()
    //AF((AF & ~0xfe) | incZ80Table(IXL.get8()))
  }

  // DEC IXL
  private final def fn0xdd0x2d(x: Int): Unit = {
    addTStates(9)
    DEC(IXL)
/*    IXL.decrement()
    AF((AF & ~0xfe) | decZ80Table(IXL & 0xff)) */
  }

  // LD IXL,nn
  private final def fn0xdd0x2e(x: Int): Unit = {
    addTStates(9)
    IXL(MMU.get8PC(PC))
  }

  // INC (IX+dd)
  private final def fn0xdd0x34(x: Int): Unit = {
    addTStates(23)
    INCIDXdd(IX)
  }

  // DEC (IX+dd)
  private final def fn0xdd0x35(x: Int): Unit = {
    addTStates(23)
    DECIDXdd(IX)
  }

  // LD (IX+dd),nn
  private final def fn0xdd0x36(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IX.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    MMU.put8(adr, MMU.get8PC(PC))
  }

  // ADD IX,SP
  private final def fn0xdd0x39(x: Int): Unit = {
    addTStates(11)
    ADD(IX, SP)
  }

  // LD B,IXH
  private final def fn0xdd0x44(x: Int): Unit = {
    addTStates(9)
    B(IXH)
  }

  // LD B,IXL
  private final def fn0xdd0x45(x: Int): Unit = {
    addTStates(9)
    B(IXL)
  }

  // LD B,(IX+dd)
  private final def fn0xdd0x46(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(B, IX)
  }

  // LD C,IXH
  private final def fn0xdd0x4c(x: Int): Unit = {
    addTStates(9)
    C(IXH)
  }

  // LD C,IXL
  private final def fn0xdd0x4d(x: Int): Unit = {
    addTStates(9)
    C(IXL)
  }

  // LD C,(IX+dd)
  private final def fn0xdd0x4e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(C, IX)
  }

  // LD D,IXH
  private final def fn0xdd0x54(x: Int): Unit = {
    addTStates(9)
    D(IXH)
  }

  // LD D,IXL
  private final def fn0xdd0x55(x: Int): Unit = {
    addTStates(9)
    D(IXL)
  }

  // LD D,(IX+dd)
  private final def fn0xdd0x56(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(D, IX)
  }

  // LD E,IXH
  private final def fn0xdd0x5c(x: Int): Unit = {
    addTStates(9)
    E(IXH)
  }

  // LD E,IXL
  private final def fn0xdd0x5d(x: Int): Unit = {
    addTStates(9)
    E(IXL)
  }

  // LD E,(IX+dd)
  private final def fn0xdd0x5e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(E, IX)
  }

  // LD IXH,B
  private final def fn0xdd0x60(x: Int): Unit = {
    addTStates(9)
    IXH(B)
  }

  // LD IXH,C
  private final def fn0xdd0x61(x: Int): Unit = {
    addTStates(9)
    IXH(C)
  }

  // LD IXH,D
  private final def fn0xdd0x62(x: Int): Unit = {
    addTStates(9)
    IXH(D)
  }

  // LD IXH,E
  private final def fn0xdd0x63(x: Int): Unit = {
    addTStates(9)
    IXH(E)
  }

  // LD IXH,IXH
  private final def fn0xdd0x64(x: Int): Unit = {
    addTStates(9)
  }

  // LD IXH,IXL
  private final def fn0xdd0x65(x: Int): Unit = {
    addTStates(9)
    IXH(IXL)
  }

  // LD H,(IX+dd)
  private final def fn0xdd0x66(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(H, IX)
  }

  // LD IXH,A
  private final def fn0xdd0x67(x: Int): Unit = {
    addTStates(9)
    IXH(A)
  }

  // LD IXL,B
  private final def fn0xdd0x68(x: Int): Unit = {
    addTStates(9)
    IXL(B)
  }

  // LD IXL,C
  private final def fn0xdd0x69(x: Int): Unit = {
    addTStates(9)
    IXL(C)
  }

  // LD IXL,D
  private final def fn0xdd0x6a(x: Int): Unit = {
    addTStates(9)
    IXL(D)
  }

  // LD IXL,E
  private final def fn0xdd0x6b(x: Int): Unit = {
    addTStates(9)
    IXL(E)
  }

  // LD IXL,IXH
  private final def fn0xdd0x6c(x: Int): Unit = {
    addTStates(9)
    IXL(IXH)
  }

  // LD IXL,IXL
  private final def fn0xdd0x6d(x: Int): Unit = {
    addTStates(9)
  }

  // LD L,(IX+dd)
  private final def fn0xdd0x6e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(L, IX)
  }

  // LD IXL,A
  private final def fn0xdd0x6f(x: Int): Unit = {
    addTStates(9)
    IXL(A)
  }

  // LD (IX+dd),B
  private final def fn0xdd0x70(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, B)
  }

  // LD (IX+dd),C
  private final def fn0xdd0x71(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, C)
  }

  // LD (IX+dd),D
  private final def fn0xdd0x72(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, D)
  }

  // LD (IX+dd),E
  private final def fn0xdd0x73(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, E)
  }

  // LD (IX+dd),H
  private final def fn0xdd0x74(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, H)
  }

  // LD (IX+dd),L
  private final def fn0xdd0x75(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, L)
  }

  // LD (IX+dd),A
  private final def fn0xdd0x77(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IX, A)
  }

  // LD A,IXH
  private final def fn0xdd0x7c(x: Int): Unit = {
    addTStates(9)
    A(IXH)
  }

  // LD A,IXL
  private final def fn0xdd0x7d(x: Int): Unit = {
    addTStates(9)
    A(IXL)
  }

  // LD A,(IX+dd)
  private final def fn0xdd0x7e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(A, IX)
  }

  // ADD A,IXH
  private final def fn0xdd0x84(x: Int): Unit = {
    addTStates(4)
    ADD(A, IXH)
  }

  // ADD A,IXL
  private final def fn0xdd0x85(x: Int): Unit = {
    addTStates(9)
    ADD(A, IXL)
  }

  // ADD A,(IX+dd)
  private final def fn0xdd0x86(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IX.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    ADD(A, MMU.get8(adr))
  }

  // ADC A,IXH
  private final def fn0xdd0x8c(x: Int): Unit = {
    addTStates(9)
    ADC(A, IXH)
  }

  // ADC A,IXL
  private final def fn0xdd0x8d(x: Int): Unit = {
    addTStates(9)
    ADC(A, IXL)
  }

  // ADC A,(IX+dd)
  private final def fn0xdd0x8e(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IX.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    ADC(A, MMU.get8(adr))
  }

  // SUB (IX+dd)
  private final def fn0xdd0x96(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IX.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    SUB(MMU.get8(adr))
  }

  // SUB IXH
  private final def fn0xdd0x94(x: Int): Unit = {
    addTStates(9)
    //setFlag(FLAG_C, clear = true)
    SUB(IXH)
  }

  // SBC A,IXH
  private final def fn0xdd0x9c(x: Int): Unit = {
    addTStates(9)
    SBC(A, IXH)
  }

  // SUB IXL
  private final def fn0xdd0x95(x: Int): Unit = {
    addTStates(9)
    //setFlag(FLAG_C, clear = true)
    SUB(IXL)
  }

  // SBC A,IXL
  private final def fn0xdd0x9d(x: Int): Unit = {
    addTStates(9)
    SBC(A, IXL)
  }

  // SBC A,(IX+dd)
  private final def fn0xdd0x9e(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IX.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    SBC(A, MMU.get8(adr))
  }

  // AND IXH
  private final def fn0xdd0xa4(x: Int): Unit = {
    addTStates(9)
    AND(IXH)

  }

  // AND IXL
  private final def fn0xdd0xa5(x: Int): Unit = {
    addTStates(9)
    AND(IXL)
  }

  // AND (IX+dd)
  private final def fn0xdd0xa6(x: Int): Unit = {
    addTStates(19)
    ANDIDXdd(IX)
  }

  // XOR IXH
  private final def fn0xdd0xac(x: Int): Unit = {
    addTStates(9)
    XOR(IXH)
  }

  // XOR IXL
  private final def fn0xdd0xad(x: Int): Unit = {
    addTStates(9)
    XOR(IXL)
  }

  // XOR (IX+DD)
  private final def fn0xdd0xae(x: Int): Unit = {
    addTStates(19)
    XORIDXdd(IX)
  }

  // OR IXH
  private final def fn0xdd0xb4(x: Int): Unit = {
    addTStates(9)
    OR(IXH)
    //AF(xororTable(((AF.get16 | IX.get16) >> 8) & 0xff))
  }

  // OR IXL
  private final def fn0xdd0xb5(x: Int): Unit = {
    addTStates(9)
    OR(IXL)
    //AF(xororTable(((AF.get16 >> 8) | IX.get16) & 0xff))
  }

  // OR (IX+dd)
  private final def fn0xdd0xb6(x: Int): Unit = {
    addTStates(19)
    ORIDXdd(IX)
  }

  // CP IXH
  private final def fn0xdd0xbc(x: Int): Unit = {
    addTStates(9)
    ICP(IXH)
  }

  // CP IXL
  private final def fn0xdd0xbd(x: Int): Unit = {
    addTStates(9)
    ICP(IXL)
  }

  // CP (IX+dd)
  private final def fn0xdd0xbe(x: Int): Unit = {
    addTStates(19)
    CPIDXdd(IX)
  }

  //  DD CB PREFIX
  private final def fn0xdd0xcb(x: Int): Unit = {
    val adr: Int = IX.get16.intValue + MMU.get8PC(PC).byteValue
    INCR(1)
    val op: Int = MMU.get8(PC)
    ddcbprefix(op, adr)
  }

  // POP IX
  private final def fn0xdd0xe1(x: Int): Unit = {
    addTStates(14)
    CHECK_LOG_WORD(SP)
    POP(IX)
  }

  // EX (SP),IX
  private final def fn0xdd0xe3(x: Int): Unit = {
    addTStates(23)
    CHECK_LOG_WORD(SP)
    val tmp: UInt = IX.get16
    POP(IX)
    PUSH(tmp.intValue)
  }

  // PUSH IX
  private final def fn0xdd0xe5(x: Int): Unit = {
    addTStates(15)
    CHECK_LOG_WORD(SP - 2)
    PUSH(IX)
  }

  // JP (IX)
  private final def fn0xdd0xe9(x: Int): Unit = {
    addTStates(8)
    PC(IX)
  }

  // LD SP,IX
  private final def fn0xdd0xf9(x: Int): Unit = {
    addTStates(10)
    SP(IX)
  }

  //**************************
  /**
   * DD Prefix
   *
   */
  private def ddprefix(op: Int): Unit = {
    ddexecMap.get(op) match {
      case Some(x) => x(op)
      case None =>
        // Ignore DD
        PC.decrement()
    }
  }

  private val edexecMap: Map[Int, Function[Int, Unit]] = Map[Int, Int => Unit](
    0x40 -> fn0xed0x40, 0x41 -> fn0xed0x41, 0x42 -> fn0xed0x42, 0x43 -> fn0xed0x43,
    0x44 -> fn0xed0xxx1, 0x4c -> fn0xed0xxx1, 0x54 -> fn0xed0xxx1, 0x64 -> fn0xed0xxx1,
    0x6c -> fn0xed0xxx1, 0x74 -> fn0xed0xxx1, 0x7c -> fn0xed0xxx1,
    0x45 -> fn0xed0xxx2, 0x55 -> fn0xed0xxx2, 0x5d -> fn0xed0xxx2, 0x65 -> fn0xed0xxx2, 0x6d -> fn0xed0xxx2,
    0x75 -> fn0xed0xxx2, 0x7d -> fn0xed0xxx2,
    0x46 -> fn0xed0x46, 0x47 -> fn0xed0x47, 0x49 -> fn0xed0x49, 0x4a -> fn0xed0x4a, 0x4b -> fn0xed0x4b,
    0x4d -> fn0xed0x4d, 0x4f -> fn0xed0x4f, 0x50 -> fn0xed0x50, 0x51 -> fn0xed0x51, 0x52 -> fn0xed0x52,
    0x53 -> fn0xed0x53, 0x56 -> fn0xed0x56, 0x57 -> fn0xed0x57, 0x58 -> fn0xed0x58, 0x59 -> fn0xed0x59,
    0x5a -> fn0xed0x5a, 0x5b -> fn0xed0x5b, 0x5e -> fn0xed0x5e, 0x5f -> fn0xed0x5f, 0x60 -> fn0xed0x60,
    0x61 -> fn0xed0x61, 0x62 -> fn0xed0x62, 0x63 -> fn0xed0x63, 0x48 -> fn0xed0x48,
    0x67 -> fn0xed0x67, 0x68 -> fn0xed0x68, 0x69 -> fn0xed0x69, 0x6a -> fn0xed0x6a, 0x6b -> fn0xed0x6b,
    0x6f -> fn0xed0x6f, 0x70 -> fn0xed0x70, 0x71 -> fn0xed0x71, 0x72 -> fn0xed0x72, 0x73 -> fn0xed0x73,
    0x78 -> fn0xed0x78, 0x79 -> fn0xed0x79, 0x7a -> fn0xed0x7a, 0x7b -> fn0xed0x7b,
    0xa0 -> fn0xed0xa0, 0xa1 -> fn0xed0xa1, 0xa2 -> fn0xed0xa2, 0xa3 -> fn0xed0xa3, 0xa8 -> fn0xed0xa8,
    0xa9 -> fn0xed0xa9, 0xaa -> fn0xed0xaa, 0xab -> fn0xed0xab, 0xb0 -> fn0xed0xb0, 0xb1 -> fn0xed0xb1,
    0xb2 -> fn0xed0xb2, 0xb3 -> fn0xed0xb3, 0xb8 -> fn0xed0xb8, 0xb9 -> fn0xed0xb9,
    0xba -> fn0xed0xba, 0xbb -> fn0xed0xbb
  )

  /**
   * ED Prefix
   *
   * @param op op code
   */
  private def edprefix(op: Int): Unit = {
    edexecMap.get(op) match {
      case Some(x) => x(op)
      case None =>
    }
  }

  // IN B,(C)
  private final def fn0xed0x40(x: Int): Unit = {
    addTStates(12)
    val temp: Int = MMU.in8(C)
    B(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))
  }

  // OUT (C),B
  private final def fn0xed0x41(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, B)
  }

  // SBC HL, BC
  private final def fn0xed0x42(x: Int): Unit = {
    addTStates(15)
    SBC(HL, BC)
  }

  // LD (nnnn),BC
  private final def fn0xed0x43(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, BC)
    PC(PC + 2)
  }

  // 0x44, 0x4c, 0x54, 0x64, 0x6c, 0x74, 0x7c  - NEG
  private final def fn0xed0xxx1(x: Int): Unit = {
    NEG()
  }

  // 0x45, 0x55, 0x5d, 0x65, 0x6d, 0x75, 0x7d - RETN
  private final def fn0xed0xxx2(x: Int): Unit = {
    RETN()
  }

  // IM 0
  private final def fn0xed0x46(x: Int): Unit = {
    addTStates(8) // Interrupt mode 0
  }

  // LD I,A
  private final def fn0xed0x47(x: Int): Unit = {
    addTStates(9)
    I((I & 0xff) | (AF & ~0xff))
  }

  // IN C,(C)
  private final def fn0xed0x48(x: Int): Unit = {
    addTStates(12)
    val temp: Int = MMU.in8(C)
    C(temp)
    AF((AF & ~0xfe) | rotateShiftTable(temp & 0xff))
  }

  // OUT (C),C
  private final def fn0xed0x49(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, C)
  }

  // ADC HL,BC
  private final def fn0xed0x4a(x: Int): Unit = {
    addTStates(15)
    ADC(BC)
  }

  // LD BC,(nnnn)
  private final def fn0xed0x4b(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    BC(MMU.get16(temp))
    PC(PC + 2)
  }

  // RETI
  private final def fn0xed0x4d(x: Int): Unit = {
    addTStates(14)
    IFF(IFF | IFF >> 1)
    CHECK_LOG_WORD(SP)
    POP(PC)
  }

  // LD R,A
  private final def fn0xed0x4f(x: Int): Unit = {
    addTStates(9)
    IR((IR & ~0xff) | ((AF >> 8) & 0xff))
  }

  // IN D,(C)
  private final def fn0xed0x50(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    D(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp))
  }

  // OUT (C),D
  private final def fn0xed0x51(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, D)
  }

  // SBC HL,DE
  private final def fn0xed0x52(x: Int): Unit = {
    addTStates(15)
    SBC(HL, DE)
  }

  // LD (nnnn),DE
  private final def fn0xed0x53(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, DE)
    PC(PC + 2)
  }

  // IM 1
  private final def fn0xed0x56(x: Int): Unit = {
    addTStates(8)
  }

  // LD A,I
  private final def fn0xed0x57(x: Int): Unit = {
    addTStates(9)
    A(I.get8())
    setS((A.get8().intValue() & FLAG_S) != 0)
    setZ(A.get8().intValue() == 0)
    setFlag(F,FLAG_H,true)
    setFlag(F,FLAG_N,true)
    setPV(if((IFF.get8() & 2) != 0) true else false)
    setUnusedFlags(A.get8().intValue())
//    AF((AF & 0x29) | (IR & ~0xff) | ((IR >> 8) & 0x80) | ({
//      if ((IR & ~0xff) == 0) 1 else 0
//    } << 6) | ((IFF & 2) << 1))
  }

  // IN E,(C)
  private final def fn0xed0x58(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    E(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp))
  }

  // OUT (C),E
  private final def fn0xed0x59(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, E)
  }

  // ADC HL,DE
  private final def fn0xed0x5a(x: Int): Unit = {
    addTStates(15)
    ADC(DE)
  }

  // LD DE,(nnnn)
  private final def fn0xed0x5b(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    DE(MMU.get16(temp))
    PC(PC + 2)
  }

  // IM 2
  private final def fn0xed0x5e(x: Int): Unit = {
    addTStates(8)
  }

  // LD A,R
  private final def fn0xed0x5f(x: Int): Unit = {
    addTStates(9)
    AF((AF & 0x29) | ((IR & 0xff) << 8) | (IR & 0x80) |
      ({
        if ((IR & 0xff) == 0) 1 else 0
      } << 6) | ((IFF & 2) << 1))
  }

  // IN H,(C)
  private final def fn0xed0x60(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    H(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp))
  }

  // OUT (C),H
  private final def fn0xed0x61(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, H)
  }

  // SBC HL,HL
  private final def fn0xed0x62(x: Int): Unit = {
    addTStates(15)
    SBC(HL, HL)
  }

  // LD (nnnn),HL
  private final def fn0xed0x63(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, HL)
    PC(PC + 2)
  }

  // RRD
  private final def fn0xed0x67(x: Int): Unit = {
    addTStates(18)
    val temp: UByte = MMU.get8(HL)
    val acu: UByte = A.get8()
    MMU.put8(HL, HIGH_DIGIT(temp) | (LOW_DIGIT(acu) << 4) & 0xff)
    AF(rrdrldTable((acu & 0xf0) | LOW_DIGIT(temp)) | (AF.get16 & 1))
  }

  // IN L,(C)
  private final def fn0xed0x68(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    L(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))
  }

  // OUT (C),L
  private final def fn0xed0x69(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, L)
  }

  // ADC HL,HL
  private final def fn0xed0x6a(x: Int): Unit = {
    addTStates(15)
    ADC(HL)
  }

  // LD HL,(nnnn)
  private final def fn0xed0x6b(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    HL(MMU.get16(temp))
    PC(PC + 2)
  }

  // RLD
  private final def fn0xed0x6f(x: Int): Unit = {
    addTStates(18)
    val temp: UByte = MMU.get8(HL)
    val acu: UByte = A.get8()
    MMU.put8(HL, (LOW_DIGIT(temp) << 4) | LOW_DIGIT(acu))
    AF(rrdrldTable((acu & 0xf0) | HIGH_DIGIT(temp)) | (F.get8().intValue & 1))
  }

  // IN (C)
  private final def fn0xed0x70(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    AF((AF & 0xfe) | rotateShiftTable(temp))
  }

  // OUT (C),0
  private final def fn0xed0x71(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, UByte(0))
  }

  // SBC HL,SP
  private final def fn0xed0x72(x: Int): Unit = {
    addTStates(15)
    SBC(HL, SP)
  }

  // LD (nnnn),SP
  private final def fn0xed0x73(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, SP)
    PC(PC + 2)
  }

  // IN A,(C)
  private final def fn0xed0x78(x: Int): Unit = {
    addTStates(12)
    val temp: UByte = MMU.in8(C)
    A(temp)
    AF((AF & 0xfe) | rotateShiftTable(temp))
  }

  // OUT (C),A
  private final def fn0xed0x79(x: Int): Unit = {
    addTStates(12)
    MMU.out8(C, A)
  }

  // ADC HL,SP
  private final def fn0xed0x7a(x: Int): Unit = {
    addTStates(15)
    ADC(SP)
  }

  // LD SP,(nnnn)
  private final def fn0xed0x7b(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    SP(MMU.get16(temp))
    PC(PC + 2)
  }

  // LDI
  private final def fn0xed0xa0(x: Int): Unit = {
    addTStates(16)
    // CHECK_BREAK_TWO_BYTES - HL & DE
    var acu: Int = MMU.get8(HL).byteValue
    HL.increment()
    MMU.put8(DE, acu)
    DE.increment()
    acu = acu + A.get8().byteValue
    AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | ({
      BC.decrement()
      if ((BC & 0xffff) != 0) 1 else 0
    } << 2))
  }

  // CPI
  private final def fn0xed0xa1(x: Int): Unit = {
    addTStates(16)
    CHECK_LOG_BYTE(HL)

    val acu: Int = A.get8().byteValue
    val temp: Int = MMU.get8(HL.get16)
    HL.increment()
    val sum:Int  = acu - temp
    val cbits:Int  = acu ^ temp ^ sum
    AF((AF & ~0xfe) | (sum & 0x80) | {
      if (((sum & 0xff) << 6) == 0) 1 else 0
    } |
      (((sum - ((cbits & 16) >> 4)) & 2) << 4) | (cbits & 16) |
      ((sum - ((cbits >> 4) & 1)) & 8) | {
      BC.decrement()
      if ((BC & 0xffff) != 0) 1 else 0
    } << 2 | 2)
    if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)

  }

  // INI
  private final def fn0xed0xa2(x: Int): Unit = {
    /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
          NF flag A is copy of bit 7 of the value read from or written to an I/O port.
          INI/INIR/IND/INDR use the C flag in stead of the L register. There is a
          catch though, because not the value of C is used, but C + 1 if it's INI/INIR or
          C - 1 if it's IND/INDR. So, first of all INI/INIR:
              HF and CF Both set if ((HL) + ((C + 1) & 255) > 255)
              PF The parity of (((HL) + ((C + 1) & 255)) & 7) xor B)                      */
    addTStates(16)
    CHECK_LOG_BYTE(HL)
    val acu: UInt = MMU.in8(C)
    MMU.put8(HL, acu.intValue)
    HL.increment()
    val temp: UByte = B.get8()
    BC(BC - 0x100)
    INOUTFLAGS_NONZERO((C + 1) & 0xff, acu.intValue, temp.intValue())
  }

  // OUTI
  private final def fn0xed0xa3(x: Int): Unit = {
    /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
    NF flag A is copy of bit 7 of the value read from or written to an I/O port.
    And now the for OUTI/OTIR/OUTD/OTDR instructions. Take state of the L
    after the increment or decrement of HL; add the value written to the I/O port
    to; call that k for now. If k > 255, then the CF and HF flags are set. The PF
    flags is set like the parity of k bitwise and'ed with 7, bitwise xor'ed with B.
    HF and CF Both set if ((HL) + L > 255)
    PF The parity of ((((HL) + L) & 7) xor B)                                       */
    addTStates(16)
    CHECK_LOG_BYTE(HL)
    val acu: UInt = MMU.get8(HL)
    MMU.out8(C, UByte(acu.byteValue()))
    HL.increment()
    val temp: UByte = B.get8()
    BC(BC - 0x100)
    INOUTFLAGS_NONZERO(L.get8(), acu.intValue, temp.intValue())
  }

  // LDD
  private final def fn0xed0xa8(x: Int): Unit = {
    addTStates(16)
    // CHECK_BREAK_TWO_BYTES HL,DE
    CHECK_LOG_BYTE(HL)
    CHECK_LOG_BYTE(DE)
    var acu: UByte = MMU.get8(HL)
    HL.decrement()
    MMU.put8(DE, acu)
    DE.decrement()
    acu = UByte((acu.byteValue + A.get8()).byteValue())
    AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | {
      BC.decrement()
      if ((BC & 0xffff) != 0) 1 else 0
    } << 2)
  }

  // CPD
  private final def fn0xed0xa9(x: Int): Unit = {
    addTStates(16)
    CHECK_LOG_BYTE(HL)
    val acu: UByte = A.get8()
    val temp: UByte = MMU.get8(HL)
    HL.decrement()
    val sum = acu.byteValue - temp.byteValue
    val cbits = acu.byteValue ^ temp.byteValue ^ sum
    AF(
      (AF & ~0xfe) | (sum & 0x80) | ({
        if ((sum & 0xff) == 0) UInt(1) else UInt(0)
      } << 6) |
        (((sum - ((cbits & 16) >> 4)) & 2) << 4) | (cbits & 16) |
        ((sum - ((cbits >> 4) & 1)) & 8) | {
        BC.decrement()
        if ((BC.get16 & 0xffff) != 0) UInt(1) else UInt(0)
      } << 2 | 2
    )
    if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)
  }

  // IND
  private final def fn0xed0xaa(x: Int): Unit = {
    /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
    NF flag A is copy of bit 7 of the value read from or written to an I/O port.
    INI/INIR/IND/INDR use the C flag in stead of the L register. There is a
    catch though, because not the value of C is used, but C + 1 if it's INI/INIR or
    C - 1 if it's IND/INDR. And last IND/INDR:
    HF and CF Both set if ((HL) + ((C - 1) & 255) > 255)
    PF The parity of (((HL) + ((C - 1) & 255)) & 7) xor B)                      */
    addTStates(16)
    CHECK_LOG_BYTE(HL)
    val acu: UByte = MMU.in8(C)
    MMU.put8(HL, acu)
    HL.decrement()
    val temp: Int = B.get8()
    BC(BC.get16 - 0x100)
    INOUTFLAGS_NONZERO((C.get8() - 1) & 0xff, acu.intValue(), temp)
  }

  // OUTD
  private final def fn0xed0xab(x: Int): Unit = {
    addTStates(16)
    CHECK_LOG_BYTE(HL)
    val acu: UInt = MMU.get8(HL)
    MMU.out8(C, acu.intValue)
    HL.decrement()
    val temp: Int = B.get8()
    BC(BC.get16 - 0x100)
    INOUTFLAGS_NONZERO(L.get8(), acu.intValue, temp.intValue())
  }

  // LDIR
  private final def fn0xed0xb0(x: Int): Unit = {
    addTStates(-5)
    // use local variable - quicker
    //var bc: Int = BC
    var acu: Int = 0
    if (BC.get16 == 0) BC(0x10000)
    while ( {
      BC.get16 != 0
    }) {
      addTStates(21)
      INCR(2)
      // CHECK_BREAK_TWO_BYTES(HL,DE)
      CHECK_LOG_BYTE(HL)
      CHECK_LOG_BYTE(DE)
      acu = MMU.get8(HL)
      HL.increment()
      MMU.put8(DE, acu)
      DE.increment()
      BC.decrement()
    }
    acu += A
    AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4))
  }

  // CPIR
  private final def fn0xed0xb1(x: Int): Unit = {
    addTStates(-5)
    val acu: Int = A.get8().intValue()
    var temp: Int = -1
    var sum: Int = -1
    var op: Int = -1

    if (BC.get16 == 0) BC(0x10000)
    while (op != 0 && sum != 0) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      temp = MMU.get8(HL.get16)
      HL.increment()
      BC.decrement()
      op = if (BC.get16 != 0) 1 else 0
      sum = acu - temp

    }
    val cbits: Int = acu ^ temp ^ sum

    val nc = {
      if ((sum & 0xff) == 0) UInt(1) else UInt(0)
    }
    AF((AF & ~0xfe) | (sum & 0x80) | ((nc << 6)) |
      (((sum - ((cbits & 16) >> 4)) & 2) << 4) |
      (cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
      op << 2 | 2)
    if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)
  }

  // INIR
  private final def fn0xed0xb2(x: Int): Unit = {
    addTStates(-5)
    var temp: Int = {
      if (B.intValue == 0) 0x100 else B.get8()
    }
    var acu: Int = 0
    while ( {
      temp != 0
    }) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      acu = MMU.in8(C)
      MMU.put8(HL, acu)
      HL.increment()
      temp -= 1
    }
    temp = B.get8()
    B(0)
    INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)
  }

  // OTIR
  private final def fn0xed0xb3(x: Int): Unit = {
    addTStates(-5)
    var temp = B.get8().intValue()
    var acu = 0
    if (temp == 0) temp = 0x100
    while ( {
      temp != 0
    }) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      acu = MMU.get8(HL)
      MMU.out8(C, acu)
      HL.increment()
      temp -= 1
    }
    temp = B
    B(0)
    INOUTFLAGS_ZERO(L, acu, temp)
  }

  // LDDR
  private final def fn0xed0xb8(x: Int): Unit = {
    addTStates(-5)
    var acu: Int = 0
    if (BC.get16 == 0) BC(0x10000)
    while ( {
      BC.get16 != 0
    }) {
      addTStates(21)
      INCR(2)
      // CHECK_BREAK_TWO_BYTES(HL,DE)
      CHECK_LOG_BYTE(HL)
      CHECK_LOG_BYTE(DE)
      acu = MMU.get8(HL)
      HL.decrement()
      MMU.put8(DE, acu)
      DE.decrement()
      BC.decrement()
    }
    acu += A.get8()
    AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4))
  }

  // CPDR
  private final def fn0xed0xb9(op: Int = 0xb9): Unit = {
    addTStates(-5)
    val acu = A.get8().byteValue
    var temp= 0
    var sum = 0

    if (BC.get16 == 0) BC(0x10000)
    while (op != 0 && sum != 0) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      temp = MMU.get8(HL).byteValue
      HL.decrement()
      BC.decrement()
      val op = {
        if (BC.get16 != 0) 1 else 0
      }
      sum = acu - temp

    }
    val cbits = acu ^ temp ^ sum
    val nc = {
      if ((sum & 0xff) == 0) 1 else 0
    }
    AF((AF & ~0xfe) | (sum & 0x80) | (nc << 6) |
      (((sum - ((cbits & 16) >> 4)) & 2) << 4) |
      (cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
      op << 2 | 2)
    if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)
  }

  // INDR
  private final def fn0xed0xba(x: Int): Unit = {
    addTStates(-5)
    var temp: Int = {
      if (B.intValue == 0) 0x100 else B.get8()
    }
    var acu: Int = 0
    while ( {
      temp != 0
    }) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      acu = MMU.in8(C)
      MMU.put8(HL, acu)
      HL.decrement()
      temp -= 1
    }
    temp = B
    B(0)
    INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)
  }

  // OTDR
  private final def fn0xed0xbb(x: Int): Unit = {
    addTStates(-5)
    var temp: Int = {
      if (B.intValue == 0) 0x100 else B
    }
    var acu: Int = 0
    while ( {
      temp != 0
    }) {
      addTStates(21)
      INCR(1)
      CHECK_LOG_BYTE(HL)
      acu = MMU.in8(C)
      MMU.put8(HL, acu)
      HL.decrement()
      temp -= 1
    }
    temp = B
    B(0)
    INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)
  }


  private val fdexecMap: Map[Int, Function[Int, Unit]] = Map[Int, Int => Unit](
    0x09 -> fn0xfd0x09, 0x19 -> fn0xfd0x19, 0x21 -> fn0xfd0x21, 0x22 -> fn0xfd0x22,
    0x23 -> fn0xfd0x23, 0x24 -> fn0xfd0x24, 0x25 -> fn0xfd0x25, 0x26 -> fn0xfd0x26, 0x29 -> fn0xfd0x29,
    0x2a -> fn0xfd0x2a, 0x2b -> fn0xfd0x2b, 0x2c -> fn0xfd0x2c, 0x2d -> fn0xfd0x2d, 0x2e -> fn0xfd0x2e,
    0xe1 -> fn0xfd0xe1, 0xe3 -> fn0xfd0xe3, 0xe5 -> fn0xfd0xe5, 0xe9 -> fn0xfd0xe9,
    0x34 -> fn0xfd0x34, 0x35 -> fn0xfd0x35, 0x36 -> fn0xfd0x36, 0x39 -> fn0xfd0x39, 0xf9 -> fn0xfd0xf9,
    0x44 -> fn0xfd0x44, 0x45 -> fn0xfd0x45, 0x46 -> fn0xfd0x46, 0x4c -> fn0xfd0x4c, 0x4d -> fn0xfd0x4d, 0x4e -> fn0xfd0x4e,
    0x54 -> fn0xfd0x54, 0x55 -> fn0xfd0x55, 0x56 -> fn0xfd0x56, 0x5c -> fn0xfd0x5c, 0x5d -> fn0xfd0x5d, 0x5e -> fn0xfd0x5e,
    0x60 -> fn0xfd0x60, 0x61 -> fn0xfd0x61, 0x62 -> fn0xfd0x62, 0x63 -> fn0xfd0x63, 0x64 -> fn0xfd0x64, 0x65 -> fn0xfd0x65,
    0x66 -> fn0xfd0x66, 0x67 -> fn0xfd0x67, 0x68 -> fn0xfd0x68, 0x69 -> fn0xfd0x69, 0x6a -> fn0xfd0x6a, 0x6b -> fn0xfd0x6b,
    0x6c -> fn0xfd0x6c, 0x6e -> fn0xfd0x6e, 0x6f -> fn0xfd0x6f, 0xcb -> fn0xfd0xcb,
    0xb4 -> fn0xfd0xb4, 0xb5 -> fn0xfd0xb5, 0xb6 -> fn0xfd0xb6, 0xbc -> fn0xfd0xbc,0xbd -> fn0xfd0xbd,0xbe -> fn0xfd0xbe,
    0x70-> fn0xfd0x70,0x71-> fn0xfd0x71,0x72-> fn0xfd0x72,0x73-> fn0xfd0x73,0x74-> fn0xfd0x74,
    0x75-> fn0xfd0x75, 0x77-> fn0xfd0x77, 0x7c-> fn0xfd0x7c,0x7d-> fn0xfd0x7d,0x7e-> fn0xfd0x7e,
    0x84-> fn0xfd0x84,0x85-> fn0xfd0x85,0x86-> fn0xfd0x86,0x8c-> fn0xfd0x8c,0x8d-> fn0xfd0x8d,0x8e-> fn0xfd0x8e,
    0x94-> fn0xfd0x94,0x95-> fn0xfd0x95,0x96-> fn0xfd0x96,0x9c-> fn0xfd0x9c,0x9d-> fn0xfd0x9d,0x9e-> fn0xfd0x9e,
    0xa4-> fn0xfd0xa4,0xa5-> fn0xfd0xa5,0xa6-> fn0xfd0xa6,0xac-> fn0xfd0xac,0xad-> fn0xfd0xad,0xae-> fn0xfd0xae
  )

  /**
   * FD Prefix
   */
  private def fdprefix(op: Int): Unit = {

    fdexecMap.get(op) match {
      case Some(x) => x(op)
      case None => PC.decrement()
    }
  }

  // ADD IY,BC
  private final def fn0xfd0x09(x: Int): Unit = {
    addTStates(15)
    ADD(IY, BC)
  }

  // ADD IY,DE
  private final def fn0xfd0x19(x: Int): Unit = {
    addTStates(15)
    ADD(IY, DE)
  }

  // LD IY,nnnn
  private final def fn0xfd0x21(x: Int): Unit = {
    addTStates(14)
    IY(MMU.get16(PC))
    PC(PC + 2)
  }

  // LD (nnnn),IY
  private final def fn0xfd0x22(x: Int): Unit = {
    addTStates(20)
    val temp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(temp)
    MMU.put16(temp, IY)
    PC(PC + 2)
  }

  // INC IY
  private final def fn0xfd0x23(x: Int): Unit = {
    addTStates(10)
    INC(IY)
    //IY.increment()
  }

  // INC IYH
  private final def fn0xfd0x24(x: Int): Unit = {
    addTStates(9)
    INC(IYH)
    //IYH.increment()
    //AF((AF & ~0xfe) | incZ80Table(IYH))
  }

  // DEC IYH
  private final def fn0xfd0x25(x: Int): Unit = {
    addTStates(9)
    DEC(IYH)
/*    IYH.decrement()
    AF((AF & ~0xfe) | decZ80Table(IYH)) */
  }

  // LD IYH,nn
  private final def fn0xfd0x26(x: Int): Unit = {
    addTStates(9)
    IYH(MMU.get8PC(PC))
  }

  // ADD IY,IY
  private final def fn0xfd0x29(x: Int): Unit = {
    addTStates(15)
    ADD(IY,IY)
/*    val sum: UInt = IY.get16 + IY.get16
    AF((AF & ~0x3b) | cbitsDup16Table(sum.intValue >> 8))
    IY(sum & 0xffff)
*/
  }

  // LD IY,(nnnn)
  private final def fn0xfd0x2a(x: Int): Unit = {
    addTStates(20)
    val tmp: Int = MMU.get16(PC)
    CHECK_LOG_WORD(tmp)
    IY(MMU.get16(tmp))
    PC(PC + 2)
  }

  // DEC IY
  private final def fn0xfd0x2b(x: Int): Unit = {
    addTStates(10)
    IY.decrement()
  }

  // INC IYL
  private final def fn0xfd0x2c(x: Int): Unit = {
    addTStates(9)
    INC(IYL)
    //IYL.increment()
    //AF((AF & ~0xfe) | incZ80Table(IYL))
  }

  // DEC IYL
  private final def fn0xfd0x2d(x: Int): Unit = {
    addTStates(9)
    DEC(IYL)
    /* IYL.decrement()
    AF((AF & ~0xfe) | decZ80Table(IYL)) */
  }

  // LD IYL,nn
  private final def fn0xfd0x2e(x: Int): Unit = {
    addTStates(9)
    IYL(MMU.get8PC(PC))
  }

  // INC (IY+dd)
  private final def fn0xfd0x34(x: Int): Unit = {
    addTStates(23)
    INCIDXdd(IY)
  }

  // DEC (IY+dd)
  private final def fn0xfd0x35(x: Int): Unit = {
    addTStates(23)
    DECIDXdd(IY)
  }

  // LD (IY+dd),nn
  private final def fn0xfd0x36(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IY.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    MMU.put8(adr, MMU.get8PC(PC))
  }

  // ADD IY,SP
  private final def fn0xfd0x39(x: Int): Unit = {
    addTStates(15)
    ADD(IY,SP)
/*    val sum: UInt = IY.get16 + SP.get16
    AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IY ^ SP ^ sum) >> 8))
    IY(sum & 0xffff) */
  }

  // LD B,IYH
  private final def fn0xfd0x44(x: Int): Unit = {
    addTStates(9)
    B(IYH)
  }

  // LD B,IYL
  private final def fn0xfd0x45(x: Int): Unit = {
    addTStates(9)
    B(IYL)
  }

  // LD B,(IY+dd)
  private final def fn0xfd0x46(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(B, IY)
  }

  // LD C,IYH
  private final def fn0xfd0x4c(x: Int): Unit = {
    addTStates(9)
    C(IYH)
  }

  // LD C,IYL
  private final def fn0xfd0x4d(x: Int): Unit = {
    addTStates(9)
    C(IYL)
  }

  // LD C,(IY+dd)
  private final def fn0xfd0x4e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(C, IY)
  }

  // LD D,IYH
  private final def fn0xfd0x54(x: Int): Unit = {
    addTStates(9)
    D(IYH)
  }

  // LD D,IYL
  private final def fn0xfd0x55(x: Int): Unit = {
    addTStates(9)
    D(IYL)
  }

  // LD D,(IY+dd)
  private final def fn0xfd0x56(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(D, IY)
  }

  // LD E,IYH
  private final def fn0xfd0x5c(x: Int): Unit = {
    addTStates(9)
    E(IYH)
  }

  // LD E,IYL
  private final def fn0xfd0x5d(x: Int): Unit = {
    addTStates(9)
    E(IYL)
  }

  // LD E,(IY+dd)
  private final def fn0xfd0x5e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(E, IY)
  }

  // LD IYH,B
  private final def fn0xfd0x60(x: Int): Unit = {
    addTStates(9)
    IYH(B)
  }

  // LD IYH,C
  private final def fn0xfd0x61(x: Int): Unit = {
    addTStates(9)
    IYH(C)
  }

  // LD IYH,D
  private final def fn0xfd0x62(x: Int): Unit = {
    addTStates(9)
    IYH(D)
  }

  // LD IYH,E
  private final def fn0xfd0x63(x: Int): Unit = {
    addTStates(9)
    IYH(E)
  }

  // LD IYH,IYH
  private final def fn0xfd0x64(x: Int): Unit = {
    addTStates(9)
  }

  // LD IYH,IYL
  private final def fn0xfd0x65(x: Int): Unit = {
    addTStates(9)
    IYH(IYL)
  }

  // LD H,(IY+dd)
  private final def fn0xfd0x66(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(H, IY)
  }

  // LD IYH,A
  private final def fn0xfd0x67(x: Int): Unit = {
    addTStates(9)
    IYH(A)
  }

  // LD IYL,B
  private final def fn0xfd0x68(x: Int): Unit = {
    addTStates(9)
    IYL(B)
  }

  // LD IYL,C
  private final def fn0xfd0x69(x: Int): Unit = {
    addTStates(9)
    IYL(C)
  }

  // LD IYL,D
  private final def fn0xfd0x6a(x: Int): Unit = {
    addTStates(9)
    IYL(D)
  }

  // LD IYL,E
  private final def fn0xfd0x6b(x: Int): Unit = {
    addTStates(9)
    IYL(E)
  }

  // LD IYL,IYH
  private final def fn0xfd0x6c(x: Int): Unit = {
    addTStates(9)
    IYL(IYH)
  }

  // LD L,(IY+dd)
  private final def fn0xfd0x6e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(L, IY)
  }

  // LD IYL,A
  private final def fn0xfd0x6f(x: Int): Unit = {
    addTStates(9)
    IYL(A)
  }

  // LD (IY+dd),B
  private final def fn0xfd0x70(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, B)
  }

  // LD (IY+dd),C
  private final def fn0xfd0x71(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, C)
  }

  // LD (IY+dd),D
  private final def fn0xfd0x72(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, D)
  }

  // LD (IY+dd),E
  private final def fn0xfd0x73(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, E)
  }

  // LD (IY+dd),H
  private final def fn0xfd0x74(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, H)
  }

  // LD (IY+dd),L
  private final def fn0xfd0x75(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, L)
  }

  // LD (IY+dd),A
  private final def fn0xfd0x77(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(IY, A)
  }

  // LD A,IYH
  private final def fn0xfd0x7c(x: Int): Unit = {
    addTStates(9)
    A(IYH)
  }

  // LD A,IYL
  private final def fn0xfd0x7d(x: Int): Unit = {
    addTStates(9)
    A(IYL)
  }

  // LD A,(IY+dd)
  private final def fn0xfd0x7e(x: Int): Unit = {
    addTStates(19)
    LDIDXdd(A, IY)
  }

  // ADD A,IYH
  private final def fn0xfd0x84(x: Int): Unit = {
    addTStates(9)
    ADD(A, IYH)
  }

  // ADD A,IYL
  private final def fn0xfd0x85(x: Int): Unit = {
    addTStates(9)
    ADD(A, IYL)
  }

  // ADD A,(IY+dd)
  private final def fn0xfd0x86(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IY.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    ADD(A, MMU.get8(adr))
  }

  // ADC A,IYH
  private final def fn0xfd0x8c(x: Int): Unit = {
    addTStates(9)
    ADC(A, IYH)
  }

  // ADC A,IYL
  private final def fn0xfd0x8d(x: Int): Unit = {
    addTStates(9)
    ADC(A, IYL)
  }

  // ADC A,(IY+dd)
  private final def fn0xfd0x8e(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IY.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    ADC(A, MMU.get8(adr))
  }

  // SUB (IY+dd)
  private final def fn0xfd0x96(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IY.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    SUB(MMU.get8(adr))
  }

  // SUB IYH
  private final def fn0xfd0x94(x: Int): Unit = {
    addTStates(9)
    //setFlag(FLAG_C, clear = true)
    SUB(IYH)
  }

  // SBC A,IYH
  private final def fn0xfd0x9c(x: Int): Unit = {
    addTStates(9)
    SBC(A, IYH)
  }

  // SUB IYL
  private final def fn0xfd0x95(x: Int): Unit = {
    addTStates(9)
    //setFlag(FLAG_C, clear = true)
    SUB(IYL)
  }

  // SBC A,IYL
  private final def fn0xfd0x9d(x: Int): Unit = {
    addTStates(9)
    SBC(A, IYL)
  }

  // SBC A,(IY+dd)
  private final def fn0xfd0x9e(x: Int): Unit = {
    addTStates(19)
    val adr: Int = IY.get16.intValue() + MMU.get8PC(PC).byteValue
    CHECK_LOG_BYTE(adr)
    SBC(A, MMU.get8(adr))
  }

  // AND IYH
  private final def fn0xfd0xa4(x: Int): Unit = {
    addTStates(9)
    AND(IYH)
  }

  // AND IYL
  private final def fn0xfd0xa5(x: Int): Unit = {
    addTStates(9)
    AND(IYL)
  }

  // AND (IY+dd)
  private final def fn0xfd0xa6(x: Int): Unit = {
    addTStates(19)
    ANDIDXdd(IY)
  }

  // XOR IYH
  private final def fn0xfd0xac(x: Int): Unit = {
    addTStates(9)
    XOR(IYH)
  }

  // XOR IYL
  private final def fn0xfd0xad(x: Int): Unit = {
    addTStates(9)
    XOR(IYL)
  }

  // XOR (IY+dd)
  private final def fn0xfd0xae(x: Int): Unit = {
    addTStates(19)
    XORIDXdd(IY)
  }

  // OR IYH
  private final def fn0xfd0xb4(x: Int): Unit = {
    addTStates(9)
    OR(IYH)
  }

  // OR IYL
  private final def fn0xfd0xb5(x: Int): Unit = {
    addTStates(9)
    OR(IYL)
  }

  // OR (IY+dd)
  private final def fn0xfd0xb6(x: Int): Unit = {
    addTStates(19)
    ORIDXdd(IY)
  }

  // CP IYH
  private final def fn0xfd0xbc(x: Int): Unit = {
    addTStates(9)
    ICP(IYH)
  }

  // CP IYL
  private final def fn0xfd0xbd(x: Int): Unit = {
    addTStates(9)
    ICP(IYL)
  }

  // CP (IY+dd)
  private final def fn0xfd0xbe(x: Int): Unit = {
    addTStates(9)
    CPIDXdd(IY)
  }

  // ******************************************************** FD CB Prefix
  private final def fn0xfd0xcb(x: Int): Unit = {
    val adr: Int = IY + MMU.get8PC(PC).byteValue
    val op: Int = MMU.get8(PC)
    fdcbprefix(op, adr)
  }

  // POP IY
  private final def fn0xfd0xe1(x: Int): Unit = {
    addTStates(14)
    CHECK_LOG_WORD(SP)
    POP(IY)
  }

  // EX (SP),IY
  private final def fn0xfd0xe3(x: Int): Unit = {
    addTStates(23)
    CHECK_LOG_WORD(SP)
    val tmp: UInt = IY.get16
    POP(IY)
    PUSH(tmp.intValue)
  }

  // PUSH IY
  private final def fn0xfd0xe5(x: Int): Unit = {
    addTStates(15)
    CHECK_LOG_WORD(SP - 2)
    PUSH(IY)
  }

  // JP (IY)
  private final def fn0xfd0xe9(x: Int): Unit = {
    addTStates(8)
    PC(IY)
  }

  // LD SP,IY
  private final def fn0xfd0xf9(x: Int): Unit = {
    addTStates(10)
    SP(IY)
  }


  /**
   * DD CB Prefix
   */
  private def ddcbprefix(op: Int, adr: Int): Unit = {

    var acu: Int = 0
    var temp: Int = 0
    var cbits: Int = 0

    op & 7 match {
      case 0 =>
        PC.increment()
        acu = B.get8()

      case 1 =>
        PC.increment()
        acu = C.get8()

      case 2 =>
        PC.increment()
        acu = D.get8()

      case 3 =>
        PC.increment()
        acu = E.get8()

      case 4 =>
        PC.increment()
        acu = H.get8()

      case 5 =>
        PC.increment()
        acu = L.get8()

      case 6 =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)

      case 7 =>
        PC.increment()
        acu = A.get8()

      case _ =>

    }
    op & 0xc0 match {
      case 0x00 =>
        // Shift/rotate
        addTStates(23)
        op & 0x38 match {
          case 0x00 => // RLC
            temp = ((acu << 1) | (acu >> 7))
            cbits = (temp & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x08 => // RRC
            temp = ((acu >> 1) | (acu << 7))
            cbits = temp & 0x80
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x10 => // RL
            temp = ((acu << 1) | {
              if (testFlag(F, FLAG_C)) 1 else 0
            })
            cbits = (acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x18 => // RR
            temp = ((acu >> 1) | ({
              if (testFlag(F, FLAG_C)) 1 else 0
            } << 7))
            cbits = acu & 1
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x20 => // SLA
            temp = (acu << 1)
            cbits = (acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x28 => // SRA
            temp = ((acu >> 1) | (acu & 0x80))
            cbits = acu & 1
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x30 => // SLIA
            temp = ((acu << 1) | 1)
            cbits = (acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x38 => // SRL
            temp = (acu >> 1)
            cbits = (acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case _ =>
        }


      case 0x40 => // BIT
        addTStates(20)
        if ((acu & (1 << ((op >> 3) & 7))) != 0) AF((AF & ~0xfe) | 0x10 | ({
          if ((op & 0x38) == 0x38) 1 else 0
        } << 7))
        else AF((AF & ~0xfe) | 0x54)
        if ((op & 7) != 6) AF(AF | (acu & 0x28))
        temp = acu

      case 0x80 => // RES
        addTStates(23)
        temp = acu & ~(1 << ((op >> 3) & 7))

      case 0xc0 => // SET
        addTStates(23)
        temp = (acu | (1 << ((op >> 3) & 7)))

      case _ =>
    }
    op & 7 match {
      case 0 =>
        B(temp)

      case 1 =>
        C(temp)

      case 2 =>
        D(temp)

      case 3 =>
        E(temp)

      case 4 =>
        H(temp)

      case 5 =>
        L(temp)

      case 6 =>
        MMU.put8(adr, UByte(temp.byteValue()))

      case 7 =>
        A(temp)

      case _ =>
    }

  }

  /**
   * FD CB Prefix ops
   */
  private def fdcbprefix(op: Int, adr: Int): Unit = {

    var acu: Int = 0
    var cbits: Int = 0
    var tmp: Int = 0

    op & 7 match {
      case 0 =>
        PC.increment()
        acu = B.get8()
      case 1 =>
        PC.increment()
        acu = C.get8()
      case 2 =>
        PC.increment()
        acu = D.get8()
      case 3 =>
        PC.increment()
        acu = E.get8()
      case 4 =>
        PC.increment()
        acu = H.get8()
      case 5 =>
        PC.increment()
        acu = L.get8()
      case 6 =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)
      case 7 =>
        PC.increment()
        acu = A.get8()
      case _ =>
    }
    op & 0xc0 match {

      case 0x00 => // shift/rotate
        op & 0x38 match {
          case 0x00 => // RLC
            tmp = ((acu << 1) | (acu >> 7))
            cbits = (tmp & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x08 => // RRC
            tmp = (acu >> 1) | (acu << 7)
            cbits = (tmp & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) (0) else (1)
            })

          case 0x10 => // RL
            tmp = (acu << 1) | {
              if (testFlag(F, FLAG_C)) 1 else 0
            }
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x18 => // RR
            tmp = (acu >> 1) | ({
              if (testFlag(F, FLAG_C)) 1 else 0
            } << 7)
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x20 => // SLA
            tmp = acu << 1
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x28 => // SRA
            tmp = ((acu >> 1) | (acu & 0x80))
            cbits = (acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x30 => // SLIA
            tmp = (acu << 1) | 1
            cbits = (acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case 0x38 => // SRL
            tmp = acu >> 1
            cbits = acu & 1
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case _ =>
        }

      case 0x40 => // BIT
        addTStates(20)
        if ( {
          if ((acu & (1 << ((op >> 3) & 7))) != 0)
            true else false
        })
          AF((AF & ~0xfe) | 0x10 | ({
            if ((op & 0x38) == 0x380) 1 else 0
          } << 7))
        else
          AF(AF | (acu & 0x28))
        tmp = acu

      case 0x80 => // RES
        addTStates(23)
        tmp = (acu & ~(1 << ((op >> 3) & 7)))

      case 0xc0 => // SET
        addTStates(23)
        tmp = (acu | (1 << ((op >> 3) & 7)))

      case _ =>
    }
    op & 7 match {
      case 0 =>
        B(tmp & 0xff)
      case 1 =>
        C(tmp & 0xff)
      case 2 =>
        D(tmp & 0xff)
      case 3 =>
        E(tmp & 0xff)
      case 4 =>
        H(tmp & 0xff)
      case 5 =>
        L(tmp & 0xff)
      case 6 =>
        MMU.put8(adr, UByte(tmp.byteValue()))

      case 7 =>
        A(tmp & 0xff)

      case _ =>
    }

  }
}


object Z80 {

  final val MnemonicsZ80: List[String] =
    List("NOP", "LD BC,#h", "LD (BC),A", "INC BC", "INC B", "DEC B", "LD B,*h", "RLCA", /*  00-07   */
      "EX AF,AF'", "ADD HL,BC", "LD A,(BC)", "DEC BC", "INC C", "DEC C", "LD C,*h", "RRCA", /*  08-0f   */
      "DJNZ $h", "LD DE,#h", "LD (DE),A", "INC DE", "INC D", "DEC D", "LD D,*h", "RLA", /*  10-17   */
      "JR $h", "ADD HL,DE", "LD A,(DE)", "DEC DE", "INC E", "DEC E", "LD E,*h", "RRA", /*  18-1f   */
      "JR NZ,$h", "LD HL,#h", "LD (#h),HL", "INC HL", "INC H", "DEC H", "LD H,*h", "DAA", /*  20-27   */
      "JR Z,$h", "ADD HL,HL", "LD HL,(#h)", "DEC HL", "INC L", "DEC L", "LD L,*h", "CPL", /*  28-2f   */
      "JR NC,$h", "LD SP,#h", "LD (#h),A", "INC SP", "INC (HL)", "DEC (HL)", "LD (HL),*h", "SCF", /*  30-37   */
      "JR C,$h", "ADD HL,SP", "LD A,(#h)", "DEC SP", "INC A", "DEC A", "LD A,*h", "CCF", /*  38-3f   */
      "LD B,B", "LD B,C", "LD B,D", "LD B,E", "LD B,H", "LD B,L", "LD B,(HL)", "LD B,A", /*  40-47   */
      "LD C,B", "LD C,C", "LD C,D", "LD C,E", "LD C,H", "LD C,L", "LD C,(HL)", "LD C,A", /*  48-4f   */
      "LD D,B", "LD D,C", "LD D,D", "LD D,E", "LD D,H", "LD D,L", "LD D,(HL)", "LD D,A", /*  50-57   */
      "LD E,B", "LD E,C", "LD E,D", "LD E,E", "LD E,H", "LD E,L", "LD E,(HL)", "LD E,A", /*  58-5f   */
      "LD H,B", "LD H,C", "LD H,D", "LD H,E", "LD H,H", "LD H,L", "LD H,(HL)", "LD H,A", /*  60-67   */
      "LD L,B", "LD L,C", "LD L,D", "LD L,E", "LD L,H", "LD L,L", "LD L,(HL)", "LD L,A", /*  68-6f   */
      "LD (HL),B", "LD (HL),C", "LD (HL),D", "LD (HL),E", "LD (HL),H", "LD (HL),L", "HALT", "LD (HL),A", /*  70-77   */
      "LD A,B", "LD A,C", "LD A,D", "LD A,E", "LD A,H", "LD A,L", "LD A,(HL)", "LD A,A", /*  78-7f   */
      "ADD A,B", "ADD A,C", "ADD A,D", "ADD A,E", "ADD A,H", "ADD A,L", "ADD A,(HL)", "ADD A,A", /*  80-87   */
      "ADC A,B", "ADC A,C", "ADC A,D", "ADC A,E", "ADC A,H", "ADC A,L", "ADC A,(HL)", "ADC A,A", /*  88-8f   */
      "SUB B", "SUB C", "SUB D", "SUB E", "SUB H", "SUB L", "SUB (HL)", "SUB A", /*  90-97   */
      "SBC A,B", "SBC A,C", "SBC A,D", "SBC A,E", "SBC A,H", "SBC A,L", "SBC A,(HL)", "SBC A,A", /*  98-9f   */
      "AND B", "AND C", "AND D", "AND E", "AND H", "AND L", "AND (HL)", "AND A", /*  a0-a7   */
      "XOR B", "XOR C", "XOR D", "XOR E", "XOR H", "XOR L", "XOR (HL)", "XOR A", /*  a8-af   */
      "OR B", "OR C", "OR D", "OR E", "OR H", "OR L", "OR (HL)", "OR A", /*  b0-b7   */
      "CP B", "CP C", "CP D", "CP E", "CP H", "CP L", "CP (HL)", "CP A", /*  b8-bf   */
      "RET NZ", "POP BC", "JP NZ,#h", "JP #h", "CALL NZ,#h", "PUSH BC", "ADD A,*h", "RST 00h", /*  c0-c7   */
      "RET Z", "RET", "JP Z,#h", "PFX_CB", "CALL Z,#h", "CALL #h", "ADC A,*h", "RST 08h", /*  c8-cf   */
      "RET NC", "POP DE", "JP NC,#h", "OUT (*h),A", "CALL NC,#h", "PUSH DE", "SUB *h", "RST 10h", /*  d0-d7   */
      "RET C", "EXX", "JP C,#h", "IN A,(*h)", "CALL C,#h", "PFX_DD", "SBC A,*h", "RST 18h", /*  d8-df   */
      "RET PO", "POP HL", "JP PO,#h", "EX (SP),HL", "CALL PO,#h", "PUSH HL", "AND *h", "RST 20h", /*  e0-e7   */
      "RET PE", "LD PC,HL", "JP PE,#h", "EX DE,HL", "CALL PE,#h", "PFX_ED", "XOR *h", "RST 28h", /*  e8-ef   */
      "RET P", "POP AF", "JP P,#h", "DI", "CALL P,#h", "PUSH AF", "OR *h", "RST 30h", /*  f0-f7   */
      "RET M", "LD SP,HL", "JP M,#h", "EI", "CALL M,#h", "PFX_FD", "CP *h", "RST 38h") /*  f8-ff   */


  final val MnemonicsCB: List[String] =
    List("RLC B", "RLC C", "RLC D", "RLC E", "RLC H", "RLC L", "RLC (HL)", "RLC A", /*  00-07   */
      "RRC B", "RRC C", "RRC D", "RRC E", "RRC H", "RRC L", "RRC (HL)", "RRC A", /*  08-0f   */
      "RL B", "RL C", "RL D", "RL E", "RL H", "RL L", "RL (HL)", "RL A", /*  10-17   */
      "RR B", "RR C", "RR D", "RR E", "RR H", "RR L", "RR (HL)", "RR A", /*  18-1f   */
      "SLA B", "SLA C", "SLA D", "SLA E", "SLA H", "SLA L", "SLA (HL)", "SLA A", /*  20-27   */
      "SRA B", "SRA C", "SRA D", "SRA E", "SRA H", "SRA L", "SRA (HL)", "SRA A", /*  28-2f   */
      "SLL B", "SLL C", "SLL D", "SLL E", "SLL H", "SLL L", "SLL (HL)", "SLL A", /*  30-37   */
      "SRL B", "SRL C", "SRL D", "SRL E", "SRL H", "SRL L", "SRL (HL)", "SRL A", /*  38-3f   */
      "BIT 0,B", "BIT 0,C", "BIT 0,D", "BIT 0,E", "BIT 0,H", "BIT 0,L", "BIT 0,(HL)", "BIT 0,A", /*  40-47   */
      "BIT 1,B", "BIT 1,C", "BIT 1,D", "BIT 1,E", "BIT 1,H", "BIT 1,L", "BIT 1,(HL)", "BIT 1,A", /*  48-4f   */
      "BIT 2,B", "BIT 2,C", "BIT 2,D", "BIT 2,E", "BIT 2,H", "BIT 2,L", "BIT 2,(HL)", "BIT 2,A", /*  50-57   */
      "BIT 3,B", "BIT 3,C", "BIT 3,D", "BIT 3,E", "BIT 3,H", "BIT 3,L", "BIT 3,(HL)", "BIT 3,A", /*  58-5f   */
      "BIT 4,B", "BIT 4,C", "BIT 4,D", "BIT 4,E", "BIT 4,H", "BIT 4,L", "BIT 4,(HL)", "BIT 4,A", /*  60-67   */
      "BIT 5,B", "BIT 5,C", "BIT 5,D", "BIT 5,E", "BIT 5,H", "BIT 5,L", "BIT 5,(HL)", "BIT 5,A", /*  68-6f   */
      "BIT 6,B", "BIT 6,C", "BIT 6,D", "BIT 6,E", "BIT 6,H", "BIT 6,L", "BIT 6,(HL)", "BIT 6,A", /*  70-77   */
      "BIT 7,B", "BIT 7,C", "BIT 7,D", "BIT 7,E", "BIT 7,H", "BIT 7,L", "BIT 7,(HL)", "BIT 7,A", /*  78-7f   */
      "RES 0,B", "RES 0,C", "RES 0,D", "RES 0,E", "RES 0,H", "RES 0,L", "RES 0,(HL)", "RES 0,A", /*  80-87   */
      "RES 1,B", "RES 1,C", "RES 1,D", "RES 1,E", "RES 1,H", "RES 1,L", "RES 1,(HL)", "RES 1,A", /*  88-8f   */
      "RES 2,B", "RES 2,C", "RES 2,D", "RES 2,E", "RES 2,H", "RES 2,L", "RES 2,(HL)", "RES 2,A", /*  90-97   */
      "RES 3,B", "RES 3,C", "RES 3,D", "RES 3,E", "RES 3,H", "RES 3,L", "RES 3,(HL)", "RES 3,A", /*  98-9f   */
      "RES 4,B", "RES 4,C", "RES 4,D", "RES 4,E", "RES 4,H", "RES 4,L", "RES 4,(HL)", "RES 4,A", /*  a0-a7   */
      "RES 5,B", "RES 5,C", "RES 5,D", "RES 5,E", "RES 5,H", "RES 5,L", "RES 5,(HL)", "RES 5,A", /*  a8-af   */
      "RES 6,B", "RES 6,C", "RES 6,D", "RES 6,E", "RES 6,H", "RES 6,L", "RES 6,(HL)", "RES 6,A", /*  b0-b7   */
      "RES 7,B", "RES 7,C", "RES 7,D", "RES 7,E", "RES 7,H", "RES 7,L", "RES 7,(HL)", "RES 7,A", /*  b8-bf   */
      "SET 0,B", "SET 0,C", "SET 0,D", "SET 0,E", "SET 0,H", "SET 0,L", "SET 0,(HL)", "SET 0,A", /*  c0-c7   */
      "SET 1,B", "SET 1,C", "SET 1,D", "SET 1,E", "SET 1,H", "SET 1,L", "SET 1,(HL)", "SET 1,A", /*  c8-cf   */
      "SET 2,B", "SET 2,C", "SET 2,D", "SET 2,E", "SET 2,H", "SET 2,L", "SET 2,(HL)", "SET 2,A", /*  d0-d7   */
      "SET 3,B", "SET 3,C", "SET 3,D", "SET 3,E", "SET 3,H", "SET 3,L", "SET 3,(HL)", "SET 3,A", /*  d8-df   */
      "SET 4,B", "SET 4,C", "SET 4,D", "SET 4,E", "SET 4,H", "SET 4,L", "SET 4,(HL)", "SET 4,A", /*  e0-e7   */
      "SET 5,B", "SET 5,C", "SET 5,D", "SET 5,E", "SET 5,H", "SET 5,L", "SET 5,(HL)", "SET 5,A", /*  e8-ef   */
      "SET 6,B", "SET 6,C", "SET 6,D", "SET 6,E", "SET 6,H", "SET 6,L", "SET 6,(HL)", "SET 6,A", /*  f0-f7   */
      "SET 7,B", "SET 7,C", "SET 7,D", "SET 7,E", "SET 7,H", "SET 7,L", "SET 7,(HL)", "SET 7,A") /*  f8-ff   */


  final val MnemonicsED: List[String] =
    List("DB EDh,00h", "DB EDh,01h", "DB EDh,02h", "DB EDh,03h", "DB EDh,04h", "DB EDh,05h", "DB EDh,06h", "DB EDh,07h", /*  00-07   */
      "DB EDh,08h", "DB EDh,09h", "DB EDh,0Ah", "DB EDh,0Bh", "DB EDh,0Ch", "DB EDh,0Dh", "DB EDh,0Eh", "DB EDh,0Fh", /*  08-0f   */
      "DB EDh,10h", "DB EDh,11h", "DB EDh,12h", "DB EDh,13h", "DB EDh,14h", "DB EDh,15h", "DB EDh,16h", "DB EDh,17h", /*  10-17   */
      "DB EDh,18h", "DB EDh,19h", "DB EDh,1Ah", "DB EDh,1Bh", "DB EDh,1Ch", "DB EDh,1Dh", "DB EDh,1Eh", "DB EDh,1Fh", /*  18-1f   */
      "DB EDh,20h", "DB EDh,21h", "DB EDh,22h", "DB EDh,23h", "DB EDh,24h", "DB EDh,25h", "DB EDh,26h", "DB EDh,27h", /*  20-27   */
      "DB EDh,28h", "DB EDh,29h", "DB EDh,2Ah", "DB EDh,2Bh", "DB EDh,2Ch", "DB EDh,2Dh", "DB EDh,2Eh", "DB EDh,2Fh", /*  28-2f   */
      "DB EDh,30h", "DB EDh,31h", "DB EDh,32h", "DB EDh,33h", "DB EDh,34h", "DB EDh,35h", "DB EDh,36h", "DB EDh,37h", /*  30-37   */
      "DB EDh,38h", "DB EDh,39h", "DB EDh,3Ah", "DB EDh,3Bh", "DB EDh,3Ch", "DB EDh,3Dh", "DB EDh,3Eh", "DB EDh,3Fh", /*  38-3f   */
      "IN B,(C)", "OUT (C),B", "SBC HL,BC", "LD (#h),BC", "NEG", "RETN", "IM 0", "LD I,A", /*  40-47   */
      "IN C,(C)", "OUT (C),C", "ADC HL,BC", "LD BC,(#h)", "DB EDh,4Ch", "RETI", "DB EDh,4Eh", "LD R,A", /*  48-4f   */
      "IN D,(C)", "OUT (C),D", "SBC HL,DE", "LD (#h),DE", "DB EDh,54h", "DB EDh,55h", "IM 1", "LD A,I", /*  50-57   */
      "IN E,(C)", "OUT (C),E", "ADC HL,DE", "LD DE,(#h)", "DB EDh,5Ch", "DB EDh,5Dh", "IM 2", "LD A,R", /*  58-5f   */
      "IN H,(C)", "OUT (C),H", "SBC HL,HL", "LD (#h),HL", "DB EDh,64h", "DB EDh,65h", "DB EDh,66h", "RRD", /*  60-67   */
      "IN L,(C)", "OUT (C),L", "ADC HL,HL", "LD HL,(#h)", "DB EDh,6Ch", "DB EDh,6Dh", "DB EDh,6Eh", "RLD", /*  68-6f   */
      "IN F,(C)", "DB EDh,71h", "SBC HL,SP", "LD (#h),SP", "DB EDh,74h", "DB EDh,75h", "DB EDh,76h", "DB EDh,77h", /*  70-77   */
      "IN A,(C)", "OUT (C),A", "ADC HL,SP", "LD SP,(#h)", "DB EDh,7Ch", "DB EDh,7Dh", "DB EDh,7Eh", "DB EDh,7Fh", /*  78-7f   */
      "DB EDh,80h", "DB EDh,81h", "DB EDh,82h", "DB EDh,83h", "DB EDh,84h", "DB EDh,85h", "DB EDh,86h", "DB EDh,87h", /*  80-87   */
      "DB EDh,88h", "DB EDh,89h", "DB EDh,8Ah", "DB EDh,8Bh", "DB EDh,8Ch", "DB EDh,8Dh", "DB EDh,8Eh", "DB EDh,8Fh", /*  88-8f   */
      "DB EDh,90h", "DB EDh,91h", "DB EDh,92h", "DB EDh,93h", "DB EDh,94h", "DB EDh,95h", "DB EDh,96h", "DB EDh,97h", /*  90-97   */
      "DB EDh,98h", "DB EDh,99h", "DB EDh,9Ah", "DB EDh,9Bh", "DB EDh,9Ch", "DB EDh,9Dh", "DB EDh,9Eh", "DB EDh,9Fh", /*  98-9f   */
      "LDI", "CPI", "INI", "OUTI", "DB EDh,A4h", "DB EDh,A5h", "DB EDh,A6h", "DB EDh,A7h", /*  a0-a7   */
      "LDD", "CPD", "IND", "OUTD", "DB EDh,ACh", "DB EDh,ADh", "DB EDh,AEh", "DB EDh,AFh", /*  a8-af   */
      "LDIR", "CPIR", "INIR", "OTIR", "DB EDh,B4h", "DB EDh,B5h", "DB EDh,B6h", "DB EDh,B7h", /*  b0-b7   */
      "LDDR", "CPDR", "INDR", "OTDR", "DB EDh,BCh", "DB EDh,BDh", "DB EDh,BEh", "DB EDh,BFh", /*  b8-bf   */
      "DB EDh,C0h", "DB EDh,C1h", "DB EDh,C2h", "DB EDh,C3h", "DB EDh,C4h", "DB EDh,C5h", "DB EDh,C6h", "DB EDh,C7h", /*  c0-c7   */
      "DB EDh,C8h", "DB EDh,C9h", "DB EDh,CAh", "DB EDh,CBh", "DB EDh,CCh", "DB EDh,CDh", "DB EDh,CEh", "DB EDh,CFh", /*  c8-cf   */
      "DB EDh,D0h", "DB EDh,D1h", "DB EDh,D2h", "DB EDh,D3h", "DB EDh,D4h", "DB EDh,D5h", "DB EDh,D6h", "DB EDh,D7h", /*  d0-d7   */
      "DB EDh,D8h", "DB EDh,D9h", "DB EDh,DAh", "DB EDh,DBh", "DB EDh,DCh", "DB EDh,DDh", "DB EDh,DEh", "DB EDh,DFh", /*  d8-df   */
      "DB EDh,E0h", "DB EDh,E1h", "DB EDh,E2h", "DB EDh,E3h", "DB EDh,E4h", "DB EDh,E5h", "DB EDh,E6h", "DB EDh,E7h", /*  e0-e7   */
      "DB EDh,E8h", "DB EDh,E9h", "DB EDh,EAh", "DB EDh,EBh", "DB EDh,ECh", "DB EDh,EDh", "DB EDh,EEh", "DB EDh,EFh", /*  e8-ef   */
      "DB EDh,F0h", "DB EDh,F1h", "DB EDh,F2h", "DB EDh,F3h", "DB EDh,F4h", "DB EDh,F5h", "DB EDh,F6h", "DB EDh,F7h", /*  f0-f7   */
      "DB EDh,F8h", "DB EDh,F9h", "DB EDh,FAh", "DB EDh,FBh", "DB EDh,FCh", "DB EDh,FDh", "DB EDh,FEh", "DB EDh,FFh") /*  f8-ff   */


  final val MnemonicsXX: List[String] =
    List("NOP", "LD BC,#h", "LD (BC),A", "INC BC", "INC B", "DEC B", "LD B,*h", "RLCA", /*  00-07   */
      "EX AF,AF'", "ADD I%,BC", "LD A,(BC)", "DEC BC", "INC C", "DEC C", "LD C,*h", "RRCA", /*  08-0f   */
      "DJNZ $h", "LD DE,#h", "LD (DE),A", "INC DE", "INC D", "DEC D", "LD D,*h", "RLA", /*  10-17   */
      "JR $h", "ADD I%,DE", "LD A,(DE)", "DEC DE", "INC E", "DEC E", "LD E,*h", "RRA", /*  18-1f   */
      "JR NZ,$h", "LD I%,#h", "LD (#h),I%", "INC I%", "INC I%H", "DEC I%H", "LD I%H,*h", "DAA", /*  20-27   */
      "JR Z,$h", "ADD I%,I%", "LD I%,(#h)", "DEC I%", "INC I%L", "DEC I%L", "LD I%L,*h", "CPL", /*  28-2f   */
      "JR NC,$h", "LD SP,#h", "LD (#h),A", "INC SP", "INC (I%+^h)", "DEC (I%+^h)", "LD (I%+^h),*h", "SCF", /*  30-37   */
      "JR C,$h", "ADD I%,SP", "LD A,(#h)", "DEC SP", "INC A", "DEC A", "LD A,*h", "CCF", /*  38-3f   */
      "LD B,B", "LD B,C", "LD B,D", "LD B,E", "LD B,I%H", "LD B,I%L", "LD B,(I%+^h)", "LD B,A", /*  40-47   */
      "LD C,B", "LD C,C", "LD C,D", "LD C,E", "LD C,I%H", "LD C,I%L", "LD C,(I%+^h)", "LD C,A", /*  48-4f   */
      "LD D,B", "LD D,C", "LD D,D", "LD D,E", "LD D,I%H", "LD D,I%L", "LD D,(I%+^h)", "LD D,A", /*  50-57   */
      "LD E,B", "LD E,C", "LD E,D", "LD E,E", "LD E,I%H", "LD E,I%L", "LD E,(I%+^h)", "LD E,A", /*  58-5f   */
      "LD I%H,B", "LD I%H,C", "LD I%H,D", "LD I%H,E", "LD I%H,I%H", "LD I%H,I%L", "LD H,(I%+^h)", "LD I%H,A", /*  60-67   */
      "LD I%L,B", "LD I%L,C", "LD I%L,D", "LD I%L,E", "LD I%L,I%H", "LD I%L,I%L", "LD L,(I%+^h)", "LD I%L,A", /*  68-6f   */
      "LD (I%+^h),B", "LD (I%+^h),C", "LD (I%+^h),D", "LD (I%+^h),E", "LD (I%+^h),H", "LD (I%+^h),L", "HALT", "LD (I%+^h),A", /*  70-77   */
      "LD A,B", "LD A,C", "LD A,D", "LD A,E", "LD A,I%H", "LD A,I%L", "LD A,(I%+^h)", "LD A,A", /*  78-7f   */
      "ADD A,B", "ADD A,C", "ADD A,D", "ADD A,E", "ADD A,I%H", "ADD A,I%L", "ADD A,(I%+^h)", "ADD A,A", /*  80-87   */
      "ADC A,B", "ADC A,C", "ADC A,D", "ADC A,E", "ADC A,I%H", "ADC A,I%L", "ADC A,(I%+^h)", "ADC A,A", /*  88-8f   */
      "SUB B", "SUB C", "SUB D", "SUB E", "SUB I%H", "SUB I%L", "SUB (I%+^h)", "SUB A", /*  90-97   */
      "SBC A,B", "SBC A,C", "SBC A,D", "SBC A,E", "SBC A,I%H", "SBC A,I%L", "SBC A,(I%+^h)", "SBC A,A", /*  98-9f   */
      "AND B", "AND C", "AND D", "AND E", "AND I%H", "AND I%L", "AND (I%+^h)", "AND A", /*  a0-a7   */
      "XOR B", "XOR C", "XOR D", "XOR E", "XOR I%H", "XOR I%L", "XOR (I%+^h)", "XOR A", /*  a8-af   */
      "OR B", "OR C", "OR D", "OR E", "OR I%H", "OR I%L", "OR (I%+^h)", "OR A", /*  b0-b7   */
      "CP B", "CP C", "CP D", "CP E", "CP I%H", "CP I%L", "CP (I%+^h)", "CP A", /*  b8-bf   */
      "RET NZ", "POP BC", "JP NZ,#h", "JP #h", "CALL NZ,#h", "PUSH BC", "ADD A,*h", "RST 00h", /*  c8-cf   */
      "RET Z", "RET", "JP Z,#h", "PFX_CB", "CALL Z,#h", "CALL #h", "ADC A,*h", "RST 08h", /*  c8-cf   */
      "RET NC", "POP DE", "JP NC,#h", "OUT (*h),A", "CALL NC,#h", "PUSH DE", "SUB *h", "RST 10h", /*  d0-d7   */
      "RET C", "EXX", "JP C,#h", "IN A,(*h)", "CALL C,#h", "PFX_DD", "SBC A,*h", "RST 18h", /*  d8-df   */
      "RET PO", "POP I%", "JP PO,#h", "EX (SP),I%", "CALL PO,#h", "PUSH I%", "AND *h", "RST 20h", /*  e0-e7   */
      "RET PE", "LD PC,I%", "JP PE,#h", "EX DE,I%", "CALL PE,#h", "PFX_ED", "XOR *h", "RST 28h", /*  e8-ef   */
      "RET P", "POP AF", "JP P,#h", "DI", "CALL P,#h", "PUSH AF", "OR *h", "RST 30h", /*  f0-f7   */
      "RET M", "LD SP,I%", "JP M,#h", "EI", "CALL M,#h", "PFX_FD", "CP *h", "RST 38h") /*  f8-ff   */


  final val MnemonicsXCB: List[String] =
    List("RLC B", "RLC C", "RLC D", "RLC E", "RLC H", "RLC L", "RLC (I%@h)", "RLC A", /*  00-07   */
      "RRC B", "RRC C", "RRC D", "RRC E", "RRC H", "RRC L", "RRC (I%@h)", "RRC A", /*  08-0f   */
      "RL B", "RL C", "RL D", "RL E", "RL H", "RL L", "RL (I%@h)", "RL A", /*  10-17   */
      "RR B", "RR C", "RR D", "RR E", "RR H", "RR L", "RR (I%@h)", "RR A", /*  18-1f   */
      "SLA B", "SLA C", "SLA D", "SLA E", "SLA H", "SLA L", "SLA (I%@h)", "SLA A", /*  20-27   */
      "SRA B", "SRA C", "SRA D", "SRA E", "SRA H", "SRA L", "SRA (I%@h)", "SRA A", /*  28-2f   */
      "SLL B", "SLL C", "SLL D", "SLL E", "SLL H", "SLL L", "SLL (I%@h)", "SLL A", /*  30-37   */
      "SRL B", "SRL C", "SRL D", "SRL E", "SRL H", "SRL L", "SRL (I%@h)", "SRL A", /*  38-3f   */
      "BIT 0,B", "BIT 0,C", "BIT 0,D", "BIT 0,E", "BIT 0,H", "BIT 0,L", "BIT 0,(I%@h)", "BIT 0,A", /*  40-47   */
      "BIT 1,B", "BIT 1,C", "BIT 1,D", "BIT 1,E", "BIT 1,H", "BIT 1,L", "BIT 1,(I%@h)", "BIT 1,A", /*  48-4f   */
      "BIT 2,B", "BIT 2,C", "BIT 2,D", "BIT 2,E", "BIT 2,H", "BIT 2,L", "BIT 2,(I%@h)", "BIT 2,A", /*  50-57   */
      "BIT 3,B", "BIT 3,C", "BIT 3,D", "BIT 3,E", "BIT 3,H", "BIT 3,L", "BIT 3,(I%@h)", "BIT 3,A", /*  58-5f   */
      "BIT 4,B", "BIT 4,C", "BIT 4,D", "BIT 4,E", "BIT 4,H", "BIT 4,L", "BIT 4,(I%@h)", "BIT 4,A", /*  60-67   */
      "BIT 5,B", "BIT 5,C", "BIT 5,D", "BIT 5,E", "BIT 5,H", "BIT 5,L", "BIT 5,(I%@h)", "BIT 5,A", /*  68-6f   */
      "BIT 6,B", "BIT 6,C", "BIT 6,D", "BIT 6,E", "BIT 6,H", "BIT 6,L", "BIT 6,(I%@h)", "BIT 6,A", /*  70-77   */
      "BIT 7,B", "BIT 7,C", "BIT 7,D", "BIT 7,E", "BIT 7,H", "BIT 7,L", "BIT 7,(I%@h)", "BIT 7,A", /*  78-7f   */
      "RES 0,B", "RES 0,C", "RES 0,D", "RES 0,E", "RES 0,H", "RES 0,L", "RES 0,(I%@h)", "RES 0,A", /*  80-87   */
      "RES 1,B", "RES 1,C", "RES 1,D", "RES 1,E", "RES 1,H", "RES 1,L", "RES 1,(I%@h)", "RES 1,A", /*  88-8f   */
      "RES 2,B", "RES 2,C", "RES 2,D", "RES 2,E", "RES 2,H", "RES 2,L", "RES 2,(I%@h)", "RES 2,A", /*  90-97   */
      "RES 3,B", "RES 3,C", "RES 3,D", "RES 3,E", "RES 3,H", "RES 3,L", "RES 3,(I%@h)", "RES 3,A", /*  98-9f   */
      "RES 4,B", "RES 4,C", "RES 4,D", "RES 4,E", "RES 4,H", "RES 4,L", "RES 4,(I%@h)", "RES 4,A", /*  a0-a7   */
      "RES 5,B", "RES 5,C", "RES 5,D", "RES 5,E", "RES 5,H", "RES 5,L", "RES 5,(I%@h)", "RES 5,A", /*  a8-af   */
      "RES 6,B", "RES 6,C", "RES 6,D", "RES 6,E", "RES 6,H", "RES 6,L", "RES 6,(I%@h)", "RES 6,A", /*  b0-b7   */
      "RES 7,B", "RES 7,C", "RES 7,D", "RES 7,E", "RES 7,H", "RES 7,L", "RES 7,(I%@h)", "RES 7,A", /*  b8-bf   */
      "SET 0,B", "SET 0,C", "SET 0,D", "SET 0,E", "SET 0,H", "SET 0,L", "SET 0,(I%@h)", "SET 0,A", /*  c0-c7   */
      "SET 1,B", "SET 1,C", "SET 1,D", "SET 1,E", "SET 1,H", "SET 1,L", "SET 1,(I%@h)", "SET 1,A", /*  c8-cf   */
      "SET 2,B", "SET 2,C", "SET 2,D", "SET 2,E", "SET 2,H", "SET 2,L", "SET 2,(I%@h)", "SET 2,A", /*  d0-d7   */
      "SET 3,B", "SET 3,C", "SET 3,D", "SET 3,E", "SET 3,H", "SET 3,L", "SET 3,(I%@h)", "SET 3,A", /*  d8-df   */
      "SET 4,B", "SET 4,C", "SET 4,D", "SET 4,E", "SET 4,H", "SET 4,L", "SET 4,(I%@h)", "SET 4,A", /*  e0-e7   */
      "SET 5,B", "SET 5,C", "SET 5,D", "SET 5,E", "SET 5,H", "SET 5,L", "SET 5,(I%@h)", "SET 5,A", /*  e8-ef   */
      "SET 6,B", "SET 6,C", "SET 6,D", "SET 6,E", "SET 6,H", "SET 6,L", "SET 6,(I%@h)", "SET 6,A", /*  f0-f7   */
      "SET 7,B", "SET 7,C", "SET 7,D", "SET 7,E", "SET 7,H", "SET 7,L", "SET 7,(I%@h)", "SET 7,A") /*  f8-ff   */


}