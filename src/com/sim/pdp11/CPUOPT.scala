package com.sim.pdp11

import com.sim.unsigned.UInt

protected object CPUOPT {

  val BUS_U: UInt = UInt(1) << 0 // Unibus
  val BUS_Q: UInt = UInt(0) // Qbus

  /* CPU models */
  val MOD_1103 = 0
  val MOD_1104 = 1
  val MOD_1105 = 2
  val MOD_1120 = 3
  val MOD_1123 = 4
  val MOD_1123P = 5
  val MOD_1124 = 6
  val MOD_1134 = 7
  val MOD_1140 = 8
  val MOD_1144 = 9
  val MOD_1145 = 10
  val MOD_1160 = 11
  val MOD_1170 = 12
  val MOD_1173 = 13
  val MOD_1153 = 14
  val MOD_1173B = 15
  val MOD_1183 = 16
  val MOD_1184 = 17
  val MOD_1193 = 18
  val MOD_1194 = 19
  val MOD_T = 20

  val CPUT_03: UInt = UInt(1) << MOD_1103 // LSI-11
  val CPUT_04: UInt = UInt(1) << MOD_1104 // 11/04
  val CPUT_05: UInt = UInt(1) << MOD_1105 // 11/05
  val CPUT_20: UInt = UInt(1) << MOD_1120 // 11/20
  val CPUT_23: UInt = UInt(1) << MOD_1123 // 11/23
  val CPUT_23P: UInt = UInt(1) << MOD_1123P // 11/23+
  val CPUT_24: UInt = UInt(1) << MOD_1124 //11/24
  val CPUT_34: UInt = UInt(1) << MOD_1134 //11/34
  val CPUT_40: UInt = UInt(1) << MOD_1140 //11/40
  val CPUT_44: UInt = UInt(1) << MOD_1144 //11/44
  val CPUT_45: UInt = UInt(1) << MOD_1145 //11/45
  val CPUT_60: UInt = UInt(1) << MOD_1160 //11/60
  val CPUT_70: UInt = UInt(1) << MOD_1170 //11/70
  val CPUT_73: UInt = UInt(1) << MOD_1173 //11/73
  val CPUT_53: UInt = UInt(1) << MOD_1153 //11/53
  val CPUT_73B: UInt = UInt(1) << MOD_1173B // 11/73B
  val CPUT_83: UInt = UInt(1) << MOD_1183 //11/83
  val CPUT_84: UInt = UInt(1) << MOD_1184 //11/84
  val CPUT_93: UInt = UInt(1) << MOD_1193 //11/93
  val CPUT_94: UInt = UInt(1) << MOD_1194 //11/94
  val CPUT_T: UInt = UInt(1) << MOD_T // T-11
  val CPUT_F: UInt = CPUT_23 | CPUT_23P | CPUT_24 // all F11's
  val CPUT_J: UInt = CPUT_53 | CPUT_73 | CPUT_73B | CPUT_83 | CPUT_84 | CPUT_93 | CPUT_94
  val CPUT_JB: UInt = CPUT_73B | CPUT_83 | CPUT_84 // KDJ11B
  val CPUT_JE: UInt = CPUT_93 | CPUT_94 //KDJ11E
  val CPUT_JU: UInt = CPUT_84 | CPUT_94 // KTJ11B UBA
  val CPUT_ALL = UInt(0xFFFFFFFF)


  /* CPU options */

  //val BUS_U : UInt   =       (UInt(1) << 0)                       /* Unibus */
  //val BUS_Q :UInt    =      UInt(0)                             /* Qbus */
  val OPT_EIS: UInt = UInt(1) << 1 // EIS
  val OPT_FIS: UInt = UInt(1) << 2 // FIS
  val OPT_FPP: UInt = UInt(1) << 3 // FPP
  val OPT_CIS: UInt = UInt(1) << 4 // CIS
  val OPT_MMU: UInt = UInt(1) << 5 // MMU
  val OPT_RH11: UInt = UInt(1) << 6 // RH11
  val OPT_PAR: UInt = UInt(1) << 7 // parity
  val OPT_UBM: UInt = UInt(1) << 8 // UBM
  val OPT_BVT: UInt = UInt(1) << 9 // BEVENT
  /* Feature sets
  SDSD                 source addr, dest addr, source fetch, dest fetch
    SR                   switch register
  DR                   display register
  RTT                  RTT instruction
  SXS                  SXT, XOR, SOB instructions
    MARK                 MARK instruction
  SPL                  SPL instruction
  MXPY                 MTPI, MTPD, MFPI, MFPD instructions
    MXPS                 MTPS, MFPS instructions
    MFPT                 MFPT instruction
  CSM                  CSM instruction
  TSWLK                TSTSET, WRLCK instructions
    PSW                  PSW register
  EXPT                 explicit PSW writes can alter T-bit
  IOSR                 general registers readable from programs in IO space
  2REG                 dual register set
  MMR3                 MMR3 register
  MMTR                 mem mgt traps
    STKLR                STKLIM register
  STKLF                fixed stack limit
    SID                  supervisor mode, I/D spaces
    ODD                  odd address trap
    HALT4                halt in kernel mode traps to 4
  JREG4                JMP/JSR R traps to 4
  STKA                 stop on stack abort
  LTCR                 LTC CSR
  LTCM                 LTC CSR<7>
    */

  val IS_SDSD: UInt = CPUT_20 | CPUT_F | CPUT_40 | CPUT_60 | CPUT_J | CPUT_T
  val HAS_SR: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70
  val HAS_DR: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_24 | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70
  val HAS_RTT: UInt = CPUT_03 | CPUT_04 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J | CPUT_T
  val HAS_SXS: UInt = CPUT_03 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J | CPUT_T
  val HAS_MARK: UInt = CPUT_03 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J
  val HAS_SPL: UInt = CPUT_44 | CPUT_45 | CPUT_70 | CPUT_J
  val HAS_MXPY: UInt = CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J
  val HAS_MXPS: UInt = CPUT_03 | CPUT_F | CPUT_34 | CPUT_J | CPUT_T
  val HAS_MFPT: UInt = CPUT_F | CPUT_44 | CPUT_J | CPUT_T
  val HAS_CSM: UInt = CPUT_44 | CPUT_J
  val HAS_TSWLK: UInt = CPUT_J
  val HAS_PSW: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J
  val HAS_EXPT: UInt = CPUT_04 | CPUT_05 | CPUT_20
  val HAS_IOSR: UInt = CPUT_04 | CPUT_05
  val HAS_2REG: UInt = CPUT_45 | CPUT_70 | CPUT_J
  val HAS_MMR3: UInt = CPUT_F | CPUT_44 | CPUT_45 | CPUT_70 | CPUT_J
  val HAS_MMTR: UInt = CPUT_45 | CPUT_70
  val HAS_STKLR: UInt = CPUT_45 | CPUT_60 | CPUT_70
  val HAS_STKLF: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_J
  val HAS_SID: UInt = CPUT_44 | CPUT_45 | CPUT_70 | CPUT_J
  val HAS_ODD: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J
  val HAS_HALT4: UInt = CPUT_44 | CPUT_45 | CPUT_70 | CPUT_J
  val HAS_JREG4: UInt = CPUT_03 | CPUT_04 | CPUT_05 | CPUT_20 | CPUT_F | CPUT_34 | CPUT_40 | CPUT_60 | CPUT_T
  val STOP_STKA: UInt = CPUT_03 | CPUT_04 | CPUT_05 | CPUT_20 | CPUT_34 | CPUT_44
  val HAS_LTCR: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_23P | CPUT_24 | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J
  val HAS_LTCM: UInt = CPUT_04 | CPUT_05 | CPUT_20 | CPUT_24 | CPUT_34 | CPUT_40 | CPUT_44 | CPUT_45 | CPUT_60 | CPUT_70 | CPUT_J

  val SOP_1103: UInt = BUS_Q | OPT_BVT
  val OPT_1103: UInt = OPT_EIS | OPT_FIS | OPT_BVT
  val PSW_1103 = UInt(0xff)

  val SOP_1104: UInt = BUS_U
  val OPT_1104 = UInt(0)
  val PSW_1104 = UInt(0xff)

  val SOP_1105: UInt = BUS_U
  val OPT_1105 = UInt(0)
  val PSW_1105 = UInt(0xff)

  val SOP_1120: UInt = BUS_U
  val OPT_1120 = UInt(0)
  val PSW_1120 = UInt(0xff)

  val SOP_1123: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU | OPT_BVT
  val OPT_1123: UInt = OPT_FPP | OPT_CIS | OPT_BVT
  val PSW_F = UInt(0xf1ff)
  val PAR_F = UInt(0xffff)
  val PDR_F = UInt(0x7f4e)
  val MM0_F = UInt(0xe06f)
  val MM3_F = UInt(0x30)

  val SOP_1123P: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1123P: UInt = OPT_FPP | OPT_CIS

  val SOP_1124: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_UBM
  val OPT_1124: UInt = OPT_FPP | OPT_CIS

  val SOP_1134: UInt = BUS_U | OPT_EIS | OPT_MMU
  val OPT_1134: UInt = OPT_FPP
  val PSW_1134 = UInt(0xf0ff)
  val PAR_1134 = UInt(0xfff)
  val PDR_1134 = UInt(0x7f4e)
  val MM0_1134 = UInt(0xe16f)

  val SOP_1140: UInt = BUS_U | OPT_EIS | OPT_MMU
  val OPT_1140: UInt = OPT_FIS
  val PSW_1140 = UInt(0xf0ff)
  val PAR_1140 = UInt(0xfff)
  val PDR_1140 = UInt(0x7f4e)
  val MM0_1140 = UInt(0xe16f)

  val SOP_1144: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_UBM
  val OPT_1144: UInt = OPT_FPP | OPT_CIS
  val PSW_1144 = UInt(0xf1ff)
  val PAR_1144 = UInt(0xffff)
  val PDR_1144 = UInt(0xff4e)
  val MM0_1144 = UInt(0xe16f)
  val MM3_1144 = UInt(0x3f)

  val SOP_1145: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_RH11
  val OPT_1145: UInt = OPT_FPP
  val PSW_1145 = UInt(0xf8ff)
  val PAR_1145 = UInt(0xfff)
  val PDR_1145 = UInt(0x7fcf)
  val MM0_1145 = UInt(0xf3ff)
  val MM3_1145 = UInt(0x7)

  val SOP_1160: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1160 = UInt(0)
  val PSW_1160 = UInt(0xf0ff)
  val PAR_1160 = UInt(0xfff)
  val PDR_1160 = UInt(0x7f4e)
  val MM0_1160 = UInt(0xe16f)

  val SOP_1170: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_UBM
  val OPT_1170: UInt = OPT_FPP | OPT_RH11
  val PSW_1170 = UInt(0xf8ff)
  val PAR_1170 = UInt(0xffff)
  val PDR_1170 = UInt(0x7fcf)
  val MM0_1170 = UInt(0xf3ff)
  val MM3_1170 = UInt(0x37)

  val SOP_1173: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1173: UInt = OPT_CIS
  val PSW_J = UInt(0xf9ff)
  val PAR_J = UInt(0xffff)
  val PDR_J = UInt(0xff4e)
  val MM0_J = UInt(0xe07f)
  val MM3_J = UInt(0x3f)

  val SOP_1153: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1153: UInt = OPT_CIS

  val SOP_1173B: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1173B: UInt = OPT_CIS

  val SOP_1183: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1183: UInt = OPT_CIS

  val SOP_1184: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_UBM | OPT_RH11
  val OPT_1184: UInt = OPT_CIS

  val SOP_1193: UInt = BUS_Q | OPT_EIS | OPT_FPP | OPT_MMU
  val OPT_1193: UInt = OPT_CIS

  val SOP_1194: UInt = BUS_U | OPT_EIS | OPT_FPP | OPT_MMU | OPT_UBM | OPT_RH11
  val OPT_1194: UInt = OPT_CIS

  /* MFPT codes */

  val MFPT_44 = UInt(1)
  val MFPT_F = UInt(3)
  val MFPT_T = UInt(4)
  val MFPT_J = UInt(5)

  val cpu_tab: Array[CPUTAB] = Array(
    CPUTAB("11/03", SOP_1103, OPT_1103, PDP11.MEMSIZE64K, PSW_1103, UInt(0), UInt(0), UInt(0), UInt(0), UInt(0)),
    CPUTAB("11/04", SOP_1104, OPT_1104, PDP11.MEMSIZE64K, PSW_1104, UInt(0), UInt(0), UInt(0), UInt(0), UInt(0)),
    CPUTAB("11/05", SOP_1105, OPT_1105, PDP11.MEMSIZE64K, PSW_1105, UInt(0), UInt(0), UInt(0), UInt(0), UInt(0)),
    CPUTAB("11/20", SOP_1120, OPT_1120, PDP11.MEMSIZE64K, PSW_1120, UInt(0), UInt(0), UInt(0), UInt(0), UInt(0)),
    CPUTAB("11/23", SOP_1123, OPT_1123, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F),
    CPUTAB("11/23+", SOP_1123P, OPT_1123P, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F),
    CPUTAB("11/24", SOP_1124, OPT_1124, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F),
    CPUTAB("11/34", SOP_1134, OPT_1134, PDP11.UNIMEMSIZE, PSW_1134, UInt(0), PAR_1134, PDR_1134, MM0_1134, UInt(0)),
    CPUTAB("11/40", SOP_1140, OPT_1140, PDP11.UNIMEMSIZE, PSW_1140, UInt(0), PAR_1140, PDR_1140, MM0_1140, UInt(0)),
    CPUTAB("11/44", SOP_1144, OPT_1144, PDP11.MAXMEMSIZE, PSW_1144, MFPT_44, PAR_1144, PDR_1144, MM0_1144, MM3_1144),
    CPUTAB("11/45", SOP_1145, OPT_1145, PDP11.UNIMEMSIZE, PSW_1145, UInt(0), PAR_1145, PDR_1145, MM0_1145, MM3_1145),
    CPUTAB("11/60", SOP_1160, OPT_1160, PDP11.UNIMEMSIZE, PSW_1160, UInt(0), PAR_1160, PDR_1160, MM0_1160, UInt(0)),
    CPUTAB("11/70", SOP_1170, OPT_1170, PDP11.MAXMEMSIZE, PSW_1170, UInt(0), PAR_1170, PDR_1170, MM0_1170, MM3_1170),
    CPUTAB("11/73", SOP_1173, OPT_1173, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/53", SOP_1153, OPT_1153, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/73B", SOP_1173B, OPT_1173B, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/83", SOP_1183, OPT_1183, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/84", SOP_1184, OPT_1184, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/93", SOP_1193, OPT_1193, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J),
    CPUTAB("11/94", SOP_1194, OPT_1194, PDP11.MAXMEMSIZE, PSW_J, MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J)
  )

}