package com.sim.pdp11

import com.sim.unsigned.UInt
import com.sim.unsigned.uint2int

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
class RK11D {

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
}