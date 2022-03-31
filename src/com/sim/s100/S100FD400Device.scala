package com.sim.s100

import com.sim.cpu.Z80MMU
import com.sim.device.{BinaryUnitOption, Bootable, PortMappedDiskDevice, SupportsOptions}
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Based on work from SIMH, by Peter Schorn and Charles E Owen.
 *
 * The 88_DISK is a 8-inch floppy controller which can control up
 * to 16 daisy-chained Pertec FD-400 hard-sectored floppy drives.
 * Each diskette has physically 77 tracks of 32 137-byte sectors
 * each.
 * The controller is interfaced to the CPU by use of 3 I/O addresses,
 * standardly, these are device numbers $08, $09, and $0A (hex).
 * Address      Mode    Function
 * -------      ----    --------
 * $08          Out     Selects and enables Controller and Drive
 * $08          In      Indicates status of Drive and Controller
 * $09          Out     Controls Disk Function
 * $09          In      Indicates current sector position of disk
 * $0A          Out     Write data
 * $0A          In      Read data
 *
 * Drive Select Out (Device $08 OUT):
 * +---+---+---+---+---+---+---+---+
 * | C | X | X | X |   Device      |
 * +---+---+---+---+---+---+---+---+
 * C = If this bit is 1, the disk controller selected by 'device' is
 *     cleared. If the bit is zero, 'device' is selected as the
 * device being controlled by subsequent I/O operations.
 * X = not used
 * Device = value zero thru 15, selects drive to be controlled.
 *
 * Drive Status In (Device $08 IN):
 * +---+---+---+---+---+---+---+---+
 * | R | Z | I | X | X | H | M | W |
 * +---+---+---+---+---+---+---+---+
 * W - When 0, write circuit ready to write another byte.
 * M - When 0, head movement is allowed
 * H - When 0, indicates head is loaded for read/write
 * X - not used (will be 0)
 * I - When 0, indicates interrupts enabled (not used by this simulator)
 * Z - When 0, indicates head is on track 0
 * R - When 0, indicates that read circuit has new byte to read
 *
 * Drive Control (Device $09 OUT):
 * +---+---+---+---+---+---+---+---+
 * | W | C | D | E | U | H | O | I |
 * +---+---+---+---+---+---+---+---+
 * I - When 1, steps head IN one track
 * O - When 1, steps head OUT one track
 * H - When 1, loads head to drive surface
 * U - When 1, unloads head
 * E - Enables interrupts (ignored by this simulator)
 * D - Disables interrupts (ignored by this simulator)
 * C - When 1 lowers head current (ignored by this simulator)
 * W - When 1, starts Write Enable sequence:   W bit on device $08
 * (see above) will go 1 and data will be read from port $0A
 * until 137 bytes have been read by the controller from
 * that port. The W bit will go off then, and the sector data
 * will be written to disk. Before you do this, you must have
 * stepped the track to the desired number, and waited until
 * the right sector number is presented on device $09 IN, then
 * set this bit.
 *
 * Sector Position (Device $09 IN):
 * As the sectors pass by the read head, they are counted and the
 * number of the current one is available in this register.
 * +---+---+---+---+---+---+---+---+
 * | X | X |  Sector Number    | T |
 * +---+---+---+---+---+---+---+---+
 * X = Not used
 * Sector number = binary of the sector number currently under the head, 0-31.
 * T = Sector True, is a 0 when the sector is positioned to read or write.
 *
 */
class S100FD400Device(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDiskDevice(machine, mmu, ports)
  with SupportsOptions with Bootable {
  override val description: String = "MITS FDisk Interface (88_DISK)"
  override val name = "FD"
  override val supportsBoot: Boolean = true

  /* NUM_OF_DSK must be power of two              */
  val NUM_OF_DSK: Int = 8
  val NUM_OF_DSK_MASK: Int = NUM_OF_DSK - 1

  // 8 inch or minidisk.  0 = 8 inch, 1 = minidisk
  var DRIVE_TYPE:Int = 0

  //  val DSK_TRACSIZE: Int = DSK_SECTSIZE * DSK_SECT
  //  val MAX_DSK_SIZE: Int = DSK_TRACSIZE * MAX_TRACKS
  val BOOTROM_SIZE_DSK: Int = 256 // size of boot rom

  val MINI_DISK_SECT: Int = 16
  val MINI_DISK_TRACKS: Int = 35
  //  val MINI_DISK_SIZE: Int = MINI_DISK_TRACKS * MINI_DISK_SECT * DSK_SECTSIZE
  val MINI_DISK_DELTA: Int = 4096 // Threshold for detecting mini disks

  var current_disk: Option[S100FD400Unit] = None

  /* op-code for LD A,<8-bit value> instruction   */
  private def LDA_INSTRUCTION = 0x3e

  /* LD A,<unitno>    */
  private def UNIT_NO_OFFSET_1 = 0x37

  /* LD a,80h | <unitno>                          */
  private def UNIT_NO_OFFSET_2 = 0xb4

  debug = false


  override def init(): Unit = {
    // Create 16 units
    for (i <- 0 until NUM_OF_DSK) {
      val du = new S100FD400Unit(this)
      addUnit(du)
    }

  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  var laststat08 = 0

  override def createUnitOptions(): Unit = {

    unitOptions.append(BinaryUnitOption("ALTAIRROM", "Use modified Altair boot ROM", value = true))

  }


  /*  I/O instruction handlers, called from the CPU module when an
      IN or OUT instruction is issued.
      Each function is passed an  read/write flag. On input, the actual
      input is passed as the return value, on output, 'data' is written
      to the device.
  */

  /* Disk Controller Status/Select */

  /*  IMPORTANT: The status flags read by port 8 IN instruction are
      INVERTED, that is, 0 is true and 1 is false. To handle this, the
      simulator keeps it's own status flags and returns the COMPLEMENT
      of the status flags when read. This makes setting/testing of the
      flag bits more logical, yet meets the simulation requirement
      that they are reversed in hardware.
  */
  def dsk08(action: Int, isWrite: Boolean): Int = { // 010

    // isWrite = false, return flags
    if (!isWrite) {

      if (current_disk.isEmpty) {
        // Log the debug action
        //Utils.outln(s"$getName: Status on un-attached disk.")

        return 0xff
      }

      // Return the complement
      val stat08 = (~current_disk.get.current_flag) & 0xff
      if (laststat08 != stat08) {
        laststat08 = stat08
        //val str = f"${(~(current_disk.get.current_flag) & 0xff).toBinaryString}%8s".replaceAll(" ", "0")
        //Utils.outln(s"$getName: Status : $str")
      }
      return stat08

    }

    if (current_disk.isDefined && current_disk.get.dirty)
      current_disk.get.writebuf()

    val disknum = action & NUM_OF_DSK_MASK
    //Utils.outln(s"$getName: Write to x08 - Disk Num: $disknum")
    //Utils.outln(s"$getName: Call from: ${machine.cpu.PC.toHexString}")

    current_disk = findUnitByNumber(disknum).asInstanceOf[Option[S100FD400Unit]]
    if (current_disk.isEmpty) {
      // Illegal drive number
      //Utils.outln(s"$getName: Illegal drive number. ($disknum)")

      return 0
    }
    val cd = current_disk.get // new current disk


    if (!cd.isAvailable) {
      // Not available (not attached?)
      //Utils.outln(s"$getName: Unit is not available. ($disknum)")

      current_disk = None
      return 0
    }


    cd.current_sector = 0xff // reset internal counters
    cd.current_byte = 0xff
    if ((action & 0x80) != 0) cd.current_flag = 0 // Disable drive?
    else { // enable drive
      cd.current_flag = 0x1a // Move head true
      if (cd.current_track == 0) cd.current_flag |= 0x40 // Track 0? Set flag
      if (cd.DSK_SECT == MINI_DISK_SECT) cd.current_flag |= 0x84 // Drive enable loads heads on minidisk
    }

    0
  }

  // Disk Drive status/functions  011
  def dsk09(action: Int, isWrite: Boolean): Int = {

    if (current_disk.isEmpty) {
      // Un-available drive selected
      //Utils.outln(s"$getName: Unavailable drive unit selected.")

      return 0xff // nothing we can do
    }

    if (!isWrite) {
      // Read sector position

      val cd = current_disk.get
      if (cd.dirty) cd.writebuf()
      //Utils.outln(s"$getName: read sector position. ST:${cd.sector_true}  Flag:${cd.current_flag}")

      if ((cd.current_flag & 0x04) != 0) { // head loaded?}
        cd.sector_true ^= 1
        if (cd.sector_true == 0) {
          cd.current_sector += 1
          if (cd.current_sector >= cd.DSK_SECT) cd.current_sector = 0
          //Utils.outln(s"*** CS now: ${cd.current_sector} CT:${cd.current_track} Byte:$readbytes")
          cd.current_byte = 0xff

        }
        // return sector number and sector true and set 'unused' bits
        // Just set to a val so it can be seen quicker in debugger.
        val rtn = (((cd.current_sector << 1) & 0x3e) | 0xc0 | cd.sector_true) & 0xff
        return rtn
      } else return 0xff // Head not loaded

    }

    val cd = current_disk.get

    // Drive functions
    if ((action & 0x01) != 0) {
      // Step head in
      if (cd.current_track == (cd.MAX_TRACKS - 1)) {
        // unnecessary step in
        //Utils.outln(s"$getName: Unnecessary step in cmd.")
      }
      cd.current_track += 1
      cd.current_flag &= 0xbf // track zero now false
      if (cd.current_track > cd.MAX_TRACKS - 1) cd.current_track = cd.MAX_TRACKS - 1
      if (cd.dirty) cd.writebuf()
      cd.current_sector = 0xff
      cd.current_byte = 0xff

      //Utils.outln(s"$getName: Step in.  CT: ${cd.current_track}")
      //Utils.outln(s"$getName: Call from: ${machine.cpu.PC.toHexString}")
    }
    if ((action & 0x02) != 0) {
      // Step head out
      if (cd.current_track == 0) {
        // Stuck disk, unnecessary step out.
        //Utils.outln(s"$getName: Stuck-disk - Unnecessary step out.")
        //Utils.outln(s"$getName: Call from: ${machine.cpu.PC.toHexString}")
      }
      cd.current_track -= 1
      if (cd.current_track < 0) {
        cd.current_track = 0
        cd.current_flag |= 0x40 // Track 0 if there
      }
      if (cd.dirty) cd.writebuf()
      cd.current_sector = 0xff
      cd.current_byte = 0xff

      //Utils.outln(s"$getName: Step out.  CT: ${cd.current_track}")
      //Utils.outln(s"$getName: Call from: ${machine.cpu.PC.toHexString}")
    }

    if (cd.dirty) cd.writebuf()

    if ((action & 0x04) != 0) {
      // head load
      //Utils.outln(s"$getName: Head load.")
      cd.current_flag |= 0x04 // turn on head loaded bit
      cd.current_flag |= 0x80 // Turn on 'read data available'


    }

    if (((action & 0x08) != 0) && (cd.DSK_SECT != MINI_DISK_SECT)) {
      // Head unload (not on mini-disk)
      cd.current_flag &= 0xfb // Turn off head loaded
      cd.current_flag &= 0x7f // Turn off read data available
      cd.current_sector = 0xff
      cd.current_byte = 0xff

      //Utils.outln(s"$getName: Head unload.")
    }

    // interrupts and head current are ignored

    if ((action & 0x80) != 0) {
      // write sequence start
      cd.current_byte = 0
      cd.current_flag |= 0x01 // enter new write data on
    }
    0
  }


  var readbytes = 0

  // Disk Data in/out
  def dsk0a(action: Int, isWrite: Boolean): Int = {

    if (current_disk.isEmpty) {
      // Un-available drive selected
      //Utils.outln(s"$getName: Unavailable drive selected for I/O.")

      return 0 // nothing we can do
    }

    val cd = current_disk.get
    if (!isWrite) {
      if (cd.current_byte >= cd.DSK_SECTSIZE) {
        // Physically read the sector
        //Utils.outln(s"$getName: READ: Sector:${cd.current_sector} Track:${cd.current_track} Byte:${readbytes}")
        readbytes = 0
        cd.readSector()
      }
      val rtn = cd.byteBuffer.get(cd.current_byte) & 0xff
      //Utils.outln(s"$getName: Read: ${rtn.intValue()} CB:${cd.current_byte} LIM:${cd.DSK_SECTSIZE}")
      //Utils.outln(s"$getName: READ: Sector:${cd.current_sector} Track:${cd.current_track} Byte:${cd.current_byte}  Value:$rtn")
      cd.current_byte += 1
      readbytes += 1
      //cd.current_flag &= 0xfe

      return rtn

    }

    if (cd.current_byte >= cd.DSK_SECTSIZE)
      cd.writebuf()
    else {
      cd.dirty = true
      cd.byteBuffer.put(cd.current_byte, (action & 0xff).byteValue())
      cd.current_byte += 1
    }
    0
  }


  /*
  The boot routine modifies the boot ROM in such as way that the specified disk is
  used for booting.
   */
  override def boot(unitno: Int, sb: StringBuilder): Boolean = {

    val unit = findUnitByNumber(unitno).asInstanceOf[Option[S100FD400Unit]]
    if (unit.isEmpty || !unit.get.isAvailable) {
      sb.append(s"$getName: Unit is not available.")
      return false
    } // No such unit?
    val cd = unit.get
    val useAltairROM = getBinaryOption("ALTAIRROM") | machine.getCPU.isBanked
    val isMiniDisk = if (cd.DSK_SECT == MINI_DISK_SECT) true else false
    if (useAltairROM) {
      if (isMiniDisk) {
        mmu.installROM(S100FD400Device.alt_bootrom_dsk.toArray,
          S100FD400Device.alt_bootrom_dsk.size, UInt(S100FD400Device.ALTAIR_ROM_LOW))
      } else {
        // check whether we are really modifying an LD A,<> instruction
        if (S100FD400Device.bootrom_dsk(UNIT_NO_OFFSET_1 - 1) == LDA_INSTRUCTION &&
          S100FD400Device.bootrom_dsk(UNIT_NO_OFFSET_2 - 1) == LDA_INSTRUCTION) {
          S100FD400Device.bootrom_dsk(UNIT_NO_OFFSET_1) = unitno & 0xff
          S100FD400Device.bootrom_dsk(UNIT_NO_OFFSET_2) = 0x80 | (unitno & 0xff)
        } else {
          // Attempt to modify non LD A,<> instruction is refused.
          sb.append(s"$getName: Incorrect boot ROM offsets detected.")
          return false
        }
        // install modified ROM
        mmu.installROM(S100FD400Device.bootrom_dsk.toArray,
          S100FD400Device.bootrom_dsk.size, UInt(S100FD400Device.ALTAIR_ROM_LOW))
      }
    }
    machine.getCPU.PC(S100FD400Device.ALTAIR_ROM_LOW)
    sb.append(f"$getName: Boot ROM start: ${S100FD400Device.ALTAIR_ROM_LOW}%04x")
    machine.getCPU.runcpu()

    true

  }

  // Perform a port action
  def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    action.intValue match {
      case 0x08 => UByte(dsk08(value.intValue, isWrite).byteValue())
      case 0x09 => UByte(dsk09(value.intValue, isWrite).byteValue())
      case 0x0A => UByte(dsk0a(value.intValue, isWrite).byteValue())
      case _ => UByte(0)
    }
  }

}

object S100FD400Device {
  // boot ROM for mini disk support
  val alt_bootrom_dsk: mutable.ListBuffer[Int] = ListBuffer(
    0x21, 0x13, 0xff, 0x11, 0x00, 0x4c, 0x0e, 0xe3, /* ff00-ff07 */
    0x7e, 0x12, 0x23, 0x13, 0x0d, 0xc2, 0x08, 0xff, /* ff08-ff0f */
    0xc3, 0x00, 0x4c, 0xf3, 0xaf, 0xd3, 0x22, 0x2f, /* ff10-ff17 */
    0xd3, 0x23, 0x3e, 0x2c, 0xd3, 0x22, 0x3e, 0x03, /* ff18-ff1f */
    0xd3, 0x10, 0xdb, 0xff, 0xe6, 0x11, 0x0f, 0x0f, /* ff20-ff27 */
    0xc6, 0x10, 0xd3, 0x10, 0x31, 0x71, 0x4d, 0xaf, /* ff28-ff2f */
    0xd3, 0x08, 0xdb, 0x08, 0xe6, 0x08, 0xc2, 0x1c, /* ff30-ff37 */
    0x4c, 0x3e, 0x04, 0xd3, 0x09, 0xc3, 0x38, 0x4c, /* ff38-ff3f */
    0xdb, 0x08, 0xe6, 0x02, 0xc2, 0x2d, 0x4c, 0x3e, /* ff40-ff47 */
    0x02, 0xd3, 0x09, 0xdb, 0x08, 0xe6, 0x40, 0xc2, /* ff48-ff4f */
    0x2d, 0x4c, 0x11, 0x00, 0x00, 0x06, 0x00, 0x3e, /* ff50-ff57 */
    0x10, 0xf5, 0xd5, 0xc5, 0xd5, 0x11, 0x86, 0x80, /* ff58-ff5f */
    0x21, 0xe3, 0x4c, 0xdb, 0x09, 0x1f, 0xda, 0x50, /* ff60-ff67 */
    0x4c, 0xe6, 0x1f, 0xb8, 0xc2, 0x50, 0x4c, 0xdb, /* ff68-ff6f */
    0x08, 0xb7, 0xfa, 0x5c, 0x4c, 0xdb, 0x0a, 0x77, /* ff70-ff77 */
    0x23, 0x1d, 0xc2, 0x5c, 0x4c, 0xe1, 0x11, 0xe6, /* ff78-ff7f */
    0x4c, 0x01, 0x80, 0x00, 0x1a, 0x77, 0xbe, 0xc2, /* ff80-ff87 */
    0xc3, 0x4c, 0x80, 0x47, 0x13, 0x23, 0x0d, 0xc2, /* ff88-ff8f */
    0x71, 0x4c, 0x1a, 0xfe, 0xff, 0xc2, 0x88, 0x4c, /* ff90-ff97 */
    0x13, 0x1a, 0xb8, 0xc1, 0xeb, 0xc2, 0xba, 0x4c, /* ff98-ff9f */
    0xf1, 0xf1, 0x2a, 0xe4, 0x4c, 0xcd, 0xdd, 0x4c, /* ffa0-ffa7 */
    0xd2, 0xb3, 0x4c, 0x04, 0x04, 0x78, 0xfe, 0x10, /* ffa8-ffaf */
    0xda, 0x44, 0x4c, 0x06, 0x01, 0xca, 0x44, 0x4c, /* ffb0-ffb7 */
    0xdb, 0x08, 0xe6, 0x02, 0xc2, 0xa5, 0x4c, 0x3e, /* ffb8-ffbf */
    0x01, 0xd3, 0x09, 0xc3, 0x42, 0x4c, 0x3e, 0x80, /* ffc0-ffc7 */
    0xd3, 0x08, 0xc3, 0x00, 0x00, 0xd1, 0xf1, 0x3d, /* ffc8-ffcf */
    0xc2, 0x46, 0x4c, 0x3e, 0x43, 0x01, 0x3e, 0x4d, /* ffd0-ffd7 */
    0xfb, 0x32, 0x00, 0x00, 0x22, 0x01, 0x00, 0x47, /* ffd8-ffdf */
    0x3e, 0x80, 0xd3, 0x08, 0x78, 0xd3, 0x01, 0xd3, /* ffe0-ffe7 */
    0x11, 0xd3, 0x05, 0xd3, 0x23, 0xc3, 0xd2, 0x4c, /* ffe8-ffef */
    0x7a, 0xbc, 0xc0, 0x7b, 0xbd, 0xc9, 0x00, 0x00, /* fff0-fff7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* fff8-ffff */
  )

  /* Altair MITS modified BOOT EPROM, fits in upper 256 byte of memory */
  val bootrom_dsk: mutable.ListBuffer[Int] = ListBuffer(
    0xf3, 0x06, 0x01, 0x3e, 0x0e, 0xd3, 0xfe, 0x05, /* ff00-ff07 */
    0xc2, 0x05, 0xff, 0x3e, 0x16, 0xd3, 0xfe, 0x3e, /* ff08-ff0f */
    0x12, 0xd3, 0xfe, 0xdb, 0xfe, 0xb7, 0xca, 0x20, /* ff10-ff17 */
    0xff, 0x3e, 0x0c, 0xd3, 0xfe, 0xaf, 0xd3, 0xfe, /* ff18-ff1f */
    0x21, 0x00, 0x5c, 0x11, 0x33, 0xff, 0x0e, 0x88, /* ff20-ff27 */
    0x1a, 0x77, 0x13, 0x23, 0x0d, 0xc2, 0x28, 0xff, /* ff28-ff2f */
    0xc3, 0x00, 0x5c, 0x31, 0x21, 0x5d, 0x3e, 0x00, /* ff30-ff37 */
    0xd3, 0x08, 0x3e, 0x04, 0xd3, 0x09, 0xc3, 0x19, /* ff38-ff3f */
    0x5c, 0xdb, 0x08, 0xe6, 0x02, 0xc2, 0x0e, 0x5c, /* ff40-ff47 */
    0x3e, 0x02, 0xd3, 0x09, 0xdb, 0x08, 0xe6, 0x40, /* ff48-ff4f */
    0xc2, 0x0e, 0x5c, 0x11, 0x00, 0x00, 0x06, 0x08, /* ff50-ff57 */
    0xc5, 0xd5, 0x11, 0x86, 0x80, 0x21, 0x88, 0x5c, /* ff58-ff5f */
    0xdb, 0x09, 0x1f, 0xda, 0x2d, 0x5c, 0xe6, 0x1f, /* ff60-ff67 */
    0xb8, 0xc2, 0x2d, 0x5c, 0xdb, 0x08, 0xb7, 0xfa, /* ff68-ff6f */
    0x39, 0x5c, 0xdb, 0x0a, 0x77, 0x23, 0x1d, 0xc2, /* ff70-ff77 */
    0x39, 0x5c, 0xd1, 0x21, 0x8b, 0x5c, 0x06, 0x80, /* ff78-ff7f */
    0x7e, 0x12, 0x23, 0x13, 0x05, 0xc2, 0x4d, 0x5c, /* ff80-ff87 */
    0xc1, 0x21, 0x00, 0x5c, 0x7a, 0xbc, 0xc2, 0x60, /* ff88-ff8f */
    0x5c, 0x7b, 0xbd, 0xd2, 0x80, 0x5c, 0x04, 0x04, /* ff90-ff97 */
    0x78, 0xfe, 0x20, 0xda, 0x25, 0x5c, 0x06, 0x01, /* ff98-ff9f */
    0xca, 0x25, 0x5c, 0xdb, 0x08, 0xe6, 0x02, 0xc2, /* ffa0-ffa7 */
    0x70, 0x5c, 0x3e, 0x01, 0xd3, 0x09, 0x06, 0x00, /* ffa8-ffaf */
    0xc3, 0x25, 0x5c, 0x3e, 0x80, 0xd3, 0x08, 0xfb, /* ffb0-ffb7 */
    0xc3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffb8-ffbf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffc0-ffc7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffc8-ffcf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffd0-ffd7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffd8-ffdf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffe0-ffe7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ffe8-ffef */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* fff0-fff7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* fff8-ffff */
  )

  /* start address of regular Altair ROM          */
  val ALTAIR_ROM_LOW = 0xff00
}