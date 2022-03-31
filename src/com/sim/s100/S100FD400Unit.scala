package com.sim.s100

import com.sim.Utils
import com.sim.device.{BasicUnit, DiskUnit}

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, READ, SPARSE, WRITE}
import java.nio.file.{Files, OpenOption, Path, Paths}
import java.util

class S100FD400Unit(device:S100FD400Device) extends BasicUnit(device) with  DiskUnit {

  val D8_DSK_SECTSIZE: Int = 137 // Size of sector
  val D8_DSK_SECT: Int = 32 // Sectors per track
  val D8_MAX_TRACKS: Int = 254 // Number of tracts, original Altair has 77 only (254)
  val D5_DSK_SECSIZE:Int = 137 // Minidisk sector size
  val D5_DSK_SECT: Int = 16 // Minidisk sectors per track
  val D5_MAX_TRACKS:Int = 35

  var sector_true :Int = 0


  MAX_TRACKS = if(device.DRIVE_TYPE != 0) D5_MAX_TRACKS else D8_MAX_TRACKS

  DSK_SECT = if(device.DRIVE_TYPE !=0) D5_DSK_SECT else D8_DSK_SECT

  DSK_SECTSIZE = if(device.DRIVE_TYPE !=0) D5_DSK_SECSIZE else D8_DSK_SECTSIZE

  override def writebuf(): Unit = {
    super.writebuf()
    current_flag &= 0xfe

  }

  // If the file is too big, assume it's a normal (8 inch) disk image.
  // TODO Handle explict option setting.
  override def setDriveAttributes(p:Path) : Unit = {
    val size = Files.size(p)
    if(size > (D5_DSK_SECSIZE * D5_DSK_SECT * D5_MAX_TRACKS)) device.DRIVE_TYPE = 0 else device.DRIVE_TYPE = 1
  }

  override val waitTime: Long =0 // TODO

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {}

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def attach(fileSpec: String, sb: StringBuilder): Boolean = {

    if (isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }

    //  if doesn't exist then assume create a new file
    val p :Path = Paths.get(fileSpec)
    val options = new util.HashSet[OpenOption]
    options.add(SPARSE)
    options.add(CREATE)
    options.add(WRITE)
    options.add(READ)

    // Optionally set up some drive parameters basic on the file.
    setDriveAttributes(p)

    fileChannel = FileChannel.open(p, options)

    // Allocate the bytebuffer
    byteBuffer = ByteBuffer.allocate(DSK_SECTSIZE)


    attachedPath = Some(p)
    capacity = DSK_SECTSIZE * DSK_SECT * MAX_TRACKS
    dirty = false

    sb.append(s"$getName: Attached: ${attachedPath.get.getFileName}\r\n")
    sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity, false)}\n\r")
    // Attaching enabled the device implicitly
    setEnable(true)

    false
  }
}
