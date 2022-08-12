package com.sim.s100

import com.sim.Utils
import com.sim.device.{BasicUnit, DiskUnit, ImageDisk}

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, READ, SPARSE, WRITE}
import java.nio.file.{OpenOption, Path, Paths}
import java.util
import scala.collection.mutable

class S100HDSKUnit(device:S100HDSKDevice) extends BasicUnit(device) with  DiskUnit with ImageDisk {

  // Format type - actually the parameters for the type
  var HDSK_FORMAT_TYPE : Option[S100HDiskParamsBase] = None

  val HDSK_CAPACITY :Int = 2048 * 32 * 128 // Default Altair HDSK capacity
  val HDSK_MAX_SECTOR_SIZE :Int = 1024 // maximum size of a sector

  var HDSK_SECTOR_SIZE:Int =0 // size of sector
  var HDSK_SECTORS_PER_TRACK:Int = 0 // sectors per track
  var HDSK_NUMBER_OF_TRACKS:Int  = 0 //number of tracks

  override val waitTime: Long = 0L

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {
    // TODO
  }

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???


  override def attach(fileSpec: String, sb: mutable.StringBuilder): Boolean = {

    if (isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }

    val p :Path = Paths.get(fileSpec)
    val options = new util.HashSet[OpenOption]
    options.add(SPARSE)
    options.add(CREATE)
    options.add(WRITE)
    options.add(READ)

    // Optionally set up some drive parameters basic on the file.
    setDriveAttributes(p)

    fileChannel = FileChannel.open(p, options)

    capacity = fileChannel.size()
    if(capacity == 0) capacity = HDSK_CAPACITY

    if(isIMD) {
      if(capacity == 0) {
        Utils.outln(s"$getName: No image specified.")
        return false
      }

      // TODO Set up disk parameters from IMD

      return true
    }

    sb.append(s"\n\rChecking format... (${Utils.formatBytes(capacity,si=false)})\n\r")
    assignFormat(capacity)

    HDSK_FORMAT_TYPE match {
      case None =>
        // No disk parameter block found
        //HDSK_FORMAT_TYPE = Some(device)
        sb.append(s"\n\r$getName: WARNING: Unsupported disk capacity, assuming HDSK type with capacity ${Utils.formatBytes(capacity,false)}\n\r")
      case Some(ft) =>
        capacity = ft.capac

    }

    // Set number of sectors per track and sector size

    attachedPath = Some(p)

    dirty = false

    HDSK_FORMAT_TYPE match {
      case Some(x) =>
        sb.append (s"$getName: Disk: ${x.desc}\n\r")
        HDSK_SECTORS_PER_TRACK = x.spt >> x.psh
        HDSK_SECTOR_SIZE = if(x.physicalSectorSize != 0) x.physicalSectorSize else 128 << x.psh
        HDSK_NUMBER_OF_TRACKS = ((capacity + HDSK_SECTORS_PER_TRACK * HDSK_SECTOR_SIZE -1) / (HDSK_SECTORS_PER_TRACK * HDSK_SECTOR_SIZE)).intValue()
      case _ =>
        sb.append(s"$getName: Disk not defined.\n\r")
        if(capacity != HDSK_NUMBER_OF_TRACKS * HDSK_SECTOR_SIZE) {
          sb.append(s"$getName: Fixing geometry.\n\r")
          if(HDSK_SECTORS_PER_TRACK == 0) HDSK_SECTORS_PER_TRACK = 32
          if(HDSK_SECTOR_SIZE == 0) HDSK_SECTOR_SIZE = 128
          if(HDSK_NUMBER_OF_TRACKS == 0) HDSK_NUMBER_OF_TRACKS = 2048
        }
    }
    sb.append(s"$getName: Attached: ${attachedPath.get.getFileName}\n\r")
    sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity, si = false)}\n\r")
    sb.append(s"$getName: Sectors/Track: $HDSK_SECTORS_PER_TRACK, Tracks: $HDSK_NUMBER_OF_TRACKS Sector Size:$HDSK_SECTOR_SIZE\n\r")
    // Attaching enabled the device implicitly
    setEnable(true)
    isAttached = true
    // Allocate the bytebuffer
    byteBuffer = ByteBuffer.allocate(HDSK_SECTOR_SIZE)

    true
  }


  override def detach(sb: mutable.StringBuilder): Boolean = {
    val ret = super.detach(sb)
    HDSK_FORMAT_TYPE = None

    ret
  }

  private def assignFormat(size:Long):Unit = {
    HDSK_FORMAT_TYPE = None
    Utils.outlnd(this, s"Looking for spec with capacity $size ${Utils.formatBytes(size,false)}")
    S100HDSKDevice.DPB.find(p => p.capac == size) match {
      case x: Some[S100HDiskParamsBase] => HDSK_FORMAT_TYPE = x
      case _ => Utils.outlnd(this, "Did not find spec.")
    }
  }
}
