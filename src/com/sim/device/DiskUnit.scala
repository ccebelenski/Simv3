package com.sim.device

import java.nio.*
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, OpenOption, Path, Paths}
import java.util
import com.sim.Utils
import com.sim.unsigned.UByte

import scala.collection.mutable

/**
  * Created by christophercebelenski on 7/18/16.
  */
trait DiskUnit extends BasicUnit with UnitAttachable with SupportsOptions {

  // The user controlled options come from the options...


  // Java filechannel
  var fileChannel: FileChannel = _
  var byteBuffer: ByteBuffer = _

  // These should be overridden
  var MAX_TRACKS: Int = 0
  var DSK_SECT :Int = 0
  var DSK_SECTSIZE :Int = 0

  // Unit specific information
  var current_track: Int = 0
  var current_sector: Int = 0
  var current_byte: Int = 0
  var current_flag: Int = 0

  var FixedCapacity: Boolean = false
  var isSequential: Boolean = false
  var isIdleEligible: Boolean = true

  // Capacity
  var capacity: Long = 0

  // I/O Start time
  var ioStartTime: Long = 0L

  var dirty: Boolean = false

  def readSector(): Unit = {
    byteBuffer.clear()
    seek()
    while (byteBuffer.hasRemaining) {
      fileChannel.read(byteBuffer)
    } 

    Utils.outlnd(this, s"$getName: Read ${byteBuffer.toString}")
    current_byte = 0
    byteBuffer.rewind()
  }

  def seek(): Unit = {
    val pos = DSK_SECTSIZE * DSK_SECT * current_track + DSK_SECTSIZE * current_sector
    Utils.outlnd(this,s"$getName: SEEK $pos : CT: $current_track CS:$current_sector")
    fileChannel.position(pos)
  }

  /**
    * Quick check for write-protect
    *
    * @return
    */
  def isWriteProtect: Boolean = {
    getBinaryOption("READONLY")
  }

  /**
    * Disk format - should be SIMH or VHD
    *
    * @return
    */
  def getDiskFormat: String = {

    getEnumValueOption("FORMAT").getOrElse("UNDEFINED")
  }

  /**
    * Checks to see if the disk is avail...
    *
    * @return
    */
  def isAvailable: Boolean = {
    if (attachedPath.isEmpty) return false
    if (capacity <= 0) return false


    true
  }


  /**
    * get disk size - #450 sim_disk.c
    * Assumes disk is attached...
    *
    * @return
    */
  protected def sim_disk_size: Long = {
    var physical_size: Long = 0L

    physical_size = getDiskFormat match {
      case "SIMH" => sim_fsize_ex(attachedPath.get)
      case _ => -1
    }
    physical_size
  }

  private def sim_fsize_ex(p: Path): Long = {
    Files.size(p)
  }


  // Override this to set drive attributes during attach()
  def setDriveAttributes(path:Path) : Unit = {}

  override def detach(sb: mutable.StringBuilder): Boolean = {

    if (!isAvailable) {
      sb.append(s"$getName: Unit is not attached.")
      return true
    }
    if (dirty) writebuf()
    fileChannel.close()
    capacity = 0
    byteBuffer.clear()

    false
  }


  def writebuf(): Unit = {
    byteBuffer.position(current_byte) // Necessary?
    var i = current_byte
    val fillval = UByte(0).byteValue
    while (i < DSK_SECTSIZE) { // null-fill rest of sector if any
      byteBuffer.put(i, fillval)
      i += 1
    }
    //Utils.outln(s"$getName: Writebuf - ${byteBuffer.toString}")
    if (!isWriteProtect) {
      seek()
      byteBuffer.rewind()
      while (byteBuffer.hasRemaining) fileChannel.write(byteBuffer)
    }

    current_byte = 0xff
    dirty = false

  }



}


object DiskCapacityFormat extends Enumeration {
  val Byte, Word = Value
}

class DiskUnitAttachedException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {
}

class UnsupportedDiskFormatException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {

}
