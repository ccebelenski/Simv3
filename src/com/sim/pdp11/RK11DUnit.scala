package com.sim.pdp11

import com.sim.Utils
import com.sim.device.{BasicUnit, DiskUnit}
import com.sim.s100.S100FD400Device

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{OpenOption, Path, Paths}
import scala.collection.mutable
import java.util
import java.nio.file.StandardOpenOption.{CREATE, READ, SPARSE, WRITE}

class RK11DUnit(device:RK11D) extends BasicUnit(device) with  DiskUnit {

  override val waitTime: Long = 0L
  
  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {}

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override def attach(fileSpec: String, sb: mutable.StringBuilder): Boolean = {

    if (isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }

    //  if doesn't exist then assume create a new file
    val p: Path = Paths.get(fileSpec)
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
    isAttached = true

    false
  }
}
