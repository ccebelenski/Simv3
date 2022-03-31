package com.sim.device

import com.sim.mux.{MuxDevice, MuxUnit}

import scala.collection.mutable

/**
  * Trait for associating a device with a MUX workflow.
  */
trait MuxAware {

  var registeredMuxDevice:Option[MuxDevice] = None

  // Map Muxunit name to device unit name
  val muxToUnitMap: mutable.HashMap[String,MuxUnitAware] = new mutable.HashMap[String,MuxUnitAware]()

  // This will be called when a character is available - the processing is of course
  // device specific.
  def muxCharacterInterrupt(unit: MuxUnit, char:Int) : Unit


  // Returns true when the device is ready to accept another character
  def checkDeviceReady: Boolean

  /**
    * callback from the MUX when it has a unit available.
    * Returns true if successful, other false (no units free)
    * @param muxunit
    */
  def MUXAttachCallback(muxunit:MuxUnit) : Boolean = {
    synchronized {
      val muxUnitName = muxunit.getName
      // First see if there are units that aren't connected to a mux
      // Need to "upshift" - we know we are a device.  Yes, bad programmer.
      val units = this.asInstanceOf[BasicDevice].getUnits
      val openUnit = units.find(p => p.asInstanceOf[MuxUnitAware].attachedMuxUnit.isEmpty)
      if (openUnit.isDefined) {
        // Got an open unit, map it.  this has better be true...
        val u = openUnit.get.asInstanceOf[MuxUnitAware]
        u.attachedMuxUnit = Some(muxunit)
        u.attachedMuxDevice = Some(muxunit.device.asInstanceOf[MuxDevice])
        muxToUnitMap.update(muxunit.getName, openUnit.get.asInstanceOf[MuxUnitAware])
        true
      } else false
    }

  }

  /** callback from the MUX when the unit is no longer available.
    *
    * @param muxunit
    */
  def MUXDetachCallback(muxunit:MuxUnit) : Unit = {
    synchronized {
      val unit = muxToUnitMap.get(muxunit.getName).orElse(None)
      if(unit.isDefined) {
        val u = unit.get
        u.attachedMuxDevice = None
        u.attachedMuxUnit = None
        muxToUnitMap.remove(muxunit.getName)

      }

    }
  }

  /**
    * Ask the mux to create a special attached unit (file)
    * @param mux
    * @return the unit that was created.
    */
  def createAttachedMuxUnit(mux:MuxDevice) : Unit = ???

  /**
    * Close a special attached mux unit (file attached).
    * The unit will no longer be available.
    * @param mux
    */
  def closeAttachedMuxUnit(mux:MuxDevice) : Unit = ???

}
