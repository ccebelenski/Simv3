package com.sim.device

import com.sim.mux.{MuxDevice, MuxUnit}

trait MuxUnitAware {

  var attachedMuxDevice: Option[MuxDevice] = None
  var attachedMuxUnit: Option[MuxUnit] = None

  def writeChar(char:Int) : Unit = {
    if(attachedMuxUnit.isDefined) attachedMuxUnit.get.writeChar(char)
  }
}
