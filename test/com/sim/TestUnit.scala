package com.sim

import com.sim.device.{BasicDevice, BasicUnit}

class TestUnit(device:BasicDevice) extends BasicUnit(device: BasicDevice){

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = ???

  override def showCommand(sb:StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  override val waitTime: Long = 0
}
