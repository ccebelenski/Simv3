package com.sim

import com.sim.device.{BasicDevice, BasicUnit}

import scala.collection.mutable

class TestUnit(device:BasicDevice) extends BasicUnit(device: BasicDevice){

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = ???

  override def showCommand(sb:mutable.StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override val waitTime: Long = 0
}
