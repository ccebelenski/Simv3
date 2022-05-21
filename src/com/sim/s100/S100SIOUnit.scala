package com.sim.s100

import com.sim.device.{BasicDevice, ConsoleUnit}

import scala.collection.mutable


class S100SIOUnit(device: S100SIODevice) extends ConsoleUnit(device: BasicDevice) {

  override val waitTime:Long = 100000L

  override def cancel(): Unit = ???

  override def showCommand(sb: mutable.StringBuilder): Unit = {
    super.showCommand(sb)
  }

  // TODO - Anything special here to do?
  override def init(): Unit = {}

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

}