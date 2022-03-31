package com.sim.device

import com.sim.machine.AbstractMachine

abstract class ConsoleDevice(machine:AbstractMachine) extends BasicDevice(machine) {
  override val description: String = "SIM virtual console device"
  override val name: String = "OP"


  override def init(): Unit = {
  }


}
