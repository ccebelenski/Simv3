package com.sim

import com.sim.machine.AbstractMachine

import scala.collection.mutable

class HaltCommand extends Command {
  commandToken = "HALT"
  commandDescription = "Halts a running CPU"
  commandHelpText = "HA<LT>"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: mutable.StringBuilder = new mutable.StringBuilder
    if(Console.cpuRunning) {
      Console.simEnvironment.simMachine match {
        case None => sb.append("SIM: No machine.  SET a MACHINE.")
        case Some(m: AbstractMachine) =>
          Console.userInterrupt = true
      }
    } else sb.append("SIM: CPU is not running.")

    Utils.outln(sb.toString)
    false
  }
}
