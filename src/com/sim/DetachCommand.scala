package com.sim

import com.sim.device.UnitAttachable
import com.sim.machine.AbstractMachine

import scala.collection.mutable

class DetachCommand extends Command {
  commandToken = "DETACH"
  commandDescription = "Detaches an image from a unit."
  commandHelpText = "Detach an image from a unit.  The unit must be attachable (eg, disk, tape, etc)."

  override def process(tokenArray: Array[String]): Boolean = {

    if (tokenArray.length == 0)
      Utils.outln(argsErrorMsg)
    else if (tokenArray.length != 1)
      Utils.outln("SIM: Invalid args: DETACH <UNIT>")
    else {
      // Correct arg count, lets get the specs
      val unitName = tokenArray(0)
      Console.simEnvironment.simMachine match {
        case None => Utils.outln("SIM: No machine.  SET a MACHINE.")
        case Some(m: AbstractMachine) =>

          m.findUnitDevice(unitName) match {
            case None => Utils.outln(s"${m.getName}: Unit $unitName not found.")
            case Some(unit) =>
              if (!unit.isInstanceOf[UnitAttachable]) {
                Utils.outln(s"${m.getName} Unit is not attachable.")
                return true
              }
              val sb = new mutable.StringBuilder
              unit.asInstanceOf[UnitAttachable].detach(sb)
              Utils.outln(sb.toString())
          }

      }
    }


    false
  }
}
