package com.sim

import com.sim.device.{BasicDevice, BasicUnit, Bootable}
import com.sim.machine.AbstractMachine

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class BootCommand extends Command {
  commandToken = "BOOT"
  commandDescription = "Attempts to boot a specific unit."
  commandHelpText = "Boot a unit.  The unit must be bootable and be attached to bootable media."


  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length == 0) {
      sb.append(s"SIM: Please specify a unit.")
    } else if(Console.cpuRunning) {
      sb.append("SIM: CPU is running already.")
    } else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => {
        val devname = tokenArray(0)
        m.findUnitDevice(devname) match {
          case None =>
            // Device unit not found...
            sb.append(s"SIM: Device unit $devname not found.")
          case Some(v) =>
            // Get the Device for this unit, see if it's bootable
            if (!v.device.isInstanceOf[Bootable]) {
              sb.append(s"SIM: Device $devname is not a boot device.")

            } else {
              val t: Future[Boolean] = Future {
                Console.cpuRunning = true
                v.device.asInstanceOf[Bootable].boot(v.unitNumber, sb)
              }
              t.onComplete {
                case Success(x) =>
                  Console.cpuRunning = false
                  Console.userInterrupt = false
                case Failure(exception) =>
                  Console.cpuRunning = false
                  Console.userInterrupt = false
              }
            }

        }
      }
    }

    Utils.outln(sb.toString())
    false
  }


}
