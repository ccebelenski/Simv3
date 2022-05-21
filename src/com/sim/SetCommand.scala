package com.sim

import java.util.regex.Pattern
import com.sim.device.SupportsOptions
import com.sim.machine.AbstractMachine

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SetCommand extends Command {

  commandToken = "SET"
  commandDescription = "Set simulator features."
  commandHelpText = "Set features of the simulator, such as machines, devices, etc."

  addSubCommand(new SetMachineCommand)
  addSubCommand(new SetDeviceCommand)

  override def process(tokenArray: Array[String]): Boolean = {
    if (tokenArray.length == 0) {
      Utils.outln(argsErrorMsg)
    } else {
      processSubCommand(tokenArray)
    }


    false
  }

}


class SetMachineCommand extends Command {
  commandToken = "MACHINE"
  commandDescription = "Set (create) the machine to be simulated."
  commandHelpText = "Defines the machine that will simulated."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {

    //noinspection SizeToLength
    if (tokenArray.length != 1) Utils.outln(s"SIM: Please specify a machine.")
    else {
      val mn = tokenArray(0)
      AbstractMachine.services.find(am => am.getName == mn) match {
        case None => Utils.outln(s"SIM: Machine $mn not valid.")
        case Some(x) =>
          if (Console.simEnvironment.simMachine.isEmpty) {
            Console.simEnvironment.simMachine = Some(x)
            x.init()
            Utils.outln(s"SIM: Machine $mn defined.")
          } else {
            Utils.outln(s"SIM: Machine is already defined: ${Console.simEnvironment.simMachine.get.getName}")
          }
      }
      if (Console.simEnvironment.simMachine.isEmpty) Utils.outln(s"SIM: Machine was not found. LIST MACHINES to get a list.")
    }

    false
  }
}

class SetDeviceCommand extends Command {
  commandToken = "DEVICE"
  commandDescription = "Set device specific attributes."
  commandHelpText = "Set attributes of devices that affect their operations."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: mutable.StringBuilder = new mutable.StringBuilder
    if (tokenArray.length == 0) {
      sb.append(s"SIM: Please specify a device.")
    } else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        val devname = tokenArray(0)


        m.findDevice(devname) match {
          case None =>
            // Device not found, look for a unit with that name.
            m.findUnitDevice(devname) match {
              case None => sb.append(s"SIM: Device $devname not present.")
              case Some(u: SupportsOptions) =>
                val options = tokenArray.slice(1, tokenArray.length)
                val opts = parseOpts(options)
                setOptions(u, opts, sb)
            }


          case Some(v) =>
            val options = tokenArray.slice(1, tokenArray.length)
            val opts = parseOpts(options)
            setOptions(v, opts, sb)
        }
    }


    Utils.outln(sb.toString)
    false
  }

  private def setOptions(x: SupportsOptions, o: List[(String, String)], sb: mutable.StringBuilder): Unit = {
    var optionsChanged = false
    o.foreach(z => {
      if(x.setOption(z._1, z._2, sb)) optionsChanged = true
    })

    if(optionsChanged) x.optionChanged(sb)
  }

  /**
   * Options parsing - options are of the form OPTION=VALUE, where value is option specific
   * VALUE can be a quoted string.
   * @param tokenArray
   * @return
   */
  private def parseOpts(tokenArray: Array[String]): List[(String, String)] = {
    val ab = new ArrayBuffer[(String, String)]

    tokenArray.foreach(str => {
      val m = Pattern.compile("([^\"]\\S*|\".+?\")\\s*").matcher(str)
      while (m.find()) {
        val o = m.group(1).split("=")
        if (o.length == 1) ab.append((o(0), ""))
        else if (o.length == 2) ab.append((o(0), o(1).replace("\"", "")))
        else Utils.outln(s"Cannot parse option: $str")
      }
    })

    ab.toList
  }
}