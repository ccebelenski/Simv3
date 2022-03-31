package com.sim

import java.io.File
import java.nio.file.Paths

import javax.swing.JFileChooser
import com.sim.device.UnitAttachable
import com.sim.machine.AbstractMachine

class AttachCommand extends Command {
  commandToken = "ATTACH"
  commandDescription = "Attaches an image to a unit."
  commandHelpText = "Attach an image to a unit.  The unit must be attachable (eg, disk, tape, etc)."

  override def process(tokenArray: Array[String]): Boolean = {

    var unitName: String = null
    var fileName: String = null

    if (tokenArray.length == 0) {
      Utils.outln(argsErrorMsg)

      return false
    }
    else if (tokenArray.length != 2) {
      if (tokenArray.length == 1) {
        // Gave unit, prompt for file
        val workingDirectory= new File(System.getProperty("user.dir"))
        val jfc = new JFileChooser()
        jfc.setCurrentDirectory(workingDirectory)
        jfc.showOpenDialog(Console.term) match {
          case JFileChooser.APPROVE_OPTION =>
            fileName = jfc.getSelectedFile.getCanonicalPath
            Utils.outln(s"SIM: File ${fileName}")
          case _ =>
        }

      }
      if(fileName == null) {
        Utils.outln("SIM: Invalid args: ATTACH <UNIT> <FILE>")
        return false
      }
    }
    // Correct arg count, lets get the specs
    unitName = tokenArray(0)
    if(fileName == null) fileName = tokenArray(1)
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
            val sb = new StringBuilder
            unit.asInstanceOf[UnitAttachable].attach(fileName, sb)
            Utils.outln(sb.toString())
        }
    }

    false
  }
}
