package com.sim

import com.sim.machine.AbstractMachine

class DisassembleCommand extends Command {

  commandToken = "DISASSEMBLE"
  commandDescription = "Disassemble memory starting at address <start>, end at option <end>"
  commandHelpText = "DIS<ASSEMBLE> [START] <END>"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length < 1 || tokenArray.length > 2) sb.append("SIM: Requires a start address, optional end address.\n\r")

    else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.\n\r")
      case Some(m: AbstractMachine) =>
        val cpu = m.getCPU
        try {
          val start = Integer.decode(tokenArray(0))
          val end: Int = if (tokenArray.length == 2) Integer.decode(tokenArray(1)) else -1
          if (end == -1) cpu.DAsm(start, sb) else cpu.DAsm(start, end, sb)
        } catch {
          case nfe: NumberFormatException =>
            sb.append("SIM: Illegal start or end address.\n\r")
        }

    }

    Utils.outln(sb.toString())
    false
  }
}