package com.sim

import com.sim.machine.AbstractMachine
import com.sim.unsigned.UInt

class MLogCommand extends Command {

  commandToken = "MLOG"
  commandDescription = "Set a memory log at address [ADDR]"
  commandHelpText = "ML<OG> [ADDR]"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length != 1) sb.append("SIM: Requires a log address.")
    else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        try {
          val addr = Integer.decode(tokenArray(0))
          m.addMemLog(UInt(addr))
        }catch {
          case nfe:NumberFormatException =>
            sb.append("SIM: Illegal address.")
        }
    }
    false
  }
}

class UnMLogCommand extends Command {
  commandToken = "UNMLOG"
  commandDescription = "Remove any memlog at address [ADDR], or [ALL] for all memlogs set"
  commandHelpText = "UNML<OG> [ADDR/ALL]"

  override def process(tokenArray: Array[String]): Boolean = {

    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length != 1) sb.append("SIM: Requires a log address, or ALL.")
    else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        if(tokenArray(0) == "ALL") m.clearMemLog()
        else
          try {
            val addr = Integer.decode(tokenArray(0))
            m.remoteMemLog(UInt(addr))
          }catch {
            case nfe:NumberFormatException =>
              sb.append("SIM: Illegal address.")
          }
    }
    false
  }
}