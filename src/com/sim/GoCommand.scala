package com.sim

import com.sim.machine.AbstractMachine
import com.sim.unsigned.UInt

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class GoCommand extends Command {
  commandToken = "GO"
  commandDescription = "Execute starting at address <start>, or last position."
  commandHelpText = "GO <START>"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder

    if (Console.cpuRunning) {
      sb.append("SIM: Invalid command while CPU is running")
    } else
      Console.simEnvironment.simMachine match {
        case None => sb.append("SIM: No machine.  SET a MACHINE.")
        case Some(m: AbstractMachine) =>
          if (tokenArray.length >= 1)
            try {
              val addr = Integer.decode(tokenArray(0))
              Console.cpuRunning = true
              val t: Future[Unit] = Future {
                m.getCPU.runcpu(false, UInt(addr))
              }
              t onComplete {
                case Success(value) =>
                  Console.cpuRunning = false
                  Console.userInterrupt = false
                case Failure(exception) =>
                  Console.cpuRunning = false
                  Console.userInterrupt = false
              }
            } catch {
              case nfe: NumberFormatException =>
                sb.append("SIM: Illegal start address.")
            }
          else {
            // No start address, just restart the CPU
            Console.cpuRunning = true
            val t: Future[Unit] = Future {
              m.getCPU.runcpu()
            }
            t onComplete {
              case Success(value) =>
                Console.cpuRunning = false
                Console.userInterrupt = false
              case Failure(exception) =>
                Console.cpuRunning = false
                Console.userInterrupt = false
            }

          }
      }
    Utils.outln(sb.toString())
    false
  }

}

class StepCommand extends Command {
  commandToken = "STEP"
  commandDescription = "Single-step the CPU"
  commandHelpText = "STEP - execute one instruction on the CPU"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (Console.cpuRunning) sb.append("SIM: Invalid command while CPU is running")
    else
      Console.simEnvironment.simMachine match {
        case None => sb.append("SIM: No machine.  SET a MACHINE.")
        case Some(m: AbstractMachine) =>
          Console.userInterrupt = false
          Console.cpuRunning = true
          m.getCPU.runcpu(singleStep = true)
          Console.cpuRunning = false
          Console.userInterrupt = false

      }
    Utils.outln(sb.toString())
    false
  }
}
