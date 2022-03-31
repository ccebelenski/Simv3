package com.sim

import java.io.ByteArrayInputStream

import com.sim.machine.AbstractMachine
import com.sim.unsigned.UByte

import scala.collection.mutable.ArrayBuffer

/**
 * Examine Memory
 */
class EM extends Command {
  commandToken = "EM"
  commandDescription = "Examine memory byte at address [ADDR] to <ADDR>"
  commandHelpText = "Examine memory"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length < 1 || tokenArray.length > 2) sb.append("SIM: Requires an address or address range.")
    else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        val cpu = m.getCPU
        try {
          val start = Integer.decode(tokenArray(0))
          if (tokenArray.length == 1) {
            // Single byte
            val addr = Integer.decode(tokenArray(0))
            cpu.examine(addr, sb)

          } else {
            // Byte range
            val startAddr = Integer.decode(tokenArray(0))
            val endAddr = Integer.decode(tokenArray(1))
            if (endAddr < startAddr || endAddr == startAddr) sb.append("SIM: Illegal range.")
            else {
              val buf = new ArrayBuffer[UByte]
              for (x <- startAddr.intValue() to endAddr.intValue()) buf += cpu.MMU.get8(x)
              HexDump.hexdump(buf.toArray, startAddr.intValue(), sb)
            }

          }
        } catch {
          case nfe: NumberFormatException =>
            sb.append("SIM: Illegal start or end address.")
        }
    }

    Utils.outln(sb.toString())
    false
  }
}

/**
 * Examine Register
 */
class ER extends Command {
  commandToken = "ER"
  commandDescription = "Examine register [R]"
  commandHelpText = "Examine register value"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length != 1) sb.append("SIM: Requires a register to examine.")
    else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        val cpu = m.getCPU
        cpu.examineRegister(tokenArray(0), sb)
    }
    Utils.outln(sb.toString())
    false
  }
}

object HexDump {

  def hexdump(ubytes: Array[UByte], startAddr: Int, sb: StringBuilder) = {
    val bytesPerClump = 4
    val clumpsPerLine = 4
    val bufferSize = bytesPerClump * clumpsPerLine

    var done = false
    var offset: Integer = 0
    val bis = new ByteArrayInputStream(ubytes.map(_.byteValue))
    var buf = new Array[Byte](bufferSize)

    while (!done) {
      val numBytesRead = bis.read(buf, 0, bufferSize)
      if (numBytesRead < 0) {
        done = true
      }
      else {
        sb.append(f"${offset + startAddr}%08x: ")

        for (i <- 0 until bufferSize) {
          if (i < numBytesRead)
            sb.append(f"${buf(i).toInt & 0xff}%02x ")
          else
            sb.append("   ")

          if ((i % bytesPerClump) == (bytesPerClump - 1)) {
            if ((i > 0) && (i < bufferSize - 1))
              sb.append(" ")
          }
        }

        sb.append("|")
        for (i <- 0 until numBytesRead) {
          if ((buf(i) >= 0x20) && (buf(i) <= 0x7e))
            sb.append(buf(i).toChar)
          else
            sb.append(".")
        }
        for (i <- numBytesRead until bufferSize) {
          sb.append(" ")
        }
        sb.append("|\n")

        offset += bufferSize
      }
    }
  }


}