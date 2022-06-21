package com.sim.s100

import com.sim.Utils
import com.sim.cpu.Z80MMU
import com.sim.device.{BinaryUnitOption, PortMappedDevice, SerialDevice, ValueUnitOption}
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable

/**
  * A simulated MITS 2SIO interface card.
  *
  * The card had two physical I/O ports which could be connected
  * to any serial I/O device that would connect to a current loop,
  * RS232, or TTY interface. Available baud rates were jumper
  * selectable for each port from 110 to 9600.
  * All I/O is via programmed I/O. Each device has a status port
  * and a data port. A write to the status port can select
  * some options for the device (0x03 will reset the port).
  * A read of the status port gets the port status:
  * +---+---+---+---+---+---+---+---+
  * | X | X | X | X | X | X | O | I |
  * +---+---+---+---+---+---+---+---+
  * I - A 1 in this bit position means a character has been received
  * on the data port and is ready to be read.
  * O - A 1 in this bit means the port is ready to receive a character
  * on the data port and transmit it out over the serial line.
  * A read to the data port gets the buffered character, a write
  * to the data port writes the character to the device.
  *
  * @param machine s100
  * @param mmu mmu
  * @param ports List of ports this unit responds to
  */
class S100SIODevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDevice(machine, mmu, ports) with SerialDevice {

  // Flags for read/write status
  val CAN_READ: UByte = UByte(1)
  val CAN_WRITE: UByte = UByte(2)


  override val description: String = "MITS 2SIO interface card"
  override val name = "SIO"

  // We only have one unit, the main serial port.  Because of this we can take some shortcuts.
  var SIOUnit: S100SIOUnit = _

  debug = false

  override def init(): Unit = {

    // Create a default serial console unit
    SIOUnit = new S100SIOUnit(this)
    addUnit(SIOUnit)

    // Register this device with the default MUX
    //    machine.findDevice("MUXA") match {
    //      case None => Utils.outln(s"$getName: Could not register device with MUXA.")
    //      case Some(x:MuxDevice) =>
    //        x.registerDevice(this)
    //        this.registeredMuxDevice = Some(x)
    //      case _ => Utils.outln(s"$getName: Misconfiguration - register device with MUXA.")
    //    }

  }

  override def showCommand(sb: mutable.StringBuilder): Unit = {
    super.showCommand(sb)

  }

  override def createUnitOptions(): Unit = {
    createSerialUnitOptions()

    unitOptions.append(BinaryUnitOption("INTERRUPT", "Status port 0 creates an interrupt when a character becomes available", value = false))
    unitOptions.append(ValueUnitOption("IOPORT", "Set I/O port to IOPORT", value = 0))

  }

  //  // Only one unit, so we take some shortcuts...
  //  override def muxCharacterInterrupt(unit: MuxUnit, char: Int): Unit = {
  //    if(!SIOUnit.inputCharacterWaiting) {
  //      mmu.cpu.keyboardInterrupt = false
  //      SIOUnit.inputCharacter = char
  //      SIOUnit.inputCharacterWaiting = true
  //      //machine.eventQueue.activate(SIOUnit, SIOUnit.waitTime)
  //    }
  //  }

  //  // Returns true when the interrupt flag for keyboard is not set.
  //  override def checkDeviceReady: Boolean = {
  //    !mmu.cpu.keyboardInterrupt
  //  }

  def interruptOff(): Unit = {
    mmu.cpu.keyboardInterrupt = false
    //machine.eventQueue.cancel(SIOUnit)
  }

  override def optionChanged(sb: mutable.StringBuilder): Unit = ???

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    //    val ch = value.toChar
    //    Utils.outln(s"$getName: Port: $action Value:$value Char: $ch isWrite: $isWrite")

    // Port 10 is status port for console terminal
    if (action == 0x10) {

      if (!isWrite) {
        if (SIOUnit.inputCharacterWaiting) {
          //Utils.outln(s"$getName: CANREAD | CANWRITE")
          return UByte((CAN_READ | CAN_WRITE).byteValue())
        } else {
          //Utils.outln(s"$getName: None")
          return CAN_WRITE
        }
      }

      UByte(0) // writes are ignored - nothing to reset anyway.

    } else if (action == 0x11) {

      if (isWrite) {
        //        if(value.intValue() < 10 || value.intValue() > 125)
        //          {
        Utils.outlnd(this, s"Char: ${value.intValue()}\t: ${value.byteValue.toChar}")
        //          }
        SIOUnit.getTerminal.print(s"${value.byteValue.toChar}")
        UByte(0)
      } else {
        // read char
        val b: Byte = if(SIOUnit.inputBuffer.nonEmpty) SIOUnit.inputBuffer.dequeue() else 0
        if (SIOUnit.inputBuffer.isEmpty) {

          SIOUnit.inputCharacterWaiting = false
        }
        //Utils.outln(b.toString)
        interruptOff()
        UByte(b)
      }

    } else if (action == 0x12) {
      if (isWrite) UByte(0) // ignore
      else CAN_WRITE
    } else {
      Utils.outlnd(this, s"$getName: Misconfiguration, write to port ${action.toHexString} value ${value.toHexString}")
      UByte(0)
    }
  }

}
