package com.sim.device

import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable.ArrayBuffer
import com.sim.unsigned.ubyte2uint
import com.sim.unsigned._
import scala.language.implicitConversions

abstract class MemoryMappedDevice(machine: AbstractMachine, val lowAddress: UInt, val highAddress: UInt) extends BasicDevice(machine) {

  override val isMemoryMapped: Boolean = true
  def handles(address: UInt) : Boolean= {

    if(address <= highAddress && address >= lowAddress){
      A.exists(a => a.address == address)
    } else false
  }

  def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    A.find(a => a.address == action) match {
      case None => new UByte(0)
      case Some(a) => a.action(value, isWrite)
    }
  }

  val A: ArrayBuffer[MappedDeviceAction] = new ArrayBuffer()



}

case class MappedDeviceAction(address : UInt, action: (UInt, Boolean) => UByte)
