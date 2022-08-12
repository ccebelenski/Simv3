package com.sim.device

import com.sim.machine.AbstractMachine
import com.sim.memory.AddressSpace
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable.ArrayBuffer
import com.sim.unsigned.ubyte2uint
import com.sim.unsigned.*

import scala.language.implicitConversions

abstract class MemoryMappedDevice(machine: AbstractMachine, lowAddress: Int, highAddress: Int)
  extends AddressSpace(lowAddress, highAddress) with BasicDevice(machine) {

  override val isMemoryMapped: Boolean = true

  def handles(address: UInt): Boolean = {

    if (address.intValue <= highAddress && address.intValue >= lowAddress) {
      A.exists(a => a.handles(address.intValue))
    } else false
  }

  def action(address: Int, isByte:Boolean , value: Int, isWrite: Boolean): Int = {
    A.find(a => a.handles(address)) match {
      case None => 0
      case Some(a) => a.action(address, isByte, value, isWrite)
    }
  }

  def addAction(action: MappedDeviceAction): Unit = {
    A.append(action)
  }

  protected val A: ArrayBuffer[MappedDeviceAction] = new ArrayBuffer(20)

}

abstract class MappedDeviceAction(val address: Int, val device: BasicDevice) {

  var oddAddress: Boolean = false

  def action(address: Int,  isByte: Boolean,value: Int, isWrite: Boolean): Int = {
    if ((address & 1) != 0) oddAddress = true
    0
  }

  def handles(address: Int): Boolean

}

// 16 bit mapping
class MappedDeviceAction16(address: Int, device: BasicDevice) extends MappedDeviceAction(address, device) {

  // Can't set this up on an odd address
  require((address & 1) == 0)

  override def action(address: Int, isByte: Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address,isByte,value, isWrite)
  }

  def handles(address: Int): Boolean = {
    (address == this.address) || (address == (this.address + 1))
  }
}

// 8 bit mapping
class MappedDeviceAction8(address: Int, device: BasicDevice) extends MappedDeviceAction(address, device) {
  override def action(address: Int, isByte:Boolean, value: Int, isWrite: Boolean): Int = {
    super.action(address,isByte,value, isWrite)
  }

  def handles(address: Int): Boolean = {
    address == this.address
  }
}
