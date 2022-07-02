package com.sim.memory

import com.sim.{Console, Utils}
import com.sim.unsigned._
import scala.language.existentials
import scala.language.implicitConversions


class MemoryAddressSpace(lowAddress: Int, highAddress: Int) extends AddressSpace(lowAddress, highAddress) {

  override val isReadOnly: Boolean = false

  private final val M: Array[UByte] = new Array[UByte](highAddress - lowAddress + 1)
  private final val mlen:Int = M.length

  for(x <- 0 to (highAddress - lowAddress).toInt) M(x) = UByte(0)

  override def put8(address: Int, value: UByte): Unit = {
    M(scaleAddress(address)) = value
  }

  override def get8(address: Int): UByte = {
    M(scaleAddress(address))
  }
  // One-time load
  override def load8(address:Int, value:UByte) : Unit = {
    put8(address,value)
  }

  private inline def scaleAddress(address:Int) : Int = {
    mlen - (highAddress - address) - 1
  }

}

class ROMAddressSpace(lowAddress: Int, highAddress: Int) extends MemoryAddressSpace(lowAddress, highAddress) {
  override val isReadOnly: Boolean = true

  override def put8(address:Int, value:UByte): Unit = {
    //Utils.outln(s"Memory: Illegal ROM write access. Addr: ${address.toHexString}")
  }
  // One-time load for ROM's
  override def load8(address:Int, value:UByte) : Unit = {
    super.put8(address,value)
  }
}