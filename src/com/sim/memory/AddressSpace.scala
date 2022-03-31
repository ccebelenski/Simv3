package com.sim.memory

import com.sim.unsigned.{UByte, UInt}

/**
 * Created by christophercebelenski on 7/18/16.
 */
abstract class AddressSpace(val lowAddress: UInt, val highAddress: UInt) {

  // is it read only? (Probably ROM, but could be a memory mapped device on a bus)
  val isReadOnly: Boolean = true

  /**
   * Returns true if address space contains the address.
   *
   * @param address address
   * @return
   */
  def containsAddress(address: UInt): Boolean = {

    if ((address >= lowAddress) && (address <= highAddress)) true else false
  }

  def put8(address: UInt, value: UByte): Unit

  def get8(address: UInt): UByte

  def load8(address:UInt, value:UByte) : Unit


}
