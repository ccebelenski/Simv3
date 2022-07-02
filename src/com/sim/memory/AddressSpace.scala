package com.sim.memory

import com.sim.unsigned.{UByte, UInt}

/**
 * Created by christophercebelenski on 7/18/16.
 */
abstract class AddressSpace(val lowAddress: Int, val highAddress: Int) {

  // is it read only? (Probably ROM, but could be a memory mapped device on a bus)
  val isReadOnly: Boolean = true

  /**
   * Returns true if address space contains the address.
   *
   * @param address address
   * @return
   */
  inline final def containsAddress(address: Int): Boolean = {

    if ((address >= lowAddress) && (address <= highAddress)) true else false
  }

  def put8(address: Int, value: UByte): Unit

  def get8(address: Int): UByte

  def load8(address:Int, value:UByte) : Unit


}
