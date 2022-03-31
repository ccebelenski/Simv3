package com.sim.device

import com.sim.cpu.BasicMMU
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt}

/**
 * Ports are relative to a base port.  If the base port is 5, and the size = 2, then action port 5 is entry 0, and action port 6 is entry 1
 *
 * @param machine
 * @param ports List of ports this unit responds to
 */
abstract class PortMappedDevice(machine: AbstractMachine, mmu: BasicMMU, val ports: List[UInt]) extends BasicDevice(machine) {

  override val isPortMapped: Boolean = true

  def handles(port: UInt): Boolean = {
    ports.contains(port)
  }

  override def setEnable(state: Boolean): Unit = {
    super.setEnable(state)
    if(state) {
      // Enabling
      mmu.mapPortMappedDevice(this)
    } else {
      // Disabling
      mmu.unMapIOPort(this)
    }
  }

  // Perform a unit action
  def action(action: UInt, value: UByte, isWrite: Boolean) : UByte



}
