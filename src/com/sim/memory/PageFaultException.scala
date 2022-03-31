package com.sim.memory

import com.sim.unsigned.UInt

class PageFaultException(addressSpace: AddressSpace, address: UInt) extends AddressSpaceException(addressSpace = addressSpace) {

}
