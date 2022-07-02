package com.sim.cpu

import com.sim.{Console, Utils}
import com.sim.device.{MemoryMappedDevice, PortMappedDevice}
import com.sim.memory.{AddressSpace, MemoryAddressSpace, ROMAddressSpace}
import com.sim.unsigned.*
import com.sim.unsigned.uint2int

import scala.language.implicitConversions

/**
 * Created by christophercebelenski on 7/18/16.
 *
 * A basic MMU that supports simple bank switching.
 *
 */
abstract class BasicMMU(val cpu: BasicCPU) {

  // Some constants - allow for overrides later

  val MAXBANKSIZE: Int = 65536 // Max memory size, power of 2
  final val MAXBANKSIZELOG2: Double = Math.log(MAXBANKSIZE / Math.log(2)) // log2 of MAXBANKSIZE
  val MAXBANKS: Int = 16 // Max number of memory banks, a power of 2
  final val MAXBANKSLOG2: Double = Math.log(MAXBANKS / Math.log(2)) // log2 of MAXBANKS
  final val MAXMEMORY: Int = MAXBANKS * MAXBANKSIZE // Maximum total memory size
  final val ADDRMASK: Int = MAXMEMORY - 1// Address mask
  final val ADDRMASKEXTENDED: Int = MAXMEMORY - 1
  final val BANKMASK: Int = MAXBANKS - 1

  val LOG2PAGESIZE: Int = 8
  final val PAGESIZE: Int = 1 << LOG2PAGESIZE

  val COMMON = 0xc000 // Addreses greater than common are in the same memory.

  final val mtab: Array[Option[MMU_ENTRY]] = new Array[Option[MMU_ENTRY]](1 + (MAXMEMORY >> LOG2PAGESIZE))
  for (x <- mtab.indices) mtab(x) = None

  // 256 Ports
  final val iotab: Array[Option[PortMappedDevice]] = new Array[Option[PortMappedDevice]](256)
  for (x <- iotab.indices) iotab(x) = None

  private var bankSelect: Int = 0
  val debug = false

  def mapRAM(baseAddress: Int, size: Int): Unit = {

    for (i <- 0 to (size >> LOG2PAGESIZE)) {
      val addr = (baseAddress & 0xfff00) + (i << LOG2PAGESIZE)
      val pageaddr = if (cpu.isBanked && addr < COMMON) addr | (bankSelect << MAXBANKSIZELOG2.toInt) else addr
      val page = pageaddr >> LOG2PAGESIZE.toInt
      val as = new MemoryAddressSpace(addr, addr + PAGESIZE - 1)
      val entry = MMU_ENTRY(memory = Some(as))
      mtab(page) = Some(entry)
      if(debug) Utils.outln(f"MMU: Mapped RAM memory space.  Page: 0x$page%04X, Addr: 0x$addr%04X - 0x${as.highAddress.intValue}%04X")
    }

  }

  def mapROM(baseAddress: UInt, size: UInt, image: Array[Int]): Unit = {
    mapAS(baseAddress, size, image, true)
  }

  def mapRAM(baseAddress: UInt, size: UInt, image: Array[Int]): Unit = {
    mapAS(baseAddress, size, image, false)

  }

  def mapAS(baseAddress: UInt, size: UInt, image: Array[Int], asROM: Boolean): Unit = {
    for (i <- 0 to (size >> LOG2PAGESIZE)) {
      val addr = (baseAddress & 0xfff00) + (i << LOG2PAGESIZE)
      val pageaddr = if (cpu.isBanked && addr < COMMON) addr | (bankSelect << MAXBANKSIZELOG2.toInt) else addr
      val page = pageaddr >> LOG2PAGESIZE
      val as: AddressSpace = {
        if (asROM) new ROMAddressSpace(addr, addr + PAGESIZE - 1)
        else new MemoryAddressSpace(addr, addr + PAGESIZE - 1)
      }
      val imgIdx = i << LOG2PAGESIZE
      val imgSize = PAGESIZE - 1
      for (x <- imgIdx to imgSize) as.load8(addr + x, UByte(image(x).byteValue()))

      val entry = MMU_ENTRY(None, memory = Some(as))
      mtab(page) = Some(entry)
      if(debug) Utils.outln(f"MMU: Mapped ROM memory space.  Page: 0x$page%04X, Addr: 0x$addr%04X - 0x${as.highAddress.intValue}%04X")
    }

  }

  def mapMemoryMappedDevice(u: MemoryMappedDevice): Unit = {

    // TODO
  }

  def mapPortMappedDevice(u: PortMappedDevice): Unit = {

    u.ports.foreach(i => {
      if (iotab(i & 0xff).isDefined) {
        if(debug) Utils.outln(f"MMU: IO Port: 0x${i & 0xff}%02X already mapped to Unit: ${iotab(i & 0xff).get.getName}")
      } else {
        iotab(i & 0xff) = Some(u)
        if(debug) Utils.outln(f"MMU: Mapping IO Port: 0x${i & 0xff}%02X Device: ${u.getName}")
      }
    })
  }

  def unMapIOPort(u: PortMappedDevice): Unit = {

    u.ports.foreach(i => {
      if (iotab(i & 0xff).isDefined) {
        if(debug) Utils.outln(f"MMU: Unmapping IO Port: 0x${i & 0xff}%02X Device: ${u.getName}")
        iotab(i & 0xff) = None
      } else {
        if(debug) Utils.outln(f"MMU: IO Port: 0x${i & 0xff}%02X is not mapped.")
      }
    })
  }

  def printAddressSpace(): Unit = {

    for (x <- mtab.indices) {
      val e = mtab(x)
      if (e.isDefined) {
        if (e.get.memory.isDefined) {
          val as = e.get.memory.get
          val msg = f"Page: 0x$x%04X : 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X : Type: ${as.getClass} Mapped: RO: ${as.isReadOnly}"
          Utils.outln(msg)
        } else if (e.get.memoryDevice.isDefined) {
          val unit = e.get.memoryDevice.get
          val msg = f"Page: 0x$x%04X : 0x${unit.lowAddress.intValue}%04X - 0x${unit.highAddress.intValue}%04X : Type: ${unit.getClass}"
          Utils.outln(msg)
        }
      }

    }

  }

  def installROM(image: Array[Int], size: Int, baseAddr: UInt): Unit = {
    mapROM(baseAddr, UInt(image.length - 1), image)
  }

  def installRAM(image: Array[Int], size: Int, baseAddr: UInt): Unit = {
    mapRAM(baseAddr, UInt(image.length - 1), image)
  }

  @inline
  def put8(register16: Register16, value: Register8): Unit = {
    put8(register16.get16.toInt, value.get8().toUByte)
  }

  @inline
  def put8(register16: Register16, value: UByte): Unit = {
    put8(register16.get16.toInt, value)
  }

  @inline
  def put8(register16: Register16, value: Int): Unit = {
    put8(register16.get16.intValue(), UByte(value.byteValue()))
  }

  @inline
  def put8(address: UShort, value: Register8): Unit = {
    put8(address.intValue(), value.get8())
  }

  def put8(address: Int, value: UByte): Unit = {
    require(address >= 0)
    var addr: Int = UInt(address) & ADDRMASK
    var pageaddr = addr
    if(cpu.isBanked && (addr < COMMON)) {
      pageaddr = addr | bankSelect << MAXBANKSIZELOG2.toInt
      addr = addr | bankSelect << MAXBANKSIZELOG2.toInt
    }
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None => Utils.outln(f"MMU: Write to non-existent memory.  Addr: 0x$addr%04X")
      case Some(e: MMU_ENTRY) =>
        if (e.memory.isDefined) {
          val as = e.memory.get
          if (as.containsAddress(UInt(addr))) as.put8(UInt(addr), value)
          else {
            val msg = f"MMU: Page table error - Addr: 0x$addr%04X address space: 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X"
            Utils.outln(msg)
          }
        } else if (e.memoryDevice.isDefined) {
          // TODO This needs work
          e.memoryDevice.get.action(UInt(addr), value, isWrite = true)
        } else Utils.outln(f"MMU: Mis-configured page/address entry - no type found. Addr: 0x$addr%04X")
    }

  }

  def out8(address: Int, value: UByte): Unit = {
    //if(address < 0x08 || address > 0x12) Utils.outln(s"MMU: Write to port: 0x${address.toHexString}, value: 0x${value.toInt.toHexString}")
    iotab(address & 0xff) match {
      case None => {
        if(debug) Utils.outln( s"MMU: Write to unconnected port.  Port:0x${address.toHexString}  Value: 0x${value.toHexString}")
        //Console.userInterrupt = true
      }
      case Some(pmu:PortMappedDevice) => pmu.action(UInt(address), value, isWrite = true)
    }

  }

  def out8(r1: Register8, value: UByte): Unit = {
    out8(r1.intValue(), value)
  }

  def out8(r1: Register8, value: Int): Unit = {
    out8(r1.intValue(), UByte(value.byteValue()))
  }

  def in8(address: Int): UByte = {
    iotab(address & 0xff) match {
      case None =>
        if(debug) Utils.outln(s"MMU: Read from unconnected port.  Port:0x${address.toHexString}")
        //Console.userInterrupt = true
        UByte(0)
      case Some(pmu) =>
        val rv = pmu.action(UInt(address), UByte(0), isWrite = false)
        //if(address < 0x08 || address > 0x12) Utils.outln(s"MMU: Read from port: 0x${address.toHexString}, RV: 0x${rv.toInt.toHexString}")
        rv
    }
  }

  // Store little endian...
  inline def put16(address: Int, value: UShort): Unit = {

    put8(address, UByte((value & 0xFF).toByte))
    put8(address + 1, UByte(((value >> 8) & 0xFF).toByte))
  }

// Get8 with PC increment (for consistency)
  inline def get8PC(pc:Register16) : UByte = {
    val nn = get8(pc.get16.intValue())
    pc.increment()
    nn
}
  

  inline def get8(address: Register16): UByte = {
    get8(address.get16.intValue())
  }


  inline def get8(address: Int): UByte = {
    require(address >= 0)
    var addr: Int = UInt(address) & ADDRMASK
    var pageaddr = addr
    if(cpu.isBanked && (addr < COMMON)) {
      pageaddr = addr | bankSelect << MAXBANKSIZELOG2.toInt
      addr = addr | bankSelect << MAXBANKSIZELOG2.toInt
    }
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None =>
        Utils.outln(f"MMU: Read from non-existent memory.  Addr: $addr%04X")
        UByte(0)
      case Some(e: MMU_ENTRY) =>
        if (e.memory.isDefined) {
          val as = e.memory.get
          if (as.containsAddress(UInt(addr))) as.get8(addr)
          else {
            val msg = f"MMU: Page table error - Addr: 0x$addr%04X address space: 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X"
            Utils.outln(msg)
            UByte(0)
          }
        } else if (e.memoryDevice.isDefined) {
          // TODO This needs work.
          e.memoryDevice.get.action(UInt(addr), UByte(0), isWrite = false)
        } else {
          Utils.outln(f"MMU: Mis-configured page/address entry - no type found. Addr: 0x$addr%04X")
          UByte(0)
        }
    }
  }


  // Retrieve little endian

  inline def get16(address: Int): UShort = {
    UShort((get8(address) | (get8(address + UInt(1)) << 8)).shortValue())
  }


  inline def get16(register16: Register16): UShort = {
    get16(register16.get16)
  }



  inline def get16(address: UShort): UShort = {
    get16(address.intValue())
  }

  def selectBank(bank: Int): Unit = this.bankSelect = bank

  def getBank: Int = this.bankSelect
}

case class MMU_ENTRY(memoryDevice: Option[MemoryMappedDevice] = None, memory: Option[AddressSpace] = None)
