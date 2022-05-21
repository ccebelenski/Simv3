package com.sim.device

import com.sim.Utils
import com.sim.cpu.{BasicMMU, Register16, Z80}
import com.sim.s100.*
import com.sim.unsigned.UInt
import org.junit.Assert.assertTrue
import org.junit.{Before, Test}

import scala.collection.mutable

class S100HDiskTests {

  var z80: Z80 = _
  var mmu: BasicMMU = _
  var machine: S100Machine = _
  var PC: Register16 = _
  var hdsk: S100HDSKDevice = _
  var fdsk: S100FD400Device = _
  var sb: mutable.StringBuilder = _

  @Before
  def setUpZ80(): Unit = {
    if (machine == null) {
      machine = new S100Machine()
      machine.init()
      z80 = machine.findDevice("Z80A").get.asInstanceOf[Z80]
      mmu = z80.MMU
      z80.setMemorySize(UInt(0x500))
      //Z80Tests.mmu.mapRAM(UInt(0x0000), UInt(0x500))
      PC = z80.registers("PC").asInstanceOf[Register16]
      sb = new mutable.StringBuilder
      z80.setOption("STOPONHALT", "true", sb)
      hdsk = machine.findDevice("HDA").get.asInstanceOf[S100HDSKDevice]
      fdsk = machine.findDevice("FDA").get.asInstanceOf[S100FD400Device]
      Utils.outln(sb.toString())
      sb.clear()
    }
  }

  @Test
  def testRAM(): Unit = {

    val image = hdsk.bootrom_hdsk
    mmu.mapRAM(UInt(0x5c00), UInt(image.size - 1), image.toArray)

    assertTrue(mmu.get8(0x5c00) == 0xf3)
    assertTrue(mmu.get8(0x5c70) == 0xc3)
    assertTrue(mmu.get8(0x5cff) == 0x00)

    val sb = new mutable.StringBuilder
    z80.DAsm(0x5c00, 0x5cff, sb)
    Utils.out(sb.toString())
  }

  @Test
  def testAttach(): Unit = {
    val units = hdsk.getUnits.asInstanceOf[Iterator[S100HDSKUnit]]
    val unit = units.next()
    Utils.outln(s"Unit: ${unit.getName}")

    // No easy way to check return from this right now
    unit.attach("Z3DOS.DSK", sb)
    Utils.outln(sb.toString())

    sb.clear()
    unit.detach(sb)
    Utils.outln(sb.toString())
  }

  @Test
  def testReadConsist(): Unit = {

    val units = hdsk.getUnits.asInstanceOf[Iterator[S100HDSKUnit]]
    val unit: S100HDSKUnit = units.next()
    Utils.outln(s"Unit: ${unit.getName}")

    // No easy way to check return from this right now
    unit.attach("cpm3.dsk", sb)
    Utils.outln(sb.toString())

    val funits = fdsk.getUnits.asInstanceOf[Iterator[S100FD400Unit]]
    val funit = funits.next()
    Utils.outln(s"FUnit: ${funit.getName}")
    sb.clear()
    funit.attach("cpm3.dsk", sb)
    Utils.outln(sb.toString())

    // Attached to both devices, something we couldn't physically do of course

    Utils.outln("cpm image attach to both controllers, testing we get the same bytes for both")

    fdsk.current_disk = Some(funit)
    funit.current_sector = 6
    funit.current_track = 7
    funit.seek()
    funit.readSector() // should have read track 4, sector 4
    val fposition = funit.fileChannel.position()
    Utils.outln(s"FUNIT POSITION AFTER READ = $fposition")


    hdsk.current_disk = Some(unit)
    unit.current_track = 7
    unit.current_sector = 6
    hdsk.selectedDMA = 0 // write to 0
    hdsk.hdsk_read()
    val position = unit.fileChannel.position()
    Utils.outln(s"UNIT POSITION AFTER READ = $position")



    val a1 = funit.byteBuffer.array()
    assertTrue(s"Should have read 137 bytes", a1.length == 137)

    for (x <- 0 until a1.length) {
      val b = z80.MMU.get8(x).byteValue
      assertTrue(s"Byte mismatch at position $x : $b : ${a1(x)}", b == a1(x))
      Utils.out(s"$b:${a1(x)} ")
    }
    assertTrue(s"Position should be the same after both reads.", fposition == position)
    Utils.outln("\n\rDone checking read.")

    sb.clear()
    funit.detach(sb)
    Utils.outln(sb.toString())

    sb.clear()
    unit.detach(sb)
    Utils.outln(sb.toString())

    assertTrue(s"Position should be the same after both reads.", fposition == position)
  }
}
