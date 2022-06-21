package com.sim

import com.sim.unsigned.{UByte, UInt}
import org.junit.Test
import org.junit.Assert.*
class TestMath {



  @Test
  def testUByte() : Unit = {

    val a : UByte = UByte(0)
    val b : UByte = UByte(1)
    val c : UByte = UByte(0xff.toByte) // -1
    val d : UByte = UByte(0xfe.toByte) // -2

    assertTrue(a == UByte(0))
    assertTrue(c.byteValue == -1)
    assertTrue(b.intValue() == 1)
    Utils.outln(s"d = ${d.intValue} : Min = ${UByte.MinValue()} : Max = ${UByte.MaxValue()}")
    assertTrue(d.byteValue() == -2)
    assertTrue(d.intValue() == 0xfe)

    assertEquals(UInt(1), a + b)
    assertEquals(-1, a.byteValue + c.byteValue)
  }

  def testUInt(): Unit = {

    val a : UInt = UInt(0)
    val b : UInt = UInt(1)
    val c : UInt = UInt(5)

  }
}
