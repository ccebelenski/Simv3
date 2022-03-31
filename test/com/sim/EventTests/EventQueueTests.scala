package com.sim.EventTests

import com.sim.{EventQueue, TestDevice, TestMachine, TestUnit, Utils}
import org.junit.Assert.*
import org.junit.Test

class EventQueueTests {
  val m = new TestMachine
  val d = new TestDevice(m)
  m.devices.append(d)
  val u = new TestUnit(m.devices.head)
  val u2 = new TestUnit(m.devices.head)
  val u3 = new TestUnit(m.devices.head)

  m.devices.head.addUnit(u)
  m.devices.head.addUnit(u2)
  m.devices.head.addUnit(u3)


  @Test
  def testPQOrder(): Unit = {
    val eq = new EventQueue

    // First test that our queue does return things in priority order
    u.time = 1
    u2.time = 2
    u3.time = 3
    EventQueue.clockQueue += u
    EventQueue.clockQueue += u3
    EventQueue.clockQueue += u2

    val el = EventQueue.getClockQueueList

    assertTrue(el.nonEmpty)
    val tu = el(0)
    Utils.outln(s"Elem 1, time=${tu.time}")
    assertTrue(tu.time == 1)
    val tu2 = el(1)
    Utils.outln(s"Elem 2, time=${tu2.time}")
    assertTrue(tu2.time == 2)
    val tu3 = el(2)
    Utils.outln(s"Elem 3, time=${tu3.time}")
    assertTrue(tu3.time == 3)

    assertTrue(EventQueue.clockQueue.nonEmpty)

    val p1 = EventQueue.clockQueue.dequeue()
    assertTrue(p1.time == 1)
    val p2 = EventQueue.clockQueue.dequeue()
    assertTrue(p2.time == 2)
    val p3 = EventQueue.clockQueue.dequeue()
    assertTrue(p3.time == 3)
    assertTrue(EventQueue.clockQueue.isEmpty)

  }

}
