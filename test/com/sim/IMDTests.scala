package com.sim

import com.sim.device.{BasicDevice, BasicUnit, DiskUnit, ImageDisk}
import org.junit.{Before, Test}

class IMDTests {

  var diskUnit : IMDUnit = _

  @Before
  def setupDummyDisk(): Unit = {

    diskUnit = new IMDUnit(null)

  }

  @Test
  def testSetup():Unit = {

  }

  // Dummy disk unit to run tests on.
  class IMDUnit(device: BasicDevice) extends BasicUnit(device) with DiskUnit with ImageDisk {
    override val waitTime: Long = 0L

    override def cancel(): Unit = ???

    override def completeAction(): Unit = ???

    override def init(): Unit = ???

    override def optionChanged(sb: StringBuilder): Unit = ???

    override def attach(filespec: String, sb: StringBuilder): Boolean = ???
  }

}
