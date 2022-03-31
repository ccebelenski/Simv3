package com.sim.machine

import com.sim.cpu.BasicCPU
import com.sim.device.{BasicDevice, BasicUnit, PortMappedDevice}
import com.sim.s100.S100Machine
import com.sim.unsigned.UInt
import com.sim.{EventQueue, Named, SimTimer, SimTimerUnit, Utils}

import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class AbstractMachine extends Named {

  val description:String = "None"

  var devices: ListBuffer[BasicDevice] = new ListBuffer[BasicDevice]

  // Create a new system event queue
  val eventQueue : EventQueue = new EventQueue


  // device and machine names are always upper case
  override def getName: String = super.getName.toUpperCase


  // Set up the master timer device - always present
  SimTimer.sim_timer_init() // set up some universal stuff.
  val simTimerDevice = new SimTimer(this)
  addDevice(simTimerDevice)
  val masterTimer = new SimTimerUnit(simTimerDevice, true)
  simTimerDevice.addUnit(masterTimer)
  // If there was no master timer already (true in this case) the timer will cause itself to do that. This
  // guarantees there's only one master timer, created here.


  Utils.outln(s"SIM: OS Tick:${SimTimer.sim_os_tick_hz}Hz\tIdle Rate:${SimTimer.sim_idle_rate_ms}ms\tClock Res:${SimTimer.sim_os_clock_resolution_ms}ms")

  /**
   * Show command for SHOW MACHINE
   */
  def showMachine() : Unit = {

    val sb = new StringBuilder
    sb.append(s"SIM: Simulated Machine: ${getName} : $description\n\r")
    sb.append(s"SIM: Available devices:\n\r")
    if(devices.isEmpty) sb.append(s"SIM: \t No devices.\n\r")
    devices.foreach(d => sb.append(s"SIM: \t ${d.getName}\tEna: ${d.isEnabled}\n\r"))

    Utils.outln(sb.toString())
  }


  def findDevice(deviceName:String) : Option[BasicDevice] = {
    devices.find(d => d.getName.equalsIgnoreCase(deviceName))
  }

  def findUnitDevice(deviceName:String) : Option[BasicUnit] = {
    var result: Option[BasicUnit] = None
    devices.foreach( d=> {
      d.getUnits.find( u => u.getName.equalsIgnoreCase(deviceName)) match {
        case None => {}
        case Some(x:BasicUnit) => result = Some(x)
        case null => throw new Exception("System check: Unknown findUnitDevice")
      }
    })

    result
  }

  def addDevice(device:BasicDevice) : Unit = {
    // TODO Name management
    device.createUnitOptions()
    device.init()
    device match {
      case device1: PortMappedDevice => getCPU.MMU.mapPortMappedDevice(device1)
      case _ =>
    }

    devices.append(device)
  }

  def removeDevice(deviceName:String) : Unit = {
    // TODO
  }

  def init() : Unit

  def getCPU: BasicCPU

  val breakpoints: mutable.HashSet[UInt] = new mutable.HashSet[UInt]()

  val memlogs: mutable.HashSet[UInt] = new mutable.HashSet[UInt]()

  /**
   * Add a breakpoint address to the list of breakpoints
   * @param address
   */
  def addBreak(address:UInt ) : Unit = {

    breakpoints += address
  }

  /**
   * Remove a breakpoint address from the list of breakpoints
   * @param address
   */
  def removeBreak(address:UInt) : Unit = {

    breakpoints -= address
  }

  /**
   * Clear (remove all) breakpoints
   */
  def clearBreaks():Unit = {

    breakpoints.clear()
  }


  /**
   * Check if the address has a breakpoint
   * @param address
   * @return true if there is a breakpoint, false otherwise
   */
  def checkBreak(address:UInt) : Boolean = {

    breakpoints(address)
  }

  def showBreaks(sb:StringBuilder):Unit = {
    breakpoints.foreach(b => {
      sb.append(f"${b.intValue}%08X\n\r")
    })
  }

  def addMemLog(address:UInt) : Unit = {
    memlogs += address
  }

  def clearMemLog():Unit = {
    memlogs.clear()
  }

  def remoteMemLog(address:UInt) : Unit = {
    memlogs -= address
  }

  def checkMemLog(address:UInt) : Boolean = {
    memlogs(address)
  }
  def showMemLogs(sb:StringBuilder):Unit = {
    memlogs.foreach(b => {
      sb.append(f"${b.intValue}%08X\n\r")
    })
  }

}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = List(new S100Machine)
}