package com.sim.device

import com.sim.Utils

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) extends Ordered[BasicUnit] with SupportsOptions {

  var time: Long = 0L
  var usecs_remaining : Long = 0L

  // How long before the schedule timer runs this unit. (simulates device time)
  val waitTime:Long


  // Unit Number - First unit is typically 0, then 1, etc.
  var unitNumber:Int = 0

  // Is this unit a timer unit?
  var isTimerUnit:Boolean = false

  private var enabled:Boolean = false
  var active: Boolean = false // Is the unit currently active? (processing event)


  val dn = s"$getName:"

  def showCommand(sb:StringBuilder): Unit = {

    sb.append(s"$dn  Unit $getName Active: $active\n\r")


    sb.append(s"${dn}Unit options:\n\r")
    unitOptions.foreach{uo => {
      uo.showOption(sb)
    }}
  }

  // device and machine names are always upper case
  def getName: String = device.getName + unitNumber

  // Action is cancelled
  def cancel(): Unit

  // Complete an action (event queue)
  def completeAction(): Unit


  // Called when the options have changed for a unit.  This allows dynamic reloading when appropriate
  // Default is to do nothing.
  def optionsChanged(): Unit = {}

  def init():Unit

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"UNIT: Unit $getName Enabled: $state")
  }

  def isEnabled: Boolean = enabled

  override def compare(that: BasicUnit): Int = this.time.compareTo(that.time)

  override def compareTo(that: BasicUnit): Int = this.time.compareTo(that.time)

}
