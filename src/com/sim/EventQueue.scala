package com.sim

import com.sim.device.BasicUnit

import scala.collection.mutable

class EventQueue {

  // Process entries on the event queue
  def processEvent():Unit  = {
    SimTimer.updateSimTime()
    if(EventQueue.clockQueue.isEmpty) {
      SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
      SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
      return
    }

    EventQueue.processingEvent = true

    while(EventQueue.clockQueue.nonEmpty && SimTimer.sim_interval <= 0){

      val unit: BasicUnit  = EventQueue.clockQueue.dequeue()
      unit.time = 0L
      if(EventQueue.clockQueue.nonEmpty)
        SimTimer.sim_interval = EventQueue.clockQueue.head.time
      else {
        SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
        SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
      }
      if(unit.usecs_remaining != 0) SimTimer.timerActivateAfter(unit,unit.usecs_remaining)
      else unit.completeAction()

    }
    if(EventQueue.clockQueue.isEmpty) {
      SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
      SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
    }

    EventQueue.processingEvent = false
  }

  // Activate (queue) event
  // Unit = unit, eventTime = relative timeout
  def activate(unit:BasicUnit, eventTime:Long) : Unit = {
    // TODO if(unit.isTimerUnit) timerActivate(unit, eventTime)
    //else {
    // _sim_activate
    if(unit.active) return
      // UPDATE_SIM_TIME
      SimTimer.updateSimTime()
    // Insert into priority queue
    EventQueue.clockQueue += unit
    // Ensure the next interval hits the top of the queue
    SimTimer.sim_interval = EventQueue.clockQueue.head.time
    //}
  }

  // queue event even if event already scheduled
  def activateAbs(unit:BasicUnit, eventTime:Long): Unit = {
    cancel(unit)
    activate(unit,eventTime)
  }

  // queue event even if event already scheduled but not before the specified time
  def activateNotBefore(unit:BasicUnit, rtime:Long) : Unit = {
    val urtime = rtime
    cancel(unit)
    val rtimenow = SimTimer.sim_rtime
    cancel(unit)
    if(0x80000000 <= urtime - rtimenow) activate(unit,0L)
    else activate(unit, urtime - rtimenow)
  }

  def activateAfter(unit:BasicUnit, usec_delay:Long) : Unit = {
    if(unit.active) return
    // TODO timerActivateAfter(unit, usec_delay)
  }

  def cancel(unit:BasicUnit) : Unit = {
    unit.cancel()
    if(EventQueue.clockQueue.isEmpty) return // Done
      SimTimer.updateSimTime()
    if(!unit.active) return
      unit.time = 0
    unit.usecs_remaining = 0
    if(EventQueue.clockQueue.nonEmpty) SimTimer.sim_interval = EventQueue.clockQueue.head.time
    else {
      SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
      SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
    }
  }

  private def _sim_activate_time(unit:BasicUnit): Long = {
    val qlist = EventQueue.getClockQueueList
    var accum:Long = 0L
    qlist.foreach( u => {
      if(u == SimTimer.internal_timer) {
        if(SimTimer.sim_interval > 0) accum = accum + SimTimer.sim_interval
      } else {
        accum = accum + u.time
      }
      if(unit == u) return accum + 1 + ((u.usecs_remaining * SimTimer.sim_timer_inst_per_sec()) /  1000000)
    })

    0
  }


  def sim_activate_time(unit:BasicUnit) : Long = {
    val x = SimTimer._sim_timer_activate_time(unit)
    if(x >= 0 )  x else _sim_activate_time(unit)
  }

}


object EventQueue {
  val clockQueue : mutable.PriorityQueue[BasicUnit] = new mutable.PriorityQueue[BasicUnit]().reverse
  var processingEvent: Boolean = false

  def getClockQueueList:List[BasicUnit] = {
    clockQueue.toList.sortBy(b => b.time)
  }
  val NOQUEUE_WAIT = 1000000L // Min check time
}