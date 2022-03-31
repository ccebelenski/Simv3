package com.sim

import com.sim.device.BasicUnit

class SimTimerUnit(override val device: SimTimer, val isCalibrated: Boolean = false) extends BasicUnit(device) {


  override val waitTime: Long = 0

  var rtc_ticks: Int = 0
  /* ticks */
  var rtc_hz = 0L
  /* tick rate */
  var rtc_last_hz = 0L
  /* prior tick rate */
  var rtc_rtime = 0L
  /* real time */
  var rtc_vtime = 0L
  /* virtual time */
  var rtc_gtime = 0L
  /* instruction time */
  var rtc_nxintv = 0L
  /* next interval */
  var rtc_based = 0L
  /* base delay */
  var rtc_currd = 0L
  /* current delay */
  var rtc_initd = 0L
  /* initial delay */
  var rtc_elapsed = 0
  /* sec since init */
  var rtc_calibrations = 0
  /* calibration count */
  var rtc_clock_skew_max = 0
  /* asynchronous max skew */
  var rtc_clock_start_gtime: Long = 0L
  /* reference instruction time for clock */
  var rtc_clock_tick_size = 0L
  /* 1/hz */
  var rtc_calib_initializations = 0
  /* Initialization Count */
  var rtc_calib_tick_time = 0
  /* ticks time */
  var rtc_calib_tick_time_tot = 0
  /* ticks time - total*/
  var rtc_calib_ticks_acked = 0
  /* ticks Acked */
  var rtc_calib_ticks_acked_tot = 0
  /* ticks Acked - total */
  var rtc_clock_ticks = 0
  /* ticks delivered since catchup base */
  var rtc_clock_ticks_tot = 0
  /* ticks delivered since catchup base - total */
  var rtc_clock_init_base_time = 0L
  /* reference time for clock initialization */
  var rtc_clock_tick_start_time = 0L
  /* reference time when ticking started */
  var rtc_clock_catchup_base_time = 0
  /* reference time for catchup ticks */
  var rtc_clock_catchup_ticks = 0
  /* Record of catchups */
  var rtc_clock_catchup_ticks_tot = 0
  /* Record of catchups - total */
  var rtc_clock_catchup_pending = false
  /* clock tick catchup pending */
  var rtc_clock_catchup_eligible = false
  /* clock tick catchup eligible */
  var rtc_clock_time_idled = 0L
  /* total time idled */
  var rtc_clock_time_idled_last = 0L
  /* total time idled */
  var rtc_clock_calib_skip_idle = 0
  /* Calibrations skipped due to idling */
  var rtc_clock_calib_gap2big = 0
  /* Calibrations skipped Gap Too Big */
  var sim_idle_cyc_ms = 0L
  /* Cycles per millisecond while not idling */

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def init(): Unit = {
    val time: Long = if (rtc_currd == 0) 1L else rtc_currd
    //841

    this.isTimerUnit = true

    rtc_clock_start_gtime = SimTimer.sim_time
    rtc_rtime = System.currentTimeMillis()

    rtc_nxintv = 1000
    rtc_ticks = 0
    rtc_last_hz = rtc_hz
    rtc_hz = 0
    rtc_based = time
    rtc_currd = time
    rtc_initd = time
    rtc_elapsed = 0
    rtc_calibrations = 0
    rtc_clock_ticks_tot += rtc_clock_ticks
    rtc_clock_ticks = 0
    rtc_calib_tick_time_tot += rtc_calib_tick_time
    rtc_calib_tick_time = 0
    rtc_clock_catchup_pending = false
    rtc_clock_catchup_eligible = false
    rtc_clock_catchup_ticks_tot += rtc_clock_catchup_ticks
    rtc_clock_catchup_ticks = 0
    rtc_calib_ticks_acked_tot += rtc_calib_ticks_acked
    rtc_calib_ticks_acked = 0
    rtc_calib_initializations += 1
    rtc_clock_init_base_time = System.currentTimeMillis()
    if (isCalibrated) rtcn_configure_calibrated_clock()
  }


  def sim_rtcn_calb(ticksper: Long): Long = {
    var new_rtime: Long = 0L
    var delta_rtime: Long = 0L
    var last_idle_pct: Long = 0
    var delta_vtime: Long = 0L
    var new_gtime: Long = 0L
    rtc_clock_tick_start_time = System.currentTimeMillis()
    rtc_last_hz = rtc_hz
    rtc_hz = ticksper
    rtcn_configure_calibrated_clock()
    if (ticksper != 0) {
      rtc_clock_tick_size = 1 / ticksper
      rtc_currd = sim_timer_inst_per_sec() / ticksper
    }

    if (ticksper == 0) /* running? */
      return 10000L
    //    if (sim_clock_unit == NULL) {                      /* Not using TIMER units? */
    //      rtc_clock_ticks += 1;
    //      rtc_calib_tick_time += rtc_clock_tick_size;
    //    }
    if (rtc_clock_catchup_pending) {
      /* catchup tick? */
      rtc_clock_catchup_ticks += 1 /* accumulating which were catchups */
      rtc_clock_catchup_pending = false
    }
    rtc_ticks = rtc_ticks + 1; /* count ticks */
    if (rtc_ticks < ticksper) /* 1 sec yet? */
      return rtc_currd
    rtc_ticks = 0; /* reset ticks */
    rtc_elapsed = rtc_elapsed + 1; /* count sec */
    //    if (!rtc_avail)                                         /* no timer? */
    //      return rtc_currd;
    if (!isCalibrated) {
      rtc_currd = sim_timer_inst_per_sec() / ticksper
      //      sim_debug (DBG_CAL, &sim_timer_dev, "calibrated calibrated tmr=%d against internal system tmr=%d, tickper=%d (result: %d)\n", tmr, sim_calb_tmr, ticksper, rtc_currd);
      return rtc_currd
    }

    new_rtime = System.currentTimeMillis() /* wall time */
    rtc_calibrations += 1 /* count calibrations */
    //    sim_debug (DBG_TRC, &sim_timer_dev, "sim_rtcn_calb(ticksper=%d, tmr=%d)\n", ticksper, tmr);
    //    if (new_rtime < rtc_rtime) {                       /* time running backwards? */
    //      /* This happens when the value returned by sim_os_msec wraps (as an uint32) */
    //      /* Wrapping will happen initially sometime before a simulator has been running */
    //      /* for 49 days approximately every 49 days thereafter. */
    //      ++rtc_clock_calib_backwards;                   /* Count statistic */
    //      sim_debug (DBG_CAL, &sim_timer_dev, "time running backwards - OldTime: %u, NewTime: %u, result: %d\n", rtc_rtime, new_rtime, rtc_currd);
    //      rtc_rtime = new_rtime;                         /* reset wall time */
    //      return rtc_currd;                              /* can't calibrate */
    //    }
    delta_rtime = new_rtime - rtc_rtime /* elapsed wtime */
    rtc_rtime = new_rtime /* adv wall time */
    rtc_vtime = rtc_vtime + 1000 /* adv sim time */
    if (delta_rtime > 30000) {
      /* gap too big? */
      /* This simulator process has somehow been suspended for a significant */
      /* amount of time.  This will certainly happen if the host system has  */
      /* slept or hibernated.  It also might happen when a simulator         */
      /* developer stops the simulator at a breakpoint (a process, not simh  */
      /* breakpoint).  To accomodate this, we set the calibration state to   */
      /* ignore what happened and proceed from here.                         */
      rtc_clock_calib_gap2big += 1 /* Count statistic */
      rtc_vtime = rtc_rtime /* sync virtual and real time */
      rtc_nxintv = 1000 /* reset next interval */
      rtc_gtime = SimTimer.sim_time /* save instruction time */
      //sim_debug (DBG_CAL, &sim_timer_dev, "gap too big: delta = %d - result: %d\n", delta_rtime, rtc_currd);
      return rtc_currd /* can't calibr */
    }
    if (delta_rtime == 0) /* avoid divide by zero  */
      last_idle_pct = 0 /* force calibration */
    else
      last_idle_pct = Math.min(100L, 100L * ((rtc_clock_time_idled - rtc_clock_time_idled_last) / delta_rtime))
    rtc_clock_time_idled_last = rtc_clock_time_idled
    if (last_idle_pct > (100 - SimTimer.sim_idle_calib_pct)) {
      rtc_rtime = new_rtime /* save wall time */
      rtc_vtime = rtc_vtime + 1000 /* adv sim time */
      rtc_gtime = SimTimer.sim_time /* save instruction time */
      rtc_clock_calib_skip_idle += 1
      //sim_debug (DBG_CAL, &sim_timer_dev, "skipping calibration due to idling (%d%%) - result: %d\n", last_idle_pct, rtc_currd)
      return rtc_currd /* avoid calibrating idle checks */
    }
    new_gtime = SimTimer.sim_time
    if ((last_idle_pct == 0) && (delta_rtime != 0))
      sim_idle_cyc_ms = (new_gtime - rtc_gtime) / delta_rtime
    //if (sim_asynch_timer) {
    /* An asynchronous clock, merely needs to divide the number of */
    /* instructions actually executed by the clock rate. */
    //  new_currd = (int32)((new_gtime - rtc_gtime)/ticksper)
    /* avoid excessive swings in the calibrated result */
    //  if (new_currd > 10*rtc_currd)              /* don't swing big too fast */
    //    new_currd = 10*rtc_currd;
    //  else
    //  if (new_currd < rtc_currd/10)          /* don't swing small too fast */
    //    new_currd = rtc_currd/10;
    //  rtc_currd = new_currd;
    //  rtc_gtime = new_gtime;                     /* save instruction time */
    //  sim_debug (DBG_CAL, &sim_timer_dev, "asynch calibration result: %d\n", rtc_currd);
    //  return rtc_currd;                          /* calibrated result */
    //}
    rtc_gtime = new_gtime; /* save instruction time */
    /* This self regulating algorithm depends directly on the assumption */
    /* that this routine is called back after processing the number of */
    /* instructions which was returned the last time it was called. */
    if (delta_rtime == 0) /* gap too small? */
      rtc_based = rtc_based * ticksper /* slew wide */
    else
      rtc_based = rtc_based * rtc_nxintv / delta_rtime /* new base rate */
    delta_vtime = rtc_vtime - rtc_rtime /* gap */
    if (delta_vtime > SimTimer.TMAX) /* limit gap */
      delta_vtime = SimTimer.TMAX
    else if (delta_vtime < -SimTimer.TMAX)
      delta_vtime = -SimTimer.TMAX
    rtc_nxintv = 1000 + delta_vtime /* next wtime */
    rtc_currd = (rtc_based * rtc_nxintv) / 1000 /* next delay */
    if (rtc_based <= 0) /* never negative or zero! */
      rtc_based = 1
    if (rtc_currd <= 0) /* never negative or zero! */
      rtc_currd = 1
    //sim_debug (DBG_CAL, &sim_timer_dev, "calibrated tmr=%d, tickper=%d (base=%d, nxintv=%u, result: %d)\n", tmr, ticksper, rtc_based, rtc_nxintv, rtc_currd);
    /* Adjust calibration for other timers which depend on this timer's calibration */
    device.getUnits.foreach(u => {
      val simunit = u.asInstanceOf[SimTimerUnit]
      if (simunit != this && simunit.rtc_hz != 0) {
        simunit.rtc_currd = (simunit.rtc_currd * ticksper) / simunit.rtc_hz
      }
    })
    //AIO_SET_INTERRUPT_LATENCY(rtc_currd * ticksper);   /* set interrrupt latency */
    rtc_currd
  }

  /* 
    This routine exists to assure that there is a single reliably calibrated 
    clock properly counting instruction execution relative to time.  The best 
    way to assure reliable calibration is to use a clock which ticks no 
    faster than the host system's clock.  This is optimal so that accurate 
    time measurements are taken.  If the simulated system doesn't have a 
    clock with an appropriate tick rate, an internal clock is run that meets 
    this requirement, 
   */
  def rtcn_configure_calibrated_clock(): Unit = {

    /* Look for a timer running slower than the host system clock */
    SimTimer.sim_internal_clock_tps = Math.min(SimTimer.CLK_TPS, SimTimer.sim_os_tick_hz)

    if (SimTimer.internal_timer == null && rtc_hz <= SimTimer.sim_os_tick_hz) {
      //sim_debug (DBG_CAL, &sim_timer_dev, "_rtcn_configure_calibrated_clock(newtmr=%d) - Cleaning up stopped timer %s support\n", newtmr, sim_uname(sim_clock_unit[sim_calb_tmr]));
      /* Migrate any coscheduled devices to the standard queue */
      /* with appropriate usecs_remaining reflecting their currently */
      /* scheduled firing time.  sim_process_event() will coschedule */
      /* appropriately. */
      /* temporarily restore prior hz to get correct remaining time */
      //rtc_hz = rtc_last_hz
      //          while (sim_clock_cosched_queue[sim_calb_tmr] != QUEUE_LIST_END) {
      //            UNIT *uptr = sim_clock_cosched_queue[sim_calb_tmr];
      //            double usecs_remaining = sim_timer_activate_time_usecs (uptr) - 1;

      //            _sim_coschedule_cancel (uptr);
      //            _sim_activate (uptr, 1);
      //            uptr->usecs_remaining = usecs_remaining;
      //          }
      //rtc_hz = 0 /* back to 0 */
      //          if (sim_clock_unit[sim_calb_tmr])
      //            sim_cancel (sim_clock_unit[sim_calb_tmr]);
      //          sim_cancel (&sim_timer_units[sim_calb_tmr]);

      /* Start the internal timer */

      //sim_debug(DBG_CAL | DBG_INT, & sim_timer_dev, "_rtcn_configure_calibrated_clock(newtmr=%d) - Starting Internal Calibrated Timer at %dHz\n", newtmr, sim_int_clk_tps);
      //SIM_INTERNAL_UNIT.action = & sim_timer_clock_tick_svc;
      //SIM_INTERNAL_UNIT.flags = UNIT_IDLE;
      //sim_register_internal_device(& sim_int_timer_dev); /* Register Internal timer device */
      SimTimer.internal_timer = this

      //sim_rtcn_init_unit(& SIM_INTERNAL_UNIT, (CLK_INIT * CLK_TPS) / sim_int_clk_tps, SIM_INTERNAL_CLK);
      this.completeAction()
      //SIM_INTERNAL_UNIT.action(& SIM_INTERNAL_UNIT); /* Force tick to activate timer */


      return
    }
    //if (sim_calb_tmr == SIM_NTIMERS) {      /* was old the internal timer? */
    //sim_debug (DBG_CAL|DBG_INT, &sim_timer_dev, "_rtcn_configure_calibrated_clock(newtmr=%d) - Stopping Internal Calibrated Timer, New Timer = %d (%dHz)\n", newtmr, tmr, rtc_hz);
    //rtc_initd = 0
    //rtc_hz = 0
    //sim_register_clock_unit_tmr (NULL, SIM_INTERNAL_CLK);
    //sim_cancel (&SIM_INTERNAL_UNIT);
    //sim_cancel (&sim_timer_units[SIM_NTIMERS]);
    //}
  }

  def sim_timer_inst_per_sec(): Long = {
    var inst_per_sec = 0L

    if (SimTimer.internal_timer == null) return inst_per_sec

    inst_per_sec = SimTimer.internal_timer.rtc_currd * SimTimer.internal_timer.rtc_hz

    if (0 == inst_per_sec)
      inst_per_sec = SimTimer.internal_timer.rtc_currd * SimTimer.sim_internal_clock_tps


    inst_per_sec
  }

  override def cancel(): Unit = ???

  override def completeAction(): Unit = {
    sim_rtcn_calb(SimTimer.sim_internal_clock_tps)
    device.machine.eventQueue.activateAfter(this, 1000000 / SimTimer.sim_internal_clock_tps)
  }

  override def showCommand(sb:StringBuilder): Unit = {
    super.showCommand(sb)
    sb.append(s"  Clock device is $getName ${if (this == SimTimer.internal_timer) "Internal Calibrated Timer"}\n")
    sb.append(s"${if (SimTimer.sim_asynch_timer) "Asynchronus" else if (rtc_hz != 0) "Calibrated" else "Uncalibrated"} Timer:\n")
    if (rtc_hz != 0) {
      sb.append(f"  Running at:                $rtc_hz%d Hz\n")
      sb.append(f"  Tick Size:                 $rtc_clock_tick_size%s\n")
      sb.append(f"  Ticks in current second:   $rtc_ticks%d\n")
    }
    sb.append(s"  Seconds Running:           $rtc_elapsed (%s)\n")
    if (this == SimTimer.internal_timer) {
      sb.append(s"  Calibration Opportunities: $rtc_calibrations\n")
      if (SimTimer.sim_idle_calib_pct != 0)
        sb.append(s"  Calib Skip Idle Thresh %%:  ${SimTimer.sim_idle_calib_pct}\n")
      if (rtc_clock_calib_skip_idle != 0)
        sb.append(s"  Calibs Skip While Idle:    $rtc_clock_calib_skip_idle\n")
      //if (rtc_clock_calib_backwards != 0)
      //sb.append( s"  Calibs Skip Backwards:     ${rtc_clock_calib_backwards}\n")
      if (rtc_clock_calib_gap2big != 0)
        sb.append(s"  Calibs Skip Gap Too Big:   $rtc_clock_calib_gap2big\n")
    }
    if (rtc_gtime != 0)
      sb.append(s"  Instruction Time:          $rtc_gtime\n")
    if ((!SimTimer.sim_asynch_timer) && (SimTimer.sim_throt_type == SimTimer.SIM_THROT_NONE)) {
      sb.append(s"  Real Time:                 $rtc_rtime\n")
      sb.append(s"  Virtual Time:              $rtc_vtime\n")
      sb.append(s"  Next Interval:             $rtc_nxintv\n")
      sb.append(s"  Base Tick Delay:           $rtc_based\n")
      sb.append(s"  Initial Insts Per Tick:    $rtc_initd\n")
    }
    sb.append(s"  Current Insts Per Tick:    $rtc_currd\n")
    sb.append(s"  Initializations:           $rtc_calib_initializations\n")
    sb.append(s"  Ticks:                     $rtc_clock_ticks\n")
    if (rtc_clock_ticks_tot + rtc_clock_ticks != rtc_clock_ticks)
      sb.append(s"  Total Ticks:               ${rtc_clock_ticks_tot + rtc_clock_ticks}\n")
    if (rtc_clock_skew_max != 0.0)
      sb.append(s"  Peak Clock Skew:           $rtc_clock_skew_max ${if (rtc_clock_skew_max < 0) "fast" else "slow"}\n")
    if (rtc_calib_ticks_acked != 0)
      sb.append(s"  Ticks Acked:               $rtc_calib_ticks_acked\n")
    if (rtc_calib_ticks_acked_tot + rtc_calib_ticks_acked != rtc_calib_ticks_acked)
      sb.append(s"  Total Ticks Acked:         ${rtc_calib_ticks_acked_tot + rtc_calib_ticks_acked}\n")
    if (rtc_calib_tick_time != 0)
      sb.append(s"  Tick Time:                 $rtc_calib_tick_time\n")
    if (rtc_calib_tick_time_tot + rtc_calib_tick_time != rtc_calib_tick_time)
      sb.append(s"  Total Tick Time:           ${rtc_calib_tick_time_tot + rtc_calib_tick_time}\n")
    if (rtc_clock_catchup_ticks != 0)
      sb.append(s"  Catchup Ticks Sched:       $rtc_clock_catchup_ticks\n")
    if (rtc_clock_catchup_ticks_tot + rtc_clock_catchup_ticks != rtc_clock_catchup_ticks)
      sb.append(s"  Total Catchup Ticks Sched: ${rtc_clock_catchup_ticks_tot + rtc_clock_catchup_ticks}\n")

    if (rtc_clock_init_base_time != 0) {
      sb.append(s"  Initialize Base Time:      $rtc_clock_init_base_time\n")
    }
    if (rtc_clock_tick_start_time != 0) {
      sb.append(s"  Tick Start Time:           $rtc_clock_tick_start_time\n")
    }
    sb.append(s"  Wall Clock Time Now:       ${System.currentTimeMillis()}\n")
    if (rtc_clock_catchup_eligible) {
      sb.append(s"  Catchup Tick Time:         ${rtc_clock_catchup_base_time + rtc_calib_tick_time}\n")
      sb.append(s"  Catchup Base Time:         $rtc_clock_catchup_base_time\n")
    }
    if (rtc_clock_time_idled != 0)
      sb.append(s"  Total Time Idled:          ${rtc_clock_time_idled / 1000}\n")
  }


}
