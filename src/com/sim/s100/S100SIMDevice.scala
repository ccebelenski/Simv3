package com.sim.s100

import com.sim.Utils
import com.sim.cpu.Z80MMU
import com.sim.device.PortMappedDevice
import com.sim.unsigned.{UByte, UInt}
import com.sim.unsigned.ubyte2Int
import com.sim.unsigned.uint2int
import com.sim.unsigned.ushort2Int
import com.sim.unsigned.ubyte2uint

import java.util
import java.util.{Calendar, Date, GregorianCalendar}
import scala.annotation.switch
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * A pseudo device for communication between the S100 and the simulator
  * Sits on Port 0xfe
  */
class S100SIMDevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDevice(machine, mmu, ports) {
  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {

    action.toInt match {
      case 0xfe =>
        if (isWrite) simh_out(value)
        else simh_in(value)
      case _ => UByte(0)
    }
  }

  //Debug control
  debug = false

  override val description: String = "SIM Device"
  override val name = "SIM"

  override def init(): Unit = {}

  override def createUnitOptions(): Unit = {}

  override def optionChanged(sb: mutable.StringBuilder): Unit = {}

  private var lastCommand: Int = 0x00


  private val printTimeCmd = 0 //  0 print the current time in milliseconds
  private val startTimerCmd = 1 //  1 start a new timer on the top of the timer stack
  private val stopTimerCmd = 2 //  2 stop timer on top of timer stack and show time difference
  private val resetPTRCmd = 3 //  3 reset the PTR device
  private val attachPTRCmd = 4 //  4 attach the PTR device
  private val detachPTRCmd = 5 //  5 detach the PTR device
  private val getSIMHVersionCmd = 6 //  6 get the current version of the SIMH pseudo device
  private val getClockZSDOSCmd = 7 //  7 get the current time in ZSDOS format
  private val setClockZSDOSCmd = 8 //  8 set the current time in ZSDOS format
  private val getClockCPM3Cmd = 9 //  9 get the current time in CP/M 3 format
  private val setClockCPM3Cmd = 10 // 0x0a set the current time in CP/M 3 format
  private val getBankSelectCmd = 11 // 11 0x0b get the selected bank
  private val setBankSelectCmd = 12 // 12 0x0c set the selected bank
  private val getCommonCmd = 13 // 13 0x0d get the base address of the common memory segment
  private val resetSIMHInterfaceCmd = 14 // 14 0x0e reset the SIMH pseudo device
  private val showTimerCmd = 15 // 15 0x0f show time difference to timer on top of stack
  private val attachPTPCmd = 16 // 16 0x10 attach PTP to the file with name at beginning of CP/M command line
  private val detachPTPCmd = 17 // 17 0x11 detach PTP
  private val hasBankedMemoryCmd = 18 // 18 0x12 determines whether machine has banked memory
  private val setZ80CPUCmd = 19 // 19 0x13 set the CPU to a Z80
  private val set8080CPUCmd = 20 // 20 0x14 set the CPU to an 8080
  private val startTimerInterruptsCmd = 21 // 21 0x15 start timer interrupts
  private val stopTimerInterruptsCmd = 22 // 22 0x16 stop timer interrupts
  private val setTimerDeltaCmd = 23 // 23 set the timer interprivate val in which interrupts occur
  private val setTimerInterruptAdrCmd = 24 // 24 set the address to call by timer interrupts
  private val resetStopWatchCmd = 25 // 25 reset the millisecond stop watch
  private val readStopWatchCmd: Int = 26 // 26 read the millisecond stop watch
  private val SIMHSleepCmd = 27 // 27 let SIMH sleep for SIMHSleep microseconds
  private val getHostOSPathSeparatorCmd = 28 // 28 obtain the file path separator of the OS under which SIMH runs
  private val getHostFilenamesCmd = 29 // 29 perform wildcard expansion and obtain list of file names
  private val readURLCmd = 30 // 30 read the contents of an URL
  private val getCPUClockFrequency = 31 // 31 get the clock frequency of the CPU
  private val setCPUClockFrequency = 32 // 32 set the clock frequency of the CPU

  private val markTime = new util.Stack[Long]()
  /* timer stack                  */
  private val version: String = "SIMH004"

  private var urlPointer: Int = 0
  private var isInReadPhase = false

  /* SIMH pseudo device status registers                                                                          */
  /* ZSDOS clock definitions                                                                                      */
  private var ClockZSDOSDelta = 0L // delta between real clock and Altair clock
  private var setClockZSDOSPos = 0 // determines state for receiving address of parameter block
  private var setClockZSDOSAdr = 0 // address in M of 6 byte parameter block for setting time
  private var getClockZSDOSPos = 0 // determines state for sending clock information

  // CPM3 clock definitions                                                                                       
  private var ClockCPM3Delta = 0L // delta between real clock and Altair clock
  private var setClockCPM3Pos = 0 // determines state for receiving address of parameter block
  private var setClockCPM3Adr = 0 // address in M of 5 byte parameter block for setting time
  private var getClockCPM3Pos = 0 // determines state for sending clock information
  private var daysCPM3SinceOrg = 0L // days since 1 Jan 1978

  // stop watch and timer related                                                                                 
  private var stopWatchDelta = 0L // stores elapsed time of stop watch
  private var getStopWatchDeltaPos = 0 // determines the state for receiving stopWatchDelta
  private var stopWatchNow = 0L // stores starting time of stop watch

  // miscellaneous                                                                                                
  private var versionPos = 0 // determines state for sending device identifier
  private val lastCPMStatus = 0 // result of last attachCPM command

  private var getCommonPos = 0 // determines state for sending the 'common' register

  // timer interrupt related
  private var timerInterruptHandler = UInt(0x0fc00) // default address of interrupt handling routine
  private var setTimerInterruptAdrPos = 0 // determines state for receiving timerInterruptHandler
  private var timerDelta = 100 // interrupt every 100 ms
  private var setTimerDeltaPos = 0 // determines state for receiving timerDelta

  private var currentTime: TM = _
  private var currentTimeValid = false

  /*  Z80 or 8080 programs communicate with the SIMH pseudo device via port 0xfe.
        The following principles apply:
    1)  For commands that do not require parameters and do not return results
        ld  a,<cmd>
        out (0feh),a
        Special case is the reset command which needs to be send 128 times to make
        sure that the internal state is properly reset.
    2)  For commands that require parameters and do not return results
        ld  a,<cmd>
        out (0feh),a
        ld  a,<p1>
        out (0feh),a
        ld  a,<p2>
        out (0feh),a
        ...
        Note: The calling program must send all parameter bytes. Otherwise
        the pseudo device is left in an undefined state.
    3)  For commands that do not require parameters and return results
        ld  a,<cmd>
        out (0feh),a
        in  a,(0feh)    ; <A> contains first byte of result
        in  a,(0feh)    ; <A> contains second byte of result
        ...
        Note: The calling program must request all bytes of the result. Otherwise
        the pseudo device is left in an undefined state.
    4)  For commands that do require parameters and return results
        ld  a,<cmd>
        out (0feh),a
        ld  a,<p1>
        out (0feh),a
        ld  a,<p2>
        out (0feh),a
        ...             ; send all parameters
        in  a,(0feh)    ; <A> contains first byte of result
        in  a,(0feh)    ; <A> contains second byte of result
        ...
*/


  private def toBCD(x: Int): Int = {
    (x / 10) * 16 + (x % 10)
  }

  private def fromBCD(x: Int): Int = {
    10 * ((0xf0 & x) >> 4) + (0x0f & x)
  }


  // Returns epoch in seconds
  private def mkCPM3Origin(): Long = {
    new GregorianCalendar(77, 11, 31, 0, 0, 0).getTimeInMillis / 1000

  }

  private var newTime: TM = _

  /* setClockZSDOSAdr points to 6 byte block in M: YY MM DD HH MM SS in BCD notation */
  def setClockZSDOS(): Unit = {
    val mmu = machine.getCPU.MMU
    val c = new GregorianCalendar()
    newTime = new TM()

    val year = fromBCD(mmu.get8(setClockZSDOSAdr))
    newTime.tm_year = if (year < 50) year + 100 else year
    newTime.tm_mon = fromBCD(mmu.get8(setClockZSDOSAdr + 1)) - 1
    newTime.tm_mday = fromBCD(mmu.get8(setClockZSDOSAdr + 2))
    newTime.tm_hour = fromBCD(mmu.get8(setClockZSDOSAdr + 3))
    newTime.tm_min = fromBCD(mmu.get8(setClockZSDOSAdr + 4))
    newTime.tm_sec = fromBCD(mmu.get8(setClockZSDOSAdr + 5))
    c.set(newTime.tm_year, newTime.tm_mon, newTime.tm_mday, newTime.tm_hour, newTime.tm_min, newTime.tm_sec)
    ClockZSDOSDelta = (c.getTimeInMillis - new Date().getTime) / 1000 // Convert to seconds
  }

  private def localDate(sec: Long): TM = {
    val t = new TM()
    val c = new GregorianCalendar()
    c.setTimeInMillis(sec * 1000)

    t.tm_year = c.get(Calendar.YEAR)
    t.tm_mon = c.get(Calendar.MONTH)
    t.tm_mday = c.get(Calendar.DAY_OF_MONTH)
    t.tm_hour = c.get(Calendar.HOUR)
    t.tm_min = c.get(Calendar.MINUTE)
    t.tm_sec = c.get(Calendar.SECOND)
    t
  }

  private def mktime(tm: TM): Long = {
    val c = new GregorianCalendar()
    c.set(Calendar.YEAR, tm.tm_year)
    c.set(Calendar.MONTH, tm.tm_mon)
    c.set(Calendar.DAY_OF_MONTH, tm.tm_mday)
    c.set(Calendar.HOUR, tm.tm_hour)
    c.set(Calendar.MINUTE, tm.tm_min)
    c.set(Calendar.SECOND, tm.tm_sec)
    c.getTimeInMillis
  }

  /* setClockCPM3Adr points to 5 byte block in M:
      0 - 1 int16:    days since 31 Dec 77
          2 BCD byte: HH
          3 BCD byte: MM
          4 BCD byte: SS                              */
  private def setClockCPM3(): Unit = {
    val mmu = machine.getCPU.MMU
    val targetSeconds = mkCPM3Origin() +
      ((mmu.get8(setClockCPM3Adr) + mmu.get8(setClockCPM3Adr + 1) * 256) * 86400) +
      fromBCD(mmu.get8(setClockCPM3Adr + 2)) * 3600 +
      fromBCD(mmu.get8(setClockCPM3Adr + 3)) * 60 +
      fromBCD(mmu.get8(setClockCPM3Adr + 4))
    // compute target year, month and day and replace hour, minute and second fields
    val targetDate = localDate(targetSeconds)

    targetDate.tm_hour = fromBCD(mmu.get8(setClockCPM3Adr + 2))
    targetDate.tm_min = fromBCD(mmu.get8(setClockCPM3Adr + 3))
    targetDate.tm_sec = fromBCD(mmu.get8(setClockCPM3Adr + 4))
    ClockCPM3Delta = mktime(targetDate) - new Date().getTime
  }

  private def simh_out(data: UByte): UByte = {
    if(data != 27 && data != 14) Utils.outlnd(this,s"Write 0xfe - $data lastCommand = $lastCommand")

    lastCommand match {
      case `readURLCmd` =>
        // Not Supported
        lastCommand = 0
        return UByte(0x00)

      case `setClockZSDOSCmd` =>
        if (setClockZSDOSPos == 0) {
          setClockZSDOSAdr = data
          setClockZSDOSPos = 1
        } else {
          setClockZSDOSAdr |= (data << 8)
          setClockZSDOS()
          setClockZSDOSPos = 0
          lastCommand = 0
        }
        return UByte(0x00)

      case `setClockCPM3Cmd` =>
        if (setClockCPM3Pos == 0) {
          setClockCPM3Adr = data
          setClockCPM3Pos = 1
        } else {
          setClockCPM3Adr |= (data << 8)
          setClockCPM3()
          setClockCPM3Pos = 0
          lastCommand = 0
        }
        return UByte(0x00)

      case `setCPUClockFrequency` =>
        // Nothing, not supported right now
        lastCommand = 0
        return UByte(0x00)

      case `setBankSelectCmd` =>
        val bnk = data & machine.getCPU.MMU.BANKMASK.intValue
        Utils.outlnd(this,s"setBankSelectCMD: $bnk")
        if (machine.getCPU.isBanked)
          machine.getCPU.MMU.selectBank(bnk)
        lastCommand = 0
        return UByte(0x00)

      case `setTimerDeltaCmd` =>
        if (setTimerDeltaPos == 0) {
          timerDelta = data
          setTimerDeltaPos = 1
        } else {
          timerDelta |= (data << 8)
          setTimerDeltaPos = 0
          lastCommand = 0
          if (timerDelta == 0) {
            timerDelta = 100 // 100 ms default timer delta
          }
        }
        return UByte(0x00)

      case `setTimerInterruptAdrCmd` =>
        if (setTimerInterruptAdrPos == 0) {
          timerInterruptHandler = data
          setTimerInterruptAdrPos = 1
        } else {
          timerInterruptHandler |= (data << 8)
          setTimerInterruptAdrPos = 0
          lastCommand = 0
        }
        return UByte(0x00)

      case _ => /* lastCommand not yet set */
      // ignored
    }
    lastCommand = data.toInt
    data.toInt match {
      case `readURLCmd` =>
        urlPointer = 0
        isInReadPhase = false

      case `getHostFilenamesCmd` => /* list files of host file directory */
      // Not supported

      case SIMHSleepCmd =>
        Thread.sleep(0, 500) // Sleep 500ns

      case `printTimeCmd` => /* print time */
        val t = new Date().getTime
        Utils.outln(s"SIM: ${machine.getCPU.PC} Current time in milliseconds = $t")

      case `startTimerCmd` => /* create a new timer on top of stack */
        markTime.push(new Date().getTime)

      case `stopTimerCmd` => /* stop timer on top of stack and show time difference */
        if (!markTime.empty()) {
          val delta = new Date().getTime - markTime.pop()
          Utils.outln(s"SIM: Timer stopped. Elapsed time in milliseconds = $delta.")
        } else
          Utils.outln(s"SIM: No timer active.")

      case `resetPTRCmd` => /* reset ptr device */
      // Not supported

      case `attachPTRCmd` => /* attach ptr to the file with name at beginning of CP/M command line */
      // Not supported

      case `detachPTRCmd` => /* detach ptr */
      // Not supported

      case `getSIMHVersionCmd` =>
        versionPos = 0

      case `getClockZSDOSCmd` =>
        var now = new Date().getTime / 1000
        now += ClockZSDOSDelta
        currentTime = localDate(now)
        currentTimeValid = true
        getClockZSDOSPos = 0

      case `setClockZSDOSCmd` =>
        setClockZSDOSPos = 0;

      case `getClockCPM3Cmd` =>
        var now = new Date().getTime / 1000
        now += ClockCPM3Delta
        currentTime = localDate(now)
        currentTimeValid = true
        daysCPM3SinceOrg = (now - mkCPM3Origin()) / 86400
        getClockCPM3Pos = 0

      case `setClockCPM3Cmd` =>
        setClockCPM3Pos = 0

      case `getCommonCmd` =>
        getCommonPos = 0

      case `getCPUClockFrequency` =>
      // Nothing, not supported

      case `setCPUClockFrequency` =>
      // Nothing, not supported

      case `getBankSelectCmd` =>
      case `setBankSelectCmd`  =>
      case `hasBankedMemoryCmd` =>
      case `getHostOSPathSeparatorCmd` =>

      case `resetSIMHInterfaceCmd` =>
        lastCommand = 0

      case `showTimerCmd` => /* show time difference to timer on top of stack */
        if (!markTime.empty()) {
          val delta = new Date().getTime - markTime.peek()
          Utils.outln(s"SIM: Timer running. Elapsed in milliseconds = $delta.")
        } else
          Utils.outln("SIM: No timer active.")

      case `attachPTPCmd` => /* attach ptp to the file with name at beginning of CP/M command line */
      // Not supported

      case `detachPTPCmd` => /* detach ptp */
      // Not supported

      case `setZ80CPUCmd` =>
      // Not supported

      case `set8080CPUCmd` =>
      // Not Supported

      case `startTimerInterruptsCmd` =>
      // Not implemented for now

      case `stopTimerInterruptsCmd` =>
      // Not implemented for now

      case `setTimerDeltaCmd` =>
        setTimerDeltaPos = 0

      case `setTimerInterruptAdrCmd` =>
        setTimerInterruptAdrPos = 0

      case `resetStopWatchCmd` =>
        stopWatchNow = new Date().getTime

      case `readStopWatchCmd` =>
        getStopWatchDeltaPos = 0
        stopWatchDelta = new Date().getTime - stopWatchNow

      case _ =>
      // Unknown command
    }

    UByte(0x00)
  }

  private def simh_in(byte: UByte): UByte = {
    Utils.outlnd(this,s"Read 0xfe - $byte lastCommand = $lastCommand")
    var result = 0

    lastCommand  match {
      case `readURLCmd` =>
        // Not supported
        lastCommand = 0

      case `getHostFilenamesCmd` =>
      // Not implemented right now

      case `attachPTRCmd` =>
        result = lastCPMStatus
        lastCommand = 0

      case `attachPTPCmd` =>
        result = lastCPMStatus
        lastCommand = 0

      case `getClockZSDOSCmd` =>
        if (currentTimeValid)
          getClockZSDOSPos match {

            case 0 =>
              result = toBCD({
                if (currentTime.tm_year > 99) currentTime.tm_year - 100 else currentTime.tm_year
              })
              getClockZSDOSPos = 1

            case 1 =>
              result = toBCD(currentTime.tm_mon + 1)
              getClockZSDOSPos = 2

            case 2 =>
              result = toBCD(currentTime.tm_mday)
              getClockZSDOSPos = 3

            case 3 =>
              result = toBCD(currentTime.tm_hour)
              getClockZSDOSPos = 4

            case 4 =>
              result = toBCD(currentTime.tm_min)
              getClockZSDOSPos = 5

            case 5 =>
              result = toBCD(currentTime.tm_sec)
              getClockZSDOSPos = 0
              lastCommand = 0
          }
        else
          result = 0
        getClockZSDOSPos = 0
        lastCommand = 0

      case `getClockCPM3Cmd` =>
        if (currentTimeValid)
          getClockCPM3Pos match {
            case 0 =>
              result = (daysCPM3SinceOrg & 0xff).intValue()
              getClockCPM3Pos = 1

            case 1 =>
              result = ((daysCPM3SinceOrg >> 8) & 0xff).intValue()
              getClockCPM3Pos = 2

            case 2 =>
              result = toBCD(currentTime.tm_hour)
              getClockCPM3Pos = 3

            case 3 =>
              result = toBCD(currentTime.tm_min)
              getClockCPM3Pos = 4

            case 4 =>
              result = toBCD(currentTime.tm_sec)
              getClockCPM3Pos = 0
              lastCommand = 0
          }
        else {
          result = 0
          getClockCPM3Pos = 0
          lastCommand = 0
        }


      case `getSIMHVersionCmd` =>
        if (versionPos != version.length) {
          result = version.charAt(versionPos)
          versionPos += 1
        }
        else {
          result = 0
          versionPos = 0
          lastCommand = 0
        }


      case `getBankSelectCmd` =>
        if (machine.getCPU.isBanked)
          result = machine.getCPU.MMU.getBank
        else result = 0
        Utils.outlnd(this, s"getBankSelectCmd: $result")
        lastCommand = 0

      case `getCommonCmd` =>
        if (getCommonPos == 0) {
          result = machine.getCPU.MMU.COMMON & 0xff
          getCommonPos = 1
        } else {
          result = (machine.getCPU.MMU.COMMON >> 8) & 0xff
          getCommonPos = 0
          lastCommand = 0
        }

      case `getCPUClockFrequency` =>
      // not supported

      case `hasBankedMemoryCmd` =>
        result = if (machine.getCPU.isBanked) machine.getCPU.MMU.MAXBANKS.intValue else 0
        Utils.outlnd(this,s"hasBankedMemoryCmd: $result")
        lastCommand = 0

      case `readStopWatchCmd` =>
        if (getStopWatchDeltaPos == 0) {
          result = (stopWatchDelta & 0xff).intValue
          getStopWatchDeltaPos = 1
        } else {
          result = ((stopWatchDelta >> 8) & 0xff).intValue
          getStopWatchDeltaPos = 0
          lastCommand = 0
        }

      case `getHostOSPathSeparatorCmd` =>
        result = '/'

      case _ =>
        result = 0
        lastCommand = 0
    }
    Utils.outlnd(this,s"Read 0xfe - result: ${result.byteValue()}")
    UByte(result.byteValue())
  }
}

class TM {
  var tm_year: Int = 0
  var tm_mon: Int = 0
  var tm_mday: Int = 0
  var tm_hour: Int = 0
  var tm_min: Int = 0
  var tm_sec: Int = 0
}