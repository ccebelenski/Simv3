package com.sim.mux

import com.sim.Utils
import com.sim.device.{BasicDevice, MuxAware, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.UInt

import java.awt.event.{KeyEvent, KeyListener}
import java.net.{ServerSocket, Socket, SocketTimeoutException}
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.collection.mutable

/**
  * Psuedo device - System MUX - multiterminal telnet server.  Units are created dynamically as connections are made.
  * By convention the first connection is the system console or primary terminal - MUXA0.  Drivers written
  * to use the MUX, such as serial devices, should be aware that units are ephemeral and come and go as
  * connections are made and broken, and should respond appropriately.
  *
  * Multiple MUX devices may exist, depending on what is needed, but must run on different ports.
  *
  * Unlike other devices a simulator may use, the MUX isn't exposed directly to the simulator, but rather
  * through other simulated devices, such as terminal devices or serial ports.
  *
  * Unlike SIMH, serial I/O isn't supported at the moment.  This could be added here, or possibly with a
  * "serial mux" type of device.
  *
  * @param machine
  */
class MuxDevice(machine: AbstractMachine) extends BasicDevice(machine: AbstractMachine) {
  override val description: String = "System MUX - telnet handler"
  override val name = "MUX"
  var initComplete: Boolean = false
  var listenThread: Thread = _
  @volatile
  var socket: ServerSocket = _
  @volatile
  var executor: ExecutorService = _

  @volatile
  var clientCount: Int = 0
  var registeredDevice: Option[MuxAware] = None

  def portnum: Int = {
    val o = getValueOption("PORTNUM")
    if (o.isDefined) o.get
    else 8888
  }

  def maxClients: Int = {
    val o = getValueOption("MAXCLIENTS")
    if (o.isDefined) o.get else 1
  }

  def getTimeout: Int = {
    val o = getValueOption("TIMEOUT")
    if (o.isDefined) o.get else 5000

  }

  var listener: MUXListener = _

  /**
    * When init() completes, it will leave behind a thread for accepting connections on the port.
    * This thread will run for life of the simulator, and shouldn't need to be reset or recreated.
    */
  override def init(): Unit = {

    listener = new MUXListener(portnum, maxClients, this)
    Utils.outln(s"$getName: Starting MUX.  Listening on port $portnum")
    listenThread = new Thread(listener)
    listenThread.start()
    initComplete = true
    // Any registered device is left alone...
  }

  /**
    * Register a serial device for callback from the mux.  Existing units are not reconfigured.
    *
    * @param device
    * @return true is successful, false otherwise
    */
  def registerDevice(device: MuxAware): Boolean = {
    if (registeredDevice.isDefined) return false
    registeredDevice = Some(device)

    true
  }

  override def showCommand(sb: mutable.StringBuilder): Unit = {
    super.showCommand(sb)
    sb.append(s"$getName: Registered device: ")
    if (registeredDevice.isDefined) sb.append(s"${registeredDevice.get.asInstanceOf[BasicDevice].getName}")
    else sb.append("None")


  }

  override def createUnitOptions(): Unit = {
    unitOptions.append(ValueUnitOption("PORTNUM", "Set port to listen on.", value = 8400))
    unitOptions.append(ValueUnitOption("MAXCLIENTS", "Maximum # of clients that can connect", value = 1))
    unitOptions.append(ValueUnitOption("TIMEOUT", "Network timeout value.", value = 5000))

  }

  override def optionChanged(sb: mutable.StringBuilder): Unit = {
    sb.append(s"\n$getName: Options have changed, device will reload.")

    listenThread.interrupt()
    listenThread.join(10000)
    init() // re-init
  }

  override def handles(value: UInt): Boolean = false

}

/**
  * this is the listener thread class.  It waits on the socket and dispatches units to handle the connections.
  *
  * @param port
  * @param maxClients
  * @param device
  */
class MUXListener(val port: Int, val maxClients: Int, val device: MuxDevice) extends Runnable {

  var isShutdown = false

  def run(): Unit = {

    try {
      device.executor = Executors.newFixedThreadPool(maxClients)
      device.socket = new ServerSocket(port)
      device.socket.setSoTimeout(device.getTimeout)

      while (!device.listenThread.isInterrupted) {
        var s: Socket = null
        try {
          s = device.socket.accept()

          if (device.clientCount < maxClients) {
            device.clientCount += 1
            val muxUnit = new MuxUnit(device, s)
            muxUnit.callbackDevice = device.registeredDevice
            device.addUnit(muxUnit)
            device.executor.execute(muxUnit)
          } else {
            Utils.outln(s"\n\n${device.getName}: Max clients exceeded.")
            s.close()
          }
        } catch {
          case st: SocketTimeoutException =>
        }
      }
    } catch {
      case i: InterruptedException =>
      case t: Throwable =>
    } finally {
      //      Utils.outln(s"\n\n${device.getName}: Shutting down MUX server... ")
      device.socket.close()
      device.executor.shutdown()
      try {
        if (!device.executor.awaitTermination(5000, TimeUnit.MILLISECONDS)) {
          device.executor.shutdownNow()
        }
      } catch {
        case x: InterruptedException =>
          device.executor.shutdownNow()
      }
    }
  }
}

