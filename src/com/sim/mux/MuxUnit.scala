package com.sim.mux

import com.sim.Utils
import com.sim.device.{BasicDevice, BasicUnit, MuxAware}

import java.net.{Socket, SocketAddress}

class MuxUnit(device: MuxDevice, var socket: Socket) extends BasicUnit(device: BasicDevice) with Runnable {

  private val socketAddress: SocketAddress = socket.getRemoteSocketAddress
  private val outputStream = socket.getOutputStream
  private val inputStream = socket.getInputStream

  override val waitTime: Long = 0 // Doesn't matter, unit is not scheduled.

  // Device we should call back on when closing
  @volatile
  var callbackDevice: Option[MuxAware] = None

  @volatile
  var char: Int = 0

    def getTimeout: Int = {
    val o = getValueOption("TIMEOUT")
    if (o.isDefined) o.get else 5000

  }

  // Call this when unregistering a callback device - either we're shutting down the unit,
  // or it's being called from the Device itself to detach it, but keep the connection open.
  private def unregisterCallbackDevice() : Unit = {
    if(callbackDevice.isDefined) callbackDevice.get.MUXDetachCallback(this)
    callbackDevice = None
  }

  override def init(): Unit = {
    socket.setSoTimeout(getTimeout)
    Utils.out(s"\n\n$getName: Telnet connection from: $socketAddress\n\n")
    // Notify our registered device we are here.
    if(callbackDevice.isDefined) {
      callbackDevice.get.MUXAttachCallback(this)
    } else Utils.outln(s"$getName: Misconfiguration - No callback device for a MUX Unit.")

}


  // Called when options changed - this is only really going to be useful on init, since
  // most options can't be changed after the socket is open.
  override def optionChanged(sb:StringBuilder) : Unit = {

    // PORT isn't a unit option
    // MAXCLIENTS isn't a unit option
    // Timeout can change, but not for the unit in progress.
  }

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)

    sb.append(s"Connected to: $socketAddress\n")
    sb.append(s"Registered callback device: ")
    if(callbackDevice.isEmpty) sb.append(s"${callbackDevice.get.asInstanceOf[BasicDevice].getName}\n\r")
    else sb.append("None\n\r")
  }

  override def cancel(): Unit = {}

  override def completeAction(): Unit = {}

  def writeChar(char: Int): Unit = {
    if (socket.isConnected) {
      outputStream.write(char)
      //outputStream.flush()
    }
  }

  override def run(): Unit = {
    while (!socket.isClosed && char != -1 || !Thread.interrupted()) {
      try {

        char = inputStream.read()
        if(callbackDevice.isDefined) {
          while(!callbackDevice.get.checkDeviceReady) {
            Thread.sleep(50)
          }
          callbackDevice.get.muxCharacterInterrupt(this, char)
        }

      } catch {
        case t: Throwable =>
        //case i:InterruptedException =>
      }
      finally {
        if(socket.isClosed || char == -1 || Thread.interrupted()) {
          socket.close()
          Utils.out(s"\n\r\n\r$getName: Telnet session terminated.\n\r\n\r")
          device.removeUnit(this)
          device.asInstanceOf[MuxDevice].clientCount-=1
          // Perform our callback, if we need to.
          unregisterCallbackDevice()
          return
        }
      }
    }
  }
}
