package com.sim.device

import com.sim.term.{Term, VT100TerminalModel}

import java.awt.{BorderLayout, Dimension}
import javax.swing.{JFrame, JRootPane, WindowConstants}

abstract class ConsoleUnit(device:BasicDevice) extends BasicUnit(device:BasicDevice) {

  val inputBuffer = new scala.collection.mutable.Queue[Byte]

  @volatile
  var inputCharacterWaiting:Boolean = false

   private val frame = new JFrame()

  //frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  private val t = new ConsoleTerminal(new VT100TerminalModel(81, 25, bufferSize = 25), this)
  private val d = new Dimension(t.getSize())

  frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  frame.setUndecorated(true)
  frame.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
  frame.setSize(d)
  frame.setPreferredSize(d)
  frame.setMinimumSize(d)
  frame.add(t)
  frame.add(BorderLayout.CENTER, t)
  frame.pack()
  frame.setTitle(getName)

  frame.setVisible(true)

  override def completeAction(): Unit = {
    if (inputCharacterWaiting) {
      device.machine.getCPU.keyboardInterrupt = true
    }

  }

  def getTerminal:ConsoleTerminal = t

  override def optionChanged(sb: StringBuilder): Unit = ???
}
