package com.sim.term

import java.awt.event.{KeyEvent, KeyListener}

//import akka.actor.ActorRef
import com.sim.term.cli.CLIMonitor
import javax.swing.JComponent

import scala.collection.mutable

class CharacterListener(val model: AbstractTerminalModel, parent: JComponent) extends KeyListener {

  private val cmdBuffer = new StringBuilder
  private var cmdPosition = 0

  private val recallBuffer = new mutable.ListBuffer[String]
  private var recallPosition = -1
  private var lastDirection: Int = 0


  private val ESC = 0x1b.toChar
  private val CSI = s"${ESC}["

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {


    if (CLIMonitor.acceptInput) {
      if (e.isActionKey) {
        e.getKeyCode match {

          case KeyEvent.VK_LEFT if cmdPosition > 0 =>
            // Output ANSI sequence to move cursor left, change our cmdPosition
            cmdPosition = cmdPosition - 1
            model.print(CSI + "D")
            parent.repaint()

          case KeyEvent.VK_RIGHT if cmdPosition != cmdBuffer.length() =>
            cmdPosition += 1
            model.print(CSI + "C")
            parent.repaint()

          case KeyEvent.VK_END if cmdBuffer.nonEmpty && (cmdPosition != cmdBuffer.length()) =>
            model.print(CSI + s"${cmdBuffer.length() - cmdPosition}C")
            cmdPosition = cmdBuffer.length()
            parent.repaint()

          case KeyEvent.VK_HOME if cmdPosition != 0 =>
            model.print(CSI + s"${cmdPosition}D")
            cmdPosition = 0
            parent.repaint()

          case KeyEvent.VK_UP if recallPosition != -1 =>
            model.print(CSI + s"${cmdPosition}D" + CSI + "K")
            cmdBuffer.clear()
            if (lastDirection == KeyEvent.VK_DOWN) recallPosition = recallPosition - 1
            //System.out.print(s"UP:$recallPosition:$lastDirection:")

            cmdBuffer.append(recallBuffer(recallPosition))
            recallPosition = Math.max(recallPosition - {
              if ((lastDirection == KeyEvent.VK_DOWN) || (lastDirection == 0)) 1 else 1
            }, 0)

            //System.out.println(s"$recallPosition")
            cmdPosition = cmdBuffer.length()
            model.print(cmdBuffer.toString)
            lastDirection = KeyEvent.VK_UP
            parent.repaint()

          case KeyEvent.VK_DOWN if recallPosition != (recallBuffer.length - 1) =>
            model.print(CSI + s"${cmdPosition}D" + CSI + "K")
            cmdBuffer.clear()
            //System.out.print(s"DOWN:$recallPosition:$lastDirection:")
            if (lastDirection == KeyEvent.VK_UP) recallPosition = recallPosition + 1
            recallPosition = Math.min(recallPosition + {
              if (lastDirection == KeyEvent.VK_UP) 1 else 1
            }, recallBuffer.length - 1)
            //System.out.println(s"$recallPosition")

            cmdBuffer.append(recallBuffer(recallPosition))
            cmdPosition = cmdBuffer.length()
            model.print(cmdBuffer.toString)
            lastDirection = KeyEvent.VK_DOWN
            parent.repaint()

          case _ => // Skip everything else
        }

      } else {
        e.getKeyChar match {
          case '\n' =>
            model.print("\r\n")
            parent.repaint()
            CLIMonitor.cmdLine = cmdBuffer.toString()
            if (CLIMonitor.cmdLine.nonEmpty) {
              recallBuffer.append(CLIMonitor.cmdLine)
              recallPosition = recallBuffer.length - 1
            }
            cmdPosition = 0
            cmdBuffer.clear()
            CLIMonitor.doNotify()
          case '\b' if cmdPosition > 0 =>
            //model.print("\b")
            cmdPosition = cmdPosition - 1
            cmdBuffer.deleteCharAt(cmdPosition)
            model.print(CSI + "D" + CSI + "K")
            model.print(cmdBuffer.substring(cmdPosition))
            model.print(CSI + s"${cmdBuffer.substring(cmdPosition).length}D")

            parent.repaint()
          case 0x03 =>
            model.print("^C\r\n")
            parent.repaint()
            cmdPosition = 0
            cmdBuffer.clear()
            CLIMonitor.cmdLine = cmdBuffer.toString()
            CLIMonitor.doNotify()
          case kc: Char if !kc.isControl && kc.toByte != -1 =>
            model.print(kc.toString)
            cmdBuffer.insert(cmdPosition, kc)
            cmdPosition += 1
            model.print(CSI + "K")
            //model.print(cmdBuffer.substring(cmdPosition -1 ))
            model.print(cmdBuffer.substring(cmdPosition))

            if (cmdPosition != cmdBuffer.length()) model.print(CSI + s"${cmdBuffer.substring(cmdPosition).length}D")

            parent.repaint()
          case _ => // Anything else we don't care
        }
      }
    }
  }

  override def keyReleased(e: KeyEvent): Unit = {}

}
