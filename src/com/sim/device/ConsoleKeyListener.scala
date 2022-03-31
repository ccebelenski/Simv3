package com.sim.device

import com.sim.term.AbstractTerminalModel

import java.awt.event.{KeyEvent, KeyListener}

class ConsoleKeyListener(val unit: ConsoleUnit) extends KeyListener {

  private val ESC: Byte = 0x1b

  override def keyTyped(e: KeyEvent): Unit = {
    if (!e.isActionKey && e.getKeyChar != '\n' && e.getKeyChar != '\r') {

      unit.inputBuffer.enqueue(e.getKeyChar.toByte)
      unit.inputCharacterWaiting = true
    }
  }


  override def keyPressed(e: KeyEvent): Unit = {
    if (e.isActionKey) {
      e.getKeyCode match {

        case KeyEvent.VK_ENTER =>
          unit.inputBuffer.enqueue(13)
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_LEFT =>
          // Output ANSI sequence to move cursor left
          // <ESC>[D
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('D')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_RIGHT =>
          // <ESC>[C
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('C')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_END =>
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('F')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_HOME =>
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('H')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_UP =>
          // <ESC>[A
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('A')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_DOWN =>
          // <ESC>[B
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('B')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_PAGE_DOWN =>
          // <ESC>[6~
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('6')
          unit.inputBuffer.enqueue('~')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_PAGE_UP =>
          // <ESC>[5~
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('5')
          unit.inputBuffer.enqueue('~')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_INSERT =>
          // <ESC>[2~
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('2')
          unit.inputBuffer.enqueue('~')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_DELETE =>
          // <ESC>[3~
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('3')
          unit.inputBuffer.enqueue('~')
          unit.inputCharacterWaiting = true


        case _ => // Skip everything else
      }

    } else {
      if (e.isControlDown && e.getKeyCode == 77) {
        unit.inputBuffer.enqueue('\r')
        unit.inputCharacterWaiting = true
      }
      else if (e.isControlDown && e.getKeyCode == 74) {
        unit.inputBuffer.enqueue('\n')
        unit.inputCharacterWaiting = true
      } else if (e.getKeyCode == KeyEvent.VK_ENTER) {
        unit.inputBuffer.enqueue('\r')
        unit.inputCharacterWaiting = true

      }
    }
    //    if (!e.isControlDown && e.getExtendedKeyCode == 10) {
    //      unit.inputBuffer.enqueue('\r')
    //      unit.inputCharacterWaiting = true
    //    } else if(e.isControlDown && e.getExtendedKeyCode == 10) {
    //      unit.inputBuffer.enqueue('\n')
    //      unit.inputCharacterWaiting = true
    //    }
  }

  override def keyReleased(e: KeyEvent): Unit = {}
}
