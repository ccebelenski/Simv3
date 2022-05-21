package com.sim.term

import java.io.{IOException, Reader, StringReader}
import scala.collection.mutable
import scala.util.control.Breaks.*

class AnsiControlSequenceParser(val listener: AnsiControlSequenceListener) {
  /**
   * The multi-byte control sequence introducer.
   */
  private val MULTI_CSI = Array[Char](27, '[')

  /**
   * The single-byte control sequence introducer.
   */
  private val SINGLE_CSI = 155

  /**
   * The buffer of data from the last call to parse. This is
   * populated with data if an escape sequence is not complete.
   */
  private var buffer = new mutable.StringBuilder

  /**
   * Parses a control sequence.
   *
   * @param reader The character reader.
   * @throws IOException if an I/O error occurs.
   */
  @throws[IOException]
  private def parseControlSequence(reader: Reader): Unit = {
    var finishedSequence :Boolean= false
    val parameters:mutable.StringBuilder = new mutable.StringBuilder
    var character:Int  = 0
    breakable {
      while ( {
        character = reader.read
        character != -1
      }) {
        if ((character >= 'a'.toInt && character <= 'z'.toInt) || (character >= 'A'.toInt && character <= 'Z'.toInt)) {
          val array = {
            val t = parameters.toString.split(';')
            if(t.length == 1 && t.head == "") Array.empty[String] else t
          }

          val seq = new AnsiControlSequence(character.toChar, array.toList)
          listener.parsedControlSequence(seq)
          finishedSequence = true
          break

        }
        else parameters.append(character.toChar)
      }
    }
    if (!finishedSequence) { // not an ideal solution if they used the two byte CSI, but it's
      // easier and cleaner than keeping track of it
      buffer.append(SINGLE_CSI.toChar)
      buffer.append(parameters)
    }
  }


  /**
   * Parses characters from the specified character reader.
   *
   * @param reader The character reader.
   * @throws IOException if an I/O error occurs.
   */
  @throws[IOException]
  private def parse(reader: Reader): Unit = {
    var text = new mutable.StringBuilder
    var character = 0
    breakable {
      while ( {
        character = reader.read
        character != -1
      }) {
        var introducedControlSequence = false
        if (character == SINGLE_CSI) introducedControlSequence = true
        else if (character == MULTI_CSI(0)) {
          val nextCharacter = reader.read
          if (nextCharacter == -1) {
            buffer.append(character.toChar)
            break

          }
          else if (nextCharacter == MULTI_CSI(1)) introducedControlSequence = true
          else {
            text.append(character.toChar)
            text.append(nextCharacter.toChar)
          }
        }
        else text.append(character.toChar)
        if (introducedControlSequence) {
          if (text.nonEmpty) {
            listener.parsedString(text.toString)
            text = new mutable.StringBuilder
          }
          parseControlSequence(reader)
        }
      }
    }
    if (text.nonEmpty) listener.parsedString(text.toString)
  }

  /**
   * Parses the specified string.
   *
   * @param str The string to parse.
   */
  def parse(str: String): Unit = {
    val s =
      if (buffer.nonEmpty) {
        val t  = buffer.toString.concat(str)
        buffer = new mutable.StringBuilder
        t
      } else str
    val reader = new StringReader(s)
    try {
      try {
        parse(reader)
      }
      finally reader.close()
    }
    catch {
      case ex: IOException =>

      /* ignore */
    }
  }

}
