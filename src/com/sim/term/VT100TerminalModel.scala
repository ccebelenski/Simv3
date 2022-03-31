package com.sim.term

import java.awt.Color
import scala.util.control.Breaks._

object VT100TerminalModel {
  /**
   * The default number of columns.
   */
  private val DEFAULT_COLUMNS = 80

  /**
   * The default number of rows.
   */
  private val DEFAULT_ROWS = 24

  /**
   * The tab width in characters.
   */
  private val TAB_WIDTH = 8
}

class VT100TerminalModel(var columns: Int = VT100TerminalModel.DEFAULT_COLUMNS,
                         var rows: Int = VT100TerminalModel.DEFAULT_ROWS,
                         var bufferSize: Int = VT100TerminalModel.DEFAULT_ROWS) extends AbstractTerminalModel {

  if (columns < 0 || rows < 0 || bufferSize < 0) throw new IllegalArgumentException("Zero or positive values only allowed for columns, rows and buffer size.")
  if (bufferSize < rows) throw new IllegalArgumentException("The buffer is too small")
  private val cells: Array[Array[TerminalCell]] = Array.ofDim(bufferSize, columns)


  /**
   * The default foreground bold flag.
   */
  private val DEFAULT_FOREGROUND_BOLD = false

  /**
   * The default background bold flag.
   */
  private val DEFAULT_BACKGROUND_BOLD = false

  /**
   * The default foreground color.
   */
  private val DEFAULT_FOREGROUND_COLOR = 7

  /**
   * The default background color.
   */
  private val DEFAULT_BACKGROUND_COLOR = 0
  /**
   * The ANSI control sequence listener.
   */
  private val listener = new Vt100Listener

  /**
   * The ANSI control sequence parser.
   */
  private val parser = new AnsiControlSequenceParser(listener)

  /**
   * The current bell strategy.
   */
  private var bellStrategy = new BellStrategy

  /**
   * The cursor row.
   */
  private var cursorRow = 0

  /**
   * The cursor column.
   */
  private var cursorColumn = 0

  /**
   * The current foreground bold flag.
   */
  private var foregroundBold = DEFAULT_FOREGROUND_BOLD

  /**
   * The current background bold flag.
   */
  private var backgroundBold = DEFAULT_BACKGROUND_BOLD

  /**
   * The current foreground color.
   */
  private var foregroundColor = DEFAULT_FOREGROUND_COLOR

  /**
   * The current background color.
   */
  private var backgroundColor = DEFAULT_BACKGROUND_COLOR


  override def getCell(column: Int, row: Int): TerminalCell = {
    if (column < 0 || row < 0 || column >= columns || row >= bufferSize) throw new IndexOutOfBoundsException
    cells(row)(column)
  }

  override def setCell(column: Int, row: Int, cell: TerminalCell): Unit = {
    if (column < 0 || row < 0 || column >= columns || row >= bufferSize) {
      throw new IndexOutOfBoundsException
    }
    cells(row)(column) = cell
  }

  override def print(str: String): Unit = {
    if (str == null) throw new NullPointerException("str")
    parser.parse(str)
  }

  override def getColumns: Int = columns

  override def getRows: Int = rows

  override def getBufferSize: Int = bufferSize

  override def getBellStrategy: BellStrategy = bellStrategy

  override def setBellStrategy(strategy: BellStrategy): Unit = {
    if (strategy == null) throw new NullPointerException("strategy")
    this.bellStrategy = strategy
  }

  override def getDefaultBackgroundColor: Color = {
    val bg = DEFAULT_BACKGROUND_COLOR
    if (DEFAULT_BACKGROUND_BOLD) SGRColor.COLOR_BRIGHT(bg)
    else SGRColor.COLOR_NORMAL(bg)
  }

  override def getDefaultForegroundColor: Color = {
    val fg = DEFAULT_FOREGROUND_COLOR
    if (DEFAULT_FOREGROUND_BOLD) SGRColor.COLOR_BRIGHT(fg)
    else SGRColor.COLOR_NORMAL(fg)
  }


  /**
   * Gets the cursor row.
   *
   * @return The cursor row.
   */
  override def getCursorRow: Int = cursorRow

  /**
   * Sets the cursor row.
   *
   * @param row The cursor row.
   * @throws IllegalArgumentException if the row is out of the valid range.
   */
  override def setCursorRow(row: Int): Unit = {
    if (row < 0 || row >= bufferSize) throw new IllegalArgumentException("row out of range")
    cursorRow = row
  }

  /**
   * Gets the cursor column.
   *
   * @return The cursor column.
   */
  override def getCursorColumn: Int = cursorColumn

  /**
   * Sets the cursor column.
   *
   * @param column The cursor column.
   * @throws IllegalArgumentException if the column is out of the valid range.
   */
  override def setCursorColumn(column: Int): Unit = {
    if (column < 0 || column >= columns) throw new IllegalArgumentException("column out of range")
    cursorColumn = column
  }

  private class Vt100Listener extends AnsiControlSequenceListener {
    /**
     * The saved cursor row.
     */
    private var savedCursorRow = -1
    /**
     * The saved cursor column.
     */
    private var savedCursorColumn = -1

    override def parsedControlSequence(seq: AnsiControlSequence): Unit = {
      val command = seq.getCommand
      var parameters = seq.getParameters
      var n = 0
      command match {
        case 'A' | 'B' | 'C' | 'D' =>
          n = 1
          if (parameters.length == 1) n = parameters.head.toInt
          if (command == 'A') moveCursorUp(n)
          else if (command == 'B') moveCursorDown(n)
          else if (command == 'C') moveCursorForward(n)
          else if (command == 'D') moveCursorBack(n)

        case 'E' | 'F' =>
          n = 1
          if (parameters.length == 1) n = parameters.head.toInt
          if (command == 'E') moveCursorDown(n)
          else if (command == 'F') moveCursorUp(n)
          setCursorColumn(0)

        case 'G' =>
          if (parameters.length == 1) {
            n = parameters.head.toInt
            setCursorColumn(n - 1)
          }

        case 'H' | 'f' =>
          if (parameters.length == 2) {
            n = 1
            var m = 1
            if (parameters.head.nonEmpty) n = parameters.head.toInt
            if (parameters(1).nonEmpty) m = parameters(1).toInt
            setCursorRow(n - 1)
            setCursorColumn(m - 1)
          }

        case 'J' =>
          n = 0
          if (parameters.length == 1) n = parameters.head.toInt
          if (n == 0) {
            var row = cursorRow
            var column = cursorColumn
            while ( {
              row < rows
            }) {
              while ( {
                column < columns
              }) {
                cells(row)(column) = null
                column += 1
              }
              column = 0
              row += 1
            }
          }
          else if (n == 1) {
            var row = cursorRow
            var column = cursorColumn
            while ( {
              row >= 0
            }) {
              while ( {
                column >= 0
              }) {
                cells(row)(column) = null
                column -= 1
              }
              column = columns - 1
              row -= 1
            }
          }
          else if (n == 2) clear()

        case 'K' =>
          n = 0
          if (parameters.length == 1) n = Integer.parseInt(parameters.head)
          if (n == 0) for (c <- cursorColumn until columns) {
            cells(cursorRow)(c) = null
          }
          else if (n == 1) for (c <- cursorColumn to 0 by -1) {
            cells(cursorRow)(c) = null
          }
          else if (n == 2) for (column <- 0 until columns) {
            cells(cursorRow)(column) = null
          }

        case 'm' =>
          if (parameters.isEmpty) parameters = List("0")
          for (parameter <- parameters) {
            if (parameter == "0") {
              foregroundColor = DEFAULT_FOREGROUND_COLOR
              backgroundColor = DEFAULT_BACKGROUND_COLOR
              backgroundBold = DEFAULT_BACKGROUND_BOLD
              foregroundBold = DEFAULT_FOREGROUND_BOLD
            }
            else if (parameter == "2") {
              backgroundBold = true
              foregroundBold = true
            }
            else if (parameter == "22") {
              backgroundBold = false
              foregroundBold = false
            }
            else if ((parameter.startsWith("3") || parameter.startsWith("4")) && parameter.length == 2) {
              val color = parameter.substring(1).toInt
              if (parameter.startsWith("3")) foregroundColor = color
              else if (parameter.startsWith("4")) backgroundColor = color
            }
          }

        case 'u' =>
          if (savedCursorColumn != -1 && savedCursorRow != -1) {
            cursorColumn = savedCursorColumn
            cursorRow = savedCursorRow
          }

        case 's' =>
          savedCursorColumn = cursorColumn
          savedCursorRow = cursorRow

        case _ => // Nothing
      }
    }

    override def parsedString(str: String): Unit = {
      for (ch <- str.toCharArray) {
        breakable {
          ch match {
            case 0x00 =>
              break // continue is not supported

            case '\r' =>
              cursorColumn = 0
              break // continue is not supported

            case '\n' =>
              //cursorColumn = 0
              cursorRow += 1
              break // continue is not supported

            case '\t' =>
              while ( {
                ({
                  cursorColumn += 1
                  cursorColumn
                } % VT100TerminalModel.TAB_WIDTH) != 0
              }) {}
              break // continue is not supported

            case 127 | 8 =>
              if (cursorColumn > 0) cells(cursorRow)({
                cursorColumn -= 1
                cursorColumn
              }) = null
              break // continue is not supported

            case 7 =>
              bellStrategy.soundBell()
              break // continue is not supported

            case _ =>
          }
          if (cursorColumn >= columns) {
            cursorColumn = 0
            cursorRow += 1
          }
          while (cursorRow >= bufferSize -1) {
            //System.out.print(s"SCROLL:$cursorRow")
            for (i <- 1 until bufferSize) {
              System.arraycopy(cells(i), 0, cells(i - 1), 0, columns)
            }
            for (i <- 0 until columns) {
              cells(bufferSize - 1)(i) = null
            }
            cursorRow = cursorRow -  1
            //System.out.println(s":$cursorRow")
          }
          //System.out.println(s"OUTPUT:$cursorRow")
          val back = if (backgroundBold) SGRColor.COLOR_BRIGHT(backgroundColor)
          else SGRColor.COLOR_NORMAL(backgroundColor)
          val fore = if (foregroundBold) SGRColor.COLOR_BRIGHT(foregroundColor)
          else SGRColor.COLOR_NORMAL(foregroundColor)
          if(ch != '\n') cells(cursorRow)({
            cursorColumn += 1
            cursorColumn -1
          }) = TerminalCell(ch, back, fore)
        }
      }
    }
  }

}
