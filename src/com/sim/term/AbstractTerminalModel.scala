package com.sim.term

import java.awt.Color

abstract class AbstractTerminalModel {

  def getBellStrategy: BellStrategy

  def setBellStrategy(strategy: BellStrategy): Unit

  /**
   * Clears the terminal.
   */
  def clear(): Unit = {
    val rows = getRows
    val columns = getColumns
    for (column <- 0 until columns) {
      for (row <- 0 until rows) {
        setCell(column, row, null)
      }
    }
  }
  /**
   * Moves the cursor back n characters.
   *
   * @param n The number of characters.
   * @throws IllegalArgumentException if n is not positive.
   */
  def moveCursorBack(n: Int): Unit = {
    assert(n >=0, "Must be positive")
    var cursorColumn = getCursorColumn - n
    if (cursorColumn < 0) cursorColumn = 0
    setCursorColumn(cursorColumn)
  }

  /**
   * Moves the cursor forward n characters.
   *
   * @param n The number of characters.
   * @throws IllegalArgumentException if n is not positive.
   */
  def moveCursorForward(n: Int): Unit = {
    assert(n >= 0, "Must be Positive")
    val columns = getColumns
    var cursorColumn = getCursorColumn + n
    if (cursorColumn >= columns) cursorColumn = columns - 1
    setCursorColumn(cursorColumn)
  }


  /**
   * Moves the cursor down n characters.
   *
   * @param n The number of characters.
   * @throws IllegalArgumentException if n is not positive.
   */
  def moveCursorDown(n: Int): Unit = {
    assert(n >= 0, "Must be positive")
    val bufferSize = getBufferSize
    var cursorRow = getCursorRow + n
    if (cursorRow >= bufferSize) cursorRow = bufferSize - 1
    setCursorRow(cursorRow)
  }

  /**
   * Moves the cursor up n characters.
   *
   * @param n The number of characters.
   * @throws IllegalArgumentException if n is not positive.
   */
  def moveCursorUp(n: Int): Unit = {
    assert(n>=0,"Must be positive")
    var cursorRow = getCursorRow - n
    if (cursorRow < 0) cursorRow = 0
    setCursorRow(cursorRow)
  }

  /**
   * Sets a cell.
   *
   * @param column The column.
   * @param row    The row.
   * @param cell   The cell.
   * @throws IndexOutOfBoundsException if the column and/or row number(s) are
   *                                   out of bounds.
   */
  def setCell(column: Int, row: Int, cell: TerminalCell): Unit

  /**
   * Gets a cell.
   *
   * @param column The column.
   * @param row    The row.
   * @return The cell.
   * @throws IndexOutOfBoundsException if the column and/or row number(s) are
   *                                   out of bounds.
   */
  def getCell(column: Int, row: Int): TerminalCell

  /**
   * Prints the specified string to the terminal at the cursor position,
   * interpreting any escape sequences/special ASCII codes the model may
   * support. Lines will be wrapped if necessary.
   *
   * @param str The string to print.
   * @throws NullPointerException if the string is { @code null}.
   */
  def print(str: String): Unit

  /**
   * Gets the number of columns.
   *
   * @return The number of columns.
   */
  def getColumns: Int

  /**
   * Gets the number of rows.
   *
   * @return The number of rows.
   */
  def getRows: Int

  /**
   * Gets the buffer size.
   *
   * @return The buffer size.
   */
  def getBufferSize: Int

  /**
   * Gets the cursor row.
   *
   * @return The cursor row.
   */
  def getCursorRow: Int

  /**
   * Sets the cursor row.
   *
   * @param row The cursor row.
   * @throws IllegalArgumentException if the row is out of the valid range.
   */
  def setCursorRow(row: Int): Unit

  /**
   * Gets the cursor column.
   *
   * @return The cursor column.
   */
  def getCursorColumn: Int

  /**
   * Sets the cursor column.
   *
   * @param column The cursor column.
   * @throws IllegalArgumentException if the column is out of the valid range.
   */
  def setCursorColumn(column: Int): Unit

  /**
   * Gets the default background color.
   *
   * @return The default background color.
   */
  def getDefaultBackgroundColor: Color

  /**
   * Gets the default foreground color.
   *
   * @return The default foreground color.
   */
  def getDefaultForegroundColor: Color

  // Set when scroll occurs, reset by user
  var scrolled: Boolean = false
  
}
