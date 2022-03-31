package com.sim.term

import java.awt._
import java.awt.event.{AdjustmentEvent, AdjustmentListener}

import javax.swing._

class Term(val model: AbstractTerminalModel) extends JComponent {

  private var lastRepaintTime:Long = 0L

  //The cell width in pixels.
  private val CELL_WIDTH = 10

  //The cell height in pixels.
  private val CELL_HEIGHT = 17
  // The font.
  val font2: Font = new Font("Monospaced", Font.PLAIN, CELL_HEIGHT)

  /**
   * The scroll bar.
   */
  private var scrollBar: Option[JScrollBar] = None

  /**
   * Initializes the terminal.
   */
  setDoubleBuffered(true)

  addKeyListener(new CharacterListener(model, this))
  setFocusTraversalKeysEnabled(false)
  setFocusable(true)
  requestFocusInWindow()
  val d = new Dimension(model.getColumns * CELL_WIDTH  , model.getRows * CELL_HEIGHT)
  setSize(d)
  setMinimumSize(d)
  setPreferredSize(d)
  setLayout(new BorderLayout(0, 0))
  private val rows = model.getRows
  private val bufferSize = model.getBufferSize
  if (bufferSize > rows) {
    scrollBar = Some(new JScrollBar(Adjustable.VERTICAL, 0, rows, 0, bufferSize + 1))
    scrollBar.get.addAdjustmentListener(new AdjustmentListener() {
      override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
        repaint()
      }
    })

    add(scrollBar.get, BorderLayout.LINE_END)
  }
  //repaint()

  /**
   * Prints a line to the terminal.
   *
   * @param str The string to print.
   */
  def println(str: String): Unit = {
    if (str != null) print(str.concat("\r\n"))
  }

  /**
   * Prints a string to the terminal.
   *
   * @param str The string to print.
   */
  def print(str: String): Unit = {
    model.print(str)
    // Refresh no faster than 30fps
    if(System.currentTimeMillis() > (lastRepaintTime + 33)  ) {
      repaint()
      lastRepaintTime = System.currentTimeMillis()
    }
  }

  override def paint(g: Graphics): Unit = {
    g.setFont(font2)
    val width = model.getColumns
    val height = model.getBufferSize
    g.setColor(model.getDefaultBackgroundColor)
    g.fillRect(0, 0, width * CELL_WIDTH, height * CELL_HEIGHT)
    val start = scrollBar match {
      case None => 0
      case Some(x) => x.getValue
    }
    for (y <- start until height) {
      for (x <- 0 until width) {
        var cell = model.getCell(x, y)
        val cursorHere = model.getCursorRow == y && model.getCursorColumn == x
        if (cursorHere && cell == null) cell = TerminalCell(' ', model.getDefaultBackgroundColor, model.getDefaultForegroundColor)
        if (cell != null) {
          val px = x * CELL_WIDTH
          val py = (y - start) * CELL_HEIGHT
          g.setColor(if (cursorHere) cell.foregroundColor
          else cell.backgroundColor)
          g.fillRect(px, py, CELL_WIDTH, CELL_HEIGHT - 3)
          g.setColor(if (cursorHere) cell.backgroundColor
          else cell.foregroundColor)
          g.drawChars(Array[Char](cell.character), 0, 1, px, py + CELL_HEIGHT - 5)
        }
      }
    }
  }


}

object Term {
  // Main method for testing
  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val t = new Term(new VT100TerminalModel(80, 24, 50))
    val d = new Dimension(t.getWidth + 15, t.getHeight + 40)
    frame.setPreferredSize(d)
    frame.setMinimumSize(d)
    frame.add(t)
    frame.pack()
    frame.setTitle("TEST")
    frame.setVisible(true)

    t.println("Test")
    Thread.sleep(4000)
    t.print("Test2")
  }
}