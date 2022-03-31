package com.sim.term

abstract class AnsiControlSequenceListener {

  /**
   * Called when a control sequence has been parsed.
   *
   * @param seq The control sequence.
   */
  def parsedControlSequence(seq: AnsiControlSequence): Unit

  /**
   * Called when a string has been parsed.
   *
   * @param str The string.
   */
  def parsedString(str: String): Unit

}
