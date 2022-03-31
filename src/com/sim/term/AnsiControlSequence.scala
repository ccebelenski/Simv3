package com.sim.term

class AnsiControlSequence(val command: Char, val parameters: List[String]) {
  /**
   * Gets the command character.
   *
   * @return The command character.
   */
  def getCommand: Char = command

  /**
   * Gets the parameters array.
   *
   * @return The parameters array.
   */
  def getParameters: List[String] = parameters

}
