package com.sim.term

import java.awt.Color

object SGRColor {

  /**
   * An array of normal intensity colors.
   */
  val COLOR_NORMAL: Array[Color] =
    Array[Color](new Color(0, 0, 0),
      new Color(128, 0, 0),
      new Color(0, 128, 0),
      new Color(128, 128, 0),
      new Color(0, 0, 128),
      new Color(128, 0, 128),
      new Color(0, 128, 128),
      new Color(192, 192, 192))

  /**
   * An array of bright intensity colors.
   */
  val COLOR_BRIGHT: Array[Color] =
    Array[Color](new Color(128, 128, 128),
      new Color(255, 0, 0),
      new Color(0, 255, 0),
      new Color(255, 255, 0),
      new Color(0, 0, 255),
      new Color(0, 0, 255),
      new Color(255, 0, 255),
      new Color(0, 255, 255),
      new Color(255, 255, 255))
}
