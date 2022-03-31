package com.sim

import com.sim.device.{BasicDevice, BasicUnit}
import com.sim.unsigned.UInt

import scala.collection.immutable.HashMap

/**
 * Created by christophercebelenski on 7/19/16.
 */
object Utils {

  def isalnum(c: Char): Boolean = {
    if ((c & 0x80) != 0) false else c.isLetterOrDigit
  }

  /** Radix independent input/output package
   * strtotv - general radix input routine
   * Inputs:
   * inptr   =       string to convert
   * radix   =       radix for input
   * Outputs:
   * value   =       converted value
   */
  def strtotv(inptr: String, radix: Int): Int = {
    var value: Int = 0
    var digit: Int = 0

    if ((radix < 2) || (radix > 36)) return 0
    val trimmed = inptr.trim.toUpperCase.toCharArray

    trimmed.foreach(c => {
      if (!isalnum(c)) return value
      if (c.isDigit) digit = c - '0'
      else if (radix <= 10) return value
      else digit = c + 10 - 'A'
      if (digit >= radix) return value
      value = (value * radix) + digit
    })
    value

  }

  import java.util.regex.Pattern

  def toBytes(filesize: String): Long = {
    var returnValue: Long = -1
    val patt = Pattern.compile("([\\d.]+)([GMK]B)", Pattern.CASE_INSENSITIVE)
    val matcher = patt.matcher(filesize)
    val powerMap: Map[String, Int] = HashMap[String, Int]("GB" -> 3, "MB" -> 2, "KB" -> 1)
    if (matcher.find) {
      val number = matcher.group(1)

      val pow: Int = powerMap(matcher.group(2).toUpperCase)
      var bytes = BigDecimal(number)
      bytes = bytes * BigDecimal(1024).pow(pow)
      returnValue = bytes.toLong
    }
    returnValue
  }


  @inline
  def outln(msg: String): Unit = {
    // quick hack to replace some basic escaped chars
   // msg.replace()
    if (Console.term != null) Console.term.println(msg) else
      System.out.println(msg)
  }

  @inline
  def out(msg: String): Unit = {
    if (Console.term != null) Console.term.print(msg) else
      System.out.print(msg)
  }

  @inline
  def outlnd(unit: BasicUnit, msg: String): Unit = {
    outlnd(unit.device, msg)
  }

  @inline
  def outlnd(device: BasicDevice, msg: String): Unit = {
    if (device.debug) outln(s"DEBUG: ${device.name} : $msg")
  }

  @inline
  def outd(unit: BasicUnit, msg: String): Unit = {
    outd(unit.device, msg)
  }

  @inline
  def outd(device: BasicDevice, msg: String): Unit = {
    if (device.debug) out(msg)
  }

  def formatBytes(bytes: Long, si: Boolean): String = {
    val unit = if (si) 1000 else 1024
    if (bytes < unit) return s"${bytes}B" // bytes + " B"
    val exp = (Math.log(bytes) / Math.log(unit)).toInt
    val pre = s"${(if (si) "kMGTPE" else "KMGTPE").charAt(exp - 1)}${if (si) "" else "i"}"
    //    val pre = (if (si) "kMGTPE" else "KMGTPE").charAt(exp - 1) + (if (si) "" else "i")
    f"${bytes / Math.pow(unit, exp)}%.1f ${pre}B"
  }

}



