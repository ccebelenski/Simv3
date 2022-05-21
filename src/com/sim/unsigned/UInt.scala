package com.sim.unsigned

import scala.annotation.switch

/**
 * Created by christophercebelenski on 7/18/16.
 */
class UInt(val intValue: Int) extends AnyVal with SmallUInt[UInt] {
  @inline
  override def toUInt: UInt = this

  @inline
  override def intRep: Int = intValue
}

object UInt {

  // Some common values
  private val zero = new UInt(0)
  private val one = new UInt(1)
  private val two = new UInt(2)
  private val three = new UInt(3)
  private val four = new UInt(4)
  private val five = new UInt(5)
  private val six = new UInt(6)
  private val seven = new UInt(7)
  private val eight = new UInt(8)
  private val nine = new UInt(9)
  private val ten = new UInt(10)
  private val eleven = new UInt(11)
  private val twelve = new UInt(12)
  private val thirteen = new UInt(13)
  private val fourteen = new UInt(14)
  private val fifteen = new UInt(15)
  private val sixteen = new UInt(16)
  private val sevf = new UInt(0x7f)
  private val ff = new UInt(0xff)
  private val ffff = new UInt(0xffff)


  def MinValue: UInt = zero

  def MaxValue: UInt = UInt(~0)

  def apply(x: Int): UInt = {
    (x: @switch) match {
      case 0x0 => zero
      case 0x1 => one
      case 0x2 => two
      case 0x3 => three
      case 0x4 => four
      case 0x5 => five
      case 0x6 => six
      case 0x7 => seven
      case 0x8 => eight
      case 0x9 => nine
      case 0xa => ten
      case 0xb => eleven
      case 0xc => twelve
      case 0xd => thirteen
      case 0xe => fourteen
      case 0xf => fifteen
      case 0x10 => sixteen
      case 0xff => ff
      case 0x7f => sevf
      case _ => new UInt(x)
    }
  }

  def unapply(x: UInt): Option[Int] = Some(x.intValue)
}
