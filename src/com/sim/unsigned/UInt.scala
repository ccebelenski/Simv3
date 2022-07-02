package com.sim.unsigned

import scala.annotation.switch

/**
 * Created by christophercebelenski on 7/18/16.
 */
class UInt(val intValue: Int) extends AnyVal with SmallUInt[UInt] {
  override def toUInt: UInt = this

  override def intRep: Int = intValue
}

object UInt {

  def MinValue: UInt = UInt(0)

  def MaxValue: UInt = UInt(~0)

  inline def apply(x: Int): UInt = {
    new UInt(x)
  }

  def unapply(x: UInt): Option[Int] = Some(x.intValue)
}
