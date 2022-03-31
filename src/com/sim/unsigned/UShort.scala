package com.sim.unsigned

/**
 * Created by christophercebelenski on 7/18/16.
 */

case class UShort(override val shortValue: Short) extends AnyVal with SmallUInt[UShort] {
  override def intValue(): Int = shortValue & 0xffff
}

object UShort {
  def MinValue(): UShort = UShort(0)

  def MaxValue(): UShort = UShort(~0)
}
