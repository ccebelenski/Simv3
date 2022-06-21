package com.sim.unsigned

/**
 * Created by christophercebelenski on 7/18/16.
 */

case class UByte(override val byteValue: Byte) extends AnyVal with SmallUInt[UByte] {
  override def intValue(): Int = byteValue & 0xff
}

object UByte {
  def MinValue(): UByte = {UByte(0)}
  def MaxValue(): UByte  = {UByte(255.byteValue)}
}
