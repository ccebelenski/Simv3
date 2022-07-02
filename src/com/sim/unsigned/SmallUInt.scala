package com.sim.unsigned

trait SmallUInt[U <: Unsigned[U, UInt, Int]] extends Any with Unsigned[U, UInt, Int] {
  def intRep: Int = intValue()

  override def toByte: Byte = {intRep.toByte}

  override def toShort: Short = {intRep.toShort}

  @inline
  inline override def toInt: Int = intRep

  override def toLong: Long = intRep & 0xffffffffL

  override def toFloat: Float = intRep.toFloat

  override def toDouble: Double = intRep.toDouble

  override def toChar: Char = intRep.toChar

  inline def toUByte: UByte = UByte(intRep.toByte)

  inline def toUShort: UShort = UShort(intRep.toShort)

  def toUInt: UInt = UInt(intRep)

  inline def toULong: ULong = ULong(intRep & 0xffffffffL)

  override def byteValue(): Byte = intRep.toByte

  override def shortValue(): Short = intRep.toShort

  override def intValue(): Int

  override def longValue(): Long = intRep & 0xffffffffL

  override def floatValue(): Float = (intRep & 0xffffffffL).toFloat

  override def doubleValue(): Double = (intRep & 0xffffffffL).toDouble

  inline def +(x: Int)(implicit d: DummyImplicit): Int = this.toInt + x

  inline def -(x: Int)(implicit d: DummyImplicit): Int = this.toInt - x

  inline def *(x: Int)(implicit d: DummyImplicit): Int = this.toInt * x

  inline def /(x: Int)(implicit d: DummyImplicit): Int = this.toInt / x

  inline def %(x: Int)(implicit d: DummyImplicit): Int = this.toInt % x

  inline def &(x: Int)(implicit d: DummyImplicit): Int = this.toInt & x

  inline def |(x: Int)(implicit d: DummyImplicit): Int = this.toInt | x

  inline def ^(x: Int)(implicit d: DummyImplicit): Int = this.toInt ^ x

  inline def +(x: Long)(implicit d: DummyImplicit): Long = this.toLong + x

  inline def -(x: Long)(implicit d: DummyImplicit): Long = this.toLong - x

  inline def *(x: Long)(implicit d: DummyImplicit): Long = this.toLong * x

  inline def /(x: Long)(implicit d: DummyImplicit): Long = this.toLong / x

  inline def %(x: Long)(implicit d: DummyImplicit): Long = this.toLong % x

  inline def &(x: Long)(implicit d: DummyImplicit): Long = this.toLong & x

  inline def |(x: Long)(implicit d: DummyImplicit): Long = this.toLong | x

  inline def ^(x: Long)(implicit d: DummyImplicit): Long = this.toLong ^ x

  inline def +(x: UByte): UInt = this + x.toUInt

  inline def -(x: UByte): UInt = this - x.toUInt

  inline def *(x: UByte): UInt = this * x.toUInt

  inline def /(x: UByte): UInt = this / x.toUInt

  inline def %(x: UByte): UInt = this % x.toUInt

  inline def &(x: UByte): UInt = this & x.toUInt

  inline def |(x: UByte): UInt = this | x.toUInt

  inline def ^(x: UByte): UInt = this ^ x.toUInt

  inline def <(x: UByte): Boolean = this < x.toUInt

  inline def >(x: UByte): Boolean = this > x.toUInt

  inline def <=(x: UByte): Boolean = this <= x.toUInt

  inline def >=(x: UByte): Boolean = this >= x.toUInt

  inline def +(x: UShort): UInt = this + x.toUInt

  inline def -(x: UShort): UInt = this - x.toUInt

  inline def *(x: UShort): UInt = this * x.toUInt

  inline def /(x: UShort): UInt = this / x.toUInt

  inline def %(x: UShort): UInt = this % x.toUInt

  inline def &(x: UShort): UInt = this & x.toUInt

  inline def |(x: UShort): UInt = this | x.toUInt

  inline def ^(x: UShort): UInt = this ^ x.toUInt

  inline def <(x: UShort): Boolean = this < x.toUInt

  inline def >(x: UShort): Boolean = this > x.toUInt

  inline def <=(x: UShort): Boolean = this <= x.toUInt

  inline def >=(x: UShort): Boolean = this >= x.toUInt

  inline def +(x: ULong): ULong = this.toULong + x

  inline def -(x: ULong): ULong = this.toULong - x

  inline def *(x: ULong): ULong = this.toULong * x

  inline def /(x: ULong): ULong = this.toULong / x

  inline def %(x: ULong): ULong = this.toULong % x

  inline def &(x: ULong): ULong = this.toULong & x

  inline def |(x: ULong): ULong = this.toULong | x

  inline def ^(x: ULong): ULong = this.toULong ^ x

  inline def <(x: ULong): Boolean = this.toULong < x

  inline def >(x: ULong): Boolean = this.toULong > x

  inline def <=(x: ULong): Boolean = this.toULong <= x

  inline def >=(x: ULong): Boolean = this.toULong >= x

  inline def +(x: UInt): UInt = UInt(intRep + x.intRep)

  inline def -(x: UInt): UInt = UInt(intRep - x.intRep)

  inline def *(x: UInt): UInt = UInt(intRep * x.intRep)

  inline def /(x: UInt): UInt = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n / m
    UInt(r.toInt)
  }

  inline def %(x: UInt): UInt = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n % m
    UInt(r.toInt)
  }

  inline def unary_+ : UInt = this.toUInt

  inline def unary_- : UInt = UInt(-intRep) // maybe just -intRep ??

  // Equality comparison to UInt is baked in

  def ==(x: Int)(implicit d: DummyImplicit): Boolean = intValue == x

  def ==(x: Long)(implicit d: DummyImplicit): Boolean = longValue == x

  // def ==(x: UInt) = longValue == x.longValue
  def ==(x: ULong): Boolean = {longValue == x.longValue}

  def ==(x: Float): Boolean = floatValue == x

  def ==(x: Double): Boolean = doubleValue == x

  def !=(x: Int)(implicit d: DummyImplicit): Boolean = intValue != x

  def !=(x: Long)(implicit d: DummyImplicit): Boolean = longValue != x

  // def !=(x: UInt) = longValue != x.longValue
  def !=(x: ULong): Boolean = longValue != x.longValue

  def !=(x: Float): Boolean = floatValue != x

  def !=(x: Double): Boolean = doubleValue != x

  /*
  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any) = x match {
    case x: SmallUInt[_] => this.toInt == x.intRep
    case x: ULong => this.toULong == x
    case x: Number => this.longValue == x.longValue && x.longValue >= 0
    case _ => false
  }
  def canEqual(x: Any) = x match {
    case _: SmallUInt[_] => true
    case _: ULong => true
    case _: Number => true
    case _ => false
  }
  */

  private def rot(x: Int) = x + Int.MinValue

  inline def <(x: UInt): Boolean = rot(intRep) < rot(x.intRep)

  inline def >(x: UInt): Boolean = rot(intRep) > rot(x.intRep)

  inline def <=(x: UInt): Boolean = rot(intRep) <= rot(x.intRep)

  inline def >=(x: UInt): Boolean = rot(intRep) >= rot(x.intRep)

  inline def &(x: UInt): UInt = UInt(intRep & x.intRep)

  inline def |(x: UInt): UInt = UInt(intRep | x.intRep)

  inline def ^(x: UInt): UInt = UInt(intRep ^ x.intRep)

  inline def unary_~ : UInt = {UInt(~intRep)}

  inline def <<(x: Int)(implicit d: DummyImplicit): UInt = UInt(intRep << x)

  inline def <<(x: Long)(implicit d: DummyImplicit): UInt = UInt(intRep << x.intValue)

  inline def <<(x: UInt): UInt = UInt(intRep << (x.toInt & 0x1f))

  inline def <<(x: ULong): UInt = UInt(intRep << (x.toLong & 0x1f).intValue)

  inline def >>(x: Int)(implicit d: DummyImplicit): UInt = UInt(intRep >>> x)

  inline def >>(x: Long)(implicit d: DummyImplicit): UInt = UInt(intRep >>> x.intValue)

  inline def >>(x: UInt): UInt = UInt(intRep >>> (x.toInt & 0x1f))

  inline def >>(x: ULong): UInt = UInt(intRep >>> (x.toLong & 0x1f).intValue)

  def >>>(x: Int)(implicit d: DummyImplicit): UInt = UInt(intRep >>> x)

  def >>>(x: Long)(implicit d: DummyImplicit): UInt = UInt(intRep >>> x.intValue)

  def >>>(x: UInt): UInt = UInt(intRep >>> (x.toInt & 0x1f))

  def >>>(x: ULong): UInt = UInt(intRep >>> (x.toLong & 0x1f).intValue)

  override def toString: String = (intRep & 0xffffffffL).toString

  inline def +(x: java.lang.String): String = this.toString + x

  def toHexString: String = (intRep & 0xffffffffL).toHexString

  def toOctalString: String = (intRep & 0xffffffffL).toOctalString

  def toBinaryString: String = (intRep & 0xffffffffL).toBinaryString
}