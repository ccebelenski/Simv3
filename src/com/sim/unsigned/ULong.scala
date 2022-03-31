package com.sim.unsigned

/**
 * Created by christophercebelenski on 7/18/16.
 */

case class ULong(override val longValue: Long) extends AnyVal with Unsigned[ULong, ULong, Long] {
  private[unsigned] def rep = longValue

  def toUByte(): UByte = UByte((rep & 0xffffffffL).toByte)

  def toUShort(): UShort = UShort((rep & 0xffffffffL).toShort)

  def toUInt(): UInt = UInt((rep & 0xffffffffL).toInt)

  def toULong(): ULong = this

  override def toByte(): Byte = (rep & 0x7fffffffffffffffL).toByte

  override def toChar(): Char = (rep & 0x7fffffffffffffffL).toChar

  override def toShort(): Short = (rep & 0x7fffffffffffffffL).toShort

  override def toInt(): Int = (rep & 0x7fffffffffffffffL).toInt

  override def toLong(): Long = rep

  override def toFloat(): Float = (rep & 0x7fffffffffffffffL).toFloat

  override def toDouble(): Double = (rep >>> 1).toDouble * 2.+(rep & 1L)

  // override def intValue = rep
  override def byteValue(): Byte = toByte()

  override def shortValue(): Short = toShort()

  override def intValue: Int = toInt()

  override def floatValue(): Float = toFloat()

  override def doubleValue(): Double = toDouble()

  def +(x: Int)(implicit d: DummyImplicit): Long = this.toLong + x

  def -(x: Int)(implicit d: DummyImplicit): Long = this.toLong - x

  def *(x: Int)(implicit d: DummyImplicit): Long = this.toLong * x

  def /(x: Int)(implicit d: DummyImplicit): Long = this.toLong / x

  def %(x: Int)(implicit d: DummyImplicit): Long = this.toLong % x

  def &(x: Int)(implicit d: DummyImplicit): Long = this.toLong & x

  def ^(x: Int)(implicit d: DummyImplicit): Long = this.toLong ^ x

  def |(x: Int)(implicit d: DummyImplicit): Long = this.toLong | x

  def +(x: Long)(implicit d: DummyImplicit): Long = this.toLong + x

  def -(x: Long)(implicit d: DummyImplicit): Long = this.toLong - x

  def *(x: Long)(implicit d: DummyImplicit): Long = this.toLong * x

  def /(x: Long)(implicit d: DummyImplicit): Long = this.toLong / x

  def %(x: Long)(implicit d: DummyImplicit): Long = this.toLong % x

  def &(x: Long)(implicit d: DummyImplicit): Long = this.toLong & x

  def ^(x: Long)(implicit d: DummyImplicit): Long = this.toLong ^ x

  def |(x: Long)(implicit d: DummyImplicit): Long = this.toLong | x

  def +(x: UByte): ULong = this + x.toULong()

  def -(x: UByte): ULong = this - x.toULong()

  def *(x: UByte): ULong = this * x.toULong()

  def /(x: UByte): ULong = this / x.toULong()

  def %(x: UByte): ULong = this % x.toULong()

  def &(x: UByte): ULong = this & x.toULong()

  def ^(x: UByte): ULong = this ^ x.toULong()

  def |(x: UByte): ULong = this | x.toULong()

  def <(x: UByte): Boolean = this < x.toULong()

  def >(x: UByte): Boolean = this > x.toULong()

  def <=(x: UByte): Boolean = this <= x.toULong()

  def >=(x: UByte): Boolean = this >= x.toULong()

  def +(x: UShort): ULong = this + x.toULong()

  def -(x: UShort): ULong = this - x.toULong()

  def *(x: UShort): ULong = this * x.toULong()

  def /(x: UShort): ULong = this / x.toULong()

  def %(x: UShort): ULong = this % x.toULong()

  def &(x: UShort): ULong = this & x.toULong()

  def ^(x: UShort): ULong = this ^ x.toULong()

  def |(x: UShort): ULong = this | x.toULong()

  def <(x: UShort): Boolean = this < x.toULong()

  def >(x: UShort): Boolean = this > x.toULong()

  def <=(x: UShort): Boolean = this <= x.toULong()

  def >=(x: UShort): Boolean = this >= x.toULong()

  def +(x: UInt): ULong = this + x.toULong()

  def -(x: UInt): ULong = this - x.toULong()

  def *(x: UInt): ULong = this * x.toULong()

  def /(x: UInt): ULong = this / x.toULong()

  def %(x: UInt): ULong = this % x.toULong()

  def &(x: UInt): ULong = this & x.toULong()

  def ^(x: UInt): ULong = this ^ x.toULong()

  def |(x: UInt): ULong = this | x.toULong()

  def <(x: UInt): Boolean = this < x.toULong()

  def >(x: UInt): Boolean = this > x.toULong()

  def <=(x: UInt): Boolean = this <= x.toULong()

  def >=(x: UInt): Boolean = this >= x.toULong()

  def +(x: ULong): ULong = ULong(rep + x.rep)

  def -(x: ULong): ULong = ULong(rep - x.rep)

  def *(x: ULong): ULong = ULong(rep * x.rep)

  private def rot(x: Long) = x + Long.MinValue

  def /(x: ULong): ULong = {
    val n = rep
    val d = x.rep

    if (true) return {
      if (d < 0) {
        if (this < x)
          ULong(0L)
        else
          ULong(1L)
      }
      else {
        val q = ((n >>> 1) / d) << 1
        val r = n - q * d
        if (ULong(r) >= x)
          ULong(q + 1)
        else
          ULong(q)
      }
    }

    val t = d >> 63
    val n1 = n & ~t
    val a = n1 >>> 1
    val b = a / d
    val q0 = b << 1
    val r = n - q0 * d
    val q = q0 + (if (ULong(r) >= x) 1L else 0L)
    ULong(q)
  }

  def %(x: ULong): ULong = {
    val n = rep
    val d = x.rep

    val t = d >> 63
    val n1 = n & ~t
    val a = n1 >>> 1
    val b = a / d
    val q0 = b << 1
    val r = n - q0 * d
    // val q = q0 + (if (ULong(r) >= ULong(d)) 1 else 0)
    ULong(r)
  }

  override def toString(): String =
    if (rep >= 0L)
      rep.toString
    else if (rep == (1 << 63))
      rep.toString.tail
    else
      (~(rep - 1)).toString

  def toHexString(): String = rep.toHexString

  def toOctalString(): String = rep.toOctalString

  def toBinaryString(): String = rep.toBinaryString

  // Equality comparison to ULong is baked in

  def ==(x: Int)(implicit d: DummyImplicit): Boolean = intValue == x

  def ==(x: Long)(implicit d: DummyImplicit): Boolean = longValue == x

  def ==(x: UInt): Boolean = longValue == x.longValue

  // def ==(x: ULong) = longValue == x.longValue
  def ==(x: Float): Boolean = floatValue == x

  def ==(x: Double): Boolean = doubleValue == x

  def !=(x: Int)(implicit d: DummyImplicit): Boolean = intValue != x

  def !=(x: Long)(implicit d: DummyImplicit): Boolean = longValue != x

  def !=(x: UInt): Boolean = longValue != x.longValue

  // def !=(x: ULong) = longValue != x.longValue
  def !=(x: Float): Boolean = floatValue != x

  def !=(x: Double): Boolean = doubleValue != x

  /*
  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any) = x match {
    case x: UInt => this == x.toULong
    case x: ULong => this.toLong == x.toLong
    case x: Int => this.toInt == x && x >= 0
    case x: Long => this.toLong == x && x >= 0
    case x: Number => this.longValue == x.longValue
    case _ => false
  }
  override def canEqual(x: Any) = x match {
    case _: UInt => true
    case _: ULong => true
    case _: Number => true
    case _ => false
  }
  */

  // Here, compare to Int
  // def ==(x: Int) = rep == x && rep >= 0
  // def !=(x: Int) = rep != x || rep < 0

  def <(x: ULong): Boolean = rot(rep) < rot(x.rep)

  def >(x: ULong): Boolean = rot(rep) > rot(x.rep)

  def <=(x: ULong): Boolean = rot(rep) <= rot(x.rep)

  def >=(x: ULong): Boolean = rot(rep) >= rot(x.rep)

  def +(x: java.lang.String): String = this.toString + x

  def &(x: ULong): ULong = ULong(rep & x.rep)

  def |(x: ULong): ULong = ULong(rep | x.rep)

  def ^(x: ULong): ULong = ULong(rep ^ x.rep)

  def <<(x: Int)(implicit d: DummyImplicit): ULong = ULong(rep << x)

  def <<(x: Long)(implicit d: DummyImplicit): ULong = ULong(rep << x)

  def <<(x: UInt): ULong = ULong(rep << (x.intRep & 0x3f))

  def <<(x: ULong): ULong = ULong(rep << (x.rep & 0x3f))

  def >>(x: Int)(implicit d: DummyImplicit): ULong = ULong(rep >>> x)

  def >>(x: Long)(implicit d: DummyImplicit): ULong = ULong(rep >>> x)

  def >>(x: UInt): ULong = ULong(rep >>> (x.intRep & 0x3f))

  def >>(x: ULong): ULong = ULong(rep >>> (x.rep & 0x3f))

  def >>>(x: Int)(implicit d: DummyImplicit): ULong = ULong(rep >>> x)

  def >>>(x: Long)(implicit d: DummyImplicit): ULong = ULong(rep >>> x)

  def >>>(x: UInt): ULong = ULong(rep >>> (x.intRep & 0x3f))

  def >>>(x: ULong): ULong = ULong(rep >>> (x.rep & 0x3f))

  def unary_+ : ULong = this

  def unary_- : ULong = ULong(-rep)

  def unary_~ : ULong = ULong(~rep)
}

object ULong {
  def MinValue: ULong = ULong(0L)

  def MaxValue: ULong = ULong(~0L)
}