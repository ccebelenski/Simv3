package com.sim.unsigned


import scala.math.ScalaNumericAnyConversions

//implicit def ubyte2uint(x: UByte): UInt = UInt(x.toInt)
given ubyte2uint: Conversion[UByte, UInt] = UInt(_)

//implicit def ushort2uint(x: UShort): UInt = UInt(x.toInt)
given ushort2uint: Conversion[UShort, UInt] = UInt(_)
//given ushort2int: Conversion[UShort, Int] = _.intValue

// implicit def ubyte2ulong(x: UByte): ULong = ULong(x.toLong)
given ubyte2ulong: Conversion[UByte, ULong] with {
  def apply(x:UByte) : ULong = ULong(x.toLong)
}

//implicit def ushort2ulong(x: UShort): ULong = ULong(x.toLong)
given ushort2ulong: Conversion[UShort, ULong] with
{def apply(x:UShort) : ULong = ULong(x.toLong)}

// implicit def uint2ulong(x: UInt): ULong = ULong(x.toLong)
given uint2ulong: Conversion[UInt, ULong] with {
  def apply(x:UInt) : ULong = ULong(x.toLong)
}

given uint2int: Conversion[UInt, Int] with {
  def apply(x:UInt) : Int = x.intValue
}

// implicit def signedIntOps(x: Int): SignedIntOps = new SignedIntOps(x)
given signedIntOps: Conversion[Int, SignedIntOps] = new SignedIntOps(_)

//implicit def signedLongOps(x: Long): SignedLongOps = new SignedLongOps(x)
given signedLongOps: Conversion[Long, SignedLongOps] = new SignedLongOps(_)

// implicit def floatOps(x: Float): FloatOps = new FloatOps(x)
given floatOps: Conversion[Float, FloatOps] = new FloatOps(_)

// implicit def doubleOps(x: Double): DoubleOps = new DoubleOps(x)
given doubleOps: Conversion[Double, DoubleOps] = new DoubleOps(_)

// implicit def signedRichIntOps(x: scala.runtime.RichInt): SignedRichIntOps = new SignedRichIntOps(x.self.asInstanceOf[Int])
given signedRichIntOps: Conversion[scala.runtime.RichInt, SignedRichIntOps] with
{def apply(x:scala.runtime.RichInt) : SignedRichIntOps = new SignedRichIntOps(x.self)}

// implicit def richUInt(x: UInt): RichUInt = new RichUInt(x)
given richUInt: Conversion[UInt, RichUInt] = new RichUInt(_)

//  implicit def richerUInt(x: UInt): RicherUInt = new RicherUInt(x.toInt)
given richerUInt: Conversion[UInt, RicherUInt] with
  def apply(x: UInt): RicherUInt = new RicherUInt(x.intValue)


// implicit def ushort2Int(x: UShort) : Int = x.intValue
given ushort2Int: Conversion[UShort, Int] = _.intValue()

// implicit def ubyte2Int(x: UByte): Int = x.intValue
given ubyte2Int: Conversion[UByte, Int] = _.intValue()

class FloatOps(x: Float) {
  def toUByte: UByte = UByte(x.toByte)

  def toUShort: UShort = UShort(x.toShort)

  def toUInt: UInt = UInt(x.toInt)

  def toULong: ULong = ULong(x.toLong)

  def +(y: UInt): Float = x + y.toFloat

  def -(y: UInt): Float = x - y.toFloat

  def *(y: UInt): Float = x * y.toFloat

  def /(y: UInt): Float = x / y.toFloat

  def %(y: UInt): Float = x % y.toFloat
}

class DoubleOps(x: Double) {
  def toUByte: UByte = UByte(x.toByte)

  def toUShort: UShort = UShort(x.toShort)

  def toUInt: UInt = UInt(x.toInt)

  def toULong: ULong = ULong(x.toLong)

  def +(y: UInt): Double = x + y.toDouble

  def -(y: UInt): Double = x - y.toDouble

  def *(y: UInt): Double = x * y.toDouble

  def /(y: UInt): Double = x / y.toDouble

  def %(y: UInt): Double = x % y.toDouble
}

class SignedIntOps(x: Int) {
  def toUByte: UByte = UByte((x & 0xff).toByte)

  def toUShort: UShort = UShort((x & 0xffff).toShort)

  def toUInt: UInt = UInt(x)

  def toULong: ULong = ULong(x.toLong)

  def +(y: UByte): Int = x + y.toInt

  def -(y: UByte): Int = x - y.toInt

  def *(y: UByte): Int = x * y.toInt

  def /(y: UByte): Int = x / y.toInt

  def %(y: UByte): Int = x % y.toInt

  def &(y: UByte): Int = x & y.toInt

  def |(y: UByte): Int = x | y.toInt

  def ^(y: UByte): Int = x ^ y.toInt

  def +(y: UShort): Int = x + y.toInt

  def -(y: UShort): Int = x - y.toInt

  def *(y: UShort): Int = x * y.toInt

  def /(y: UShort): Int = x / y.toInt

  def %(y: UShort): Int = x % y.toInt

  def &(y: UShort): Int = x & y.toInt

  def |(y: UShort): Int = x | y.toInt

  def ^(y: UShort): Int = x ^ y.toInt

  def +(y: UInt): Int = x + y.toInt

  def -(y: UInt): Int = x - y.toInt

  def *(y: UInt): Int = x * y.toInt

  def /(y: UInt): Int = x / y.toInt

  def %(y: UInt): Int = x % y.toInt

  def &(y: UInt): Int = x & y.toInt

  def |(y: UInt): Int = x | y.toInt

  def ^(y: UInt): Int = x ^ y.toInt

}

class SignedLongOps(x: Long) {
  def toUByte:UByte  = {UByte((x & 0xffL).toByte)}

  def toUShort: UShort = UShort((x & 0xffffL).toShort)

  def toUInt: UInt = UInt((x & 0xffffffffL).toInt)

  def toULong: ULong = ULong(x)

  def +(y: UInt): Long = x + y.toLong

  def -(y: UInt): Long = x - y.toLong

  def *(y: UInt): Long = x * y.toLong

  def /(y: UInt): Long = x / y.toLong

  def %(y: UInt): Long = x % y.toLong

  def &(y: UInt): Long = x & y.toLong

  def |(y: UInt): Long = x | y.toLong

  def ^(y: UInt): Long = x ^ y.toLong
}

class SignedRichIntOps(x: Int) {
  def to(y: UInt): Range.Inclusive = x to y.toInt

  def until(y: UInt): Range = x until y.toInt
  // def max(that: UInt) = if (x < that) that else x
  // def min(that: UInt) = if (x > that) that else x
}

trait UByteOrdering extends Ordering[UByte] {
  def compare(x: UByte, y: UByte): Int =
    if (x < y) -1
    else if (x == y) 0
    else 1
}

implicit object UByteOrdering extends UByteOrdering

trait UShortOrdering extends Ordering[UShort] {
  def compare(x: UShort, y: UShort): Int =
    if (x < y) -1
    else if (x == y) 0
    else 1
}

implicit object UShortOrdering extends UShortOrdering

trait UIntOrdering extends Ordering[UInt] {
  def compare(x: UInt, y: UInt): Int =
    if (x < y) -1
    else if (x == y) 0
    else 1
}

implicit object UIntOrdering extends UIntOrdering

trait ULongOrdering extends Ordering[ULong] {
  def compare(x: ULong, y: ULong): Int =
    if (x < y) -1
    else if (x == y) 0
    else 1
}

implicit object ULongOrdering extends ULongOrdering

trait UByteIsIntegral extends Integral[UByte] {
  def plus(x: UByte, y: UByte): UByte = (x + y).toUByte

  def minus(x: UByte, y: UByte): UByte = (x - y).toUByte

  def times(x: UByte, y: UByte): UByte = (x * y).toUByte

  def quot(x: UByte, y: UByte): UByte = (x / y).toUByte

  def rem(x: UByte, y: UByte): UByte = (x % y).toUByte

  def negate(x: UByte): UByte = (-x).toUByte

  def fromInt(x: Int): UByte = UByte(x.toByte)

  def toInt(x: UByte): Int = x.toInt

  def toLong(x: UByte): Long = x.toLong

  def toFloat(x: UByte): Float = x.toFloat

  def toDouble(x: UByte): Double = x.toDouble
}

implicit object UByteIsIntegral extends UByteIsIntegral with UByteOrdering {
  override def parseString(str: String): Option[UByte] = ???
}

trait UShortIsIntegral extends Integral[UShort] {
  def plus(x: UShort, y: UShort): UShort = (x + y).toUShort

  def minus(x: UShort, y: UShort): UShort = (x - y).toUShort

  def times(x: UShort, y: UShort): UShort = (x * y).toUShort

  def quot(x: UShort, y: UShort): UShort = (x / y).toUShort

  def rem(x: UShort, y: UShort): UShort = (x % y).toUShort

  def negate(x: UShort): UShort = (-x).toUShort

  def fromInt(x: Int): UShort = UShort(x.toShort)

  def toInt(x: UShort): Int = x.toInt

  def toLong(x: UShort): Long = x.toLong

  def toFloat(x: UShort): Float = x.toFloat

  def toDouble(x: UShort): Double = x.toDouble
}

implicit object UShortIsIntegral extends UShortIsIntegral with UShortOrdering {
  override def parseString(str: String): Option[UShort] = ???
}

trait UIntIsIntegral extends Integral[UInt] {
  def plus(x: UInt, y: UInt): UInt = x + y

  def minus(x: UInt, y: UInt): UInt = x - y

  def times(x: UInt, y: UInt): UInt = x * y

  def quot(x: UInt, y: UInt): UInt = x / y

  def rem(x: UInt, y: UInt): UInt = x % y

  def negate(x: UInt): UInt = -x

  def fromInt(x: Int): UInt = UInt(x)

  def toInt(x: UInt): Int = x.toInt

  def toLong(x: UInt): Long = x.toLong

  def toFloat(x: UInt): Float = x.toFloat

  def toDouble(x: UInt): Double = x.toDouble
}

implicit object UIntIsIntegral extends UIntIsIntegral with UIntOrdering {
  override def parseString(str: String): Option[UInt] = ???
}

trait ULongIsIntegral extends Integral[ULong] {
  def plus(x: ULong, y: ULong): ULong = x + y

  def minus(x: ULong, y: ULong): ULong = x - y

  def times(x: ULong, y: ULong): ULong = x * y

  def quot(x: ULong, y: ULong): ULong = x / y

  def rem(x: ULong, y: ULong): ULong = x % y

  def negate(x: ULong): ULong = -x

  def fromInt(x: Int): ULong = ULong(x)

  def toInt(x: ULong): Int = x.toInt

  def toLong(x: ULong): Long = x.toLong

  def toFloat(x: ULong): Float = x.toFloat

  def toDouble(x: ULong): Double = x.toDouble
}

implicit object ULongIsIntegral extends ULongIsIntegral with ULongOrdering {
  override def parseString(str: String): Option[ULong] = ???
}

class RicherUInt(rep: Int) {
  def bitCount: Int = Integer.bitCount(rep)

  def highestOneBit: Int = Integer.highestOneBit(rep)

  def lowestOneBit: Int = Integer.lowestOneBit(rep)

  def numberOfLeadingZeros: Int = Integer.numberOfLeadingZeros(rep)

  def numberOfTrailingZeros: Int = Integer.numberOfTrailingZeros(rep)

  def reverse: UInt = UInt(Integer.reverse(rep))

  def reverseBytes: UInt = UInt(Integer.reverseBytes(rep))

  def rotateLeft(dist: Int): UInt = UInt(Integer.rotateLeft(rep, dist))

  def rotateRight(dist: Int): UInt = UInt(Integer.rotateRight(rep, dist))

  def signum: Int = if (rep == 0) 0 else 1
}

class RichUInt(x: UInt) {
  def to(y: UInt): Range.Inclusive = x.toInt to y.toInt

  def until(y: UInt): Range = x.toInt until y.toInt

  def compare(y: UInt): Int = {
    if (x < y) 1
    else if (x > y) -1
    else 0
  }

  def max(y: UInt): UInt = if (x < y) y else x

  def min(y: UInt): UInt = if (x > y) y else x

  def +(y: Int): Int = x.toInt + y

  def -(y: Int): Int = x.toInt - y

  def *(y: Int): Int = x.toInt * y

  def /(y: Int): Int = x.toInt / y

  def %(y: Int): Int = x.toInt % y

  def +(y: Long): Long = x.toLong + y

  def -(y: Long): Long = x.toLong - y

  def *(y: Long): Long = x.toLong * y

  def /(y: Long): Long = x.toLong / y

  def %(y: Long): Long = x.toLong % y

  def +(y: Float): Float = x.toFloat + y

  def -(y: Float): Float = x.toFloat - y

  def *(y: Float): Float = x.toFloat * y

  def /(y: Float): Float = x.toFloat / y

  def %(y: Float): Float = x.toFloat % y

  def +(y: Double): Double = x.toDouble + y

  def -(y: Double): Double = x.toDouble - y

  def *(y: Double): Double = x.toDouble * y

  def /(y: Double): Double = x.toDouble / y

  def %(y: Double): Double = x.toDouble % y

  def abs: UInt = x
}
/**
 * Created by christophercebelenski on 7/18/16.
 */

trait Unsigned[U <: Unsigned[U, Promoted, SignedPromoted], Promoted <: Unsigned[_, Promoted, SignedPromoted], SignedPromoted] extends Any with ScalaNumericAnyConversions {

  def toByte: Byte

  def toChar: Char

  def toShort: Short

  def toInt: Int

  def toLong: Long

  def toFloat: Float

  def toDouble: Double

  def toUByte: UByte
  def toUShort: UShort
  def toUInt: UInt
  def toULong: ULong

  def byteValue(): Byte

  def shortValue(): Short

  def intValue(): Int

  def longValue(): Long

  def floatValue(): Float

  def doubleValue(): Double

  // Implementing ScalaNumber
  def isWhole: Boolean = {
    true
  }

  def underlying(): Unsigned[U, Promoted, SignedPromoted] = {
    this
  }

  def +(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def -(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def *(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def /(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def %(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def &(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def |(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def ^(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def +(x: UByte): Promoted
  def -(x: UByte): Promoted
  def *(x: UByte): Promoted
  def /(x: UByte): Promoted
  def %(x: UByte): Promoted
  def &(x: UByte): Promoted
  def |(x: UByte): Promoted
  def ^(x: UByte): Promoted
  def <(x: UByte): Boolean
  def >(x: UByte): Boolean
  def <=(x: UByte): Boolean
  def >=(x: UByte): Boolean

  def +(x: UShort): Promoted
  def -(x: UShort): Promoted
  def *(x: UShort): Promoted
  def /(x: UShort): Promoted
  def %(x: UShort): Promoted
  def &(x: UShort): Promoted
  def |(x: UShort): Promoted
  def ^(x: UShort): Promoted
  def <(x: UShort): Boolean
  def >(x: UShort): Boolean
  def <=(x: UShort): Boolean
  def >=(x: UShort): Boolean

  def +(x: ULong): ULong
  def -(x: ULong): ULong
  def *(x: ULong): ULong
  def /(x: ULong): ULong
  def %(x: ULong): ULong
  def &(x: ULong): ULong
  def |(x: ULong): ULong
  def ^(x: ULong): ULong
  def <(x: ULong): Boolean
  def >(x: ULong): Boolean
  def <=(x: ULong): Boolean
  def >=(x: ULong): Boolean

  def +(x: UInt): Promoted
  def -(x: UInt): Promoted
  def *(x: UInt): Promoted
  def /(x: UInt): Promoted
  def %(x: UInt): Promoted
  def &(x : UInt): Promoted
  def |(x : UInt): Promoted
  def ^(x : UInt): Promoted
  def <(x: UInt): Boolean
  def >(x: UInt): Boolean
  def <=(x: UInt): Boolean
  def >=(x: UInt): Boolean

  def unary_+ : Promoted

  def unary_- : Promoted

  // Equality comparison to UInt is baked in

  /*
    // Override equals to allow comparison with other number types.
    // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
    // comparing a number on the left with a UInt on the right.
    // This is an (undocumented?) hack and might change in the future.
    override def equals(x: Any): Boolean
    def canEqual(x: Any): Boolean
    */

  def unary_~ : Promoted

  def <<(x: Int)(implicit d: DummyImplicit): Promoted

  def >>(x: Int)(implicit d: DummyImplicit): Promoted

  def >>>(x: Int)(implicit d: DummyImplicit): Promoted

  def <<(x: Long)(implicit d: DummyImplicit): Promoted

  def >>(x: Long)(implicit d: DummyImplicit): Promoted

  def >>>(x: Long)(implicit d: DummyImplicit): Promoted
  def <<(x : UInt): Promoted
  def >>(x : UInt): Promoted
  def >>>(x : UInt): Promoted
  def <<(x : ULong): Promoted
  def >>(x : ULong): Promoted
  def >>>(x : ULong): Promoted

  def toString: String

  def +(x: java.lang.String): String

  def toHexString: String

  def toOctalString: String

  def toBinaryString: String
}