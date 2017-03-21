/*
 * Copyright (c) 2016, Robby, Kansas State University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.logika.math

import org.apfloat.Apint
import org.sireum.logika.{B, Z, U8, U16, U32, U64}
import spire.math.{ULong, UInt, UShort, UByte}

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait _UT {
  def bitWidth: Int

  sealed trait _Value extends ScalaNumericConversions with Comparable[_Value] with LogikaIntegralNumber {
    final override def compareTo(other: _Value): Int = toZ.compareTo(other.toZ)
  }
}


object _U8 extends _UT with LogikaNumberCompanion {
  final val Min: U8 = ValueImpl(UByte.MinValue)
  final val Max: U8 = ValueImpl(UByte.MaxValue)

  final override def bitWidth = 8

  final override def random: U8 =
    ValueImpl(UByte(new Random().nextInt))

  sealed trait Value extends _Value {
    final def bitWidth: Int = _U8.this.bitWidth

    def +(other: U8): U8

    def -(other: U8): U8

    def *(other: U8): U8

    def /(other: U8): U8

    def %(other: U8): U8

    final def >(other: U8): B = value > other.value

    final def >=(other: U8): B = value >= other.value

    final def <(other: U8): B = value < other.value

    final def <=(other: U8): B = value <= other.value

    def unary_~(): U8

    def &(other: U8): U8

    def |(other: U8): U8

    def ^|(other: U8): U8

    def <<(distance: U8): U8

    def >>>(distance: U8): U8

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    override def underlying: Z = toZ

    def value: UByte
  }

  private[logika] final case class ValueImpl(value: UByte) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def +(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^|(other: U8): U8 = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): U8 = ValueImpl(~value)

    override def <<(distance: U8): U8 = ValueImpl(value << distance.toInt)

    override def >>>(distance: U8): U8 = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value.toBigInt)

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == _Z(other)
      case other: BigInt => toZ == _Z(other)
      case _ => false
    }
  }
}


object _U16 extends _UT with LogikaNumberCompanion {
  final val Min: U16 = ValueImpl(UShort.MinValue)
  final val Max: U16 = ValueImpl(UShort.MaxValue)

  final override def bitWidth = 16

  final override def random: U16 =
    ValueImpl(UShort(new Random().nextInt))

  sealed trait Value extends _Value {
    final def bitWidth: Int = _U16.this.bitWidth

    def +(other: U16): U16

    def -(other: U16): U16

    def *(other: U16): U16

    def /(other: U16): U16

    def %(other: U16): U16

    final def >(other: U16): B = value > other.value

    final def >=(other: U16): B = value >= other.value

    final def <(other: U16): B = value < other.value

    final def <=(other: U16): B = value <= other.value

    def unary_~(): U16

    def &(other: U16): U16

    def |(other: U16): U16

    def ^|(other: U16): U16

    def <<(distance: U16): U16

    def >>>(distance: U16): U16

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    override def underlying: Z = toZ

    def value: UShort
  }

  private[logika] final case class ValueImpl(value: UShort) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def +(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^|(other: U16): U16 = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): U16 = ValueImpl(~value)

    override def <<(distance: U16): U16 = ValueImpl(value << distance.toInt)

    override def >>>(distance: U16): U16 = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value.toBigInt)

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == _Z(other)
      case other: BigInt => toZ == _Z(other)
      case _ => false
    }
  }
}


object _U32 extends _UT with LogikaNumberCompanion {
  final val Min: U32 = ValueImpl(UInt.MinValue)
  final val Max: U32 = ValueImpl(UInt.MaxValue)

  final override def bitWidth = 32

  final override def random: U32 =
    ValueImpl(UInt(new Random().nextInt))

  sealed trait Value extends _Value {
    final def bitWidth: Int = _U32.this.bitWidth

    def +(other: U32): U32

    def -(other: U32): U32

    def *(other: U32): U32

    def /(other: U32): U32

    def %(other: U32): U32

    final def >(other: U32): B = value > other.value

    final def >=(other: U32): B = value >= other.value

    final def <(other: U32): B = value < other.value

    final def <=(other: U32): B = value <= other.value

    def unary_~(): U32

    def &(other: U32): U32

    def |(other: U32): U32

    def ^|(other: U32): U32

    def <<(distance: U32): U32

    def >>>(distance: U32): U32

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    override def underlying: Z = toZ

    def value: UInt
  }

  private[logika] final case class ValueImpl(value: UInt) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def +(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^|(other: U32): U32 = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): U32 = ValueImpl(~value)

    override def <<(distance: U32): U32 = ValueImpl(value << distance.toInt)

    override def >>>(distance: U32): U32 = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value.toBigInt)

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == _Z(other)
      case other: BigInt => toZ == _Z(other)
      case _ => false
    }
  }
}


object _U64 extends _UT with LogikaNumberCompanion {
  final val Min: U64 = ValueImpl(ULong.MinValue)
  final val Max: U64 = ValueImpl(ULong.MaxValue)

  final override def bitWidth = 64

  final override def random: U64 =
    ValueImpl(ULong(new Random().nextLong))

  sealed trait Value extends _Value {
    final def bitWidth: Int = _U64.this.bitWidth

    def +(other: U64): U64

    def -(other: U64): U64

    def *(other: U64): U64

    def /(other: U64): U64

    def %(other: U64): U64

    final def >(other: U64): B = value > other.value

    final def >=(other: U64): B = value >= other.value

    final def <(other: U64): B = value < other.value

    final def <=(other: U64): B = value <= other.value

    def unary_~(): U64

    def &(other: U64): U64

    def |(other: U64): U64

    def ^|(other: U64): U64

    def <<(distance: U64): U64

    def >>>(distance: U64): U64

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    override def underlying: Z = toZ

    def value: ULong
  }

  private[logika] final case class ValueImpl(value: ULong) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def +(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^|(other: U64): U64 = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): U64 = ValueImpl(~value)

    override def <<(distance: U64): U64 = ValueImpl(value << distance.toInt)

    override def >>>(distance: U64): U64 = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value.toBigInt)

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == _Z(other)
      case other: BigInt => toZ == _Z(other)
      case _ => false
    }
  }
}
