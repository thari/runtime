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
import org.sireum.logika.{B, S16, S32, S64, S8, Z}

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait _ST {
  def bitWidth: Int

  sealed trait _Value extends ScalaNumericConversions with Comparable[_Value] with _LogikaIntegralNumber {
    final def bitWidth: Int = _ST.this.bitWidth

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    def compareTo(other: _Value): Int =
      toLong.compareTo(other.toLong)
  }
}

object _S8 extends _ST with LogikaNumberCompanion {
  final val Min: _Value = ValueImpl(Byte.MinValue)
  final val Max: _Value = ValueImpl(Byte.MaxValue)

  final override def bitWidth = 8

  final override def random: S8 =
    _S8.ValueImpl(new Random().nextInt.toByte)

  sealed trait Value extends _Value {
    def +(other: S8): S8

    def -(other: S8): S8

    def *(other: S8): S8

    def /(other: S8): S8

    def %(other: S8): S8

    def unary_-(): S8

    final def >(other: S8): B = value > other.value

    final def >=(other: S8): B = value >= other.value

    final def <(other: S8): B = value < other.value

    final def <=(other: S8): B = value <= other.value

    def >>(distance: S8): S8

    def unary_~(): S8

    def &(other: S8): S8

    def |(other: S8): S8

    def ^|(other: S8): S8

    def <<(distance: S8): S8

    def >>>(distance: S8): S8

    override def underlying: java.lang.Byte

    def value: Byte
  }

  private[logika] final case class ValueImpl(value: Byte) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def underlying: java.lang.Byte = value

    override def +(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toByte)
    }

    override def -(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toByte)
    }

    override def *(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toByte)
    }

    override def /(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toByte)
    }

    override def %(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toByte)
    }

    override def &(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toByte)
    }

    override def |(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toByte)
    }

    override def ^|(other: S8): S8 = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toByte)
    }

    override def unary_~(): S8 = ValueImpl((~value & 0xFF).toByte)

    override def unary_-(): S8 = ValueImpl((-value).toByte)

    override def <<(distance: S8): S8 = ValueImpl((value << distance.toInt).toByte)

    override def >>(distance: S8): S8 = ValueImpl((value >> distance.toInt).toByte)

    override def >>>(distance: S8): S8 = ValueImpl(((value & 0xFF) >>> distance.toInt).toByte)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value)

    override def equals(other: Any): Boolean = other match {
      case other: _LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
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

object _S16 extends _ST with LogikaNumberCompanion {
  final val Min: _Value = ValueImpl(Short.MinValue)
  final val Max: _Value = ValueImpl(Short.MaxValue)

  final override def bitWidth = 16

  final override def random: S16 =
    _S16.ValueImpl(new Random().nextInt.toShort)

  sealed trait Value extends _Value {
    def +(other: S16): S16

    def -(other: S16): S16

    def *(other: S16): S16

    def /(other: S16): S16

    def %(other: S16): S16

    def unary_-(): S16

    final def >(other: S16): B = value > other.value

    final def >=(other: S16): B = value >= other.value

    final def <(other: S16): B = value < other.value

    final def <=(other: S16): B = value <= other.value

    def >>(distance: S16): S16

    def unary_~(): S16

    def &(other: S16): S16

    def |(other: S16): S16

    def ^|(other: S16): S16

    def <<(distance: S16): S16

    def >>>(distance: S16): S16

    override def underlying: java.lang.Short

    def value: Short
  }

  private[logika] final case class ValueImpl(value: Short) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def underlying: java.lang.Short = value

    override def +(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toByte)
    }

    override def -(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toByte)
    }

    override def *(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toByte)
    }

    override def /(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toByte)
    }

    override def %(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toByte)
    }

    override def &(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toByte)
    }

    override def |(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toByte)
    }

    override def ^|(other: S16): S16 = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toByte)
    }

    override def unary_~(): S16 = ValueImpl((~value & 0xFF).toByte)

    override def unary_-(): S16 = ValueImpl((-value).toByte)

    override def <<(distance: S16): S16 = ValueImpl((value << distance.toInt).toByte)

    override def >>(distance: S16): S16 = ValueImpl((value >> distance.toInt).toByte)

    override def >>>(distance: S16): S16 = ValueImpl(((value & 0xFF) >>> distance.toInt).toByte)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value)

    override def equals(other: Any): Boolean = other match {
      case other: _LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
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


object _S32 extends _ST with LogikaNumberCompanion {
  final val Min: _Value = ValueImpl(Int.MinValue)
  final val Max: _Value = ValueImpl(Int.MaxValue)

  final override def bitWidth = 32

  final override def random: S32 =
    _S32.ValueImpl(new Random().nextInt)

  sealed trait Value extends _Value {
    def +(other: S32): S32

    def -(other: S32): S32

    def *(other: S32): S32

    def /(other: S32): S32

    def %(other: S32): S32

    def unary_-(): S32

    final def >(other: S32): B = value > other.value

    final def >=(other: S32): B = value >= other.value

    final def <(other: S32): B = value < other.value

    final def <=(other: S32): B = value <= other.value

    def >>(distance: S32): S32

    def unary_~(): S32

    def &(other: S32): S32

    def |(other: S32): S32

    def ^|(other: S32): S32

    def <<(distance: S32): S32

    def >>>(distance: S32): S32

    override def underlying: java.lang.Integer

    def value: Int
  }

  private[logika] final case class ValueImpl(value: Int) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def underlying: java.lang.Integer = value

    override def +(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toByte)
    }

    override def -(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toByte)
    }

    override def *(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toByte)
    }

    override def /(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toByte)
    }

    override def %(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toByte)
    }

    override def &(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toByte)
    }

    override def |(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toByte)
    }

    override def ^|(other: S32): S32 = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toByte)
    }

    override def unary_~(): S32 = ValueImpl((~value & 0xFF).toByte)

    override def unary_-(): S32 = ValueImpl((-value).toByte)

    override def <<(distance: S32): S32 = ValueImpl((value << distance.toInt).toByte)

    override def >>(distance: S32): S32 = ValueImpl((value >> distance.toInt).toByte)

    override def >>>(distance: S32): S32 = ValueImpl(((value & 0xFF) >>> distance.toInt).toByte)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value)

    override def equals(other: Any): Boolean = other match {
      case other: _LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
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


object _S64 extends _ST with LogikaNumberCompanion {
  final val Min: _Value = ValueImpl(Long.MinValue)
  final val Max: _Value = ValueImpl(Long.MaxValue)

  final override def bitWidth = 64

  final override def random: S64 =
    _S64.ValueImpl(new Random().nextLong)

  sealed trait Value extends _Value {
    def +(other: S64): S64

    def -(other: S64): S64

    def *(other: S64): S64

    def /(other: S64): S64

    def %(other: S64): S64

    def unary_-(): S64

    final def >(other: S64): B = value > other.value

    final def >=(other: S64): B = value >= other.value

    final def <(other: S64): B = value < other.value

    final def <=(other: S64): B = value <= other.value

    def >>(distance: S64): S64

    def unary_~(): S64

    def &(other: S64): S64

    def |(other: S64): S64

    def ^|(other: S64): S64

    def <<(distance: S64): S64

    def >>>(distance: S64): S64

    override def underlying: java.lang.Long

    def value: Long
  }

  private[logika] final case class ValueImpl(value: Long) extends Value {
    override def doubleValue: Double = value.toDouble

    override def floatValue: Float = value.toFloat

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def underlying: java.lang.Long = value

    override def +(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toByte)
    }

    override def -(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toByte)
    }

    override def *(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toByte)
    }

    override def /(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toByte)
    }

    override def %(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toByte)
    }

    override def &(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toByte)
    }

    override def |(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toByte)
    }

    override def ^|(other: S64): S64 = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toByte)
    }

    override def unary_~(): S64 = ValueImpl((~value & 0xFF).toByte)

    override def unary_-(): S64 = ValueImpl((-value).toByte)

    override def <<(distance: S64): S64 = ValueImpl((value << distance.toInt).toByte)

    override def >>(distance: S64): S64 = ValueImpl((value >> distance.toInt).toByte)

    override def >>>(distance: S64): S64 = ValueImpl(((value & 0xFF) >>> distance.toInt).toByte)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = _Z(value)

    override def equals(other: Any): Boolean = other match {
      case other: _LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
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
