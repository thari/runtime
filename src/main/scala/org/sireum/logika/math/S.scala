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
import org.sireum.logika._

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait S {
  def bitWidth: Int

  sealed trait Value extends ScalaNumericConversions with Comparable[Value] with LogikaModuloIntegralNumber[Value] {

    def +(other: Value): Value

    def -(other: Value): Value

    def *(other: Value): Value

    def /(other: Value): Value

    def %(other: Value): Value

    final def >(other: Value): B = toZ > other.toZ

    final def >=(other: Value): B = toZ >= other.toZ

    final def <(other: Value): B = toZ < other.toZ

    final def <=(other: Value): B = toZ <= other.toZ

    def >>(distance: N8): Value

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def doubleValue = toZ.toDouble

    final override def floatValue = toZ.toFloat

    final override def intValue = toZ.toInt

    final override def longValue = toZ.toLong

    final override def underlying = toZ

    final override def compareTo(other: Value): Int =
      toZ.compareTo(other.toZ)

    final override def isWhole: B = true

    final override def toString: String = toZ.toString
  }

}

object S8 extends S with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(Byte.MinValue)
  final val Max: Value = ValueImpl(Byte.MaxValue)

  final override def bitWidth = 8

  final override def random: S8 =
    S8.ValueImpl(new Random().nextInt.toByte)

  private[math] final case class ValueImpl(value: Byte) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toByte)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toByte)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toByte)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toByte)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toByte)
    }

    override def &(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toByte)
    }

    override def |(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toByte)
    }

    override def ^(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toByte)
    }

    override def unary_~(): Value = ValueImpl((~value & 0xFF).toByte)

    override def <<(distance: N8): Value = ValueImpl((value << distance.toInt).toByte)

    override def >>(distance: N8): Value = ValueImpl((value >> distance.toInt).toByte)

    override def >>>(distance: N8): Value = ValueImpl((value >>> distance.toInt).toByte)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == Z(other)
      case other: BigInt => toZ == Z(other)
      case _ => false
    }
  }

}

object S16 extends S with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(Short.MinValue)
  final val Max: Value = ValueImpl(Short.MaxValue)

  final override def bitWidth = 16

  final override def random: S16 =
    S16.ValueImpl(new Random().nextInt.toShort)

  private[math] final case class ValueImpl(value: Short) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value + other.value).toShort)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value - other.value).toShort)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value * other.value).toShort)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value / other.value).toShort)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value % other.value).toShort)
    }

    override def &(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value & other.value).toShort)
    }

    override def |(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value | other.value).toShort)
    }

    override def ^(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl((value ^ other.value).toShort)
    }

    override def unary_~(): Value = ValueImpl((~value & 0xFFFF).toShort)

    override def <<(distance: N8): Value = ValueImpl((value << distance.toInt).toShort)

    override def >>(distance: N8): Value = ValueImpl((value >> distance.toInt).toShort)

    override def >>>(distance: N8): Value = ValueImpl((value >>> distance.toInt).toShort)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == Z(other)
      case other: BigInt => toZ == Z(other)
      case _ => false
    }
  }

}

object S32 extends S with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(Int.MinValue)
  final val Max: Value = ValueImpl(Int.MaxValue)

  final override def bitWidth = 32

  final override def random: S32 =
    S32.ValueImpl(new Random().nextInt)

  private[math] final case class ValueImpl(value: Int) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): Value = ValueImpl(~value)

    override def <<(distance: N8): Value = ValueImpl(value << distance.toInt)

    override def >>(distance: N8): Value = ValueImpl(value >> distance.toInt)

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == Z(other)
      case other: BigInt => toZ == Z(other)
      case _ => false
    }
  }

}

object S64 extends S with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(Long.MinValue)
  final val Max: Value = ValueImpl(Long.MaxValue)

  final override def bitWidth = 64

  final override def random: S64 =
    S64.ValueImpl(new Random().nextLong)

  private[math] final case class ValueImpl(value: Long) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def &(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value & other.value)
    }

    override def |(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value | other.value)
    }

    override def ^(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value ^ other.value)
    }

    override def unary_~(): Value = ValueImpl(~value)

    override def <<(distance: N8): Value = ValueImpl(value << distance.toInt)

    override def >>(distance: N8): Value = ValueImpl(value >> distance.toInt)

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other
      case other: Char => value.toChar == other
      case other: Short => value.toShort == other
      case other: Int => value.toInt == other
      case other: Long => value.toLong == other
      case other: java.math.BigInteger => toZ == Z(other)
      case other: BigInt => toZ == Z(other)
      case _ => false
    }
  }

}
