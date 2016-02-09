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
import spire.math.{ULong, UInt, UShort, UByte}

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait U {
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

object U8 extends U with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(UByte.MinValue)
  final val Max: Value = ValueImpl(UByte.MaxValue)

  final override def bitWidth = 8

  final override def random: U8 =
    U8.ValueImpl(UByte(new Random().nextInt))

  private[math] final case class ValueImpl(value: UByte) extends Value {
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

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value.toBigInt)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
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

object U16 extends U with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(UShort.MinValue)
  final val Max: Value = ValueImpl(UShort.MaxValue)

  final override def bitWidth = 16

  final override def random: U16 =
    U16.ValueImpl(UShort(new Random().nextInt))

  private[math] final case class ValueImpl(value: UShort) extends Value {
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

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value.toBigInt)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
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

object U32 extends U with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(UInt.MinValue)
  final val Max: Value = ValueImpl(UInt.MaxValue)

  final override def bitWidth = 32

  final override def random: U32 =
    U32.ValueImpl(UInt(new Random().nextInt))

  private[math] final case class ValueImpl(value: UInt) extends Value {
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

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value.toBigInt)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
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

object U64 extends U with LogikaNumberCompanion {
  final val Min: Value = ValueImpl(ULong.MinValue)
  final val Max: Value = ValueImpl(ULong.MaxValue)

  final override def bitWidth = 64

  final override def random: U64 =
    U64.ValueImpl(ULong(new Random().nextLong))

  private[math] final case class ValueImpl(value: ULong) extends Value {
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

    override def >>>(distance: N8): Value = ValueImpl(value >>> distance.toInt)

    override val hashCode: Int = value.hashCode

    override def toZ: Z = Z(value.toBigInt)

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value.toInt == other.toInt
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
