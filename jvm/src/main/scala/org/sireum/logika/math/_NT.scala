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

import scala.math.ScalaNumericConversions
import scala.util.Random
import org.sireum.logika.{B, N16, N32, N64, N8, Z}
import spire.math.{UByte, UShort, UInt, ULong}

sealed trait _NT {
  def bitWidth: Int
  def Min: _Value
  def Max: _Value

  sealed trait _Value extends ScalaNumericConversions with Comparable[_Value] with LogikaIntegralNumber {
    final def bitWidth: Int = _NT.this.bitWidth

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def compareTo(other: _Value): Int =
      toZ.compareTo(other.toZ)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString

    final override def underlying: Z = toZ
  }
}

object _N8 extends _NT with LogikaNumberCompanion {
  final override val bitWidth = 8
  final override val Min: N8 = ValueImpl(UByte(0))
  final override val Max: N8 = ValueImpl(UByte((BigInt(2).pow(bitWidth) - 1).toInt))

  private[math] final def checkRange(value: Int): N8 = {
    if (!(Min.toInt <= value && value <= Max.toInt))
      throw new IllegalArgumentException
    ValueImpl(UByte(value))
  }

  private[math] final def checkRange(value: Z): N8 = {
    if (!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(UByte(value.toInt))
  }

  final override def random: N8 = ValueImpl(UByte(new Random().nextInt))

  sealed trait Value extends _Value {
    def +(other: N8): N8 = checkRange(toInt + other.toInt)

    def -(other: N8): N8 = checkRange(toInt - other.toInt)

    def *(other: N8): N8 = checkRange(toInt * other.toInt)

    def /(other: N8): N8 = checkRange(toInt / other.toInt)

    def %(other: N8): N8 = checkRange(toInt % other.toInt)

    def >(other: N8): B = value > other.value

    def >=(other: N8): B = value >= other.value

    def <(other: N8): B = value < other.value

    def <=(other: N8): B = value <= other.value

    def value: UByte
  }

  private[math] final case class ValueImpl(value: UByte) extends Value {
    override def intValue: Int = value.toInt
    override def longValue: Long = value.toLong
    override def floatValue: Float = value.toFloat
    override def doubleValue: Double = value.toDouble
    override def toZ: Z = _Z(value.toInt)

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || _Z(value.toBigInt).equals(other.toZ)
      case other: Byte => _Z(value.toBigInt) == _Z(other)
      case other: Char => _Z(value.toBigInt) == _Z(other)
      case other: Short => _Z(value.toBigInt) == _Z(other)
      case other: Int => _Z(value.toBigInt) == _Z(other)
      case other: Long => _Z(value.toBigInt) == _Z(other)
      case other: java.math.BigInteger => _Z(value.toBigInt) == _Z(other)
      case other: BigInt => _Z(value.toBigInt) == _Z(other)
      case _ => false
    }
  }
}

object _N16 extends _NT with LogikaNumberCompanion {
  final override val bitWidth = 16
  final override val Min: N16 = ValueImpl(UShort(0))
  final override val Max: N16 = ValueImpl(UShort((BigInt(2).pow(bitWidth) - 1).toInt))

  private[math] final def checkRange(value: Int): N16 = {
    if (!(Min.toInt <= value && value <= Max.toInt))
      throw new IllegalArgumentException
    ValueImpl(UShort(value))
  }

  private[math] final def checkRange(value: Z): N16 = {
    if (!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(UShort(value.toInt))
  }

  final override def random: N16 = ValueImpl(UShort(new Random().nextInt))

  sealed trait Value extends _Value {
    def +(other: N16): N16 = checkRange(toInt + other.toInt)

    def -(other: N16): N16 = checkRange(toInt - other.toInt)

    def *(other: N16): N16 = checkRange(toInt * other.toInt)

    def /(other: N16): N16 = checkRange(toInt / other.toInt)

    def %(other: N16): N16 = checkRange(toInt % other.toInt)

    def >(other: N16): B = value > other.value

    def >=(other: N16): B = value >= other.value

    def <(other: N16): B = value < other.value

    def <=(other: N16): B = value <= other.value

    def value: UShort
  }

  private[math] final case class ValueImpl(value: UShort) extends Value {
    override def intValue: Int = value.toInt
    override def longValue: Long = value.toLong
    override def floatValue: Float = value.toFloat
    override def doubleValue: Double = value.toDouble
    override def toZ: Z = _Z(value.toInt)

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || _Z(value.toBigInt).equals(other.toZ)
      case other: Byte => _Z(value.toBigInt) == _Z(other)
      case other: Char => _Z(value.toBigInt) == _Z(other)
      case other: Short => _Z(value.toBigInt) == _Z(other)
      case other: Int => _Z(value.toBigInt) == _Z(other)
      case other: Long => _Z(value.toBigInt) == _Z(other)
      case other: java.math.BigInteger => _Z(value.toBigInt) == _Z(other)
      case other: BigInt => _Z(value.toBigInt) == _Z(other)
      case _ => false
    }
  }
}

object _N32 extends _NT with LogikaNumberCompanion {
  final override val bitWidth = 32
  final override val Min: N32 = ValueImpl(UInt(0))
  final override val Max: N32 = ValueImpl(UInt((BigInt(2).pow(bitWidth) - 1).toLong))

  private[math] final def checkRange(value: Long): N32 = {
    if (!(Min.toLong <= value && value <= Max.toLong))
      throw new IllegalArgumentException
    ValueImpl(UInt(value))
  }

  private[math] final def checkRange(value: Z): N32 = {
    if (!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(UInt(value.toLong))
  }

  final override def random: N32 = ValueImpl(UInt(new Random().nextLong))

  sealed trait Value extends _Value {
    def +(other: N32): N32 = checkRange(toLong + other.toLong)

    def -(other: N32): N32 = checkRange(toLong - other.toLong)

    def *(other: N32): N32 = checkRange(toLong * other.toLong)

    def /(other: N32): N32 = checkRange(toLong / other.toLong)

    def %(other: N32): N32 = checkRange(toLong % other.toLong)

    def >(other: N32): B = value > other.value

    def >=(other: N32): B = value >= other.value

    def <(other: N32): B = value < other.value

    def <=(other: N32): B = value <= other.value

    def value: UInt
  }

  private[math] final case class ValueImpl(value: UInt) extends Value {
    override def intValue: Int = value.toInt
    override def longValue: Long = value.toLong
    override def floatValue: Float = value.toFloat
    override def doubleValue: Double = value.toDouble
    override def toZ: Z = _Z(value.toLong)

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || _Z(value.toBigInt).equals(other.toZ)
      case other: Byte => _Z(value.toBigInt) == _Z(other)
      case other: Char => _Z(value.toBigInt) == _Z(other)
      case other: Short => _Z(value.toBigInt) == _Z(other)
      case other: Int => _Z(value.toBigInt) == _Z(other)
      case other: Long => _Z(value.toBigInt) == _Z(other)
      case other: java.math.BigInteger => _Z(value.toBigInt) == _Z(other)
      case other: BigInt => _Z(value.toBigInt) == _Z(other)
      case _ => false
    }
  }
}

object _N64 extends _NT with LogikaNumberCompanion {
  final override val bitWidth = 64
  final override val Min: N64 = ValueImpl(ULong(0))
  final override val Max: N64 = ValueImpl(ULong.fromBigInt(BigInt(2).pow(bitWidth) - 1))

  private[math] final def checkRange(value: Z): N64 = {
    if (!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(ULong.fromBigInt(value.toBigInt))
  }

  final override def random: N64 = ValueImpl(ULong(new Random().nextLong))

  sealed trait Value extends _Value {
    def +(other: N64): N64 = checkRange(toZ + other.toZ)

    def -(other: N64): N64 = checkRange(toZ - other.toZ)

    def *(other: N64): N64 = checkRange(toZ * other.toZ)

    def /(other: N64): N64 = checkRange(toZ / other.toZ)

    def %(other: N64): N64 = checkRange(toZ % other.toZ)

    def >(other: N64): B = value > other.value

    def >=(other: N64): B = value >= other.value

    def <(other: N64): B = value < other.value

    def <=(other: N64): B = value <= other.value

    def value: ULong
  }

  private[math] final case class ValueImpl(value: ULong) extends Value {
    override def intValue: Int = value.toInt
    override def longValue: Long = value.toLong
    override def floatValue: Float = value.toFloat
    override def doubleValue: Double = value.toDouble
    override def toZ: Z = _Z(value.toLong)

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || _Z(value.toBigInt).equals(other.toZ)
      case other: Byte => _Z(value.toBigInt) == _Z(other)
      case other: Char => _Z(value.toBigInt) == _Z(other)
      case other: Short => _Z(value.toBigInt) == _Z(other)
      case other: Int => _Z(value.toBigInt) == _Z(other)
      case other: Long => _Z(value.toBigInt) == _Z(other)
      case other: java.math.BigInteger => _Z(value.toBigInt) == _Z(other)
      case other: BigInt => _Z(value.toBigInt) == _Z(other)
      case _ => false
    }
  }
}
