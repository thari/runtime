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
import org.sireum.logika.{B, Z, Z8, Z16, Z32, Z64}

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait _ZT {
  def bitWidth: Int
  def Min: _Value
  def Max: _Value

  sealed trait _Value extends ScalaNumericConversions with Comparable[_Value] with LogikaIntegralNumber {
    final def bitWidth: Int = _ZT.this.bitWidth

    final def toZ: Z = _Z(toLong)

    final def toBigInteger: java.math.BigInteger = BigInt(toLong).bigInteger

    final def toBigInt: BigInt = BigInt(toLong)

    final def toApint: Apint = new Apint(toBigInteger)

    final override def compareTo(other: _Value): Int =
      toLong.compareTo(other.toLong)

    final override def isWhole: Boolean = true

    final override def toString: String = toLong.toString
  }
}

object _Z8 extends _ZT with LogikaNumberCompanion {
  final override def bitWidth = 8
  final override val Min: Z8 = ValueImpl(BigInt(-2).pow(bitWidth - 1).toByte)
  final override val Max: Z8 = ValueImpl((BigInt(2).pow(bitWidth - 1) - 1).toByte)

  final override def random: Z8 = ValueImpl(new Random().nextInt.toByte)

  private[math] final def checkRange(value: Int): Z8 = {
    if(!(Min.toInt <= value && value <= Max.toInt))
      throw new IllegalArgumentException
    ValueImpl(value.toByte)
  }

  private[math] final def checkRange(value: Z): Z8 = {
    if(!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(value.toByte)
  }

  sealed trait Value extends _Value {
    final def +(other: Z8): Z8 = checkRange(toInt + other.toInt)

    final def -(other: Z8): Z8 = checkRange(toInt - other.toInt)

    final def *(other: Z8): Z8 = checkRange(toInt * other.toInt)

    final def /(other: Z8): Z8 = checkRange(toInt / other.toInt)

    final def %(other: Z8): Z8 = checkRange(toInt % other.toInt)

    final def >(other: Z8): B = value > other.value

    final def >=(other: Z8): B = value >= other.value

    final def <(other: Z8): B = value < other.value

    final def <=(other: Z8): B = value <= other.value

    final def unary_-(): Z8 = checkRange(-toInt)

    override def underlying: java.lang.Byte

    def value: Byte
  }

  private[math] final case class ValueImpl(value: Byte) extends Value {
    override def intValue: Int = value
    override def longValue: Long = value
    override def floatValue: Float = value.floatValue
    override def doubleValue: Double = value.doubleValue
    override def underlying: java.lang.Byte = value

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other.toLong
      case other: Char => value == other.toLong
      case other: Short => value == other.toLong
      case other: Int => value == other.toLong
      case other: Long => value == other.toLong
      case other: java.math.BigInteger => _Z(value) == _Z(other)
      case other: BigInt => _Z(value) == _Z(other)
      case _ => false
    }
  }
}

object _Z16 extends _ZT with LogikaNumberCompanion {
  final override def bitWidth = 16
  final override val Min: Z16 = ValueImpl(BigInt(-2).pow(bitWidth - 1).toShort)
  final override val Max: Z16 = ValueImpl((BigInt(2).pow(bitWidth - 1) - 1).toShort)

  final override def random: Z16 = ValueImpl(new Random().nextInt.toShort)

  private[math] final def checkRange(value: Int): Z16 = {
    if(!(Min.toInt <= value && value <= Max.toInt))
      throw new IllegalArgumentException
    ValueImpl(value.toShort)
  }

  private[math] final def checkRange(value: Z): Z16 = {
    if(!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(value.toShort)
  }

  sealed trait Value extends _Value {
    final def +(other: Z16): Z16 = checkRange(toInt + other.toInt)

    final def -(other: Z16): Z16 = checkRange(toInt - other.toInt)

    final def *(other: Z16): Z16 = checkRange(toInt * other.toInt)

    final def /(other: Z16): Z16 = checkRange(toInt / other.toInt)

    final def %(other: Z16): Z16 = checkRange(toInt % other.toInt)

    final def >(other: Z16): B = value > other.value

    final def >=(other: Z16): B = value >= other.value

    final def <(other: Z16): B = value < other.value

    final def <=(other: Z16): B = value <= other.value

    final def unary_-(): Z16 = checkRange(-toInt)

    override def underlying: java.lang.Short

    def value: Short
  }

  private[math] final case class ValueImpl(value: Short) extends Value {
    override def intValue: Int = value
    override def longValue: Long = value
    override def floatValue: Float = value.floatValue
    override def doubleValue: Double = value.doubleValue
    override def underlying: java.lang.Short = value

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other.toLong
      case other: Char => value == other.toLong
      case other: Short => value == other.toLong
      case other: Int => value == other.toLong
      case other: Long => value == other.toLong
      case other: java.math.BigInteger => _Z(value) == _Z(other)
      case other: BigInt => _Z(value) == _Z(other)
      case _ => false
    }
  }
}

object _Z32 extends _ZT with LogikaNumberCompanion {
  final override def bitWidth = 32
  final override val Min: Z32 = ValueImpl(BigInt(-2).pow(bitWidth - 1).toInt)
  final override val Max: Z32 = ValueImpl((BigInt(2).pow(bitWidth - 1) - 1).toInt)

  final override def random: Z32 = ValueImpl(new Random().nextInt)

  private[math] final def checkRange(value: Long): Z32 = {
    if(!(Min.toLong <= value && value <= Max.toLong))
      throw new IllegalArgumentException
    ValueImpl(value.toInt)
  }

  private[math] final def checkRange(value: Z): Z32 = {
    if(!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(value.toInt)
  }

  sealed trait Value extends _Value {
    final def +(other: Z32): Z32 = checkRange(toLong + other.toLong)

    final def -(other: Z32): Z32 = checkRange(toLong - other.toLong)

    final def *(other: Z32): Z32 = checkRange(toLong * other.toLong)

    final def /(other: Z32): Z32 = checkRange(toLong / other.toLong)

    final def %(other: Z32): Z32 = checkRange(toLong % other.toLong)

    final def >(other: Z32): B = value > other.value

    final def >=(other: Z32): B = value >= other.value

    final def <(other: Z32): B = value < other.value

    final def <=(other: Z32): B = value <= other.value

    final def unary_-(): Z32 = checkRange(-toLong)

    override def underlying: java.lang.Integer

    def value: Int
  }

  private[math] final case class ValueImpl(value: Int) extends Value {
    override def intValue: Int = value
    override def longValue: Long = value
    override def floatValue: Float = value.floatValue
    override def doubleValue: Double = value.doubleValue
    override def underlying: java.lang.Integer = value

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other.toLong
      case other: Char => value == other.toLong
      case other: Short => value == other.toLong
      case other: Int => value == other.toLong
      case other: Long => value == other.toLong
      case other: java.math.BigInteger => _Z(value) == _Z(other)
      case other: BigInt => _Z(value) == _Z(other)
      case _ => false
    }
  }
}

object _Z64 extends _ZT with LogikaNumberCompanion {
  final override def bitWidth = 64
  final override val Min: Z64 = ValueImpl(BigInt(-2).pow(bitWidth - 1).toLong)
  final override val Max: Z64 = ValueImpl((BigInt(2).pow(bitWidth - 1) - 1).toLong)

  final override def random: Z64 = ValueImpl(new Random().nextLong)

  private[math] final def checkRange(value: Z): Z64 = {
    if(!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(value.toLong)
  }

  sealed trait Value extends _Value {
    final def +(other: Z64): Z64 = checkRange(toZ + other.toZ)

    final def -(other: Z64): Z64 = checkRange(toZ - other.toZ)

    final def *(other: Z64): Z64 = checkRange(toZ * other.toZ)

    final def /(other: Z64): Z64 = checkRange(toZ / other.toZ)

    final def %(other: Z64): Z64 = checkRange(toZ % other.toZ)

    final def >(other: Z64): B = value > other.value

    final def >=(other: Z64): B = value >= other.value

    final def <(other: Z64): B = value < other.value

    final def <=(other: Z64): B = value <= other.value

    final def unary_-(): Z64 = checkRange(-toZ)

    override def underlying: java.lang.Long

    def value: Long
  }

  private[math] final case class ValueImpl(value: Long) extends Value {
    override def intValue: Int = value.toInt
    override def longValue: Long = value
    override def floatValue: Float = value.floatValue
    override def doubleValue: Double = value.doubleValue
    override def underlying: java.lang.Long = value

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other.toLong
      case other: Char => value == other.toLong
      case other: Short => value == other.toLong
      case other: Int => value == other.toLong
      case other: Long => value == other.toLong
      case other: java.math.BigInteger => _Z(value) == _Z(other)
      case other: BigInt => _Z(value) == _Z(other)
      case _ => false
    }
  }
}
