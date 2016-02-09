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

sealed trait ZRange {
  final val Min: Value = ValueImpl(BigInt(-2).pow(bitWidth).toLong)
  final val Max: Value = ValueImpl((BigInt(2).pow(bitWidth) - 1).toLong)

  def bitWidth: Int

  private[math] final def checkRange(value: Z): Value = {
    assert(Z(Min.toLong) <= value && value <= Z(Max.toLong))
    ValueImpl(value.toLong)
  }

  sealed trait Value extends ScalaNumericConversions with Comparable[Value] with LogikaIntegralNumber {
    final def +(other: Value): Value = checkRange(toZ + other.toZ)

    final def -(other: Value): Value = checkRange(toZ - other.toZ)

    final def *(other: Value): Value = checkRange(toZ * other.toZ)

    final def /(other: Value): Value = checkRange(toZ / other.toZ)

    final def %(other: Value): Value = checkRange(toZ % other.toZ)

    final def >(other: Value): B = toLong > other.toLong

    final def >=(other: Value): B = toLong >= other.toLong

    final def <(other: Value): B = toLong < other.toLong

    final def <=(other: Value): B = toLong <= other.toLong

    final def toZ: Z = Z(toLong)

    final def toBigInteger: java.math.BigInteger = BigInt(toLong).bigInteger

    final def toBigInt: BigInt = BigInt(toLong)

    final def toApint: Apint = new Apint(toBigInteger)

    final override def doubleValue = toLong.doubleValue

    final override def floatValue = toLong.floatValue

    final override def intValue = toLong.intValue

    final override def underlying = new java.lang.Long(toLong)

    final override def compareTo(other: Value): Int =
      toLong.compareTo(other.toLong)

    final override def isWhole: B = true

    final override def toString: String = toLong.toString
  }

  private[math] final case class ValueImpl(value: Long) extends Value {
    override def longValue = value

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): B = other match {
      case other: LogikaIntegralNumber => (this eq other) || toZ.equals(other.toZ)
      case other: Byte => value == other.toLong
      case other: Char => value == other.toLong
      case other: Short => value == other.toLong
      case other: Int => value == other.toLong
      case other: Long => value == other.toLong
      case other: java.math.BigInteger => Z(value) == Z(other)
      case other: BigInt => Z(value) == Z(other)
      case _ => false
    }
  }

}

object Z8 extends ZRange with LogikaNumberCompanion {
  final override def bitWidth = 8

  final override def random: Z8 = Z8.ValueImpl(new Random().nextInt.toByte)
}

object Z16 extends ZRange with LogikaNumberCompanion {
  final override def bitWidth = 16

  final override def random: Z16 = Z16.ValueImpl(new Random().nextInt.toShort)
}

object Z32 extends ZRange with LogikaNumberCompanion {
  final override def bitWidth = 32

  final override def random: Z32 = Z32.ValueImpl(new Random().nextInt)
}

object Z64 extends ZRange with LogikaNumberCompanion {
  final override def bitWidth = 64

  final override def random: Z64 = Z64.ValueImpl(new Random().nextLong)
}
