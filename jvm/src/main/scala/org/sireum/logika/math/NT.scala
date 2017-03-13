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
import org.sireum.logika.B

sealed trait NT {
  final val Min: Value = ValueImpl(Z.zero)
  final val Max: Value = ValueImpl(Z(BigInt(2).pow(bitWidth)) - 1)

  def bitWidth: Int

  private[math] final def checkRange(value: Z): Value = {
    if (!(Min.toZ <= value && value <= Max.toZ))
      throw new IllegalArgumentException
    ValueImpl(value)
  }

  sealed trait Value extends ScalaNumericConversions with Comparable[Value] with LogikaIntegralNumber {
    final def bitWidth: Int = NT.this.bitWidth

    final def +(other: Value): Value = checkRange(toZ + other.toZ)

    final def -(other: Value): Value = checkRange(toZ - other.toZ)

    final def *(other: Value): Value = checkRange(toZ * other.toZ)

    final def /(other: Value): Value = checkRange(toZ / other.toZ)

    final def %(other: Value): Value = checkRange(toZ % other.toZ)

    final def >(other: Value): B = toZ > other.toZ

    final def >=(other: Value): B = toZ >= other.toZ

    final def <(other: Value): B = toZ < other.toZ

    final def <=(other: Value): B = toZ <= other.toZ

    final def toBigInteger: java.math.BigInteger = toBigInt.bigInteger

    final def toBigInt: BigInt = toZ.toBigInt

    final def toApint: Apint = new Apint(toBigInteger)

    final override def doubleValue: Double = toZ.toDouble

    final override def floatValue: Float = toZ.toFloat

    final override def intValue: Int = toZ.toInt

    final override def longValue: Long = toZ.toLong

    final override def underlying: Z = toZ

    final override def compareTo(other: Value): Int =
      toZ.compareTo(other.toZ)

    final override def isWhole: Boolean = true

    final override def toString: String = toZ.toString
  }

  private[math] final case class ValueImpl(value: Z) extends Value {
    override val hashCode: Int = value.hashCode

    override def toZ: Z = value

    override def equals(other: Any): Boolean = other match {
      case other: LogikaIntegralNumber => (this eq other) || value.equals(other.toZ)
      case other: Byte => value == Z(other)
      case other: Char => value == Z(other)
      case other: Short => value == Z(other)
      case other: Int => value == Z(other)
      case other: Long => value == Z(other)
      case other: java.math.BigInteger => value == Z(other)
      case other: BigInt => value == Z(other)
      case _ => false
    }
  }

}

object N8 extends NT with LogikaNumberCompanion {
  final override def bitWidth = 8

  final override def random: N8.Value =
    N8.ValueImpl(Z(new Random().nextInt.toByte) + Z8.Max.toZ + 1)
}

object N16 extends NT with LogikaNumberCompanion {
  final override def bitWidth = 16

  final override def random: N16.Value =
    N16.ValueImpl(Z(new Random().nextInt.toShort) + Z16.Max.toZ + 1)
}

object N32 extends NT with LogikaNumberCompanion {
  final override def bitWidth = 32

  final override def random: N32.Value =
    N32.ValueImpl(Z(new Random().nextInt) + Z32.Max.toZ + 1)
}

object N64 extends NT with LogikaNumberCompanion {
  final override def bitWidth = 64

  final override def random: N64.Value =
    N64.ValueImpl(Z(new Random().nextLong) + Z64.Max.toZ + 1)
}
