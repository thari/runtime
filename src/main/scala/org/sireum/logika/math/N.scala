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

import org.sireum.logika._

object N {

  final val zero: N = NImpl(Z.zero)
  final val one: N = NImpl(Z.one)

  @inline
  final def apply(n: Int): N = apply(Z(n))

  @inline
  final def apply(n: Long): N = apply(Z(n))

  @inline
  final def apply(n: String): N = apply(Z(n))

  @inline
  final def apply(n: BigInt): N = apply(Z(n))

  @inline
  final def apply(n: java.math.BigInteger): N = apply(Z(n))

  @inline
  final def apply(n: Apint): N = apply(Z(n))

  @inline
  final def apply(z: Z): N = if (z <= 0) zero else NImpl(z)
}

sealed trait N extends ScalaNumericConversions with Comparable[N] {
  def +(other: N): N = N(toZ + other.toZ)

  def -(other: N): N = N(toZ - other.toZ)

  def *(other: N): N = N(toZ * other.toZ)

  def /(other: N): N = N(toZ / other.toZ)

  def %(other: N): N = N(toZ % other.toZ)

  def >(other: N): B = toZ > other.toZ

  def >=(other: N): B = toZ >= other.toZ

  def <(other: N): B = toZ < other.toZ

  def <=(other: N): B = toZ <= other.toZ

  def +(other: Int): N = this + N(other)

  def -(other: Int): N = this - N(other)

  def *(other: Int): N = this * N(other)

  def /(other: Int): N = this / N(other)

  def %(other: Int): N = this % N(other)

  def <(other: Int): B = this < N(other)

  def <=(other: Int): B = this <= N(other)

  def >(other: Int): B = this > N(other)

  def >=(other: Int): B = this >= N(other)

  def +(other: Long): N = this + N(other)

  def -(other: Long): N = this - N(other)

  def *(other: Long): N = this * N(other)

  def /(other: Long): N = this / N(other)

  def %(other: Long): N = this % N(other)

  def <(other: Long): B = this < N(other)

  def <=(other: Long): B = this <= N(other)

  def >(other: Long): B = this > N(other)

  def >=(other: Long): B = this >= N(other)

  def +(other: Z): Z = toZ + other

  def -(other: Z): Z = toZ - other

  def *(other: Z): Z = toZ * other

  def /(other: Z): Z = toZ / other

  def %(other: Z): Z = toZ % other

  def >(other: Z): B = toZ > other

  def >=(other: Z): B = toZ >= other

  def <(other: Z): B = toZ < other

  def <=(other: Z): B = toZ <= other

  def toZ: Z

  def toBigInteger: java.math.BigInteger = toZ.toBigInteger

  def toBigInt: BigInt = toZ.toBigInt

  def toZApint: Apint = toZ.toZApint

  final override def doubleValue = toBigInt.doubleValue

  final override def floatValue = toBigInt.floatValue

  final override def intValue = toBigInt.intValue

  final override def longValue = toBigInt.longValue

  final override def underlying = toBigInteger

  final override def isWhole = true
}

final private case class NImpl(value: Z) extends N {
  def toZ: Z = value

  override lazy val hashCode: Int = toBigInt.hashCode

  override def equals(other: Any): B = other match {
    case NImpl(z) => (value eq z) || value.equals(z)
    case other: Z => (value eq other) || value.equals(other)
    case other: Byte => Z(new Apint(other)) == value
    case other: Char => Z(new Apint(other)) == value
    case other: Short => Z(new Apint(other)) == value
    case other: Int => Z(new Apint(other)) == value
    case other: Long => Z(new Apint(other)) == value
    case other: java.math.BigInteger => Z(new Apint(other)) == value
    case other: BigInt => Z(new Apint(other.bigInteger)) == value
    case _ => false
  }

  override def compareTo(other: N): Int = other match {
    case other: NImpl => value.compareTo(other.value)
  }
}
