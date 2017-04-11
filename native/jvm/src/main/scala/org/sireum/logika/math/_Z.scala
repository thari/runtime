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

import org.apfloat._
import org.sireum.logika.{B, Z}

object _Z {
  final private[logika] val intMin = new Apint(Int.MinValue)
  final private[logika] val intMax = new Apint(Int.MaxValue)
  final private[logika] val longMin = new Apint(Long.MinValue)
  final private[logika] val longMax = new Apint(Long.MaxValue)

  final val zero: Z = _Z(0)
  final val one: Z = _Z(1)

  lazy val Min: Z =
    if (defaultBitWidth == 0) sys.error("Unbounded integer does not have a minimum value.")
    else apply(BigInt(-2).pow(defaultBitWidth - 1))

  lazy val Max: Z =
    if (defaultBitWidth == 0) sys.error("Unbounded integer does not have a maximum value.")
    else apply(BigInt(2).pow(defaultBitWidth - 1) - 1)

  @inline
  final def apply(z: Int): Z = apply(new Apint(z))

  @inline
  final def apply(z: Long): Z = apply(new Apint(z))

  @inline
  final def apply(z: String): Z = {
    val s = z.replaceAll(" ", "")
    if (s.startsWith("0x")) apply(BigInt(s.substring(2), 16))
    else apply(BigInt(s))
  }

  @inline
  final def apply(z: BigInt): Z = apply(z.bigInteger)

  @inline
  final def apply(z: java.math.BigInteger): Z = apply(new Apint(z))

  @inline
  final def apply(z: Apint): Z = _ZApint(z).pack

  final def random: Z = _Z(BigInt(
    numbits = new scala.util.Random().nextInt(1024),
    rnd = new scala.util.Random()))
}

sealed trait _Z extends Comparable[_Z] {
  def unary_- : Z

  def +(other: Z): Z

  def -(other: Z): Z

  def *(other: Z): Z

  def /(other: Z): Z

  def %(other: Z): Z

  def >(other: Z): B

  def >=(other: Z): B

  def <(other: Z): B

  def <=(other: Z): B

  final def +(other: Int): Z = this + _Z(other)

  final def -(other: Int): Z = this - _Z(other)

  final def *(other: Int): Z = this * _Z(other)

  final def /(other: Int): Z = this / _Z(other)

  final def %(other: Int): Z = this % _Z(other)

  final def <(other: Int): B = this < _Z(other)

  final def <=(other: Int): B = this <= _Z(other)

  final def >(other: Int): B = this > _Z(other)

  final def >=(other: Int): B = this >= _Z(other)

  final def +(other: Long): Z = this + _Z(other)

  final def -(other: Long): Z = this - _Z(other)

  final def *(other: Long): Z = this * _Z(other)

  final def /(other: Long): Z = this / _Z(other)

  final def %(other: Long): Z = this % _Z(other)

  final def <(other: Long): B = this < _Z(other)

  final def <=(other: Long): B = this <= _Z(other)

  final def >(other: Long): B = this > _Z(other)

  final def >=(other: Long): B = this >= _Z(other)

  final def toZ: Z = this

  def toApint: Apint

  def toBigInt: BigInt

  override def hashCode: Int

  override def equals(other: Any): Boolean
}

private[logika] final case class _ZLong(value: Long) extends _Z {
  override def unary_- : Z =
    if (value == Long.MinValue) -upgrade else _Z(-value)

  override def +(other: Z): Z = other match {
    case _ZLong(n) =>
      val r = value + n
      if (((value ^ r) & (n ^ r)) < 0L) upgrade + other
      else _ZLong(r)
    case _ => upgrade + other
  }

  override def -(other: Z): Z = other match {
    case _ZLong(n) =>
      val r = value - n
      if (((value ^ r) & (n ^ r)) < 0L) upgrade - other
      else _ZLong(r)
    case _ => upgrade - other
  }

  override def *(other: Z): Z = other match {
    case _ZLong(n) =>
      val r = value * n
      if (r == 0) return _Z.zero
      if (n > value) {
        if (((n == -1) && (value == Long.MinValue)) || (r / n != value))
          return upgrade * other
      } else {
        if (((value == -1) && (n == Long.MinValue)) || (r / value != n))
          return upgrade * other
      }
      _ZLong(r)
    case _ => upgrade * other
  }

  override def /(other: Z): Z = other match {
    case _ZLong(n) =>
      val r = value / n
      if ((value == Long.MinValue) && (n == -1)) upgrade / other
      else _ZLong(r)
    case _ => upgrade / other
  }

  override def %(other: Z): Z = upgrade % other

  override def hashCode: Int = value.hashCode

  override def equals(other: Any): Boolean = other match {
    case _ZLong(n) => value == n
    case _ZApint(n) => toApint == n
    case other: Byte => value == other.toLong
    case other: Char => value == other.toLong
    case other: Short => value == other.toLong
    case other: Int => value == other.toLong
    case other: Long => value == other
    case other: spire.math.UByte => value == other.toLong
    case other: spire.math.UShort => value == other.toLong
    case other: spire.math.UInt => value == other.toLong
    case other: spire.math.ULong => upgrade == _Z(other.toBigInt)
    case other: java.math.BigInteger => upgrade == _Z(other)
    case other: BigInt => upgrade == _Z(other)
    case _ => false
  }

  override def compareTo(other: Z): Int = other match {
    case _ZLong(n) => value.compareTo(n)
    case _ => upgrade.compareTo(other)
  }

  override def >(other: Z): B = other match {
    case _ZLong(n) => value > n
    case _ => upgrade > other
  }

  override def >=(other: Z): B = other match {
    case _ZLong(n) => value >= n
    case _ => upgrade >= other
  }

  override def <(other: Z): B = other match {
    case _ZLong(n) => value < n
    case _ => upgrade < other
  }

  override def <=(other: Z): B = other match {
    case _ZLong(n) => value <= n
    case _ => upgrade <= other
  }

  override def toApint: Apint = new Apint(value)

  override def toBigInt: BigInt = BigInt(value)

  override def toString: String = value.toString

  private def upgrade: _ZApint = _ZApint(toApint)
}

private[logika] final case class _ZApint(value: Apint) extends _Z {
  def unary_- : Z = _ZApint(value.negate)

  def +(other: Z): Z = _ZApint(value.add(other.toApint))

  def -(other: Z): Z = _ZApint(value.subtract(other.toApint)).pack

  def *(other: Z): Z = _ZApint(value.multiply(other.toApint))

  def /(other: Z): Z = _ZApint(value.divide(other.toApint)).pack

  def %(other: Z): Z = _ZApint(value.mod(other.toApint)).pack

  override lazy val hashCode: Int = toApint.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: _ZLong => value == other.toApint
    case _ZApint(n) => value == n
    case other: Byte => value == new Apint(other)
    case other: Char => value == new Apint(other)
    case other: Short => value == new Apint(other)
    case other: Int => value == new Apint(other)
    case other: Long => value == new Apint(other)
    case other: spire.math.UByte => value == new Apint(other.toLong)
    case other: spire.math.UShort => value == new Apint(other.toLong)
    case other: spire.math.UInt => value == new Apint(other.toLong)
    case other: spire.math.ULong => value == new Apint(other.toBigInt.bigInteger)
    case other: java.math.BigInteger => value == new Apint(other)
    case other: BigInt => value == new Apint(other.bigInteger)
    case _ => false
  }

  override def compareTo(other: Z): Int = other match {
    case _ZLong(n) => value.compareTo(new Apint(n))
    case other: _ZApint => value.compareTo(other.value)
  }

  def <(other: Z): B = value.compareTo(other.toApint) < 0

  def <=(other: Z): B = value.compareTo(other.toApint) <= 0

  def >(other: Z): B = value.compareTo(other.toApint) > 0

  def >=(other: Z): B = value.compareTo(other.toApint) >= 0

  override def toApint: Apint = value

  override def toBigInt: BigInt = BigInt(value.toBigInteger)

  override def toString: String = value.toString

  private[math] def pack: Z =
    if ((value.compareTo(_Z.longMin) >= 0) && (value.compareTo(_Z.longMax) <= 0))
      _ZLong(value.longValue)
    else this
}
