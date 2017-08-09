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

package org.sireum.math

import org.sireum._Type.Alias._
import org.sireum.ops.ZOps_Ext
import org.sireum.{_Jsonable, _Range}

object _Z {
  final private[sireum] val intMin = BigInt(Int.MinValue)
  final private[sireum] val intMax = BigInt(Int.MaxValue)
  final private[sireum] val longMin = BigInt(Long.MinValue)
  final private[sireum] val longMax = BigInt(Long.MaxValue)

  final val zero: Z = _Z(0)
  final val one: Z = _Z(1)

  @inline
  final def apply(z: Int): Z = _ZLong(z)

  @inline
  final def apply(z: Long): Z = _ZLong(z)

  @inline
  final def apply(z: Predef.String): Z = {
    val s = z.replaceAll(" ", "")
    if (s.startsWith("0x")) apply(BigInt(s.substring(2), 16))
    else apply(BigInt(s))
  }

  @inline
  final def apply(z: BigInt): Z = _ZBigInt(z).pack

  @inline
  final def apply(z: java.math.BigInteger): Z = apply(BigInt(z))

  final def random: Z = apply(BigInt(
    numbits = new scala.util.Random().nextInt(1024),
    rnd = new scala.util.Random()))
}

sealed trait _Z extends Comparable[_Z] with _Jsonable {

  @inline final def unary_- : Z = ZOps_Ext.unary_-(this)

  @inline final def +(other: Z): Z = ZOps_Ext.+(this, other)

  @inline final def -(other: Z): Z = ZOps_Ext.-(this, other)

  @inline final def *(other: Z): Z = ZOps_Ext.*(this, other)

  @inline final def /(other: Z): Z = ZOps_Ext./(this, other)

  @inline final def %(other: Z): Z = ZOps_Ext.%(this, other)

  @inline final def >(other: Z): B = ZOps_Ext.>(this, other)

  @inline final def >=(other: Z): B = ZOps_Ext.>=(this, other)

  @inline final def <(other: Z): B = ZOps_Ext.<(this, other)

  @inline final def <=(other: Z): B = ZOps_Ext.<=(this, other)

  @inline final def until(hi: Z): _Range[Z] = new _Range(this, i => i < hi, (n, m) => n + m, 1)

  @inline final def to(hi: Z): _Range[Z] = new _Range(this, i => i <= hi, (n, m) => n + m, 1)

  @inline final def +(other: Int): Z = this + _ZLong(other)

  @inline final def -(other: Int): Z = this - _ZLong(other)

  @inline final def *(other: Int): Z = this * _ZLong(other)

  @inline final def /(other: Int): Z = this / _ZLong(other)

  @inline final def %(other: Int): Z = this % _ZLong(other)

  @inline final def <(other: Int): B = this < _ZLong(other)

  @inline final def <=(other: Int): B = this <= _ZLong(other)

  @inline final def >(other: Int): B = this > _ZLong(other)

  @inline final def >=(other: Int): B = this >= _ZLong(other)

  @inline final def +(other: Long): Z = this + _ZLong(other)

  @inline final def -(other: Long): Z = this - _ZLong(other)

  @inline final def *(other: Long): Z = this * _ZLong(other)

  @inline final def /(other: Long): Z = this / _ZLong(other)

  @inline final def %(other: Long): Z = this % _ZLong(other)

  @inline final def <(other: Long): B = this < _ZLong(other)

  @inline final def <=(other: Long): B = this <= _ZLong(other)

  @inline final def >(other: Long): B = this > _ZLong(other)

  @inline final def >=(other: Long): B = this >= _ZLong(other)

  @inline final def hash: Z = ZOps_Ext.hash(this)

  @inline final def isEqual(other: Z): B = ZOps_Ext.isEqual(this, other)

  @inline final override def hashCode: Int = hash.toInt

  @inline final def string: String = ZOps_Ext.string(this)

  final override def equals(other: Any): Boolean = other match {
    case other: Z => ZOps_Ext.isEqual(this, other).value
    case other: Byte => ZOps_Ext.isEqual(this, _ZLong(other.toLong))
    case other: Char => ZOps_Ext.isEqual(this, _ZLong(other.toLong))
    case other: Short => ZOps_Ext.isEqual(this, _ZLong(other.toLong))
    case other: Int => ZOps_Ext.isEqual(this, _ZLong(other.toLong))
    case other: Long => ZOps_Ext.isEqual(this, _ZLong(other.toLong))
    case other: java.math.BigInteger => ZOps_Ext.isEqual(this, BigInt(other))
    case other: BigInt => ZOps_Ext.isEqual(this, other)
    case _ => false
  }

  final override def compareTo(other: Z): Int = (this, other) match {
    case (_ZLong(n1), _ZLong(n2)) => n1.compareTo(n2)
    case _ => this.toBigInt.compareTo(other.toBigInt)
  }

  private[sireum] final def toByte: Byte = this match {
    case _ZLong(n) => n.toByte
    case _ZBigInt(n) => n.toByte
  }

  private[sireum] final def toShort: Short = this match {
    case _ZLong(n) => n.toShort
    case _ZBigInt(n) => n.toShort
  }

  final def toInt: Int = this match {
    case _ZLong(n) => n.toInt
    case _ZBigInt(n) => n.toInt
  }

  private[sireum] final def toLong: Long = this match {
    case _ZLong(n) => n
    case _ZBigInt(n) => n.toLong
  }

  private[sireum] final def toBigInt: BigInt = this match {
    case _ZLong(n) => BigInt(n)
    case _ZBigInt(n) => n
  }

  @inline final override def toString: Predef.String = this match {
    case _ZLong(n) => n.toString
    case _ => toBigInt.toString
  }

  final def toZ: Z = this
}

private[sireum] final case class _ZLong(value: Long) extends _Z

private[sireum] final case class _ZBigInt(value: BigInt) extends _Z {
  private[sireum] def pack: Z =
    if ((value.compareTo(_Z.longMin) >= 0) && (value.compareTo(_Z.longMax) <= 0))
      _ZLong(value.longValue)
    else this
}