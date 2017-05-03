/*
 Copyright (c) 2017, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.math

import org.sireum._
import spire.math._

private[sireum] object Numbers {

  val z8Min: Z = _Z(Byte.MinValue)
  val z8Max: Z = _Z(Byte.MaxValue)
  val z16Min: Z = _Z(Short.MinValue)
  val z16Max: Z = _Z(Short.MaxValue)
  val z32Min: Z = _Z(Int.MinValue)
  val z32Max: Z = _Z(Int.MaxValue)
  val z64Min: Z = _Z(Long.MinValue)
  val z64Max: Z = _Z(Long.MaxValue)
  val nMin: Z = _Z.zero
  val n8Min: Z = _Z.zero
  val un8Max: Int = UByte.MaxValue.toInt
  val n8Max: Z = _Z(un8Max)
  val n16Min: Z = _Z.zero
  val un16Max: Int = UShort.MaxValue.toInt
  val n16Max: Z = _Z(un16Max)
  val n32Min: Z = _Z.zero
  val un32Max: Long = UInt.MaxValue.toLong
  val n32Max: Z = _Z(un32Max)
  val n64Min: Z = _Z.zero
  val n64Max: Z = _Z(ULong.MaxValue.toBigInt)

  def toZ(n: Int): Z = org.sireum.math._Z(n)

  def toZ(n: Long): Z = org.sireum.math._Z(n)

  def toZ(n: BigInt): Z = org.sireum.math._Z(n)

  def toZ8(n: Int): Z8 = {
    require(Byte.MinValue <= n && n <= Byte.MaxValue)
    _Z8(n.toByte)
  }

  def toZ8(n: Z): Z8 = {
    require(z8Min <= n && n <= z8Max)
    _Z8(n.toByte)
  }

  def toZ16(n: Int): Z16 = {
    require(Short.MinValue <= n && n <= Short.MaxValue)
    _Z16(n.toShort)
  }

  def toZ16(n: Z): Z16 = {
    require(z16Min <= n && n <= z16Max)
    _Z16(n.toShort)
  }

  def toZ32(n: Long): Z32 = {
    require(Int.MinValue <= n && n <= Int.MaxValue)
    _Z32(n.toInt)
  }

  def toZ32(n: Z): Z32 = {
    require(z32Min <= n && n <= z32Max)
    _Z32(n.toInt)
  }

  def toZ64(n: Long): Z64 = _Z64(n)

  def toZ64(n: Z): Z64 = {
    require(z64Min <= n && n <= z64Max)
    _Z64(n.toLong)
  }

  def toS8(n: Int): S8 = _S8(n.toByte)

  def toS8(n: Z): S8 = _S8(n.toByte)

  def toS8Exact(n: Int): S8 = {
    require(Byte.MinValue <= n && n <= Byte.MaxValue)
    toS8(n)
  }

  def toS8Exact(n: Z): S8 = {
    require(z8Min <= n && n <= z8Max)
    toS8(n)
  }

  def toS16(n: Int): S16 = _S16(n.toShort)

  def toS16(n: Z): S16 = _S16(n.toShort)

  def toS16Exact(n: Int): S16 = {
    require(Short.MinValue <= n && n <= Short.MaxValue)
    toS16(n)
  }

  def toS16Exact(n: Z): S16 = {
    require(z16Min <= n && n <= z16Max)
    toS16(n)
  }

  def toS32(n: Int): S32 = _S32(n)

  def toS32(n: Z): S32 = _S32(n.toInt)

  def toS32Exact(n: Long): S32 = {
    require(Int.MinValue <= n && n <= Int.MaxValue)
    toS32(n)
  }

  def toS32Exact(n: Z): S32 = {
    require(z32Min <= n && n <= z32Max)
    toS32(n)
  }

  def toS64(n: Long): S64 = _S64(n)

  def toS64(n: Z): S64 = _S64(n.toLong)

  def toS64Exact(n: Int): S64 = {
    require(Long.MinValue <= n && n <= Long.MaxValue)
    toS64(n)
  }

  def toS64Exact(n: Z): S64 = {
    require(z64Min <= n && n <= z64Max)
    toS64(n)
  }

  def toN(n: Int): N = _N(n)

  def toN(n: Long): N = _N(n)

  def toN(n: Z): N = _N(n)

  def toN8(n: Int): N8 = {
    require(0 <= n && n <= un8Max)
    _N8(UByte(n).signed)
  }

  def toN8(n: Z): N8 = {
    require(n8Min <= n && n <= n8Max)
    _N8(UByte(n.toInt).signed)
  }

  def toN16(n: Int): N16 = {
    require(0 <= n && n <= un16Max)
    _N16(UShort(n).signed.toShort)
  }

  def toN16(n: Z): N16 = {
    require(n16Min <= n && n <= n16Max)
    _N16(UShort(n.toInt).signed.toShort)
  }

  def toN32(n: Long): N32 = {
    require(0 <= n && n <= un32Max)
    _N32(UInt(n).signed)
  }

  def toN32(n: Z): N32 = {
    require(n32Min <= n && n <= n32Max)
    _N32(UInt(n.toLong).signed)
  }

  def toN64(n: Long): N64 = {
    require(0 <= n)
    _N64(ULong(n).signed)
  }

  def toN64(n: Z): N64 = {
    require(n64Min <= n && n <= n64Max)
    _N64(ULong.fromBigInt(n.toBigInt).signed)
  }

  def toU8(n: UByte): U8 = _U8(n.signed)

  def toU8(n: Z): U8 = _U8(UByte(n.toInt).signed)

  def toU8Exact(n: Int): U8 = {
    require(0 <= n && n <= un8Max)
    toU8(n)
  }

  def toU8Exact(n: Z): U8 = {
    require(n8Min <= n && n <= n8Max)
    toU8(n)
  }

  def toU16(n: UShort): U16 = _U16(n.signed.toShort)

  def toU16(n: Z): U16 = _U16(UShort(n.toInt).signed.toShort)

  def toU16Exact(n: Int): U16 = {
    require(0 <= n && n <= un16Max)
    toU16(n)
  }

  def toU16Exact(n: Z): U16 = {
    require(n16Min <= n && n <= n16Max)
    toU16(n)
  }

  def toU32(n: UInt): U32 = _U32(n.signed)

  def toU32(n: Z): U32 = _U32(UInt(n.toLong).signed)

  def toU32Exact(n: Long): U32 = {
    require(0 <= n && n <= un32Max)
    toU32(n)
  }

  def toU32Exact(n: Z): U32 = {
    require(n32Min <= n && n <= n32Max)
    toU32(n)
  }

  def toU64(n: ULong): U64 = _U64(n.signed)

  def toU64(n: Z): U64 = _U64(ULong.fromBigInt(n.toBigInt).signed)

  def toU64Exact(n: Long): U64 = {
    require(0 <= n)
    toU64(n)
  }

  def toU64Exact(n: Z): U64 = {
    require(n64Min <= n && n <= n64Max)
    toU64(n)
  }

  def toF32(n: Float): F32 = _F32(n)

  def toF64(n: Double): F64 = _F64(n)

  def toR(s: Predef.String): R = _R(spire.math.Real(s))

  def toR(n: Real): R = _R(n)
}

import Numbers._

object _Z8 {
  def apply(value: Byte): Z8 = new _Z8(value)
}

final class _Z8(val value: Byte) extends AnyVal with _Rich {
  private type T = Z8

  def unary_- : T = toZ8(-value)
  def +(other: T): T = toZ8(value.toInt + other.value.toInt)
  def -(other: T): T = toZ8(value.toInt - other.value.toInt)
  def *(other: T): T = toZ8(value.toInt * other.value.toInt)
  def /(other: T): T = toZ8(value.toInt / other.value.toInt)
  def %(other: T): T = toZ8(value.toInt % other.value.toInt)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _Z16 {
  def apply(value: Short): Z16 = new _Z16(value)
}

final class _Z16(val value: Short) extends AnyVal with _Rich {
  private type T = Z16

  def unary_- : T = toZ16(-value)
  def +(other: T): T = toZ16(value.toInt + other.value.toInt)
  def -(other: T): T = toZ16(value.toInt - other.value.toInt)
  def *(other: T): T = toZ16(value.toInt * other.value.toInt)
  def /(other: T): T = toZ16(value.toInt / other.value.toInt)
  def %(other: T): T = toZ16(value.toInt % other.value.toInt)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _Z32 {
  def apply(value: Int): Z32 = new _Z32(value)
}

final class _Z32(val value: Int) extends AnyVal with _Rich {
  private type T = Z32

  def unary_- : T = toZ32(-value)
  def +(other: T): T = toZ32(value.toLong + other.value.toLong)
  def -(other: T): T = toZ32(value.toLong - other.value.toLong)
  def *(other: T): T = toZ32(value.toLong * other.value.toLong)
  def /(other: T): T = toZ32(value.toLong / other.value.toLong)
  def %(other: T): T = toZ32(value.toLong % other.value.toLong)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _Z64 {
  def apply(value: Long): Z64 = new _Z64(value)
}

final class _Z64(val value: Long) extends AnyVal with _Rich {
  private type T = Z64

  def unary_- : T = toZ64(-value)
  def +(other: T): T = toZ64(_Z(value) + _Z(other.value))
  def -(other: T): T = toZ64(_Z(value) - _Z(other.value))
  def *(other: T): T = toZ64(_Z(value) * _Z(other.value))
  def /(other: T): T = toZ64(_Z(value) / _Z(other.value))
  def %(other: T): T = toZ64(_Z(value) % _Z(other.value))
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _S8 {
  def apply(value: Byte): S8 = new _S8(value)
}

final class _S8(val value: Byte) extends AnyVal with _Rich {
  private type T = S8

  def unary_- : T = toS8(-value)
  def unary_~ : T = toS8(~value)
  def +(other: T): T = toS8(value + other.value)
  def -(other: T): T = toS8(value - other.value)
  def *(other: T): T = toS8(value * other.value)
  def /(other: T): T = toS8(value / other.value)
  def %(other: T): T = toS8(value % other.value)
  def <<(other: T): T = toS8(value << other.value)
  def >>(other: T): T = toS8(value >> other.value)
  def >>>(other: T): T = toS8(value >>> other.value)
  def &(other: T): T = toS8(value & other.value)
  def |(other: T): T = toS8(value | other.value)
  def |^(other: T): T = toS8(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _S16 {
  def apply(value: Short): S16 = new _S16(value)
}

final class _S16(val value: Short) extends AnyVal with _Rich {
  private type T = S16

  def unary_- : T = toS16(-value)
  def unary_~ : T = toS16(~value)
  def +(other: T): T = toS16(value + other.value)
  def -(other: T): T = toS16(value - other.value)
  def *(other: T): T = toS16(value * other.value)
  def /(other: T): T = toS16(value / other.value)
  def %(other: T): T = toS16(value % other.value)
  def <<(other: T): T = toS16(value << other.value)
  def >>(other: T): T = toS16(value >> other.value)
  def >>>(other: T): T = toS16(value >>> other.value)
  def &(other: T): T = toS16(value & other.value)
  def |(other: T): T = toS16(value | other.value)
  def |^(other: T): T = toS16(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _S32 {
  def apply(value: Int): S32 = new _S32(value)
}

final class _S32(val value: Int) extends AnyVal {
  private type T = S32

  def unary_- : T = toS32(-value)
  def unary_~ : T = toS32(~value)
  def +(other: T): T = toS32(value + other.value)
  def -(other: T): T = toS32(value - other.value)
  def *(other: T): T = toS32(value * other.value)
  def /(other: T): T = toS32(value / other.value)
  def %(other: T): T = toS32(value % other.value)
  def <<(other: T): T = toS32(value << other.value)
  def >>(other: T): T = toS32(value >> other.value)
  def >>>(other: T): T = toS32(value >>> other.value)
  def &(other: T): T = toS32(value & other.value)
  def |(other: T): T = toS32(value | other.value)
  def |^(other: T): T = toS32(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _S64 {
  def apply(value: Long): S64 = new _S64(value)
}

final class _S64(val value: Long) extends AnyVal with _Rich {
  private type T = S64

  def unary_- : T = toS64(-value)
  def unary_~ : T = toS64(~value)
  def +(other: T): T = toS64(value + other.value)
  def -(other: T): T = toS64(value - other.value)
  def *(other: T): T = toS64(value * other.value)
  def /(other: T): T = toS64(value / other.value)
  def %(other: T): T = toS64(value % other.value)
  def <<(other: T): T = toS64(value << other.value)
  def >>(other: T): T = toS64(value >> other.value)
  def >>>(other: T): T = toS64(value >>> other.value)
  def &(other: T): T = toS64(value & other.value)
  def |(other: T): T = toS64(value | other.value)
  def |^(other: T): T = toS64(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _N8 {
  def apply(value: Byte): N8 = new _N8(value)
}

final class _N8(val signed: Byte) extends AnyVal with _Rich {
  private type T = N8

  private[sireum] def value: UByte = UByte(signed)
  def +(other: T): T = toN8(value.toInt + other.value.toInt)
  def -(other: T): T = toN8(value.toInt - other.value.toInt)
  def *(other: T): T = toN8(value.toInt * other.value.toInt)
  def /(other: T): T = toN8(value.toInt / other.value.toInt)
  def %(other: T): T = toN8(value.toInt % other.value.toInt)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _N16 {
  def apply(value: Short): N16 = new _N16(value)
}

final class _N16(val signed: Short) extends AnyVal with _Rich {
  private type T = N16

  private[sireum] def value: UShort = UShort(signed)
  def +(other: T): T = toN16(value.toInt + other.value.toInt)
  def -(other: T): T = toN16(value.toInt - other.value.toInt)
  def *(other: T): T = toN16(value.toInt * other.value.toInt)
  def /(other: T): T = toN16(value.toInt / other.value.toInt)
  def %(other: T): T = toN16(value.toInt % other.value.toInt)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _N32 {
  def apply(value: Int): N32 = new _N32(value)
}

final class _N32(val signed: Int) extends AnyVal {
  private type T = N32

  private[sireum] def value: UInt = UInt(signed)
  def +(other: T): T = toN32(value.toLong + other.value.toLong)
  def -(other: T): T = toN32(value.toLong - other.value.toLong)
  def *(other: T): T = toN32(value.toLong * other.value.toLong)
  def /(other: T): T = toN32(value.toLong / other.value.toLong)
  def %(other: T): T = toN32(value.toLong % other.value.toLong)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _N64 {
  def apply(value: Long): N64 = new _N64(value)
}

final class _N64(val signed: Long) extends AnyVal with _Rich {
  private type T = N64

  private[sireum] def value: ULong = ULong.fromLong(signed)
  def +(other: T): T = toN64(_Z(value.toBigInt) + _Z(other.value.toBigInt))
  def -(other: T): T = toN64(_Z(value.toBigInt) - _Z(other.value.toBigInt))
  def *(other: T): T = toN64(_Z(value.toBigInt) * _Z(other.value.toBigInt))
  def /(other: T): T = toN64(_Z(value.toBigInt) / _Z(other.value.toBigInt))
  def %(other: T): T = toN64(_Z(value.toBigInt) % _Z(other.value.toBigInt))
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _U8 {
  def apply(value: Byte): U8 = new _U8(value)
}

final class _U8(val signed: Byte) extends AnyVal with _Rich {
  private type T = U8

  private[sireum] def value: UByte = UByte(signed)
  def unary_~ : T = toU8(~value)
  def +(other: T): T = toU8(value + other.value)
  def -(other: T): T = toU8(value - other.value)
  def *(other: T): T = toU8(value * other.value)
  def /(other: T): T = toU8(value / other.value)
  def %(other: T): T = toU8(value % other.value)
  def <<(other: T): T = _U8((value << other.value.toInt).signed)
  def >>>(other: T): T = _U8((value >>> other.value.toInt).signed)
  def &(other: T): T = toU8(value & other.value)
  def |(other: T): T = toU8(value | other.value)
  def |^(other: T): T = toU8(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _U16 {
  def apply(value: Short): U16 = new _U16(value)
}

final class _U16(val signed: Short) extends AnyVal with _Rich {
  private type T = U16

  private[sireum] def value: UShort = UShort(signed)
  def unary_~ : T = toU16(~value)
  def +(other: T): T = toU16(value + other.value)
  def -(other: T): T = toU16(value - other.value)
  def *(other: T): T = toU16(value * other.value)
  def /(other: T): T = toU16(value / other.value)
  def %(other: T): T = toU16(value % other.value)
  def <<(other: T): T = _U16((value << other.value.toInt).signed.toShort)
  def >>>(other: T): T = _U16((value >>> other.value.toInt).signed.toShort)
  def &(other: T): T = toU16(value & other.value)
  def |(other: T): T = toU16(value | other.value)
  def |^(other: T): T = toU16(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _U32 {
  def apply(value: Int): U32 = new _U32(value)
}

final class _U32(val signed: Int) extends AnyVal with _Rich {
  private type T = U32

  private[sireum] def value: UInt = UInt(signed)
  def unary_~ : T = toU32(~value)
  def +(other: T): T = toU32(value + other.value)
  def -(other: T): T = toU32(value - other.value)
  def *(other: T): T = toU32(value * other.value)
  def /(other: T): T = toU32(value / other.value)
  def %(other: T): T = toU32(value % other.value)
  def <<(other: T): T = _U32((value << other.value.toInt).signed)
  def >>>(other: T): T = _U32((value >>> other.value.toInt).signed)
  def &(other: T): T = toU32(value & other.value)
  def |(other: T): T = toU32(value | other.value)
  def |^(other: T): T = toU32(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}

object _U64 {
  def apply(value: Long): U64 = new _U64(value)
}

final class _U64(val signed: Long) extends AnyVal with _Rich {
  private type T = U64

  private[sireum] def value: ULong = ULong.fromLong(signed)
  def unary_~ : T = toU64(~value)
  def +(other: T): T = toU64(value + other.value)
  def -(other: T): T = toU64(value - other.value)
  def *(other: T): T = toU64(value * other.value)
  def /(other: T): T = toU64(value / other.value)
  def %(other: T): T = toU64(value % other.value)
  def <<(other: T): T = _U64((value << other.value.toInt).signed)
  def >>>(other: T): T = _U64((value >>> other.value.toInt).signed)
  def &(other: T): T = toU64(value & other.value)
  def |(other: T): T = toU64(value | other.value)
  def |^(other: T): T = toU64(value ^ other.value)
  def <(other: T): B = value < other.value
  def <=(other: T): B = value <= other.value
  def >(other: T): B = value > other.value
  def >=(other: T): B = value >= other.value
  def ===(other: T): B = value == other.value
  def =!=(other: T): B = value != other.value
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}


object _F32 {
  def apply(value: Float): F32 = new _F32(value)
}

final class _F32(val value: Float) extends AnyVal with _Rich {
  private type T = F32

  def unary_- : T = toF32(-value)
  def +(other: T): T = toF32(value + other.value)
  def -(other: T): T = toF32(value - other.value)
  def *(other: T): T = toF32(value * other.value)
  def /(other: T): T = toF32(value / other.value)
  def %(other: T): T = toF32(value % other.value)
  def <(other: T): B = value < other.value
  def >(other: T): B = value > other.value
  override def toString: Predef.String = value.toString
}


object _F64 {
  def apply(value: Double): F64 = new _F64(value)
}

final class _F64(val value: Double) extends AnyVal with _Rich {
  private type T = F64

  def unary_- : T = toF64(-value)
  def +(other: T): T = toF64(value + other.value)
  def -(other: T): T = toF64(value - other.value)
  def *(other: T): T = toF64(value * other.value)
  def /(other: T): T = toF64(value / other.value)
  def %(other: T): T = toF64(value % other.value)
  def <(other: T): B = value < other.value
  def >(other: T): B = value > other.value
  override def toString: Predef.String = value.toString
}


object _R {
  def apply(value: Real): R = new _R(value)
}

final class _R(val value: Real) extends AnyVal with _Rich {
  private type T = R

  def unary_- : T = toR(-value)
  def +(other: T): T = toR(value + other.value)
  def -(other: T): T = toR(value - other.value)
  def *(other: T): T = toR(value * other.value)
  def /(other: T): T = toR(value / other.value)
  def %(other: T): T = toR(value % other.value)
  def <(other: T): B = value.compare(other.value) < 0
  def >(other: T): B = value.compare(other.value) > 0
  def <=(other: T): B = value.compare(other.value) <= 0
  def >=(other: T): B = value.compare(other.value) >= 0
  def ===(other: T): B = value.compare(other.value) == 0
  def =!=(other: T): B = value.compare(other.value) != 0
  def ≤(other: T): B = this <= other
  def ≥(other: T): B = this >= other
  def ≠(other: T): B = this =!= other
  override def toString: Predef.String = value.toString
}
