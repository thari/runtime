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

package org.sireum

object String_Ext {

  @pure def fromB(b: B): String = if (b.value) "T" else "F"

  @pure def fromC(c: C): String = c.value.toString

  @pure def fromZ(n: Z): String = n.toString

  @pure def fromZ8(n: Z8): String = n.toString

  @pure def fromZ16(n: Z16): String = n.toString

  @pure def fromZ32(n: Z32): String = n.toString

  @pure def fromZ64(n: Z64): String = n.toString

  @pure def fromN(n: N): String = n.toString

  @pure def fromN8(n: N8): String = n.toString

  @pure def fromN16(n: N16): String = n.toString

  @pure def fromN32(n: N32): String = n.toString

  @pure def fromN64(n: N64): String = n.toString

  @pure def fromS8(n: S8): String = n.toString

  @pure def fromS16(n: S16): String = n.toString

  @pure def fromS32(n: S32): String = n.toString

  @pure def fromS64(n: S64): String = n.toString

  @pure def fromU8(n: U8): String = n.toString

  @pure def fromU16(n: U16): String = n.toString

  @pure def fromU32(n: U32): String = n.toString

  @pure def fromU64(n: U64): String = n.toString

  @pure def fromF32(n: F32): String = n.value match {
    case Float.NaN => "NaN"
    case Float.PositiveInfinity => "Infinity"
    case Float.NegativeInfinity => "-Infinity"
    case v => v.toString
  }

  @pure def fromF64(n: F64): String = n.value match {
    case Double.NaN => "NaN"
    case Double.PositiveInfinity => "Infinity"
    case Double.NegativeInfinity => "-Infinity"
    case v => v.toString
  }

  @pure def fromR(n: R): String = n.toString

  @pure def fromIS[I, V](s: IS[I, V]): String = s.toString

  @pure def fromMS[I, V](s: MS[I, V]): String = s.toString

  @pure def fromValues(s: ISZ[C]): String = _2String(new Predef.String(s.elements.map(_.value).toArray))

  @pure def toB(s: String): Option[B] = s.value match {
    case "T" | "true" | "⊤" => Some(T)
    case "F" | "false" | "_|_" | "⊥" => Some(F)
    case _ => None()
  }

  @pure def toC(s: String): Option[C] = if (s.value.length == 1) Some(s.value.head) else None()

  @pure def toHexC(s: String): Option[C] =
    try {
      val n = BigInt(s.value, 16)
      if (0 <= n && n <= math.Numbers.n16Max.toBigInt) Some(_2C(n.toChar))
      else None()
    } catch {
      case _: Throwable => None()
    }

  @pure def toZ(s: String): Option[Z] =
    try Some(_Z(BigInt(s.value))) catch {
      case _: Throwable => None()
    }

  @pure def toZ8(s: String): Option[Z8] =
    try Some(math.Numbers.toZ8(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toZ16(s: String): Option[Z16] =
    try Some(math.Numbers.toZ16(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toZ32(s: String): Option[Z32] =
    try Some(math.Numbers.toZ32(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toZ64(s: String): Option[Z64] =
    try Some(math.Numbers.toZ64(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toN(s: String): Option[N] =
    try Some(math.Numbers.toN(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toN8(s: String): Option[N8] =
    try Some(math.Numbers.toN8(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toN16(s: String): Option[N16] =
    try Some(math.Numbers.toN16(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toN32(s: String): Option[N32] =
    try Some(math.Numbers.toN32(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toN64(s: String): Option[N64] =
    try Some(math.Numbers.toN64(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toS8(s: String): Option[S8] =
    try Some(math.Numbers.toS8(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toS16(s: String): Option[S16] =
    try Some(math.Numbers.toS16(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toS32(s: String): Option[S32] =
    try Some(math.Numbers.toS32(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toS64(s: String): Option[S64] =
    try Some(math.Numbers.toS64(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toU8(s: String): Option[U8] =
    try Some(math.Numbers.toU8(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toU16(s: String): Option[U16] =
    try Some(math.Numbers.toU16(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toU32(s: String): Option[U32] =
    try Some(math.Numbers.toU32(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toU64(s: String): Option[U64] =
    try Some(math.Numbers.toU64(_Z(BigInt(s.value)))) catch {
      case _: Throwable => None()
    }

  @pure def toF32(s: String): Option[F32] = s.value match {
    case "NaN" => Some(_2F32(Float.NaN))
    case "Infinity" => Some(_2F32(Float.PositiveInfinity))
    case "-Infinity" => Some(_2F32(Float.NegativeInfinity))
    case v => try Some(_2F32(v.toFloat)) catch {
      case _: Throwable => None()
    }
  }

  @pure def toF64(s: String): Option[F64] = s.value match {
    case "NaN" => Some(_2F64(Double.NaN))
    case "Infinity" => Some(_2F64(Double.PositiveInfinity))
    case "-Infinity" => Some(_2F64(Double.NegativeInfinity))
    case v => try Some(_2F64(v.toDouble)) catch {
      case _: Throwable => None()
    }
  }

  @pure def toR(s: String): Option[R] =
    try Some(math.Numbers.toR(s.value)) catch {
      case _: Throwable => None()
    }

  @pure def toValues(s: String): ISZ[C] = IS[Z, C](s.value.toCharArray.map(_2C): _*)

}