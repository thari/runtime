/*
 * Copyright (c) 2017, Robby, Kansas State University
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

package org.sireum

import scala.collection.mutable.ArrayBuffer

import _Helper._

object B_Ext {
  def random: B = new java.util.Random().nextBoolean

  @pure def toB(b: B): B = b

  @pure def toZ(b: B): Z = if (b) 1 else 0

  @pure def toZ8(b: B): Z8 = if (b) z8"1" else z8"0"

  @pure def toZ16(b: B): Z16 = if (b) z16"1" else z16"0"

  @pure def toZ32(b: B): Z32 = if (b) z32"1" else z32"0"

  @pure def toZ64(b: B): Z64 = if (b) z64"1" else z64"0"

  @pure def toN(b: B): N = if (b) n"1" else n"0"

  @pure def toN8(b: B): N8 = if (b) n8"1" else n8"0"

  @pure def toN16(b: B): N16 = if (b) n16"1" else n16"0"

  @pure def toN32(b: B): N32 = if (b) n32"1" else n32"0"

  @pure def toN64(b: B): N64 = if (b) n64"1" else n64"0"

  @pure def toS8(b: B): S8 = if (b) s8"1" else s8"0"

  @pure def toS16(b: B): S16 = if (b) s16"1" else s16"0"

  @pure def toS32(b: B): S32 = if (b) s32"1" else s32"0"

  @pure def toS64(b: B): S64 = if (b) s64"1" else s64"0"

  @pure def toU8(b: B): U8 = if (b) u8"1" else u8"0"

  @pure def toU16(b: B): U16 = if (b) u16"1" else u16"0"

  @pure def toU32(b: B): U32 = if (b) u32"1" else u32"0"

  @pure def toU64(b: B): U64 = if (b) u64"1" else u64"0"

  @pure def toF32(b: B): F32 = if (b) f32"1.0" else f32"0.0"

  @pure def toF64(b: B): F64 = if (b) f64"1.0" else f64"0.0"

  @pure def toR(b: B): R = if (b) r"1.0" else r"0.0"
}


object Z_Ext {
  def random: Z = math._Z.random

  @pure def isInRangeSigned8(n: Z): B = -128 <= n && n <= 127

  @pure def isInRangeSigned16(n: Z): B = -32768 <= n && n <= 32767

  @pure def isInRangeSigned32(n: Z): B = -2147483648 <= n && n <= 2147483647

  @pure def isInRangeSigned64(n: Z): B = -9223372036854775808l <= n && n <= 9223372036854775807l

  @pure def isInRangeN(n: Z): B = 0 <= n

  @pure def isInRangeUnsigned8(n: Z): B = 0 <= n && n <= 255

  @pure def isInRangeUnsigned16(n: Z): B = 0 <= n && n <= 65535

  @pure def isInRangeUnsigned32(n: Z): B = 0 <= n && n <= 4294967295l

  @pure def isInRangeUnsigned64(n: Z): B = 0 <= n && n <= z"18446744073709551615"

  @pure def toB(n: Z): B = n != z"0"

  @pure def toZ(n: Z): Z = n

  @pure def toZ8(n: Z): Z8 = math.Numbers.toZ8(n)

  @pure def toZ16(n: Z): Z16 = math.Numbers.toZ16(n)

  @pure def toZ32(n: Z): Z32 = math.Numbers.toZ32(n)

  @pure def toZ64(n: Z): Z64 = math.Numbers.toZ64(n)

  @pure def toN(n: Z): N = math.Numbers.toN(n)

  @pure def toN8(n: Z): N8 = math.Numbers.toN8(n)

  @pure def toN16(n: Z): N16 = math.Numbers.toN16(n)

  @pure def toN32(n: Z): N32 = math.Numbers.toN32(n)

  @pure def toN64(n: Z): N64 = math.Numbers.toN64(n)

  @pure def toS8(n: Z): S8 = {
    require(isInRangeSigned8(n))
    math.Numbers.toS8(n)
  }

  @pure def toS16(n: Z): S16 = {
    require(isInRangeSigned16(n))
    math.Numbers.toS16(n)
  }

  @pure def toS32(n: Z): S32 = {
    require(isInRangeSigned32(n))
    math.Numbers.toS32(n)
  }

  @pure def toS64(n: Z): S64 = {
    require(isInRangeSigned64(n))
    math.Numbers.toS64(n)
  }

  @pure def toU8(n: Z): U8 = {
    require(isInRangeUnsigned8(n))
    math.Numbers.toU8(n)
  }

  @pure def toU16(n: Z): U16 = {
    require(isInRangeUnsigned16(n))
    math.Numbers.toU16(n)
  }

  @pure def toU32(n: Z): U32 = {
    require(isInRangeUnsigned32(n))
    math.Numbers.toU32(n)
  }

  @pure def toU64(n: Z): U64 = {
    require(isInRangeUnsigned64(n))
    math.Numbers.toU64(n)
  }

  @pure def toR(n: Z): R = R(n.toString)
}


object Z8_Ext {
  def random: Z8 = math._Z8(new java.util.Random().nextInt.toByte)

  @pure def toB(n: Z8): B = n != z8"0"

  @pure def toZ(n: Z8): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: Z8): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: Z8): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: Z8): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: Z8): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: Z8): N = math.Numbers.toN(n.value)

  @pure def toN8(n: Z8): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: Z8): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: Z8): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: Z8): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: Z8): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: Z8): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: Z8): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: Z8): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: Z8): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: Z8): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: Z8): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: Z8): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toR(n: Z8): R = R(n.toString)
}


object Z16_Ext {
  def random: Z16 = math._Z16(new java.util.Random().nextInt.toShort)

  @pure def toB(n: Z16): B = n != z16"0"

  @pure def toZ(n: Z16): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: Z16): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: Z16): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: Z16): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: Z16): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: Z16): N = math.Numbers.toN(n.value)

  @pure def toN8(n: Z16): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: Z16): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: Z16): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: Z16): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: Z16): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: Z16): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: Z16): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: Z16): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: Z16): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: Z16): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: Z16): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: Z16): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toR(n: Z16): R = R(n.toString)
}


object Z32_Ext {
  def random: Z32 = math._Z32(new java.util.Random().nextInt)

  @pure def toB(n: Z32): B = n != z32"0"

  @pure def toZ(n: Z32): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: Z32): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: Z32): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: Z32): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: Z32): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: Z32): N = math.Numbers.toN(n.value)

  @pure def toN8(n: Z32): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: Z32): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: Z32): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: Z32): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: Z32): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: Z32): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: Z32): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: Z32): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: Z32): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: Z32): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: Z32): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: Z32): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toR(n: Z32): R = R(n.toString)
}


object Z64_Ext {
  def random: Z64 = math._Z64(new java.util.Random().nextLong)

  @pure def toB(n: Z64): B = n != z64"0"

  @pure def toZ(n: Z64): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: Z64): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: Z64): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: Z64): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: Z64): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: Z64): N = math.Numbers.toN(n.value)

  @pure def toN8(n: Z64): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: Z64): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: Z64): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: Z64): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: Z64): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: Z64): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: Z64): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: Z64): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: Z64): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: Z64): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: Z64): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: Z64): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toR(n: Z64): R = R(n.toString)
}


object N_Ext {
  def random: N = math._N.random

  @pure def toB(n: N): B = n != n"0"

  @pure def toZ(n: N): Z = n.value

  @pure def toZ8(n: N): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: N): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: N): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: N): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: N): N = math.Numbers.toN(n.value)

  @pure def toN8(n: N): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: N): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: N): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: N): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: N): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: N): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: N): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: N): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: N): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: N): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: N): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: N): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toR(n: N): R = R(n.toString)
}


object N8_Ext {
  def random: N8 = math._N8(new java.util.Random().nextInt.toByte)

  @pure def toB(n: N8): B = n != n8"0"

  @pure def toZ(n: N8): Z = math.Numbers.toZ(n.value.toInt)

  @pure def toZ8(n: N8): Z8 = math.Numbers.toZ8(n.value.toInt)

  @pure def toZ16(n: N8): Z16 = math.Numbers.toZ16(n.value.toInt)

  @pure def toZ32(n: N8): Z32 = math.Numbers.toZ32(n.value.toInt)

  @pure def toZ64(n: N8): Z64 = math.Numbers.toZ64(n.value.toInt)

  @pure def toN(n: N8): N = math.Numbers.toN(n.value.toInt)

  @pure def toN8(n: N8): N8 = math.Numbers.toN8(n.value.toInt)

  @pure def toN16(n: N8): N16 = math.Numbers.toN16(n.value.toInt)

  @pure def toN32(n: N8): N32 = math.Numbers.toN32(n.value.toInt)

  @pure def toN64(n: N8): N64 = math.Numbers.toN64(n.value.toInt)

  @pure def toS8(n: N8): S8 = math.Numbers.toS8Exact(n.value.toInt)

  @pure def toS16(n: N8): S16 = math.Numbers.toS16Exact(n.value.toInt)

  @pure def toS32(n: N8): S32 = math.Numbers.toS32Exact(n.value.toInt)

  @pure def toS64(n: N8): S64 = math.Numbers.toS64Exact(n.value.toInt)

  @pure def toU8(n: N8): U8 = math.Numbers.toU8Exact(n.value.toInt)

  @pure def toU16(n: N8): U16 = math.Numbers.toU16Exact(n.value.toInt)

  @pure def toU32(n: N8): U32 = math.Numbers.toU32Exact(n.value.toInt)

  @pure def toU64(n: N8): U64 = math.Numbers.toU64Exact(n.value.toInt)

  @pure def toR(n: N8): R = R(n.toString)
}


object N16_Ext {
  def random: N16 = math._N16(new java.util.Random().nextInt.toShort)

  @pure def toB(n: N16): B = n != n16"0"

  @pure def toZ(n: N16): Z = math.Numbers.toZ(n.value.toInt)

  @pure def toZ8(n: N16): Z8 = math.Numbers.toZ8(n.value.toInt)

  @pure def toZ16(n: N16): Z16 = math.Numbers.toZ16(n.value.toInt)

  @pure def toZ32(n: N16): Z32 = math.Numbers.toZ32(n.value.toInt)

  @pure def toZ64(n: N16): Z64 = math.Numbers.toZ64(n.value.toInt)

  @pure def toN(n: N16): N = math.Numbers.toN(n.value.toInt)

  @pure def toN8(n: N16): N8 = math.Numbers.toN8(n.value.toInt)

  @pure def toN16(n: N16): N16 = math.Numbers.toN16(n.value.toInt)

  @pure def toN32(n: N16): N32 = math.Numbers.toN32(n.value.toInt)

  @pure def toN64(n: N16): N64 = math.Numbers.toN64(n.value.toInt)

  @pure def toS8(n: N16): S8 = math.Numbers.toS8Exact(n.value.toInt)

  @pure def toS16(n: N16): S16 = math.Numbers.toS16Exact(n.value.toInt)

  @pure def toS32(n: N16): S32 = math.Numbers.toS32Exact(n.value.toInt)

  @pure def toS64(n: N16): S64 = math.Numbers.toS64Exact(n.value.toInt)

  @pure def toU8(n: N16): U8 = math.Numbers.toU8Exact(n.value.toInt)

  @pure def toU16(n: N16): U16 = math.Numbers.toU16Exact(n.value.toInt)

  @pure def toU32(n: N16): U32 = math.Numbers.toU32Exact(n.value.toInt)

  @pure def toU64(n: N16): U64 = math.Numbers.toU64Exact(n.value.toInt)

  @pure def toR(n: N16): R = R(n.toString)
}


object N32_Ext {
  def random: N32 = math._N32(new java.util.Random().nextInt)

  @pure def toB(n: N32): B = n != n32"0"

  @pure def toZ(n: N32): Z = math.Numbers.toZ(n.value.toLong)

  @pure def toZ8(n: N32): Z8 = math.Numbers.toZ8(n.value.toLong)

  @pure def toZ16(n: N32): Z16 = math.Numbers.toZ16(n.value.toLong)

  @pure def toZ32(n: N32): Z32 = math.Numbers.toZ32(n.value.toLong)

  @pure def toZ64(n: N32): Z64 = math.Numbers.toZ64(n.value.toLong)

  @pure def toN(n: N32): N = math.Numbers.toN(n.value.toLong)

  @pure def toN8(n: N32): N8 = math.Numbers.toN8(n.value.toLong)

  @pure def toN16(n: N32): N16 = math.Numbers.toN16(n.value.toLong)

  @pure def toN32(n: N32): N32 = math.Numbers.toN32(n.value.toLong)

  @pure def toN64(n: N32): N64 = math.Numbers.toN64(n.value.toLong)

  @pure def toS8(n: N32): S8 = math.Numbers.toS8Exact(n.value.toLong)

  @pure def toS16(n: N32): S16 = math.Numbers.toS16Exact(n.value.toLong)

  @pure def toS32(n: N32): S32 = math.Numbers.toS32Exact(n.value.toLong)

  @pure def toS64(n: N32): S64 = math.Numbers.toS64Exact(n.value.toLong)

  @pure def toU8(n: N32): U8 = math.Numbers.toU8Exact(n.value.toLong)

  @pure def toU16(n: N32): U16 = math.Numbers.toU16Exact(n.value.toLong)

  @pure def toU32(n: N32): U32 = math.Numbers.toU32Exact(n.value.toLong)

  @pure def toU64(n: N32): U64 = math.Numbers.toU64Exact(n.value.toLong)

  @pure def toR(n: N32): R = R(n.toString)
}


object N64_Ext {
  def random: N64 = math._N64(new java.util.Random().nextLong)

  @pure def toB(n: N64): B = n != n64"0"

  @pure def toZ(n: N64): Z = math.Numbers.toZ(n.value.toBigInt)

  @pure def toZ8(n: N64): Z8 = math.Numbers.toZ8(n.value.toBigInt)

  @pure def toZ16(n: N64): Z16 = math.Numbers.toZ16(n.value.toBigInt)

  @pure def toZ32(n: N64): Z32 = math.Numbers.toZ32(n.value.toBigInt)

  @pure def toZ64(n: N64): Z64 = math.Numbers.toZ64(n.value.toBigInt)

  @pure def toN(n: N64): N = math.Numbers.toN(n.value.toBigInt)

  @pure def toN8(n: N64): N8 = math.Numbers.toN8(n.value.toBigInt)

  @pure def toN16(n: N64): N16 = math.Numbers.toN16(n.value.toBigInt)

  @pure def toN32(n: N64): N32 = math.Numbers.toN32(n.value.toBigInt)

  @pure def toN64(n: N64): N64 = math.Numbers.toN64(n.value.toBigInt)

  @pure def toS8(n: N64): S8 = math.Numbers.toS8Exact(n.value.toBigInt)

  @pure def toS16(n: N64): S16 = math.Numbers.toS16Exact(n.value.toBigInt)

  @pure def toS32(n: N64): S32 = math.Numbers.toS32Exact(n.value.toBigInt)

  @pure def toS64(n: N64): S64 = math.Numbers.toS64Exact(n.value.toBigInt)

  @pure def toU8(n: N64): U8 = math.Numbers.toU8Exact(n.value.toBigInt)

  @pure def toU16(n: N64): U16 = math.Numbers.toU16Exact(n.value.toBigInt)

  @pure def toU32(n: N64): U32 = math.Numbers.toU32Exact(n.value.toBigInt)

  @pure def toU64(n: N64): U64 = math.Numbers.toU64Exact(n.value.toBigInt)

  @pure def toR(n: N64): R = R(n.toString)
}


object S8_Ext {
  def random: S8 = math._S8(new java.util.Random().nextInt.toByte)

  @pure def toB(n: S8): B = n != s8"0"

  @pure def toZ(n: S8): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: S8): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: S8): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: S8): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: S8): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: S8): N = math.Numbers.toN(n.value)

  @pure def toN8(n: S8): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: S8): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: S8): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: S8): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: S8): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: S8): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: S8): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: S8): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: S8): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: S8): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: S8): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: S8): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toRawU8(n: S8): U8 = math.Numbers.toU8(spire.math.UByte(n.value))
}


object S16_Ext {

  def random: S16 = math._S16(new java.util.Random().nextInt.toShort)

  @pure def toB(n: S16): B = n != s16"0"

  @pure def toZ(n: S16): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: S16): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: S16): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: S16): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: S16): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: S16): N = math.Numbers.toN(n.value)

  @pure def toN8(n: S16): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: S16): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: S16): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: S16): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: S16): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: S16): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: S16): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: S16): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: S16): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: S16): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: S16): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: S16): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toRawU16(n: S16): U16 = math.Numbers.toU16(spire.math.UShort(n.value))
}


object S32_Ext {
  def random: S32 = math._S32(new java.util.Random().nextInt)

  @pure def toB(n: S32): B = n != s32"0"

  @pure def toZ(n: S32): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: S32): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: S32): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: S32): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: S32): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: S32): N = math.Numbers.toN(n.value)

  @pure def toN8(n: S32): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: S32): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: S32): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: S32): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: S32): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: S32): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: S32): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: S32): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: S32): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: S32): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: S32): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: S32): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toRawU32(n: S32): U32 = math.Numbers.toU32(spire.math.UInt(n.value))
}


object S64_Ext {
  def random: S64 = math._S64(new java.util.Random().nextLong)

  @pure def toB(n: S64): B = n != s64"0"

  @pure def toZ(n: S64): Z = math.Numbers.toZ(n.value)

  @pure def toZ8(n: S64): Z8 = math.Numbers.toZ8(n.value)

  @pure def toZ16(n: S64): Z16 = math.Numbers.toZ16(n.value)

  @pure def toZ32(n: S64): Z32 = math.Numbers.toZ32(n.value)

  @pure def toZ64(n: S64): Z64 = math.Numbers.toZ64(n.value)

  @pure def toN(n: S64): N = math.Numbers.toN(n.value)

  @pure def toN8(n: S64): N8 = math.Numbers.toN8(n.value)

  @pure def toN16(n: S64): N16 = math.Numbers.toN16(n.value)

  @pure def toN32(n: S64): N32 = math.Numbers.toN32(n.value)

  @pure def toN64(n: S64): N64 = math.Numbers.toN64(n.value)

  @pure def toS8(n: S64): S8 = math.Numbers.toS8Exact(n.value)

  @pure def toS16(n: S64): S16 = math.Numbers.toS16Exact(n.value)

  @pure def toS32(n: S64): S32 = math.Numbers.toS32Exact(n.value)

  @pure def toS64(n: S64): S64 = math.Numbers.toS64Exact(n.value)

  @pure def toU8(n: S64): U8 = math.Numbers.toU8Exact(n.value)

  @pure def toU16(n: S64): U16 = math.Numbers.toU16Exact(n.value)

  @pure def toU32(n: S64): U32 = math.Numbers.toU32Exact(n.value)

  @pure def toU64(n: S64): U64 = math.Numbers.toU64Exact(n.value)

  @pure def toRawU64(n: S64): U64 = math.Numbers.toU64(spire.math.ULong(n.value))
}


object U8_Ext {
  def random: U8 = math._U8(new java.util.Random().nextInt.toByte)

  @pure def toB(n: U8): B = n != u8"0"

  @pure def toZ(n: U8): Z = math.Numbers.toZ(n.value.toInt)

  @pure def toZ8(n: U8): Z8 = math.Numbers.toZ8(n.value.toInt)

  @pure def toZ16(n: U8): Z16 = math.Numbers.toZ16(n.value.toInt)

  @pure def toZ32(n: U8): Z32 = math.Numbers.toZ32(n.value.toInt)

  @pure def toZ64(n: U8): Z64 = math.Numbers.toZ64(n.value.toInt)

  @pure def toN(n: U8): N = math.Numbers.toN(n.value.toInt)

  @pure def toN8(n: U8): N8 = math.Numbers.toN8(n.value.toInt)

  @pure def toN16(n: U8): N16 = math.Numbers.toN16(n.value.toInt)

  @pure def toN32(n: U8): N32 = math.Numbers.toN32(n.value.toInt)

  @pure def toN64(n: U8): N64 = math.Numbers.toN64(n.value.toInt)

  @pure def toS8(n: U8): S8 = math.Numbers.toS8Exact(n.value.toInt)

  @pure def toS16(n: U8): S16 = math.Numbers.toS16Exact(n.value.toInt)

  @pure def toS32(n: U8): S32 = math.Numbers.toS32Exact(n.value.toInt)

  @pure def toS64(n: U8): S64 = math.Numbers.toS64Exact(n.value.toInt)

  @pure def toU8(n: U8): U8 = math.Numbers.toU8Exact(n.value.toInt)

  @pure def toU16(n: U8): U16 = math.Numbers.toU16Exact(n.value.toInt)

  @pure def toU32(n: U8): U32 = math.Numbers.toU32Exact(n.value.toInt)

  @pure def toU64(n: U8): U64 = math.Numbers.toU64Exact(n.value.toInt)

  @pure def toRawS8(n: U8): S8 = math.Numbers.toS8(n.value.signed)
}


object U16_Ext {
  def random: U16 = math._U16(new java.util.Random().nextInt.toShort)

  @pure def toB(n: U16): B = n != u16"0"

  @pure def toZ(n: U16): Z = math.Numbers.toZ(n.value.toInt)

  @pure def toZ8(n: U16): Z8 = math.Numbers.toZ8(n.value.toInt)

  @pure def toZ16(n: U16): Z16 = math.Numbers.toZ16(n.value.toInt)

  @pure def toZ32(n: U16): Z32 = math.Numbers.toZ32(n.value.toInt)

  @pure def toZ64(n: U16): Z64 = math.Numbers.toZ64(n.value.toInt)

  @pure def toN(n: U16): N = math.Numbers.toN(n.value.toInt)

  @pure def toN8(n: U16): N8 = math.Numbers.toN8(n.value.toInt)

  @pure def toN16(n: U16): N16 = math.Numbers.toN16(n.value.toInt)

  @pure def toN32(n: U16): N32 = math.Numbers.toN32(n.value.toInt)

  @pure def toN64(n: U16): N64 = math.Numbers.toN64(n.value.toInt)

  @pure def toS8(n: U16): S8 = math.Numbers.toS8Exact(n.value.toInt)

  @pure def toS16(n: U16): S16 = math.Numbers.toS16Exact(n.value.toInt)

  @pure def toS32(n: U16): S32 = math.Numbers.toS32Exact(n.value.toInt)

  @pure def toS64(n: U16): S64 = math.Numbers.toS64Exact(n.value.toInt)

  @pure def toU8(n: U16): U8 = math.Numbers.toU8Exact(n.value.toInt)

  @pure def toU16(n: U16): U16 = math.Numbers.toU16Exact(n.value.toInt)

  @pure def toU32(n: U16): U32 = math.Numbers.toU32Exact(n.value.toInt)

  @pure def toU64(n: U16): U64 = math.Numbers.toU64Exact(n.value.toInt)

  @pure def toRawS16(n: U16): S16 = math.Numbers.toS16(n.value.signed)
}


object U32_Ext {
  def random: U32 = math._U32(new java.util.Random().nextInt)

  @pure def toB(n: U32): B = n != u32"0"

  @pure def toZ(n: U32): Z = math.Numbers.toZ(n.value.toLong)

  @pure def toZ8(n: U32): Z8 = math.Numbers.toZ8(n.value.toLong)

  @pure def toZ16(n: U32): Z16 = math.Numbers.toZ16(n.value.toLong)

  @pure def toZ32(n: U32): Z32 = math.Numbers.toZ32(n.value.toLong)

  @pure def toZ64(n: U32): Z64 = math.Numbers.toZ64(n.value.toLong)

  @pure def toN(n: U32): N = math.Numbers.toN(n.value.toLong)

  @pure def toN8(n: U32): N8 = math.Numbers.toN8(n.value.toLong)

  @pure def toN16(n: U32): N16 = math.Numbers.toN16(n.value.toLong)

  @pure def toN32(n: U32): N32 = math.Numbers.toN32(n.value.toLong)

  @pure def toN64(n: U32): N64 = math.Numbers.toN64(n.value.toLong)

  @pure def toS8(n: U32): S8 = math.Numbers.toS8Exact(n.value.toLong)

  @pure def toS16(n: U32): S16 = math.Numbers.toS16Exact(n.value.toLong)

  @pure def toS32(n: U32): S32 = math.Numbers.toS32Exact(n.value.toLong)

  @pure def toS64(n: U32): S64 = math.Numbers.toS64Exact(n.value.toLong)

  @pure def toU8(n: U32): U8 = math.Numbers.toU8Exact(n.value.toLong)

  @pure def toU16(n: U32): U16 = math.Numbers.toU16Exact(n.value.toLong)

  @pure def toU32(n: U32): U32 = math.Numbers.toU32Exact(n.value.toLong)

  @pure def toU64(n: U32): U64 = math.Numbers.toU64Exact(n.value.toLong)

  @pure def toRawS32(n: U32): S32 = math.Numbers.toS32(n.value.signed)

  @pure def toRawF32(n: U32): F32 = math.Numbers.toF32(java.lang.Float.intBitsToFloat(n.value.signed))
}


object U64_Ext {
  def random: U64 = math._U64(new java.util.Random().nextLong)

  @pure def toB(n: U64): B = n != u64"0"

  @pure def toZ(n: U64): Z = math.Numbers.toZ(n.value.toBigInt)

  @pure def toZ8(n: U64): Z8 = math.Numbers.toZ8(n.value.toBigInt)

  @pure def toZ16(n: U64): Z16 = math.Numbers.toZ16(n.value.toBigInt)

  @pure def toZ32(n: U64): Z32 = math.Numbers.toZ32(n.value.toBigInt)

  @pure def toZ64(n: U64): Z64 = math.Numbers.toZ64(n.value.toBigInt)

  @pure def toN(n: U64): N = math.Numbers.toN(n.value.toBigInt)

  @pure def toN8(n: U64): N8 = math.Numbers.toN8(n.value.toBigInt)

  @pure def toN16(n: U64): N16 = math.Numbers.toN16(n.value.toBigInt)

  @pure def toN32(n: U64): N32 = math.Numbers.toN32(n.value.toBigInt)

  @pure def toN64(n: U64): N64 = math.Numbers.toN64(n.value.toBigInt)

  @pure def toS8(n: U64): S8 = math.Numbers.toS8Exact(n.value.toBigInt)

  @pure def toS16(n: U64): S16 = math.Numbers.toS16Exact(n.value.toBigInt)

  @pure def toS32(n: U64): S32 = math.Numbers.toS32Exact(n.value.toBigInt)

  @pure def toS64(n: U64): S64 = math.Numbers.toS64Exact(n.value.toBigInt)

  @pure def toU8(n: U64): U8 = math.Numbers.toU8Exact(n.value.toBigInt)

  @pure def toU16(n: U64): U16 = math.Numbers.toU16Exact(n.value.toBigInt)

  @pure def toU32(n: U64): U32 = math.Numbers.toU32Exact(n.value.toBigInt)

  @pure def toU64(n: U64): U64 = math.Numbers.toU64Exact(n.value.toBigInt)

  @pure def toRawS64(n: U64): S64 = math.Numbers.toS64(n.value.signed)

  @pure def toRawF64(n: U64): F64 = math.Numbers.toF64(java.lang.Double.longBitsToDouble(n.value.signed))
}


object F32_Ext {
  def random: F32 = math._F32(new java.util.Random().nextFloat)

  @pure def toB(n: F32): B = n != f32"0.0"

  @pure def toRawU32(n: F32): U32 = math.Numbers.toU32(spire.math.UInt(java.lang.Float.floatToRawIntBits(n.value)))

  @pure def toF32(n: F32): F32 = n
}


object F64_Ext {
  def random: F64 = math._F64(new java.util.Random().nextDouble)

  @pure def toB(n: F64): B = n != f64"0.0"

  @pure def toRawU64(n: F64): U64 = math.Numbers.toU64(spire.math.ULong.fromLong(java.lang.Double.doubleToRawLongBits(n.value)))

  @pure def toF64(n: F64): F64 = n
}


object R_Ext {
  def random: R = R.random

  @pure def toB(n: R): B = n != r"0.0"

  @pure def toZ(n: R): Z = math.Numbers.toZ(n.value.floor.apply(0).toBigInt)

  @pure def toN(n: R): N = {
    require(n >= r"0.0")
    math.Numbers.toN(n.value.floor.apply(0).toBigInt)
  }

  @pure def toR(n: R): R = n
}

import _Type._

object SI_Ext {

  @pure def append[I: TT, E](s: IS[I, E], e: E): IS[I, E] = s :+ e

  @pure def prepend[I: TT, E](s: IS[I, E], e: E): IS[I, E] = e +: s

  @pure def appends[I: TT, E](s1: IS[I, E], s2: IS[I, E]): IS[I, E] = s1 ++ s2

  @pure def toMS[I: TT, E](s: IS[I, E]): MS[I, E] = collection._MS[I, E](s.elements: _*)

  @pure def chunk[I: TT, E](s: IS[I, E], size: I): IS[I, IS[I, E]] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt % sizeInt == 0)
    val sz = sSizeInt / sizeInt
    val result = ArrayBuffer[IS[I, E]]()
    for (i <- 0 until sz) {
      val chunk = ArrayBuffer[E]()
      for (j <- 0 until sizeInt) {
        chunk += s(i * sizeInt + j)
      }
      result += collection._IS(chunk: _*)
    }
    collection._IS(result: _*)
  }

  @pure def drop[I: TT, E](s: IS[I, E], size: I): IS[I, E] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt >= sizeInt)
    val result = ArrayBuffer[E]()
    for (i <- sizeInt until sSizeInt) {
      result += s(i)
    }
    collection._IS(result: _*)
  }

  @pure def foldLeft[I: TT, E, R](s: IS[I, E], f: (R, E) => R, init: R): R = {
    var r = init
    for (e <- s.elements) r = f(r, e)
    r
  }

  @pure def foldRight[I: TT, E, R](s: IS[I, E], f: (R, E) => R, init: R): R = {
    var r = init
    for (e <- s.elements.reverseIterator) r = f(r, e)
    r
  }

  @pure def map[I: TT, E1, E2](s: IS[I, E1], f: E1 => E2): IS[I, E2] =
    collection._IS[I, E2](s.elements.map(f): _*)

  @pure def take[I: TT, E](s: IS[I, E], size: I): IS[I, E] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt >= sizeInt)
    var result = ArrayBuffer[E]()
    for (i <- 0 until sizeInt) {
      result += s(i)
    }
    collection._IS(result: _*)
  }

  @pure def fromU8[I: TT](n: U8): IS[I, B] =
    collection._IS[I, B]((0 until 8).map { i =>
      val mask = u8"1" << math.Numbers.toU8(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU16[I: TT](n: U16): IS[I, B] =
    collection._IS[I, B]((0 until 16).map { i =>
      val mask = u16"1" << math.Numbers.toU16(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU32[I: TT](n: U32): IS[I, B] =
    collection._IS[I, B]((0 until 32).map { i =>
      val mask = u32"1" << math.Numbers.toU32(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU64[I: TT](n: U64): IS[I, B] =
    collection._IS[I, B]((0 until 64).map { i =>
      val mask = u64"1" << math.Numbers.toU64(i)
      (n & mask) != mask : B
    }: _*)

  @pure def toU8[I: TT](s: IS[I, B]): U8 = {
    require(s.size == z"8")
    var result = u8"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u8"1" << math.Numbers.toU8(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU16[I: TT](s: IS[I, B]): U16 = {
    require(s.size == z"16")
    var result = u16"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u16"1" << math.Numbers.toU16(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU32[I: TT](s: IS[I, B]): U32 = {
    require(s.size == z"32")
    var result = u32"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u32"1" << math.Numbers.toU32(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU64[I: TT](s: IS[I, B]): U64 = {
    require(s.size == z"64")
    var result = u64"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u64"1" << math.Numbers.toU64(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }
}

object SM_Ext {

  @pure def append[I: TT, E](s: MS[I, E], e: E): MS[I, E] = s :+ e

  @pure def prepend[I: TT, E](s: MS[I, E], e: E): MS[I, E] = e +: s

  @pure def appends[I: TT, E](s1: MS[I, E], s2: MS[I, E]): MS[I, E] = s1 ++ s2

  @pure def toIS[I: TT, E](s: MS[I, E]): IS[I, E] = collection._IS(s.elements: _*)

  @pure def chunk[I: TT, E](s: MS[I, E], size: I): MS[I, MS[I, E]] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt % sizeInt == 0)
    val sz = sSizeInt / sizeInt
    val result = ArrayBuffer[MS[I, E]]()
    for (i <- 0 until sz) {
      val chunk = ArrayBuffer[E]()
      for (j <- 0 until sizeInt) {
        chunk += _Clonable.clone(s(i * sizeInt + j))
      }
      result += collection._MS(chunk: _*)
    }
    collection._MS(result: _*)
  }

  @pure def drop[I: TT, E](s: MS[I, E], size: I): MS[I, E] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt >= sizeInt)
    val result = ArrayBuffer[E]()
    for (i <- sizeInt until sSizeInt) {
      result += s(i)
    }
    collection._MS(result: _*)
  }

  @pure def foldLeft[I: TT, E, R](s: MS[I, E], f: (R, E) => R, init: R): R = {
    var r = init
    for (e <- s.elements) r = f(r, e)
    r
  }

  @pure def foldRight[I: TT, E, R](s: MS[I, E], f: (R, E) => R, init: R): R = {
    var r = init
    for (e <- s.elements.reverseIterator) r = f(r, e)
    r
  }

  @pure def map[I: TT, E1, E2](s: MS[I, E1], f: E1 => E2): MS[I, E2] =
    collection._MS[I, E2](s.elements.map(e => f(_Clonable.clone(e))): _*)

  def transform[I: TT, E](s: MS[I, E], f: E => E): Unit =
    for (i <- s.indices.map(x => ln2int(x))) {
      s(i) = _Clonable.clone(f(s(i)))
    }

  @pure def take[I: TT, E](s: MS[I, E], size: I): MS[I, E] = {
    val sizeInt = ln2int(size)
    val sSizeInt = ln2int(s.size)
    require(sSizeInt >= sizeInt)
    var result = ArrayBuffer[E]()
    for (i <- 0 until sizeInt) {
      result += _Clonable.clone(s(i))
    }
    collection._MS(result: _*)
  }

  @pure def fromU8[I: TT](n: U8): MS[I, B] =
    collection._MS[I, B]((0 until 8).map { i =>
      val mask = u8"1" << math.Numbers.toU8(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU16[I: TT](n: U16): MS[I, B] =
    collection._MS[I, B]((0 until 16).map { i =>
      val mask = u16"1" << math.Numbers.toU16(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU32[I: TT](n: U32): MS[I, B] =
    collection._MS[I, B]((0 until 32).map { i =>
      val mask = u32"1" << math.Numbers.toU32(i)
      (n & mask) != mask : B
    }: _*)

  @pure def fromU64[I: TT](n: U64): MS[I, B] =
    collection._MS[I, B]((0 until 64).map { i =>
      val mask = u64"1" << math.Numbers.toU64(i)
      (n & mask) != mask : B
    }: _*)

  @pure def toU8[I: TT](s: MS[I, B]): U8 = {
    require(s.size == z"8")
    var result = u8"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u8"1" << math.Numbers.toU8(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU16[I: TT](s: MS[I, B]): U16 = {
    require(s.size == z"16")
    var result = u16"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u16"1" << math.Numbers.toU16(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU32[I: TT](s: MS[I, B]): U32 = {
    require(s.size == z"32")
    var result = u32"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u32"1" << math.Numbers.toU32(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU64[I: TT](s: MS[I, B]): U64 = {
    require(s.size == z"64")
    var result = u64"0"
    for (i <- 0 until ln2int(s.size)) {
      val mask = u64"1" << math.Numbers.toU64(i)
      if (s(i)) {
        result = result | mask
      }
    }
    result
  }
}