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

package org.sireum.logika

import spire.math._
import scala.collection.mutable.ArrayBuffer

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

  @pure def isInRangeZ8(n: Z): B = -128 <= n && n <= 127

  @pure def isInRangeZ16(n: Z): B = -32768 <= n && n <= 32767

  @pure def isInRangeZ32(n: Z): B = -2147483648 <= n && n <= 2147483647

  @pure def isInRangeZ64(n: Z): B = z"-9223372036854775808" <= n && n <= z"9223372036854775807"

  @pure def isInRangeN8(n: Z): B = 0 <= n && n <= 255

  @pure def isInRangeN16(n: Z): B = 0 <= n && n <= 65535

  @pure def isInRangeN32(n: Z): B = 0 <= n && n <= z"4294967295"

  @pure def isInRangeN64(n: Z): B = 0 <= n && n <= z"18446744073709551615"

  @pure def toB(n: Z): B = n != z"0"

  @pure def toZ(n: Z): Z = n

  @pure def toZ8(n: Z): Z8 = n.toZ8

  @pure def toZ16(n: Z): Z16 = n.toZ16

  @pure def toZ32(n: Z): Z32 = n.toZ32

  @pure def toZ64(n: Z): Z64 = n.toZ64

  @pure def toN(n: Z): N = n.toN

  @pure def toN8(n: Z): N8 = n.toN8

  @pure def toN16(n: Z): N16 = n.toN16

  @pure def toN32(n: Z): N32 = n.toN32

  @pure def toN64(n: Z): N64 = n.toN64

  @pure def toS8(n: Z): S8 = n.toS8

  @pure def toS16(n: Z): S16 = n.toS16

  @pure def toS32(n: Z): S32 = n.toS32

  @pure def toS64(n: Z): S64 = n.toS64

  @pure def toU8(n: Z): U8 = n.toU8

  @pure def toU16(n: Z): U16 = n.toU16

  @pure def toU32(n: Z): U32 = n.toU32

  @pure def toU64(n: Z): U64 = n.toU64

  @pure def toR(n: Z): R = math._R(n.toString)
}


object Z8_Ext {
  def random: Z8 = math._Z8.random

  @pure def toB(n: Z8): B = n != z8"0"

  @pure def toZ(n: Z8): Z = n.toZ

  @pure def toZ8(n: Z8): Z8 = n

  @pure def toZ16(n: Z8): Z16 = n.toZ16

  @pure def toZ32(n: Z8): Z32 = n.toZ32

  @pure def toZ64(n: Z8): Z64 = n.toZ64

  @pure def toN(n: Z8): N = n.toN

  @pure def toN8(n: Z8): N8 = n.toN8

  @pure def toN16(n: Z8): N16 = n.toN16

  @pure def toN32(n: Z8): N32 = n.toN32

  @pure def toN64(n: Z8): N64 = n.toN64

  @pure def toS8(n: Z8): S8 = n.toS8

  @pure def toS16(n: Z8): S16 = n.toS16

  @pure def toS32(n: Z8): S32 = n.toS32

  @pure def toS64(n: Z8): S64 = n.toS64

  @pure def toU8(n: Z8): U8 = n.toU8

  @pure def toU16(n: Z8): U16 = n.toU16

  @pure def toU32(n: Z8): U32 = n.toU32

  @pure def toU64(n: Z8): U64 = n.toU64

  @pure def toR(n: Z8): R = math._R(n.toString)
}


object Z16_Ext {
  def random: Z16 = math._Z16.random

  @pure def toB(n: Z16): B = n != z16"0"

  @pure def toZ(n: Z16): Z = n.toZ

  @pure def toZ8(n: Z16): Z8 = n.toZ8

  @pure def toZ16(n: Z16): Z16 = n

  @pure def toZ32(n: Z16): Z32 = n.toZ32

  @pure def toZ64(n: Z16): Z64 = n.toZ64

  @pure def toN(n: Z16): N = n.toN

  @pure def toN8(n: Z16): N8 = n.toN8

  @pure def toN16(n: Z16): N16 = n.toN16

  @pure def toN32(n: Z16): N32 = n.toN32

  @pure def toN64(n: Z16): N64 = n.toN64

  @pure def toS8(n: Z16): S8 = n.toS8

  @pure def toS16(n: Z16): S16 = n.toS16

  @pure def toS32(n: Z16): S32 = n.toS32

  @pure def toS64(n: Z16): S64 = n.toS64

  @pure def toU8(n: Z16): U8 = n.toU8

  @pure def toU16(n: Z16): U16 = n.toU16

  @pure def toU32(n: Z16): U32 = n.toU32

  @pure def toU64(n: Z16): U64 = n.toU64

  @pure def toR(n: Z16): R = math._R(n.toString)
}


object Z32_Ext {
  def random: Z32 = math._Z32.random

  @pure def toB(n: Z32): B = n != z32"0"

  @pure def toZ(n: Z32): Z = n.toZ

  @pure def toZ8(n: Z32): Z8 = n.toZ8

  @pure def toZ16(n: Z32): Z16 = n.toZ16

  @pure def toZ32(n: Z32): Z32 = n

  @pure def toZ64(n: Z32): Z64 = n.toZ64

  @pure def toN(n: Z32): N = n.toN

  @pure def toN8(n: Z32): N8 = n.toN8

  @pure def toN16(n: Z32): N16 = n.toN16

  @pure def toN32(n: Z32): N32 = n.toN32

  @pure def toN64(n: Z32): N64 = n.toN64

  @pure def toS8(n: Z32): S8 = n.toS8

  @pure def toS16(n: Z32): S16 = n.toS16

  @pure def toS32(n: Z32): S32 = n.toS32

  @pure def toS64(n: Z32): S64 = n.toS64

  @pure def toU8(n: Z32): U8 = n.toU8

  @pure def toU16(n: Z32): U16 = n.toU16

  @pure def toU32(n: Z32): U32 = n.toU32

  @pure def toU64(n: Z32): U64 = n.toU64

  @pure def toR(n: Z32): R = math._R(n.toString)
}


object Z64_Ext {
  def random: Z64 = math._Z64.random

  @pure def toB(n: Z64): B = n != z64"0"

  @pure def toZ(n: Z64): Z = n.toZ

  @pure def toZ8(n: Z64): Z8 = n.toZ8

  @pure def toZ16(n: Z64): Z16 = n.toZ16

  @pure def toZ32(n: Z64): Z32 = n.toZ32

  @pure def toZ64(n: Z64): Z64 = n

  @pure def toN(n: Z64): N = n.toN

  @pure def toN8(n: Z64): N8 = n.toN8

  @pure def toN16(n: Z64): N16 = n.toN16

  @pure def toN32(n: Z64): N32 = n.toN32

  @pure def toN64(n: Z64): N64 = n.toN64

  @pure def toS8(n: Z64): S8 = n.toS8

  @pure def toS16(n: Z64): S16 = n.toS16

  @pure def toS32(n: Z64): S32 = n.toS32

  @pure def toS64(n: Z64): S64 = n.toS64

  @pure def toU8(n: Z64): U8 = n.toU8

  @pure def toU16(n: Z64): U16 = n.toU16

  @pure def toU32(n: Z64): U32 = n.toU32

  @pure def toU64(n: Z64): U64 = n.toU64

  @pure def toR(n: Z64): R = math._R(n.toString)
}


object N_Ext {
  def random: N = math._N.random

  @pure def toB(n: N): B = n != n"0"

  @pure def toZ(n: N): Z = n.toZ

  @pure def toZ8(n: N): Z8 = n.toZ8

  @pure def toZ16(n: N): Z16 = n.toZ16

  @pure def toZ32(n: N): Z32 = n.toZ32

  @pure def toZ64(n: N): Z64 = n.toZ64

  @pure def toN(n: N): N = n

  @pure def toN8(n: N): N8 = n.toN8

  @pure def toN16(n: N): N16 = n.toN16

  @pure def toN32(n: N): N32 = n.toN32

  @pure def toN64(n: N): N64 = n.toN64

  @pure def toS8(n: N): S8 = n.toS8

  @pure def toS16(n: N): S16 = n.toS16

  @pure def toS32(n: N): S32 = n.toS32

  @pure def toS64(n: N): S64 = n.toS64

  @pure def toU8(n: N): U8 = n.toU8

  @pure def toU16(n: N): U16 = n.toU16

  @pure def toU32(n: N): U32 = n.toU32

  @pure def toU64(n: N): U64 = n.toU64

  @pure def toR(n: N): R = math._R(n.toString)
}


object N8_Ext {
  def random: N8 = math._N8.random

  @pure def toB(n: N8): B = n != n8"0"

  @pure def toZ(n: N8): Z = n.toZ

  @pure def toZ8(n: N8): Z8 = n.toZ8

  @pure def toZ16(n: N8): Z16 = n.toZ16

  @pure def toZ32(n: N8): Z32 = n.toZ32

  @pure def toZ64(n: N8): Z64 = n.toZ64

  @pure def toN(n: N8): N = n.toN

  @pure def toN8(n: N8): N8 = n

  @pure def toN16(n: N8): N16 = n.toN16

  @pure def toN32(n: N8): N32 = n.toN32

  @pure def toN64(n: N8): N64 = n.toN64

  @pure def toS8(n: N8): S8 = n.toS8

  @pure def toS16(n: N8): S16 = n.toS16

  @pure def toS32(n: N8): S32 = n.toS32

  @pure def toS64(n: N8): S64 = n.toS64

  @pure def toU8(n: N8): U8 = n.toU8

  @pure def toU16(n: N8): U16 = n.toU16

  @pure def toU32(n: N8): U32 = n.toU32

  @pure def toU64(n: N8): U64 = n.toU64

  @pure def toR(n: N8): R = math._R(n.toString)
}


object N16_Ext {
  def random: N16 = math._N16.random

  @pure def toB(n: N16): B = n != n16"0"

  @pure def toZ(n: N16): Z = n.toZ

  @pure def toZ8(n: N16): Z8 = n.toZ8

  @pure def toZ16(n: N16): Z16 = n.toZ16

  @pure def toZ32(n: N16): Z32 = n.toZ32

  @pure def toZ64(n: N16): Z64 = n.toZ64

  @pure def toN(n: N16): N = n.toN

  @pure def toN8(n: N16): N8 = n.toN8

  @pure def toN16(n: N16): N16 = n

  @pure def toN32(n: N16): N32 = n.toN32

  @pure def toN64(n: N16): N64 = n.toN64

  @pure def toS8(n: N16): S8 = n.toS8

  @pure def toS16(n: N16): S16 = n.toS16

  @pure def toS32(n: N16): S32 = n.toS32

  @pure def toS64(n: N16): S64 = n.toS64

  @pure def toU8(n: N16): U8 = n.toU8

  @pure def toU16(n: N16): U16 = n.toU16

  @pure def toU32(n: N16): U32 = n.toU32

  @pure def toU64(n: N16): U64 = n.toU64

  @pure def toR(n: N16): R = math._R(n.toString)
}


object N32_Ext {
  def random: N32 = math._N32.random

  @pure def toB(n: N32): B = n != n32"0"

  @pure def toZ(n: N32): Z = n.toZ

  @pure def toZ8(n: N32): Z8 = n.toZ8

  @pure def toZ16(n: N32): Z16 = n.toZ16

  @pure def toZ32(n: N32): Z32 = n.toZ32

  @pure def toZ64(n: N32): Z64 = n.toZ64

  @pure def toN(n: N32): N = n.toN

  @pure def toN8(n: N32): N8 = n.toN8

  @pure def toN16(n: N32): N16 = n.toN16

  @pure def toN32(n: N32): N32 = n

  @pure def toN64(n: N32): N64 = n.toN64

  @pure def toS8(n: N32): S8 = n.toS8

  @pure def toS16(n: N32): S16 = n.toS16

  @pure def toS32(n: N32): S32 = n.toS32

  @pure def toS64(n: N32): S64 = n.toS64

  @pure def toU8(n: N32): U8 = n.toU8

  @pure def toU16(n: N32): U16 = n.toU16

  @pure def toU32(n: N32): U32 = n.toU32

  @pure def toU64(n: N32): U64 = n.toU64

  @pure def toR(n: N32): R = math._R(n.toString)
}


object N64_Ext {
  def random: N64 = math._N64.random

  @pure def toB(n: N64): B = n != n64"0"

  @pure def toZ(n: N64): Z = n.toZ

  @pure def toZ8(n: N64): Z8 = n.toZ8

  @pure def toZ16(n: N64): Z16 = n.toZ16

  @pure def toZ32(n: N64): Z32 = n.toZ32

  @pure def toZ64(n: N64): Z64 = n.toZ64

  @pure def toN(n: N64): N = n.toN

  @pure def toN8(n: N64): N8 = n.toN8

  @pure def toN16(n: N64): N16 = n.toN16

  @pure def toN32(n: N64): N32 = n.toN32

  @pure def toN64(n: N64): N64 = n

  @pure def toS8(n: N64): S8 = n.toS8

  @pure def toS16(n: N64): S16 = n.toS16

  @pure def toS32(n: N64): S32 = n.toS32

  @pure def toS64(n: N64): S64 = n.toS64

  @pure def toU8(n: N64): U8 = n.toU8

  @pure def toU16(n: N64): U16 = n.toU16

  @pure def toU32(n: N64): U32 = n.toU32

  @pure def toU64(n: N64): U64 = n.toU64

  @pure def toR(n: N64): R = math._R(n.toString)
}


object S8_Ext {
  def random: S8 = math._S8.random

  @pure def toB(n: S8): B = n != s8"0"

  @pure def toZ(n: S8): Z = n.toZ

  @pure def toZ8(n: S8): Z8 = n.toZ8

  @pure def toZ16(n: S8): Z16 = n.toZ16

  @pure def toZ32(n: S8): Z32 = n.toZ32

  @pure def toZ64(n: S8): Z64 = n.toZ64

  @pure def toN(n: S8): N = n.toN

  @pure def toN8(n: S8): N8 = n.toN8

  @pure def toN16(n: S8): N16 = n.toN16

  @pure def toN32(n: S8): N32 = n.toN32

  @pure def toN64(n: S8): N64 = n.toN64

  @pure def toS8(n: S8): S8 = n

  @pure def toS16(n: S8): S16 = n.toS16

  @pure def toS32(n: S8): S32 = n.toS32

  @pure def toS64(n: S8): S64 = n.toS64

  @pure def toU8(n: S8): U8 = n.toU8

  @pure def toU16(n: S8): U16 = n.toU16

  @pure def toU32(n: S8): U32 = n.toU32

  @pure def toU64(n: S8): U64 = n.toU64

  @pure def toRawU8(n: S8): U8 = math._U8.ValueImpl(UByte(n.value))
}


object S16_Ext {

  def random: S16 = math._S16.random

  @pure def toB(n: S16): B = n != s16"0"

  @pure def toZ(n: S16): Z = n.toZ

  @pure def toZ8(n: S16): Z8 = n.toZ8

  @pure def toZ16(n: S16): Z16 = n.toZ16

  @pure def toZ32(n: S16): Z32 = n.toZ32

  @pure def toZ64(n: S16): Z64 = n.toZ64

  @pure def toN(n: S16): N = n.toN

  @pure def toN8(n: S16): N8 = n.toN8

  @pure def toN16(n: S16): N16 = n.toN16

  @pure def toN32(n: S16): N32 = n.toN32

  @pure def toN64(n: S16): N64 = n.toN64

  @pure def toS8(n: S16): S8 = n.toS8

  @pure def toS16(n: S16): S16 = n

  @pure def toS32(n: S16): S32 = n.toS32

  @pure def toS64(n: S16): S64 = n.toS64

  @pure def toU8(n: S16): U8 = n.toU8

  @pure def toU16(n: S16): U16 = n.toU16

  @pure def toU32(n: S16): U32 = n.toU32

  @pure def toU64(n: S16): U64 = n.toU64

  @pure def toRawU16(n: S16): U16 = math._U16.ValueImpl(UShort(n.value))
}


object S32_Ext {
  def random: S32 = math._S32.random

  @pure def toB(n: S32): B = n != s32"0"

  @pure def toZ(n: S32): Z = n.toZ

  @pure def toZ8(n: S32): Z8 = n.toZ8

  @pure def toZ16(n: S32): Z16 = n.toZ16

  @pure def toZ32(n: S32): Z32 = n.toZ32

  @pure def toZ64(n: S32): Z64 = n.toZ64

  @pure def toN(n: S32): N = n.toN

  @pure def toN8(n: S32): N8 = n.toN8

  @pure def toN16(n: S32): N16 = n.toN16

  @pure def toN32(n: S32): N32 = n.toN32

  @pure def toN64(n: S32): N64 = n.toN64

  @pure def toS8(n: S32): S8 = n.toS8

  @pure def toS16(n: S32): S16 = n.toS16

  @pure def toS32(n: S32): S32 = n

  @pure def toS64(n: S32): S64 = n.toS64

  @pure def toU8(n: S32): U8 = n.toU8

  @pure def toU16(n: S32): U16 = n.toU16

  @pure def toU32(n: S32): U32 = n.toU32

  @pure def toU64(n: S32): U64 = n.toU64

  @pure def toRawU32(n: S32): U32 = math._U32.ValueImpl(UInt(n.value))
}


object S64_Ext {
  def random: S64 = math._S64.random

  @pure def toB(n: S64): B = n != s64"0"

  @pure def toZ(n: S64): Z = n.toZ

  @pure def toZ8(n: S64): Z8 = n.toZ8

  @pure def toZ16(n: S64): Z16 = n.toZ16

  @pure def toZ32(n: S64): Z32 = n.toZ32

  @pure def toZ64(n: S64): Z64 = n.toZ64

  @pure def toN(n: S64): N = n.toN

  @pure def toN8(n: S64): N8 = n.toN8

  @pure def toN16(n: S64): N16 = n.toN16

  @pure def toN32(n: S64): N32 = n.toN32

  @pure def toN64(n: S64): N64 = n.toN64

  @pure def toS8(n: S64): S8 = n.toS8

  @pure def toS16(n: S64): S16 = n.toS16

  @pure def toS32(n: S64): S32 = n.toS32

  @pure def toS64(n: S64): S64 = n

  @pure def toU8(n: S64): U8 = n.toU8

  @pure def toU16(n: S64): U16 = n.toU16

  @pure def toU32(n: S64): U32 = n.toU32

  @pure def toU64(n: S64): U64 = n.toU64

  @pure def toRawU64(n: S64): U64 = math._U64.ValueImpl(ULong(n.value))
}


object U8_Ext {
  def random: U8 = math._U8.random

  @pure def toB(n: U8): B = n != u8"0"

  @pure def toZ(n: U8): Z = n.toZ

  @pure def toZ8(n: U8): Z8 = n.toZ8

  @pure def toZ16(n: U8): Z16 = n.toZ16

  @pure def toZ32(n: U8): Z32 = n.toZ32

  @pure def toZ64(n: U8): Z64 = n.toZ64

  @pure def toN(n: U8): N = n.toN

  @pure def toN8(n: U8): N8 = n.toN8

  @pure def toN16(n: U8): N16 = n.toN16

  @pure def toN32(n: U8): N32 = n.toN32

  @pure def toN64(n: U8): N64 = n.toN64

  @pure def toS8(n: U8): S8 = n.toS8

  @pure def toS16(n: U8): S16 = n.toS16

  @pure def toS32(n: U8): S32 = n.toS32

  @pure def toS64(n: U8): S64 = n.toS64

  @pure def toU8(n: U8): U8 = n

  @pure def toU16(n: U8): U16 = n.toU16

  @pure def toU32(n: U8): U32 = n.toU32

  @pure def toU64(n: U8): U64 = n.toU64

  @pure def toRawS8(n: U8): S8 = math._S8.ValueImpl(n.value.toByte)
}


object U16_Ext {
  def random: U16 = math._U16.random

  @pure def toB(n: U16): B = n != u16"0"

  @pure def toZ(n: U16): Z = n.toZ

  @pure def toZ8(n: U16): Z8 = n.toZ8

  @pure def toZ16(n: U16): Z16 = n.toZ16

  @pure def toZ32(n: U16): Z32 = n.toZ32

  @pure def toZ64(n: U16): Z64 = n.toZ64

  @pure def toN(n: U16): N = n.toN

  @pure def toN8(n: U16): N8 = n.toN8

  @pure def toN16(n: U16): N16 = n.toN16

  @pure def toN32(n: U16): N32 = n.toN32

  @pure def toN64(n: U16): N64 = n.toN64

  @pure def toS8(n: U16): S8 = n.toS8

  @pure def toS16(n: U16): S16 = n.toS16

  @pure def toS32(n: U16): S32 = n.toS32

  @pure def toS64(n: U16): S64 = n.toS64

  @pure def toU8(n: U16): U8 = n.toU8

  @pure def toU16(n: U16): U16 = n

  @pure def toU32(n: U16): U32 = n.toU32

  @pure def toU64(n: U16): U64 = n.toU64

  @pure def toRawS16(n: U16): S16 = math._S16.ValueImpl(n.value.toShort)
}


object U32_Ext {
  def random: U32 = math._U32.random

  @pure def toB(n: U32): B = n != u32"0"

  @pure def toZ(n: U32): Z = n.toZ

  @pure def toZ8(n: U32): Z8 = n.toZ8

  @pure def toZ16(n: U32): Z16 = n.toZ16

  @pure def toZ32(n: U32): Z32 = n.toZ32

  @pure def toZ64(n: U32): Z64 = n.toZ64

  @pure def toN(n: U32): N = n.toN

  @pure def toN8(n: U32): N8 = n.toN8

  @pure def toN16(n: U32): N16 = n.toN16

  @pure def toN32(n: U32): N32 = n.toN32

  @pure def toN64(n: U32): N64 = n.toN64

  @pure def toS8(n: U32): S8 = n.toS8

  @pure def toS16(n: U32): S16 = n.toS16

  @pure def toS32(n: U32): S32 = n.toS32

  @pure def toS64(n: U32): S64 = n.toS64

  @pure def toU8(n: U32): U8 = n.toU8

  @pure def toU16(n: U32): U16 = n.toU16

  @pure def toU32(n: U32): U32 = n

  @pure def toU64(n: U32): U64 = n.toU64

  @pure def toRawS32(n: U32): S32 = math._S32.ValueImpl(n.value.toInt)

  @pure def toRawF32(n: U32): F32 = math._F32.ValueImpl(java.lang.Float.intBitsToFloat(n.value.toInt))
}


object U64_Ext {
  def random: U64 = math._U64.random

  @pure def toB(n: U64): B = n != u64"0"

  @pure def toZ(n: U64): Z = n.toZ

  @pure def toZ8(n: U64): Z8 = n.toZ8

  @pure def toZ16(n: U64): Z16 = n.toZ16

  @pure def toZ32(n: U64): Z32 = n.toZ32

  @pure def toZ64(n: U64): Z64 = n.toZ64

  @pure def toN(n: U64): N = n.toN

  @pure def toN8(n: U64): N8 = n.toN8

  @pure def toN16(n: U64): N16 = n.toN16

  @pure def toN32(n: U64): N32 = n.toN32

  @pure def toN64(n: U64): N64 = n.toN64

  @pure def toS8(n: U64): S8 = n.toS8

  @pure def toS16(n: U64): S16 = n.toS16

  @pure def toS32(n: U64): S32 = n.toS32

  @pure def toS64(n: U64): S64 = n.toS64

  @pure def toU8(n: U64): U8 = n.toU8

  @pure def toU16(n: U64): U16 = n.toU16

  @pure def toU32(n: U64): U32 = n.toU32

  @pure def toU64(n: U64): U64 = n

  @pure def toRawS64(n: U64): S64 = math._S64.ValueImpl(n.value.toLong)

  @pure def toRawF64(n: U64): F64 = math._F64.ValueImpl(java.lang.Double.longBitsToDouble(n.value.toLong))
}


object F32_Ext {
  def random: F32 = math._F32.random

  @pure def toB(n: F32): B = n != f32"0.0"

  @pure def toRawU32(n: F32): U32 = math._U32.ValueImpl(UInt(java.lang.Float.floatToRawIntBits(n.value)))

  @pure def toF32(n: F32): F32 = n
}


object F64_Ext {
  def random: F64 = math._F64.random

  @pure def toB(n: F64): B = n != f64"0.0"

  @pure def toRawU64(n: F64): U64 = math._U64.ValueImpl(ULong(java.lang.Double.doubleToRawLongBits(n.value)))

  @pure def toF64(n: F64): F64 = n
}


object R_Ext {
  def random: R = math._R.random

  @pure def toB(n: R): B = n != r"0.0"

  @pure def toZ(n: R): Z = math._Z(n.value.floor.apply(0).toBigInt)

  @pure def toN(n: R): N = {
    require(n >= r"0.0")
    math._N(n.value.floor.apply(0).toBigInt)
  }

  @pure def toR(n: R): R = n
}


object SI_Ext {

  @pure def append[I <: INT, E](s: IS[I, E], e: E): IS[I, E] = s :+ e

  @pure def prepend[I <: INT, E](s: IS[I, E], e: E): IS[I, E] = e +: s

  @pure def appends[I <: INT, E](s1: IS[I, E], s2: IS[I, E]): IS[I, E] = s1 ++ s2

  @pure def toMS[I <: INT, E](s: IS[I, E]): MS[I, E] = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      new collection.MSImpl(s.sz, ArrayBuffer(s.data.map(_clone): _*))
  }

  @pure def chunk[I <: INT, E](s: IS[I, E], size: I): IS[I, IS[I, E]] = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz % sizeInt == 0)
      val sz = s.sz / sizeInt
      var result = Vector[IS[I, E]]()
      for (i <- 0 until sz) {
        var chunk = Vector[E]()
        for (j <- 0 until sizeInt) {
          chunk +:= _clone(s.data(i * sizeInt + j))
        }
        result +:= new collection.ISImpl(sizeInt, chunk)
      }
      new collection.ISImpl(sz, result)
  }

  @pure def drop[I <: INT, E](s: IS[I, E], size: I): IS[I, E] = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz >= sizeInt)
      var result = Vector[E]()
      for (i <- sizeInt until s.sz) {
        result +:= _clone(s.data(i))
      }
      new collection.ISImpl(s.sz - sizeInt, result)
  }

  @pure def foldLeft[I <: INT, E, R](s: IS[I, E], @pure f: (R, E) => R, init: R): R = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      var r = init
      for (e <- s.data) {
        r = f(r, e)
      }
      r
  }

  @pure def foldRight[I <: INT, E, R](s: IS[I, E], @pure f: (R, E) => R, init: R): R = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      var r = init
      for (e <- s.data.reverseIterator) {
        r = f(r, e)
      }
      r
  }

  @pure def map[I <: INT, E1, E2](s: IS[I, E1], @pure f: E1 => E2): IS[I, E2] = s match {
    case (s: collection.ISImpl[I, E1]@unchecked) =>
      new collection.ISImpl[I, E2](s.sz, _clone(s.data.map(f)))
  }

  @pure def take[I <: INT, E](s: IS[I, E], size: I): IS[I, E] = s match {
    case (s: collection.ISImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz >= sizeInt)
      var result = Vector[E]()
      for (i <- 0 until sizeInt) {
        result +:= _clone(s.data(i))
      }
      new collection.ISImpl(sizeInt, result)
  }

  @pure def fromU8[I <: INT](n: U8): IS[I, B] = {
    new collection.ISImpl[I, B](8, (0 until 8).toVector.map { i =>
      val mask = u8"1" << i.toU8
      _2B((n & mask) != mask)
    })
  }

  @pure def fromU16[I <: INT](n: U16): IS[I, B] = {
    new collection.ISImpl[I, B](16, (0 until 16).toVector.map { i =>
      val mask = u16"1" << i.toU16
      _2B((n & mask) != mask)
    })
  }

  @pure def fromU32[I <: INT](n: U32): IS[I, B] = {
    new collection.ISImpl[I, B](32, (0 until 32).toVector.map { i =>
      val mask = u32"1" << i.toU32
      _2B((n & mask) != mask)
    })
  }

  @pure def fromU64[I <: INT](n: U64): IS[I, B] = {
    new collection.ISImpl[I, B](64, (0 until 64).toVector.map { i =>
      val mask = u64"1" << i.toU64
      _2B((n & mask) != mask)
    })
  }

  @pure def toU8[I <: INT](s: IS[I, B]): U8 = {
    require(s.size == z"8")
    var result = u8"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u8"1" << i.toU8
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU16[I <: INT](s: IS[I, B]): U16 = {
    require(s.size == z"16")
    var result = u16"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u16"1" << i.toU16
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU32[I <: INT](s: IS[I, B]): U32 = {
    require(s.size == z"32")
    var result = u32"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u32"1" << i.toU32
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU64[I <: INT](s: IS[I, B]): U64 = {
    require(s.size == z"64")
    var result = u64"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u64"1" << i.toU64
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }
}

object SM_Ext {

  @pure def append[I <: INT, E](s: MS[I, E], e: E): MS[I, E] = s :+ e

  @pure def prepend[I <: INT, E](s: MS[I, E], e: E): MS[I, E] = e +: s

  @pure def appends[I <: INT, E](s1: MS[I, E], s2: MS[I, E]): MS[I, E] = s1 ++ s2

  @pure def toIS[I <: INT, E](s: MS[I, E]): IS[I, E] = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      new collection.ISImpl(s.sz, Vector(s.data.map(_clone): _*))
  }

  @pure def chunk[I <: INT, E](s: MS[I, E], size: I): MS[I, MS[I, E]] = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz % sizeInt == 0)
      val sz = s.sz / sizeInt
      val result = new ArrayBuffer[MS[I, E]](sz)
      for (i <- 0 until sz) {
        val chunk = new ArrayBuffer[E](sizeInt)
        for (j <- 0 until sizeInt) {
          chunk += _clone(s.data(i * sizeInt + j))
        }
        result += new collection.MSImpl(sizeInt, chunk)
      }
      new collection.MSImpl(sz, result)
  }

  @pure def drop[I <: INT, E](s: MS[I, E], size: I): MS[I, E] = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz >= sizeInt)
      val rSize = s.sz - sizeInt
      val result = new ArrayBuffer[E](rSize)
      for (i <- sizeInt until s.sz) {
        result += _clone(s.data(i))
      }
      new collection.MSImpl(rSize, result)
  }

  @pure def foldLeft[I <: INT, E, R](s: MS[I, E], @pure f: (R, E) => R, init: R): R = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      var r = init
      for (e <- s.data) {
        r = f(r, e)
      }
      r
  }

  @pure def foldRight[I <: INT, E, R](s: MS[I, E], @pure f: (R, E) => R, init: R): R = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      var r = init
      for (e <- s.data.reverseIterator) {
        r = f(r, e)
      }
      r
  }

  @pure def map[I <: INT, E1, E2](s: MS[I, E1], @pure f: E1 => E2): MS[I, E2] = s match {
    case (s: collection.MSImpl[I, E1]@unchecked) =>
      new collection.MSImpl[I, E2](s.sz, _clone(s.data.map(f)))
  }

  def transform[I <: INT, E](s: MS[I, E], @pure f: E => E): Unit = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      for (i <- s.data.indices) {
        s.data(i) = _clone(f(s.data(i)))
      }
  }

  @pure def take[I <: INT, E](s: MS[I, E], size: I): MS[I, E] = s match {
    case (s: collection.MSImpl[I, E]@unchecked) =>
      val sizeInt = size.toZ.toInt
      require(s.sz >= sizeInt)
      var result = new ArrayBuffer[E](sizeInt)
      for (i <- 0 until sizeInt) {
        result += _clone(s.data(i))
      }
      new collection.MSImpl(sizeInt, result)
  }

  @pure def fromU8[I <: INT](n: U8): MS[I, B] = {
    new collection.MSImpl[I, B](8, ArrayBuffer((0 until 8).map { i =>
      val mask = u8"1" << i.toU8
      _2B((n & mask) != mask)
    }: _*))
  }

  @pure def fromU16[I <: INT](n: U16): MS[I, B] = {
    new collection.MSImpl[I, B](16, ArrayBuffer((0 until 16).map { i =>
      val mask = u16"1" << i.toU16
      _2B((n & mask) != mask)
    }: _*))
  }

  @pure def fromU32[I <: INT](n: U32): MS[I, B] = {
    new collection.MSImpl[I, B](32, ArrayBuffer((0 until 32).map { i =>
      val mask = u32"1" << i.toU32
      _2B((n & mask) != mask)
    }: _*))
  }

  @pure def fromU64[I <: INT](n: U64): MS[I, B] = {
    new collection.MSImpl[I, B](64, ArrayBuffer((0 until 64).map { i =>
      val mask = u64"1" << i.toU64
      _2B((n & mask) != mask)
    }: _*))
  }

  @pure def toU8[I <: INT](s: MS[I, B]): U8 = {
    require(s.size == z"8")
    var result = u8"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u8"1" << i.toU8
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU16[I <: INT](s: MS[I, B]): U16 = {
    require(s.size == z"16")
    var result = u16"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u16"1" << i.toU16
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU32[I <: INT](s: MS[I, B]): U32 = {
    require(s.size == z"32")
    var result = u32"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u32"1" << i.toU32
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }

  @pure def toU64[I <: INT](s: MS[I, B]): U64 = {
    require(s.size == z"64")
    var result = u64"0"
    for (i <- 0 until s.size.toZ32.value) {
      val mask = u64"1" << i.toU64
      if (s(_Z(i))) {
        result = result | mask
      }
    }
    result
  }
}