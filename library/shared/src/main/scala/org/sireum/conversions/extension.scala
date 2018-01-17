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

package org.sireum.conversions

import org.sireum._
import org.sireum.Z8._
import org.sireum.Z16._
import org.sireum.Z32._
import org.sireum.Z64._
import org.sireum.N._
import org.sireum.N8._
import org.sireum.N16._
import org.sireum.N32._
import org.sireum.N64._
import org.sireum.S8._
import org.sireum.S16._
import org.sireum.S32._
import org.sireum.S64._
import org.sireum.U8._
import org.sireum.U16._
import org.sireum.U32._
import org.sireum.U64._

object B_Ext {

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

object C_Ext {
  @pure def toU16(c: C): U16 = org.sireum.U16(c.value)
}


object Z_Ext {

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

  @pure def toZ8(n: Z): Z8 = org.sireum.Z8(n)

  @pure def toZ16(n: Z): Z16 = org.sireum.Z16(n)

  @pure def toZ32(n: Z): Z32 = org.sireum.Z32(n)

  @pure def toZ64(n: Z): Z64 = org.sireum.Z64(n)

  @pure def toN(n: Z): N = org.sireum.N(n)

  @pure def toN8(n: Z): N8 = org.sireum.N8(n)

  @pure def toN16(n: Z): N16 = org.sireum.N16(n)

  @pure def toN32(n: Z): N32 = org.sireum.N32(n)

  @pure def toN64(n: Z): N64 = org.sireum.N64(n)

  @pure def toS8(n: Z): S8 = {
    require(isInRangeSigned8(n.toMP))
    org.sireum.S8(n)
  }

  @pure def toS16(n: Z): S16 = {
    require(isInRangeSigned16(n.toMP))
    org.sireum.S16(n)
  }

  @pure def toS32(n: Z): S32 = {
    require(isInRangeSigned32(n.toMP))
    org.sireum.S32(n)
  }

  @pure def toS64(n: Z): S64 = {
    require(isInRangeSigned64(n.toMP))
    org.sireum.S64(n)
  }

  @pure def toU8(n: Z): U8 = {
    require(isInRangeUnsigned8(n.toMP))
    org.sireum.U8(n)
  }

  @pure def toU16(n: Z): U16 = {
    require(isInRangeUnsigned16(n.toMP))
    org.sireum.U16(n)
  }

  @pure def toU32(n: Z): U32 = {
    require(isInRangeUnsigned32(n.toMP))
    org.sireum.U32(n)
  }

  @pure def toU64(n: Z): U64 = {
    require(isInRangeUnsigned64(n))
    org.sireum.U64(n)
  }

  @pure def toR(n: Z): R = org.sireum.R.$String(n.toString)

  @pure def toBinary(n: Z): ISZ[U8] = {
    var r = ISZ[U8]()
    for (e <- n.toBigInt.toByteArray) {
      r = r :+ org.sireum.U8(e)
    }
    r
  }

  @pure def fromBinary(bin: ISZ[U8]): Z = {
    org.sireum.Z(scala.BigInt(bin.elements.toArray.map(_.value)))
  }
}


object Z8_Ext {

  @pure def toB(n: Z8): B = n != z8"0"

  @pure def toZ(n: Z8): Z = n.toMP

  @pure def toZ8(n: Z8): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: Z8): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: Z8): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: Z8): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: Z8): N = org.sireum.N(n.toMP)

  @pure def toN8(n: Z8): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: Z8): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: Z8): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: Z8): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: Z8): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: Z8): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: Z8): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: Z8): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: Z8): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: Z8): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: Z8): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: Z8): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: Z8): R = org.sireum.R.$String(n.toString)
}


object Z16_Ext {

  @pure def toB(n: Z16): B = n != z16"0"

  @pure def toZ(n: Z16): Z = n.toMP

  @pure def toZ8(n: Z16): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: Z16): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: Z16): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: Z16): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: Z16): N = org.sireum.N(n.toMP)

  @pure def toN8(n: Z16): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: Z16): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: Z16): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: Z16): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: Z16): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: Z16): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: Z16): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: Z16): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: Z16): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: Z16): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: Z16): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: Z16): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: Z16): R = org.sireum.R.$String(n.toString)
}


object Z32_Ext {

  @pure def toB(n: Z32): B = n != z32"0"

  @pure def toZ(n: Z32): Z = n.toMP

  @pure def toZ8(n: Z32): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: Z32): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: Z32): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: Z32): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: Z32): N = org.sireum.N(n.toMP)

  @pure def toN8(n: Z32): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: Z32): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: Z32): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: Z32): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: Z32): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: Z32): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: Z32): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: Z32): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: Z32): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: Z32): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: Z32): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: Z32): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: Z32): R = org.sireum.R.$String(n.toString)
}


object Z64_Ext {

  @pure def toB(n: Z64): B = n != z64"0"

  @pure def toZ(n: Z64): Z = n.toMP

  @pure def toZ8(n: Z64): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: Z64): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: Z64): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: Z64): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: Z64): N = org.sireum.N(n.toMP)

  @pure def toN8(n: Z64): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: Z64): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: Z64): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: Z64): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: Z64): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: Z64): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: Z64): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: Z64): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: Z64): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: Z64): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: Z64): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: Z64): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: Z64): R = org.sireum.R.$String(n.toString)
}


object N_Ext {

  @pure def toB(n: N): B = n != n"0"

  @pure def toZ(n: N): Z = n.toMP

  @pure def toZ8(n: N): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: N): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: N): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: N): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: N): N = org.sireum.N(n.toMP)

  @pure def toN8(n: N): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: N): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: N): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: N): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: N): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: N): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: N): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: N): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: N): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: N): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: N): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: N): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: N): R = org.sireum.R.$String(n.toString)
}


object N8_Ext {

  @pure def toB(n: N8): B = n != n8"0"

  @pure def toZ(n: N8): Z = n.toMP

  @pure def toZ8(n: N8): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: N8): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: N8): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: N8): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: N8): N = org.sireum.N(n.toMP)

  @pure def toN8(n: N8): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: N8): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: N8): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: N8): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: N8): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: N8): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: N8): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: N8): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: N8): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: N8): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: N8): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: N8): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: N8): R = org.sireum.R.$String(n.toString)
}


object N16_Ext {

  @pure def toB(n: N16): B = n != n16"0"

  @pure def toZ(n: N16): Z = n.toMP

  @pure def toZ8(n: N16): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: N16): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: N16): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: N16): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: N16): N = org.sireum.N(n.toMP)

  @pure def toN8(n: N16): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: N16): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: N16): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: N16): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: N16): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: N16): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: N16): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: N16): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: N16): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: N16): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: N16): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: N16): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: N16): R = org.sireum.R.$String(n.toString)
}


object N32_Ext {

  @pure def toB(n: N32): B = n != n32"0"

  @pure def toZ(n: N32): Z = n.toMP

  @pure def toZ8(n: N32): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: N32): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: N32): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: N32): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: N32): N = org.sireum.N(n.toMP)

  @pure def toN8(n: N32): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: N32): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: N32): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: N32): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: N32): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: N32): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: N32): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: N32): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: N32): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: N32): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: N32): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: N32): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: N32): R = org.sireum.R.$String(n.toString)
}


object N64_Ext {

  @pure def toB(n: N64): B = n != n64"0"

  @pure def toZ(n: N64): Z = n.toMP

  @pure def toZ8(n: N64): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: N64): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: N64): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: N64): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: N64): N = org.sireum.N(n.toMP)

  @pure def toN8(n: N64): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: N64): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: N64): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: N64): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: N64): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: N64): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: N64): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: N64): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: N64): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: N64): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: N64): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: N64): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: N64): R = org.sireum.R.$String(n.toString)
}


object S8_Ext {

  @pure def toB(n: S8): B = n != s8"0"

  @pure def toZ(n: S8): Z = n.toMP

  @pure def toZ8(n: S8): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: S8): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: S8): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: S8): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: S8): N = org.sireum.N(n.toMP)

  @pure def toN8(n: S8): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: S8): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: S8): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: S8): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: S8): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: S8): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: S8): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: S8): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: S8): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: S8): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: S8): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: S8): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: S8): R = org.sireum.R.$String(n.toString)

  @pure def toRawU8(n: S8): U8 = org.sireum.U8(n.value)
}


object S16_Ext {

  @pure def toB(n: S16): B = n != s16"0"

  @pure def toZ(n: S16): Z = n.toMP

  @pure def toZ8(n: S16): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: S16): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: S16): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: S16): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: S16): N = org.sireum.N(n.toMP)

  @pure def toN8(n: S16): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: S16): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: S16): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: S16): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: S16): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: S16): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: S16): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: S16): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: S16): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: S16): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: S16): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: S16): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: S16): R = org.sireum.R.$String(n.toString)

  @pure def toRawU16(n: S16): U16 = org.sireum.U16(n.value)
}


object S32_Ext {

  @pure def toB(n: S32): B = n != s32"0"

  @pure def toZ(n: S32): Z = n.toMP

  @pure def toZ8(n: S32): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: S32): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: S32): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: S32): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: S32): N = org.sireum.N(n.toMP)

  @pure def toN8(n: S32): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: S32): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: S32): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: S32): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: S32): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: S32): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: S32): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: S32): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: S32): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: S32): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: S32): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: S32): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: S32): R = org.sireum.R.$String(n.toString)

  @pure def toRawU32(n: S32): U32 = org.sireum.U32(n.value)
}


object S64_Ext {

  @pure def toB(n: S64): B = n != s64"0"

  @pure def toZ(n: S64): Z = n.toMP

  @pure def toZ8(n: S64): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: S64): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: S64): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: S64): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: S64): N = org.sireum.N(n.toMP)

  @pure def toN8(n: S64): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: S64): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: S64): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: S64): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: S64): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: S64): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: S64): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: S64): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: S64): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: S64): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: S64): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: S64): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: S64): R = org.sireum.R.$String(n.toString)

  @pure def toRawU64(n: S64): U64 = org.sireum.U64(n.value)
}


object U8_Ext {

  @pure def toB(n: U8): B = n != u8"0"

  @pure def toZ(n: U8): Z = n.toMP

  @pure def toZ8(n: U8): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: U8): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: U8): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: U8): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: U8): N = org.sireum.N(n.toMP)

  @pure def toN8(n: U8): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: U8): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: U8): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: U8): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: U8): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: U8): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: U8): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: U8): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: U8): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: U8): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: U8): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: U8): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: U8): R = org.sireum.R.$String(n.toString)

  @pure def toRawS8(n: U8): S8 = org.sireum.S8(n.value)
}


object U16_Ext {

  @pure def toB(n: U16): B = n != u16"0"

  @pure def toZ(n: U16): Z = n.toMP

  @pure def toZ8(n: U16): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: U16): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: U16): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: U16): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: U16): N = org.sireum.N(n.toMP)

  @pure def toN8(n: U16): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: U16): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: U16): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: U16): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: U16): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: U16): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: U16): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: U16): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: U16): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: U16): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: U16): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: U16): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: U16): R = org.sireum.R.$String(n.toString)

  @pure def toRawS16(n: U16): S16 = org.sireum.S16(n.value)

  @pure def toC(n: U16): C = org.sireum.C(n.value.toChar)
}


object U32_Ext {

  @pure def toB(n: U32): B = n != u32"0"

  @pure def toZ(n: U32): Z = n.toMP

  @pure def toZ8(n: U32): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: U32): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: U32): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: U32): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: U32): N = org.sireum.N(n.toMP)

  @pure def toN8(n: U32): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: U32): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: U32): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: U32): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: U32): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: U32): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: U32): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: U32): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: U32): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: U32): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: U32): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: U32): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: U32): R = org.sireum.R.$String(n.toString)

  @pure def toRawS32(n: U32): S32 = org.sireum.S32(n.value)

  @pure def toRawF32(n: U32): F32 = org.sireum.F32(_root_.java.lang.Float.intBitsToFloat(n.value))
}


object U64_Ext {

  @pure def toB(n: U64): B = n != u64"0"

  @pure def toZ(n: U64): Z = n.toMP

  @pure def toZ8(n: U64): Z8 = org.sireum.Z8(n.toMP)

  @pure def toZ16(n: U64): Z16 = org.sireum.Z16(n.toMP)

  @pure def toZ32(n: U64): Z32 = org.sireum.Z32(n.toMP)

  @pure def toZ64(n: U64): Z64 = org.sireum.Z64(n.toMP)

  @pure def toN(n: U64): N = org.sireum.N(n.toMP)

  @pure def toN8(n: U64): N8 = org.sireum.N8(n.toMP)

  @pure def toN16(n: U64): N16 = org.sireum.N16(n.toMP)

  @pure def toN32(n: U64): N32 = org.sireum.N32(n.toMP)

  @pure def toN64(n: U64): N64 = org.sireum.N64(n.toMP)

  @pure def toS8(n: U64): S8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned8(mp))
    org.sireum.S8(mp)
  }

  @pure def toS16(n: U64): S16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned16(mp))
    org.sireum.S16(mp)
  }

  @pure def toS32(n: U64): S32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned32(mp))
    org.sireum.S32(mp)
  }

  @pure def toS64(n: U64): S64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeSigned64(mp))
    org.sireum.S64(mp)
  }

  @pure def toU8(n: U64): U8 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned8(mp))
    org.sireum.U8(mp)
  }

  @pure def toU16(n: U64): U16 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned16(mp))
    org.sireum.U16(mp)
  }

  @pure def toU32(n: U64): U32 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned32(mp))
    org.sireum.U32(mp)
  }

  @pure def toU64(n: U64): U64 = {
    val mp = n.toMP
    require(Z_Ext.isInRangeUnsigned64(mp))
    org.sireum.U64(mp)
  }

  @pure def toR(n: U64): R = org.sireum.R.$String(n.toString)

  @pure def toRawS64(n: U64): S64 = org.sireum.S64(n.value)

  @pure def toRawF64(n: U64): F64 = org.sireum.F64(_root_.java.lang.Double.longBitsToDouble(n.value))
}


object F32_Ext {

  @pure def toB(n: F32): B = n != f32"0.0"

  @pure def toRawU32(n: F32): U32 = org.sireum.U32(_root_.java.lang.Float.floatToRawIntBits(n.value))

  @pure def toF32(n: F32): F32 = n
}


object F64_Ext {

  @pure def toB(n: F64): B = n != f64"0.0"

  @pure def toRawU64(n: F64): U64 = org.sireum.U64(_root_.java.lang.Double.doubleToRawLongBits(n.value))

  @pure def toF64(n: F64): F64 = n
}


object R_Ext {

  @pure def toB(n: R): B = n != r"0.0"

  @pure def toZ(n: R): Z = org.sireum.Z(n.value.floor.apply(0).toBigInt)

  @pure def toN(n: R): N = {
    require(n >= r"0.0")
    org.sireum.N(n.value.floor.apply(0).toBigInt)
  }

  @pure def toR(n: R): R = n
}

object String_Ext {

  @pure def fromBis[I](bs: IS[I, U8]): String = {
    if (bs.isEmpty) return ""
    new _root_.java.lang.String(bs.elements.toArray.map(_.value), "UTF-8")
  }

  @pure def fromBms[I](bs: MS[I, U8]): String = {
    if (bs.isEmpty) return ""
    new _root_.java.lang.String(bs.elements.toArray.map(_.value), "UTF-8")
  }

  @pure def fromCis[I](cs: IS[I, C]): String = {
    if (cs.isEmpty) return ""
    new Predef.String(cs.data.asInstanceOf[scala.Array[scala.Char]], 0, cs.length.toInt)
  }

  @pure def fromCms[I](cs: MS[I, C]): String = {
    if (cs.isEmpty) return ""
    new Predef.String(cs.data.asInstanceOf[scala.Array[scala.Char]], 0, cs.length.toInt)
  }

  @pure def toBis(s: String): IS[Z, U8] =
    IS[Z, U8](s.value.getBytes("UTF-8").map(org.sireum.U8(_)): _*)

  @pure def toBms(s: String): MS[Z, U8] =
    MS[Z, U8](s.value.getBytes("UTF-8").map(org.sireum.U8(_)): _*)

  @pure def toCis(s: String): IS[Z, C] =
    IS[Z, C](s.value.map(org.sireum.C.apply): _*)

  @pure def toCms(s: String): MS[Z, C] =
    MS[Z, C](s.value.map(org.sireum.C.apply): _*)
}