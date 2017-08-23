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
/*
package org.sireum

@ext object B {
  def random: B = $

  @pure def toB(b: B): B =
    l""" ensures result ≡ b """

  @pure def toZ(b: B): Z =
    l""" ensures result ≡ (if (b) 1 else 0) """

  @pure def toZ8(b: B): Z8 =
    l""" ensures result ≡ (if (b) z8"1" else z8"0") """

  @pure def toZ16(b: B): Z16 =
    l""" ensures result ≡ (if (b) z16"1" else z16"0") """

  @pure def toZ32(b: B): Z32 =
    l""" ensures result ≡ (if (b) z32"1" else z32"0") """

  @pure def toZ64(b: B): Z64 =
    l""" ensures result ≡ (if (b) z64"1" else z64"0") """

  @pure def toN(b: B): N =
    l""" ensures result ≡ (if (b) n"1" else n"0") """

  @pure def toN8(b: B): N8 =
    l""" ensures result ≡ (if (b) n8"1" else n8"0") """

  @pure def toN16(b: B): N16 =
    l""" ensures result ≡ (if (b) n16"1" else n16"0") """

  @pure def toN32(b: B): N32 =
    l""" ensures result ≡ (if (b) n32"1" else n32"0") """

  @pure def toN64(b: B): N64 =
    l""" ensures result ≡ (if (b) n64"1" else n64"0") """

  @pure def toS8(b: B): S8 =
    l""" ensures result ≡ (if (b) s8"1" else s8"0") """

  @pure def toS16(b: B): S16 =
    l""" ensures result ≡ (if (b) s16"1" else s16"0") """

  @pure def toS32(b: B): S32 =
    l""" ensures result ≡ (if (b) s32"1" else s32"0") """

  @pure def toS64(b: B): S64 =
    l""" ensures result ≡ (if (b) s64"1" else s64"0") """

  @pure def toU8(b: B): U8 =
    l""" ensures result ≡ (if (b) u8"1" else u8"0") """

  @pure def toU16(b: B): U16 =
    l""" ensures result ≡ (if (b) u16"1" else u16"0") """

  @pure def toU32(b: B): U32 =
    l""" ensures result ≡ (if (b) u32"1" else u32"0") """

  @pure def toU64(b: B): U64 =
    l""" ensures result ≡ (if (b) u64"1" else u64"0") """

  @pure def toF32(b: B): F32 =
    l""" ensures result ≡ (if (b) f32"1.0" else f32"0.0") """

  @pure def toF64(b: B): F64 =
    l""" ensures result ≡ (if (b) f64"1.0" else f64"0.0") """

  @pure def toR(b: B): R =
    l""" ensures result ≡ (if (b) r"1.0" else r"0.0") """
}


@ext object Z {
  def random: Z = $

  @pure def isInRangeSigned8(n: Z): B =
    l""" ensures result ≡ (-128 ≤ n ∧ n ≤ 127) """

  @pure def isInRangeSigned16(n: Z): B =
    l""" ensures result ≡ (-32768 ≤ n ∧ n ≤ 32767) """

  @pure def isInRangeSigned32(n: Z): B =
    l""" ensures result ≡ (-2147483648 ≤ n ∧ n ≤ 2147483647) """

  @pure def isInRangeSigned64(n: Z): B =
    l""" ensures result ≡ (z"-9223372036854775808" ≤ n ∧ n ≤ z"9223372036854775807") """

  @pure def isInRangeUnsigned8(n: Z): B =
    l""" ensures result ≡ (0 ≤ n ∧ n ≤ 255) """

  @pure def isInRangeUnsigned16(n: Z): B =
    l""" ensures result ≡ (0 ≤ n ∧ n ≤ 65535) """

  @pure def isInRangeUnsigned32(n: Z): B =
    l""" ensures result ≡ (0 ≤ n ∧ n ≤ z"4294967295") """

  @pure def isInRangeUnsigned64(n: Z): B =
    l""" ensures result ≡ (0 ≤ n ∧ n ≤ z"18446744073709551615") """

  @pure def toB(n: Z): B =
    l""" ensures result ≡ (n ≠ 0) """

  @pure def toZ(n: Z): Z =
    l""" ensures result ≡ n """

  @pure def toZ8(n: Z): Z8 =
    l""" requires -128 ≤ n ∧ n ≤ 127
         ensures  Z8.toZ(result) ≡ n """

  @pure def toZ16(n: Z): Z16 =
    l""" requires -32768 ≤ n ∧ n ≤ 32767
         ensures  Z16.toZ(result) ≡ n    """

  @pure def toZ32(n: Z): Z32 =
    l""" requires -2147483648 ≤ n ∧ n ≤ 2147483647
         ensures  Z32.toZ(result) ≡ n              """

  @pure def toZ64(n: Z): Z64 =
    l""" requires z"-9223372036854775808" ≤ n ∧ n ≤ z"9223372036854775807"
         ensures  Z64.toZ(result) ≡ n                                      """

  @pure def toN(n: Z): N =
    l""" requires n ≥ 0
         ensures  N.toZ(result) ≡ n """

  @pure def toN8(n: Z): N8 =
    l""" requires 0 ≤ n ∧ n ≤ 255
         ensures  N8.toZ(result) ≡ n """

  @pure def toN16(n: Z): N16 =
    l""" requires 0 ≤ n ∧ n ≤ 65535
         ensures  N16.toZ(result) ≡ n """

  @pure def toN32(n: Z): N32 =
    l""" requires 0 ≤ n ∧ n ≤ z"4294967295"
         ensures  N32.toZ(result) ≡ n      """

  @pure def toN64(n: Z): N64 =
    l""" requires 0 ≤ n ∧ n ≤ z"18446744073709551615"
         ensures  N64.toZ(result) ≡ n                 """

  @pure def toS8(n: Z): S8 =
    l""" requires -128 ≤ n ∧ n ≤ 127
         ensures  S8.toZ(result) ≡ n """

  @pure def toS16(n: Z): S16 =
    l""" requires -32768 ≤ n ∧ n ≤ 32767
         ensures  S16.toZ(result) ≡ n   """

  @pure def toS32(n: Z): S32 =
    l""" requires -2147483648 ≤ n ∧ n ≤ 2147483647
         ensures  S32.toZ(result) ≡ n              """

  @pure def toS64(n: Z): S64 =
    l""" requires z"-9223372036854775808" ≤ n ∧ n ≤ z"9223372036854775807"
         ensures  S64.toZ(result) ≡ n                                      """

  @pure def toU8(n: Z): U8 =
    l""" requires 0 ≤ n ∧ n ≤ 255
         ensures  U8.toZ(result) ≡ n """

  @pure def toU16(n: Z): U16 =
    l""" requires 0 ≤ n ∧ n ≤ 65535
         ensures  U16.toZ(result) ≡ n """

  @pure def toU32(n: Z): U32 =
    l""" requires 0 ≤ n ∧ n ≤ z"4294967295"
         ensures  U32.toZ(result) ≡ n       """

  @pure def toU64(n: Z): U64 =
    l""" requires 0 ≤ n ∧ n ≤ z"18446744073709551615"
         ensures  U64.toZ(result) ≡ n                 """

  /* @first */
  @pure def toR(n: Z): R = $
}


@ext object Z8 {
  val Max: Z8 = z8"127"

  val Min: Z8 = z8"-128"

  l""" invariant Max ≡ z8"127"
                 Min ≡ z8"-128" """

  def random: Z8 = $

  @pure def toB(n: Z8): B =
    l""" ensures result ≡ (n ≠ z8"0") """

  /* @first */
  @pure def toZ(n: Z8): Z = $

  @pure def toZ8(n: Z8): Z8 =
    l""" ensures result ≡ n """

  @pure def toZ16(n: Z8): Z16 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: Z8): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: Z8): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: Z8): N =
    l""" requires n ≥ z8"0"
         ensures  N.toZ(result) ≡ toZ(n) """

  @pure def toN8(n: Z8): N8 =
    l""" requires n ≥ z8"0"
         ensures  N8.toZ(result) ≡ toZ(n) """

  @pure def toN16(n: Z8): N16 =
    l""" requires n ≥ z8"0"
         ensures  N16.toZ(result) ≡ toZ(n) """

  @pure def toN32(n: Z8): N32 =
    l""" requires n ≥ z8"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: Z8): N64 =
    l""" requires n ≥ z8"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  @pure def toS8(n: Z8): S8 =
    l""" ensures S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: Z8): S16 =
    l""" ensures S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: Z8): S32 =
    l""" ensures S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: Z8): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: Z8): U8 =
    l""" requires n ≥ z8"0"
         ensures  U8.toZ(result) ≡ toZ(n) """

  @pure def toU16(n: Z8): U16 =
    l""" requires n ≥ z8"0"
         ensures  U16.toZ(result) ≡ toZ(n) """

  @pure def toU32(n: Z8): U32 =
    l""" requires n ≥ z8"0"
         ensures  U32.toZ(result) ≡ toZ(n) """

  @pure def toU64(n: Z8): U64 =
    l""" requires n ≥ z8"0"
         ensures  U64.toZ(result) ≡ toZ(n) """

  @pure def toR(n: Z8): R =
    l""" ensures result ≡ Z.toR(toZ(n)) """
}


@ext object Z16 {
  val Max: Z16 = z16"32767"

  val Min: Z16 = z16"-32768"

  l""" invariant Max ≡ z16"32767"
                 Min ≡ z16"-32768" """

  def random: Z16 = $

  @pure def toB(n: Z16): B =
    l""" ensures result ≡ (n ≠ z16"0") """

  /* @first */
  @pure def toZ(n: Z16): Z = $

  @pure def toZ8(n: Z16): Z8 =
    l""" requires z16"-128" ≤ n ∧ n ≤ z16"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: Z16): Z16 =
    l""" ensures result ≡ n """

  @pure def toZ32(n: Z16): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: Z16): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: Z16): N =
    l""" requires n ≥ z16"0"
         ensures  N.toZ(result) ≡ toZ(n) """

  @pure def toN8(n: Z16): N8 =
    l""" requires z16"0" ≤ n ∧ n ≤ z16"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: Z16): N16 =
    l""" requires n ≥ z16"0"
         ensures  N16.toZ(result) ≡ toZ(n) """

  @pure def toN32(n: Z16): N32 =
    l""" requires n ≥ z16"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: Z16): N64 =
    l""" requires n ≥ z16"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  @pure def toS8(n: Z16): S8 =
    l""" requires z16"-128" ≤ n ∧ n ≤ z16"127"
         ensures  S8.toZ(result) ≡ toZ(n)      """

  @pure def toS16(n: Z16): S16 =
    l""" ensures S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: Z16): S32 =
    l""" ensures S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: Z16): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: Z16): U8 =
    l""" requires z16"0" ≤ n ∧ n ≤ z16"255"
         ensures  U8.toZ(result) ≡ toZ(n)   """

  @pure def toU16(n: Z16): U16 =
    l""" requires n ≥ z16"0"
         ensures  U16.toZ(result) ≡ toZ(n) """

  @pure def toU32(n: Z16): U32 =
    l""" requires n ≥ z16"0"
         ensures  U32.toZ(result) ≡ toZ(n) """

  @pure def toU64(n: Z16): U64 =
    l""" requires n ≥ z16"0"
         ensures  U64.toNZ(result) ≡ toZ(n) """

  @pure def toR(n: Z16): R =
    l""" ensures result ≡ Z.toR(toZ(n)) """
}


@ext object Z32 {
  val Max: Z32 = z32"2147483647"

  val Min: Z32 = z32"-2147483648"

  l""" invariant Max ≡ z32"2147483647"
                 Min ≡ z32"-2147483648" """

  def random: Z32 = $

  @pure def toB(n: Z32): B =
    l""" ensures result ≡ (n ≠ z32"0") """

  /* @first */
  @pure def toZ(n: Z32): Z = $

  @pure def toZ8(n: Z32): Z8 =
    l""" requires z32"-128" ≤ n ∧ n ≤ z32"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: Z32): Z16 =
    l""" requires z32"-32768" ≤ n ∧ n ≤ z32"32767"
         ensures  Z16.toZ(result) ≡ toZ(n)         """

  @pure def toZ32(n: Z32): Z32 =
    l""" ensures result ≡ n """

  @pure def toZ64(n: Z32): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: Z32): N =
    l""" requires n ≥ z32"0"
         ensures  N.toZ(result) ≡ toZ(n) """

  @pure def toN8(n: Z32): N8 =
    l""" requires z32"0" ≤ n ∧ n ≤ z32"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: Z32): N16 =
    l""" requires z32"0" ≤ n ∧ n ≤ z32"65535"
         ensures  N16.toZ(result) ≡ toZ(n)    """

  @pure def toN32(n: Z32): N32 =
    l""" requires n ≥ z32"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: Z32): N64 =
    l""" requires n ≥ z32"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  @pure def toS8(n: Z32): S8 =
    l""" requires z32"-128" ≤ n ∧ n ≤ z32"127"
         ensures  S8.toZ(result) ≡ toZ(n)      """

  @pure def toS16(n: Z32): S16 =
    l""" requires z32"-32768" ≤ n ∧ n ≤ z32"32767"
         ensures  S16.toZ(result) ≡ toZ(n)         """

  @pure def toS32(n: Z32): S32 =
    l""" ensures S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: Z32): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: Z32): U8 =
    l""" requires z32"0" ≤ n ∧ n ≤ z32"255"
         ensures  U8.toZ(result) ≡ toZ(n)   """

  @pure def toU16(n: Z32): U16 =
    l""" requires z32"0" ≤ n ∧ n ≤ z32"65535"
         ensures  U16.toZ(result) ≡ toZ(n)    """

  @pure def toU32(n: Z32): U32 =
    l""" requires n ≥ z32"0"
         ensures  U32.toZ(result) ≡ toZ(n) """

  @pure def toU64(n: Z32): U64 =
    l""" requires n ≥ z32"0"
         ensures  U64.toZ(result) ≡ toZ(n) """

  @pure def toR(n: Z32): R =
    l""" ensures result ≡ Z.toR(toZ(n)) """
}


@ext object Z64 {
  val Max: Z64 = z64"9223372036854775807"

  val Min: Z64 = z64"-9223372036854775808"

  l""" invariant Max ≡ z64"9223372036854775807"
                 Min ≡ z64"-9223372036854775808" """

  def random: Z64 = $

  @pure def toB(n: Z64): B =
    l""" ensures result ≡ (n ≠ z64"0") """

  /* @first */
  @pure def toZ(n: Z64): Z = $

  @pure def toZ8(n: Z64): Z8 =
    l""" requires z64"-128" ≤ n ∧ n ≤ z64"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: Z64): Z16 =
    l""" requires z64"-32768" ≤ n ∧ n ≤ z64"32767"
         ensures  Z16.toZ(result) ≡ toZ(n)         """

  @pure def toZ32(n: Z64): Z32 =
    l""" requires z64"-2147483648" ≤ n ∧ n ≤ z64"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n)                   """

  @pure def toZ64(n: Z64): Z64 =
    l""" ensures result ≡ n """

  @pure def toN(n: Z64): N =
    l""" requires n ≥ z64"0"
         ensures  N.toZ(result) ≡ toZ(n) """

  @pure def toN8(n: Z64): N8 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: Z64): N16 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"65535"
         ensures  N16.toZ(result) ≡ toZ(n)    """

  @pure def toN32(n: Z64): N32 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"4294967295"
         ensures  N32.toZ(result) ≡ toZ(n)         """

  @pure def toN64(n: Z64): N64 =
    l""" ensures N64.toZ(result) ≡ toZ(n) """

  @pure def toS8(n: Z64): S8 =
    l""" requires z64"-128" ≤ n ∧ n ≤ z64"127"
         ensures  S8.toZ(result) ≡ toZ(n)      """

  @pure def toS16(n: Z64): S16 =
    l""" requires z64"-32768" ≤ n ∧ n ≤ z64"32767"
         ensures  S16.toZ(result) ≡ toZ(n)         """

  @pure def toS32(n: Z64): S32 =
    l""" requires z64"-2147483648" ≤ n ∧ n ≤ z64"2147483647"
         ensures  S32.toZ(result) ≡ toZ(n)                   """

  @pure def toS64(n: Z64): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: Z64): U8 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"255"
         ensures  U8.toZ(result) ≡ toZ(n)   """

  @pure def toU16(n: Z64): U16 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"65535"
         ensures  U16.toZ(result) ≡ toZ(n)    """

  @pure def toU32(n: Z64): U32 =
    l""" requires z64"0" ≤ n ∧ n ≤ z64"4294967295"
         ensures  U32.toZ(result) ≡ toZ(n)         """

  @pure def toU64(n: Z64): U64 =
    l""" requires z64"0" ≤ n
         ensures  U64.toZ(result) ≡ toZ(n) """

  @pure def toR(n: Z64): R =
    l""" ensures result ≡ Z.toR(toZ(n)) """
}


@ext object N {
  def random: N = $

  @pure def toB(n: N): B =
    l""" ensures result ≡ (n ≠ n"0") """

  /* @first */
  @pure def toZ(n: N): Z = $

  @pure def toZ8(n: N): Z8 =
    l""" requires n ≤ n"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: N): Z16 =
    l""" requires n ≤ n"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: N): Z32 =
    l""" requires n ≤ n"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: N): Z64 =
    l""" requires n ≤ n"9223372036854775807"
         ensures  Z64.toZ(result) ≡ toZ(n)   """

  @pure def toN(n: N): N =
    l""" ensures result ≡ n """

  @pure def toN8(n: N): N8 =
    l""" requires n ≤ n"255"
         ensures  N8.toN(result) ≡ n """

  @pure def toN16(n: N): N16 =
    l""" requires n ≤ n"65535"
         ensures  N16.toN(result) ≡ n """

  @pure def toN32(n: N): N32 =
    l""" requires n ≤ n"4294967295"
         ensures  N32.toN(result) ≡ n """

  @pure def toN64(n: N): N64 =
    l""" requires n ≤ n"18446744073709551615"
         ensures  N64.toN(result) ≡ n         """

  @pure def toS8(n: N): S8 =
    l""" requires n ≤ n"127"
         ensures  S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: N): S16 =
    l""" requires n ≤ n"32767"
         ensures  S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: N): S32 =
    l""" requires n ≤ n"2147483647"
         ensures  S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: N): S64 =
    l""" requires n ≤ n"9223372036854775807"
         ensures  S64.toZ(result) ≡ toZ(n)   """

  @pure def toU8(n: N): U8 =
    l""" requires n ≤ n"255"
         ensures  U8.toN(result) ≡ n """

  @pure def toU16(n: N): U16 =
    l""" requires n ≤ n"65535"
         ensures  U16.toN(result) ≡ n """

  @pure def toU32(n: N): U32 =
    l""" requires n ≤ n"4294967295"
         ensures  U32.toN(result) ≡ n """

  @pure def toU64(n: N): U64 =
    l""" requires n ≤ n"18446744073709551615"
         ensures  U64.toN(result) ≡ n         """

  /* @first */
  @pure def toR(n: N): R = $
}


@ext object N8 {
  val Max: N8 = n8"255"

  l""" invariant Max ≡ n8"255" """

  def random: N8 = $

  @pure def toB(n: N8): B =
    l""" ensures result ≡ (n ≠ n8"0") """

  /* @first */
  @pure def toZ(n: N8): Z = $

  @pure def toZ8(n: N8): Z8 =
    l""" requires n ≤ n8"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: N8): Z16 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: N8): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: N8): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toN(n: N8): N = $

  @pure def toN8(n: N8): N8 =
    l""" ensures result ≡ n """

  @pure def toN16(n: N8): N16 =
    l""" ensures N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: N8): N32 =
    l""" ensures N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: N8): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  @pure def toS8(n: N8): S8 =
    l""" requires n ≤ n8"127"
         ensures  S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: N8): S16 =
    l""" ensures S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: N8): S32 =
    l""" ensures S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: N8): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: N8): U8 =
    l""" ensures U8.toN(result) ≡ toN(n) """

  @pure def toU16(n: N8): U16 =
    l""" ensures U16.toN(result) ≡ toN(n) """

  @pure def toU32(n: N8): U32 =
    l""" ensures U32.toN(result) ≡ toN(n) """

  @pure def toU64(n: N8): U64 =
    l""" ensures U64.toN(result) ≡ toN(n) """

  @pure def toR(n: N8): R =
    l""" ensures result ≡ N.toR(toN(n)) """
}


@ext object N16 {
  val Max: N16 = n16"65535"

  l""" invariant Max ≡ n16"65535" """

  def random: N16 = $

  @pure def toB(n: N16): B =
    l""" ensures result ≡ (n ≠ n16"0") """

  /* @first */
  @pure def toZ(n: N16): Z = $

  @pure def toZ8(n: N16): Z8 =
    l""" requires n ≤ n16"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: N16): Z16 =
    l""" requires n ≤ n16"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: N16): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: N16): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toN(n: N16): N = $

  @pure def toN8(n: N16): N8 =
    l""" requires n ≤ n16"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: N16): N16 =
    l""" ensures N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: N16): N32 =
    l""" ensures N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: N16): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  @pure def toS8(n: N16): S8 =
    l""" requires n ≤ n16"127"
         ensures  S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: N16): S16 =
    l""" requires n ≤ n16"32767"
         ensures  S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: N16): S32 =
    l""" ensures S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: N16): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: N16): U8 =
    l""" requires n ≤ n16"255"
         ensures  U8.toN(result) ≡ toN(n) """

  @pure def toU16(n: N16): U16 =
    l""" ensures U16.toN(result) ≡ toN(n) """

  @pure def toU32(n: N16): U32 =
    l""" ensures U32.toN(result) ≡ toN(n) """

  @pure def toU64(n: N16): U64 =
    l""" ensures U64.toN(result) ≡ toN(n) """

  @pure def toR(n: N16): R =
    l""" ensures result ≡ N.toR(toN(n)) """
}


@ext object N32 {
  val Max: N32 = n32"4294967295"

  l""" invariant Max ≡ n32"4294967295" """

  def random: N32 = $

  @pure def toB(n: N32): B =
    l""" ensures result ≡ (n ≠ n32"0") """

  /* @first */
  @pure def toZ(n: N32): Z = $

  @pure def toZ8(n: N32): Z8 =
    l""" requires n ≤ n32"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: N32): Z16 =
    l""" requires n ≤ n32"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: N32): Z32 =
    l""" requires n ≤ n32"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: N32): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toN(n: N32): N = $

  @pure def toN8(n: N32): N8 =
    l""" requires n ≤ n32"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: N32): N16 =
    l""" requires n ≤ n32"65535"
         ensures  N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: N32): N32 =
    l""" ensures result ≡ n """

  @pure def toN64(n: N32): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  @pure def toS8(n: N32): S8 =
    l""" requires n ≤ n32"127"
         ensures  S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: N32): S16 =
    l""" requires n ≤ n32"32767"
         ensures  S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: N32): S32 =
    l""" requires n ≤ n32"2147483647"
         ensures  S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: N32): S64 =
    l""" ensures S64.toZ(result) ≡ toZ(n) """

  @pure def toU8(n: N32): U8 =
    l""" requires n ≤ n32"255"
         ensures  U8.toN(result) ≡ toN(n) """

  @pure def toU16(n: N32): U16 =
    l""" requires n ≤ n32"65535"
         ensures  U16.toN(result) ≡ toN(n) """

  @pure def toU32(n: N32): U32 =
    l""" ensures U32.toN(result) ≡ toN(n) """

  @pure def toU64(n: N32): U64 =
    l""" ensures U32.toN(result) ≡ toN(n) """

  @pure def toR(n: N32): R =
    l""" ensures result ≡ N.toR(toN(n)) """
}


@ext object N64 {
  val Max: N64 = n64"18446744073709551615"

  l""" invariant Max ≡ n64"18446744073709551615" """

  def random: N64 = $

  @pure def toB(n: N64): B =
    l""" ensures result ≡ (n ≠ n64"0") """

  /* @first */
  @pure def toZ(n: N64): Z = $

  @pure def toZ8(n: N64): Z8 =
    l""" requires n ≤ n64"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: N64): Z16 =
    l""" requires n ≤ n64"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: N64): Z32 =
    l""" requires n ≤ n64"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: N64): Z64 =
    l""" requires n ≤ n64"9223372036854775807"
         ensures  Z64.toZ(result) ≡ toZ(n)     """

  /* @first */
  @pure def toN(n: N64): N = $

  @pure def toN8(n: N64): N8 =
    l""" requires n ≤ n64"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: N64): N16 =
    l""" requires n ≤ n64"65535"
         ensures  N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: N64): N32 =
    l""" requires n ≤ n64"4294967295"
         ensures  N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: N64): N64 =
    l""" ensures result ≡ n """

  @pure def toS8(n: N64): S8 =
    l""" requires n ≤ n64"127"
         ensures  S8.toZ(result) ≡ toZ(n) """

  @pure def toS16(n: N64): S16 =
    l""" requires n ≤ n64"32767"
         ensures  S16.toZ(result) ≡ toZ(n) """

  @pure def toS32(n: N64): S32 =
    l""" requires n ≤ n64"2147483647"
         ensures  S32.toZ(result) ≡ toZ(n) """

  @pure def toS64(n: N64): S64 =
    l""" requires n ≤ n64"9223372036854775807"
         ensures  S64.toZ(result) ≡ toZ(n)     """

  @pure def toU8(n: N64): U8 =
    l""" requires n ≤ n64"255"
         ensures  U8.toN(result) ≡ toN(n) """

  @pure def toU16(n: N64): U16 =
    l""" requires n ≤ n64"65535"
         ensures  U16.toN(result) ≡ toN(n) """

  @pure def toU32(n: N64): U32 =
    l""" requires n ≤ n64"4294967295"
         ensures  U32.toN(result) ≡ toN(n) """

  @pure def toU64(n: N64): U64 =
    l""" ensures U64.toN(result) ≡ toN(n) """

  @pure def toR(n: N64): R =
    l""" ensures result ≡ N.toR(toN(n)) """
}


@ext object S8 {
  val Max: S8 = s8"127"

  val Min: S8 = s8"-128"

  l""" invariant Max ≡ s8"127"
                 Min ≡ s8"-128" """

  def random: S8 = $

  @pure def toB(n: S8): B =
    l""" ensures result ≡ (n ≠ s8"0") """

  @pure def toZ(n: S8): Z =
    l""" ensures result ≡ (if (n ≥ s8"0")
                               (if ((n & s8"0x01") ≠ s8"0x01") 0 else 0x01) +
                               (if ((n & s8"0x02") ≠ s8"0x02") 0 else 0x02) +
                               (if ((n & s8"0x04") ≠ s8"0x04") 0 else 0x04) +
                               (if ((n & s8"0x08") ≠ s8"0x08") 0 else 0x08) +
                               (if ((n & s8"0x10") ≠ s8"0x10") 0 else 0x10) +
                               (if ((n & s8"0x20") ≠ s8"0x20") 0 else 0x20) +
                               (if ((n & s8"0x40") ≠ s8"0x40") 0 else 0x40)
                           else
                             -((if ((n & s8"0x01") ≡ s8"0x01") 0 else 0x01) +
                               (if ((n & s8"0x02") ≡ s8"0x02") 0 else 0x02) +
                               (if ((n & s8"0x04") ≡ s8"0x04") 0 else 0x04) +
                               (if ((n & s8"0x08") ≡ s8"0x08") 0 else 0x08) +
                               (if ((n & s8"0x10") ≡ s8"0x10") 0 else 0x10) +
                               (if ((n & s8"0x20") ≡ s8"0x20") 0 else 0x20) +
                               (if ((n & s8"0x40") ≡ s8"0x40") 0 else 0x40) + 1)) """

  @pure def toZ8(n: S8): Z8 =
    l""" ensures Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: S8): Z16 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: S8): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: S8): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: S8): N =
    l""" requires n ≥ s8"0"
         ensures  result ≡ (if ((n & s8"0x01") ≠ s8"0x01") n"0" else n"0x01") +
                           (if ((n & s8"0x02") ≠ s8"0x02") n"0" else n"0x02") +
                           (if ((n & s8"0x04") ≠ s8"0x04") n"0" else n"0x04") +
                           (if ((n & s8"0x08") ≠ s8"0x08") n"0" else n"0x08") +
                           (if ((n & s8"0x10") ≠ s8"0x10") n"0" else n"0x10") +
                           (if ((n & s8"0x20") ≠ s8"0x20") n"0" else n"0x20") +
                           (if ((n & s8"0x40") ≠ s8"0x40") n"0" else n"0x40")   """

  @pure def toN8(n: S8): N8 =
    l""" requires n ≥ s8"0"
         ensures  N8.toZ(result) ≡ toZ(n) """

  @pure def toN16(n: S8): N16 =
    l""" requires n ≥ s8"0"
         ensures  N16.toZ(result) ≡ toZ(n) """

  @pure def toN32(n: S8): N32 =
    l""" requires n ≥ s8"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: S8): N64 =
    l""" requires n ≥ s8"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  @pure def toS8(n: S8): S8 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toS16(n: S8): S16 = $

  /* @first */
  @pure def toS32(n: S8): S32 = $

  /* @first */
  @pure def toS64(n: S8): S64 = $

  /* @first */
  @pure def toU8(n: S8): U8 =
  l""" requires n ≥ s8"0" """

  /* @first */
  @pure def toRawU8(n: S8): U8 = $

  /* @first */
  @pure def toU16(n: S8): U16 =
  l""" requires n ≥ s8"0" """

  /* @first */
  @pure def toU32(n: S8): U32 =
  l""" requires n ≥ s8"0" """

  /* @first */
  @pure def toU64(n: S8): U64 =
  l""" requires n ≥ s8"0" """
}


@ext object S16 {
  val Max: S16 = s16"32767"

  val Min: S16 = s16"-32768"

  l""" invariant Max ≡ s16"32767"
                 Min ≡ s16"-32768" """

  def random: S16 = $

  @pure def toB(n: S16): B =
    l""" ensures result ≡ (n ≠ s16"0") """

  @pure def toZ(n: S16): Z =
    l""" ensures result ≡ (if (n ≥ s16"0")
                               (if ((n & s16"0x0001") ≠ s16"0x0001") 0 else 0x0001) +
                               (if ((n & s16"0x0002") ≠ s16"0x0002") 0 else 0x0002) +
                               (if ((n & s16"0x0004") ≠ s16"0x0004") 0 else 0x0004) +
                               (if ((n & s16"0x0008") ≠ s16"0x0008") 0 else 0x0008) +
                               (if ((n & s16"0x0010") ≠ s16"0x0010") 0 else 0x0010) +
                               (if ((n & s16"0x0020") ≠ s16"0x0020") 0 else 0x0020) +
                               (if ((n & s16"0x0040") ≠ s16"0x0040") 0 else 0x0040) +
                               (if ((n & s16"0x0080") ≠ s16"0x0080") 0 else 0x0080) +
                               (if ((n & s16"0x0100") ≠ s16"0x0100") 0 else 0x0100) +
                               (if ((n & s16"0x0200") ≠ s16"0x0200") 0 else 0x0200) +
                               (if ((n & s16"0x0400") ≠ s16"0x0400") 0 else 0x0400) +
                               (if ((n & s16"0x0800") ≠ s16"0x0800") 0 else 0x0800) +
                               (if ((n & s16"0x1000") ≠ s16"0x1000") 0 else 0x1000) +
                               (if ((n & s16"0x2000") ≠ s16"0x2000") 0 else 0x2000) +
                               (if ((n & s16"0x4000") ≠ s16"0x4000") 0 else 0x4000)
                           else
                             -((if ((n & s16"0x0001") ≡ s16"0x0001") 0 else 0x0001) +
                               (if ((n & s16"0x0002") ≡ s16"0x0002") 0 else 0x0002) +
                               (if ((n & s16"0x0004") ≡ s16"0x0004") 0 else 0x0004) +
                               (if ((n & s16"0x0008") ≡ s16"0x0008") 0 else 0x0008) +
                               (if ((n & s16"0x0010") ≡ s16"0x0010") 0 else 0x0010) +
                               (if ((n & s16"0x0020") ≡ s16"0x0020") 0 else 0x0020) +
                               (if ((n & s16"0x0040") ≡ s16"0x0040") 0 else 0x0040) +
                               (if ((n & s16"0x0080") ≡ s16"0x0080") 0 else 0x0080) +
                               (if ((n & s16"0x0100") ≡ s16"0x0100") 0 else 0x0100) +
                               (if ((n & s16"0x0200") ≡ s16"0x0200") 0 else 0x0200) +
                               (if ((n & s16"0x0400") ≡ s16"0x0400") 0 else 0x0400) +
                               (if ((n & s16"0x0800") ≡ s16"0x0800") 0 else 0x0800) +
                               (if ((n & s16"0x1000") ≡ s16"0x1000") 0 else 0x1000) +
                               (if ((n & s16"0x2000") ≡ s16"0x2000") 0 else 0x2000) +
                               (if ((n & s16"0x4000") ≡ s16"0x4000") 0 else 0x4000) + 1)) """

  @pure def toZ8(n: S16): Z8 =
    l""" requires s16"-128" ≤ n ∧ n ≤ s16"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: S16): Z16 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: S16): Z32 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: S16): Z64 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toN(n: S16): N =
    l""" requires n ≥ s16"0"
         ensures  result ≡ (if ((n & s16"0x0001") ≠ s16"0x0001") n"0" else n"0x0001") +
                           (if ((n & s16"0x0002") ≠ s16"0x0002") n"0" else n"0x0002") +
                           (if ((n & s16"0x0004") ≠ s16"0x0004") n"0" else n"0x0004") +
                           (if ((n & s16"0x0008") ≠ s16"0x0008") n"0" else n"0x0008") +
                           (if ((n & s16"0x0010") ≠ s16"0x0010") n"0" else n"0x0010") +
                           (if ((n & s16"0x0020") ≠ s16"0x0020") n"0" else n"0x0020") +
                           (if ((n & s16"0x0040") ≠ s16"0x0040") n"0" else n"0x0040") +
                           (if ((n & s16"0x0080") ≠ s16"0x0080") n"0" else n"0x0080") +
                           (if ((n & s16"0x0100") ≠ s16"0x0100") n"0" else n"0x0100") +
                           (if ((n & s16"0x0200") ≠ s16"0x0200") n"0" else n"0x0200") +
                           (if ((n & s16"0x0400") ≠ s16"0x0400") n"0" else n"0x0400") +
                           (if ((n & s16"0x0800") ≠ s16"0x0800") n"0" else n"0x0800") +
                           (if ((n & s16"0x1000") ≠ s16"0x1000") n"0" else n"0x1000") +
                           (if ((n & s16"0x2000") ≠ s16"0x2000") n"0" else n"0x2000") +
                           (if ((n & s16"0x4000") ≠ s16"0x4000") n"0" else n"0x4000")   """

  @pure def toN8(n: S16): N8 =
    l""" requires s16"0" ≤ n ∧ n ≤ s16"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: S16): N16 =
    l""" requires n ≥ s16"0"
         ensures  N16.toZ(result) ≡ toZ(n) """

  @pure def toN32(n: S16): N32 =
    l""" requires n ≥ s16"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: S16): N64 =
    l""" requires n ≥ s16"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toS8(n: S16): S8 =
  l""" requires s16"-128" ≤ n ∧ n ≤ s16"127" """

  @pure def toS16(n: S16): S16 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toS32(n: S16): S32 = $

  /* @first */
  @pure def toS64(n: S16): S64 = $

  /* @first */
  @pure def toU8(n: S16): U8 =
  l""" requires s16"0" ≤ n ∧ n ≤ s16"255" """

  /* @first */
  @pure def toU16(n: S16): U16 =
  l""" requires n ≥ s16"0" """

  /* @first */
  @pure def toRawU16(n: S16): U16 = $

  /* @first */
  @pure def toU32(n: S16): U32 =
  l""" requires n ≥ s16"0" """

  /* @first */
  @pure def toU64(n: S16): U64 =
  l""" requires n ≥ s16"0" """
}


@ext object S32 {
  val Max: S32 = s32"2147483647"

  val Min: S32 = s32"-2147483648"

  l""" invariant Max ≡ s32"2147483647"
                  Min ≡ s32"-2147483648" """

  def random: S32 = $

  @pure def toB(n: S32): B =
    l""" ensures result ≡ (n ≠ s32"0") """

  @pure def toZ(n: S32): Z =
    l""" ensures result ≡ (if (n ≥ s32"0")
                               (if ((n & s32"0x00000001") ≠ s32"0x00000001") 0 else 0x00000001) +
                               (if ((n & s32"0x00000002") ≠ s32"0x00000002") 0 else 0x00000002) +
                               (if ((n & s32"0x00000004") ≠ s32"0x00000004") 0 else 0x00000004) +
                               (if ((n & s32"0x00000008") ≠ s32"0x00000008") 0 else 0x00000008) +
                               (if ((n & s32"0x00000010") ≠ s32"0x00000010") 0 else 0x00000010) +
                               (if ((n & s32"0x00000020") ≠ s32"0x00000020") 0 else 0x00000020) +
                               (if ((n & s32"0x00000040") ≠ s32"0x00000040") 0 else 0x00000040) +
                               (if ((n & s32"0x00000080") ≠ s32"0x00000080") 0 else 0x00000080) +
                               (if ((n & s32"0x00000100") ≠ s32"0x00000100") 0 else 0x00000100) +
                               (if ((n & s32"0x00000200") ≠ s32"0x00000200") 0 else 0x00000200) +
                               (if ((n & s32"0x00000400") ≠ s32"0x00000400") 0 else 0x00000400) +
                               (if ((n & s32"0x00000800") ≠ s32"0x00000800") 0 else 0x00000800) +
                               (if ((n & s32"0x00001000") ≠ s32"0x00001000") 0 else 0x00001000) +
                               (if ((n & s32"0x00002000") ≠ s32"0x00002000") 0 else 0x00002000) +
                               (if ((n & s32"0x00004000") ≠ s32"0x00004000") 0 else 0x00004000) +
                               (if ((n & s32"0x00008000") ≠ s32"0x00008000") 0 else 0x00008000) +
                               (if ((n & s32"0x00010000") ≠ s32"0x00010000") 0 else 0x00010000) +
                               (if ((n & s32"0x00020000") ≠ s32"0x00020000") 0 else 0x00020000) +
                               (if ((n & s32"0x00040000") ≠ s32"0x00040000") 0 else 0x00040000) +
                               (if ((n & s32"0x00080000") ≠ s32"0x00080000") 0 else 0x00080000) +
                               (if ((n & s32"0x00100000") ≠ s32"0x00100000") 0 else 0x00100000) +
                               (if ((n & s32"0x00200000") ≠ s32"0x00200000") 0 else 0x00200000) +
                               (if ((n & s32"0x00400000") ≠ s32"0x00400000") 0 else 0x00400000) +
                               (if ((n & s32"0x00800000") ≠ s32"0x00800000") 0 else 0x00800000) +
                               (if ((n & s32"0x01000000") ≠ s32"0x01000000") 0 else 0x01000000) +
                               (if ((n & s32"0x02000000") ≠ s32"0x02000000") 0 else 0x02000000) +
                               (if ((n & s32"0x04000000") ≠ s32"0x04000000") 0 else 0x04000000) +
                               (if ((n & s32"0x08000000") ≠ s32"0x08000000") 0 else 0x08000000) +
                               (if ((n & s32"0x10000000") ≠ s32"0x10000000") 0 else 0x10000000) +
                               (if ((n & s32"0x20000000") ≠ s32"0x20000000") 0 else 0x20000000) +
                               (if ((n & s32"0x40000000") ≠ s32"0x40000000") 0 else 0x40000000)
                           else
                             -((if ((n & s32"0x00000001") ≠ s32"0x00000001") 0 else 0x00000001) +
                               (if ((n & s32"0x00000002") ≠ s32"0x00000002") 0 else 0x00000002) +
                               (if ((n & s32"0x00000004") ≠ s32"0x00000004") 0 else 0x00000004) +
                               (if ((n & s32"0x00000008") ≠ s32"0x00000008") 0 else 0x00000008) +
                               (if ((n & s32"0x00000010") ≠ s32"0x00000010") 0 else 0x00000010) +
                               (if ((n & s32"0x00000020") ≠ s32"0x00000020") 0 else 0x00000020) +
                               (if ((n & s32"0x00000040") ≠ s32"0x00000040") 0 else 0x00000040) +
                               (if ((n & s32"0x00000080") ≠ s32"0x00000080") 0 else 0x00000080) +
                               (if ((n & s32"0x00000100") ≠ s32"0x00000100") 0 else 0x00000100) +
                               (if ((n & s32"0x00000200") ≠ s32"0x00000200") 0 else 0x00000200) +
                               (if ((n & s32"0x00000400") ≠ s32"0x00000400") 0 else 0x00000400) +
                               (if ((n & s32"0x00000800") ≠ s32"0x00000800") 0 else 0x00000800) +
                               (if ((n & s32"0x00001000") ≠ s32"0x00001000") 0 else 0x00001000) +
                               (if ((n & s32"0x00002000") ≠ s32"0x00002000") 0 else 0x00002000) +
                               (if ((n & s32"0x00004000") ≠ s32"0x00004000") 0 else 0x00004000) +
                               (if ((n & s32"0x00008000") ≠ s32"0x00008000") 0 else 0x00008000) +
                               (if ((n & s32"0x00010000") ≠ s32"0x00010000") 0 else 0x00010000) +
                               (if ((n & s32"0x00020000") ≠ s32"0x00020000") 0 else 0x00020000) +
                               (if ((n & s32"0x00040000") ≠ s32"0x00040000") 0 else 0x00040000) +
                               (if ((n & s32"0x00080000") ≠ s32"0x00080000") 0 else 0x00080000) +
                               (if ((n & s32"0x00100000") ≠ s32"0x00100000") 0 else 0x00100000) +
                               (if ((n & s32"0x00200000") ≠ s32"0x00200000") 0 else 0x00200000) +
                               (if ((n & s32"0x00400000") ≠ s32"0x00400000") 0 else 0x00400000) +
                               (if ((n & s32"0x00800000") ≠ s32"0x00800000") 0 else 0x00800000) +
                               (if ((n & s32"0x01000000") ≠ s32"0x01000000") 0 else 0x01000000) +
                               (if ((n & s32"0x02000000") ≠ s32"0x02000000") 0 else 0x02000000) +
                               (if ((n & s32"0x04000000") ≠ s32"0x04000000") 0 else 0x04000000) +
                               (if ((n & s32"0x08000000") ≠ s32"0x08000000") 0 else 0x08000000) +
                               (if ((n & s32"0x10000000") ≠ s32"0x10000000") 0 else 0x10000000) +
                               (if ((n & s32"0x20000000") ≠ s32"0x20000000") 0 else 0x20000000) +
                               (if ((n & s32"0x40000000") ≠ s32"0x40000000") 0 else 0x40000000) + 1)) """

  @pure def toZ8(n: S32): Z8 =
    l""" requires s32"-128" ≤ n ∧ n ≤ s32"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: S32): Z16 =
    l""" requires s32"-32768" ≤ n ∧ n ≤ s32"32767"
         ensures  Z16.toZ(result) ≡ toZ(n)         """

  @pure def toZ32(n: S32): Z32 =
    l""" ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: S32): Z64 =
    l""" ensures  Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: S32): N =
    l""" requires n ≥ s32"0"
         ensures  result ≡ (if ((n & s32"0x00000001") ≠ s32"0x00000001") 0 else 0x00000001) +
                           (if ((n & s32"0x00000002") ≠ s32"0x00000002") 0 else 0x00000002) +
                           (if ((n & s32"0x00000004") ≠ s32"0x00000004") 0 else 0x00000004) +
                           (if ((n & s32"0x00000008") ≠ s32"0x00000008") 0 else 0x00000008) +
                           (if ((n & s32"0x00000010") ≠ s32"0x00000010") 0 else 0x00000010) +
                           (if ((n & s32"0x00000020") ≠ s32"0x00000020") 0 else 0x00000020) +
                           (if ((n & s32"0x00000040") ≠ s32"0x00000040") 0 else 0x00000040) +
                           (if ((n & s32"0x00000080") ≠ s32"0x00000080") 0 else 0x00000080) +
                           (if ((n & s32"0x00000100") ≠ s32"0x00000100") 0 else 0x00000100) +
                           (if ((n & s32"0x00000200") ≠ s32"0x00000200") 0 else 0x00000200) +
                           (if ((n & s32"0x00000400") ≠ s32"0x00000400") 0 else 0x00000400) +
                           (if ((n & s32"0x00000800") ≠ s32"0x00000800") 0 else 0x00000800) +
                           (if ((n & s32"0x00001000") ≠ s32"0x00001000") 0 else 0x00001000) +
                           (if ((n & s32"0x00002000") ≠ s32"0x00002000") 0 else 0x00002000) +
                           (if ((n & s32"0x00004000") ≠ s32"0x00004000") 0 else 0x00004000) +
                           (if ((n & s32"0x00008000") ≠ s32"0x00008000") 0 else 0x00008000) +
                           (if ((n & s32"0x00010000") ≠ s32"0x00010000") 0 else 0x00010000) +
                           (if ((n & s32"0x00020000") ≠ s32"0x00020000") 0 else 0x00020000) +
                           (if ((n & s32"0x00040000") ≠ s32"0x00040000") 0 else 0x00040000) +
                           (if ((n & s32"0x00080000") ≠ s32"0x00080000") 0 else 0x00080000) +
                           (if ((n & s32"0x00100000") ≠ s32"0x00100000") 0 else 0x00100000) +
                           (if ((n & s32"0x00200000") ≠ s32"0x00200000") 0 else 0x00200000) +
                           (if ((n & s32"0x00400000") ≠ s32"0x00400000") 0 else 0x00400000) +
                           (if ((n & s32"0x00800000") ≠ s32"0x00800000") 0 else 0x00800000) +
                           (if ((n & s32"0x01000000") ≠ s32"0x01000000") 0 else 0x01000000) +
                           (if ((n & s32"0x02000000") ≠ s32"0x02000000") 0 else 0x02000000) +
                           (if ((n & s32"0x04000000") ≠ s32"0x04000000") 0 else 0x04000000) +
                           (if ((n & s32"0x08000000") ≠ s32"0x08000000") 0 else 0x08000000) +
                           (if ((n & s32"0x10000000") ≠ s32"0x10000000") 0 else 0x10000000) +
                           (if ((n & s32"0x20000000") ≠ s32"0x20000000") 0 else 0x20000000) +
                           (if ((n & s32"0x40000000") ≠ s32"0x40000000") 0 else 0x40000000)   """

  @pure def toN8(n: S32): N8 =
    l""" requires s32"0" ≤ n ∧ n ≤ s32"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: S32): N16 =
    l""" requires s32"0" ≤ n ∧ n ≤ s32"65535"
         ensures  N16.toZ(result) ≡ toZ(n)    """

  @pure def toN32(n: S32): N32 =
    l""" requires n ≥ s32"0"
         ensures  N32.toZ(result) ≡ toZ(n) """

  @pure def toN64(n: S32): N64 =
    l""" requires n ≥ s32"0"
         ensures  N64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toS8(n: S32): S8 =
  l""" requires s32"-128" ≤ n ∧ n ≤ s32"127" """

  /* @first */
  @pure def toS16(n: S32): S16 =
  l""" requires s32"-32768" ≤ n ∧ n ≤ s32"32767" """

  @pure def toS32(n: S32): S32 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toS64(n: S32): S64 = $

  /* @first */
  @pure def toU8(n: S32): U8 =
  l""" requires s32"0" ≤ n ∧ n ≤ s32"255" """

  /* @first */
  @pure def toU16(n: S32): U16 =
  l""" requires s32"0" ≤ n ∧ n ≤ s32"65535" """

  /* @first */
  @pure def toU32(n: S32): U32 =
  l""" requires n ≥ s32"0" """

  /* @first */
  @pure def toRawU32(n: S32): U32 = $

  /* @first */
  @pure def toU64(n: S32): U64 =
  l""" requires n ≥ s32"0" """
}


@ext object S64 {
  val Max: S64 = s64"9223372036854775807"

  val Min: S64 = s64"-9223372036854775808"

  l""" invariant Max ≡ s64"9223372036854775807"
                 Min ≡ s64"-9223372036854775808" """

  def random: S64 = $

  @pure def toB(n: S64): B =
    l""" ensures result ≡ (n ≠ s64"0") """

  @pure def toZ(n: S64): Z =
    l""" ensures result ≡ (if (n ≥ s64"0")
                               (if ((n & s64"0x0000000000000001") ≠ s64"0x0000000000000001") 0 else 0x0000000000000001l) +
                               (if ((n & s64"0x0000000000000002") ≠ s64"0x0000000000000002") 0 else 0x0000000000000002l) +
                               (if ((n & s64"0x0000000000000004") ≠ s64"0x0000000000000004") 0 else 0x0000000000000004l) +
                               (if ((n & s64"0x0000000000000008") ≠ s64"0x0000000000000008") 0 else 0x0000000000000008l) +
                               (if ((n & s64"0x0000000000000010") ≠ s64"0x0000000000000010") 0 else 0x0000000000000010l) +
                               (if ((n & s64"0x0000000000000020") ≠ s64"0x0000000000000020") 0 else 0x0000000000000020l) +
                               (if ((n & s64"0x0000000000000040") ≠ s64"0x0000000000000040") 0 else 0x0000000000000040l) +
                               (if ((n & s64"0x0000000000000080") ≠ s64"0x0000000000000080") 0 else 0x0000000000000080l) +
                               (if ((n & s64"0x0000000000000100") ≠ s64"0x0000000000000100") 0 else 0x0000000000000100l) +
                               (if ((n & s64"0x0000000000000200") ≠ s64"0x0000000000000200") 0 else 0x0000000000000200l) +
                               (if ((n & s64"0x0000000000000400") ≠ s64"0x0000000000000400") 0 else 0x0000000000000400l) +
                               (if ((n & s64"0x0000000000000800") ≠ s64"0x0000000000000800") 0 else 0x0000000000000800l) +
                               (if ((n & s64"0x0000000000001000") ≠ s64"0x0000000000001000") 0 else 0x0000000000001000l) +
                               (if ((n & s64"0x0000000000002000") ≠ s64"0x0000000000002000") 0 else 0x0000000000002000l) +
                               (if ((n & s64"0x0000000000004000") ≠ s64"0x0000000000004000") 0 else 0x0000000000004000l) +
                               (if ((n & s64"0x0000000000008000") ≠ s64"0x0000000000008000") 0 else 0x0000000000008000l) +
                               (if ((n & s64"0x0000000000010000") ≠ s64"0x0000000000010000") 0 else 0x0000000000010000l) +
                               (if ((n & s64"0x0000000000020000") ≠ s64"0x0000000000020000") 0 else 0x0000000000020000l) +
                               (if ((n & s64"0x0000000000040000") ≠ s64"0x0000000000040000") 0 else 0x0000000000040000l) +
                               (if ((n & s64"0x0000000000080000") ≠ s64"0x0000000000080000") 0 else 0x0000000000080000l) +
                               (if ((n & s64"0x0000000000100000") ≠ s64"0x0000000000100000") 0 else 0x0000000000100000l) +
                               (if ((n & s64"0x0000000000200000") ≠ s64"0x0000000000200000") 0 else 0x0000000000200000l) +
                               (if ((n & s64"0x0000000000400000") ≠ s64"0x0000000000400000") 0 else 0x0000000000400000l) +
                               (if ((n & s64"0x0000000000800000") ≠ s64"0x0000000000800000") 0 else 0x0000000000800000l) +
                               (if ((n & s64"0x0000000001000000") ≠ s64"0x0000000001000000") 0 else 0x0000000001000000l) +
                               (if ((n & s64"0x0000000002000000") ≠ s64"0x0000000002000000") 0 else 0x0000000002000000l) +
                               (if ((n & s64"0x0000000004000000") ≠ s64"0x0000000004000000") 0 else 0x0000000004000000l) +
                               (if ((n & s64"0x0000000008000000") ≠ s64"0x0000000008000000") 0 else 0x0000000008000000l) +
                               (if ((n & s64"0x0000000010000000") ≠ s64"0x0000000010000000") 0 else 0x0000000010000000l) +
                               (if ((n & s64"0x0000000020000000") ≠ s64"0x0000000020000000") 0 else 0x0000000020000000l) +
                               (if ((n & s64"0x0000000040000000") ≠ s64"0x0000000040000000") 0 else 0x0000000040000000l) +
                               (if ((n & s64"0x0000000080000000") ≠ s64"0x0000000080000000") 0 else 0x0000000080000000l) +
                               (if ((n & s64"0x0000000100000000") ≠ s64"0x0000000100000000") 0 else 0x0000000100000000l) +
                               (if ((n & s64"0x0000000200000000") ≠ s64"0x0000000200000000") 0 else 0x0000000200000000l) +
                               (if ((n & s64"0x0000000400000000") ≠ s64"0x0000000400000000") 0 else 0x0000000400000000l) +
                               (if ((n & s64"0x0000000800000000") ≠ s64"0x0000000800000000") 0 else 0x0000000800000000l) +
                               (if ((n & s64"0x0000001000000000") ≠ s64"0x0000001000000000") 0 else 0x0000001000000000l) +
                               (if ((n & s64"0x0000002000000000") ≠ s64"0x0000002000000000") 0 else 0x0000002000000000l) +
                               (if ((n & s64"0x0000004000000000") ≠ s64"0x0000004000000000") 0 else 0x0000004000000000l) +
                               (if ((n & s64"0x0000008000000000") ≠ s64"0x0000008000000000") 0 else 0x0000008000000000l) +
                               (if ((n & s64"0x0000010000000000") ≠ s64"0x0000010000000000") 0 else 0x0000010000000000l) +
                               (if ((n & s64"0x0000020000000000") ≠ s64"0x0000020000000000") 0 else 0x0000020000000000l) +
                               (if ((n & s64"0x0000040000000000") ≠ s64"0x0000040000000000") 0 else 0x0000040000000000l) +
                               (if ((n & s64"0x0000080000000000") ≠ s64"0x0000080000000000") 0 else 0x0000080000000000l) +
                               (if ((n & s64"0x0000100000000000") ≠ s64"0x0000100000000000") 0 else 0x0000100000000000l) +
                               (if ((n & s64"0x0000200000000000") ≠ s64"0x0000200000000000") 0 else 0x0000200000000000l) +
                               (if ((n & s64"0x0000400000000000") ≠ s64"0x0000400000000000") 0 else 0x0000400000000000l) +
                               (if ((n & s64"0x0000800000000000") ≠ s64"0x0000800000000000") 0 else 0x0000800000000000l) +
                               (if ((n & s64"0x0001000000000000") ≠ s64"0x0001000000000000") 0 else 0x0001000000000000l) +
                               (if ((n & s64"0x0002000000000000") ≠ s64"0x0002000000000000") 0 else 0x0002000000000000l) +
                               (if ((n & s64"0x0004000000000000") ≠ s64"0x0004000000000000") 0 else 0x0004000000000000l) +
                               (if ((n & s64"0x0008000000000000") ≠ s64"0x0008000000000000") 0 else 0x0008000000000000l) +
                               (if ((n & s64"0x0010000000000000") ≠ s64"0x0010000000000000") 0 else 0x0010000000000000l) +
                               (if ((n & s64"0x0020000000000000") ≠ s64"0x0020000000000000") 0 else 0x0020000000000000l) +
                               (if ((n & s64"0x0040000000000000") ≠ s64"0x0040000000000000") 0 else 0x0040000000000000l) +
                               (if ((n & s64"0x0080000000000000") ≠ s64"0x0080000000000000") 0 else 0x0080000000000000l) +
                               (if ((n & s64"0x0100000000000000") ≠ s64"0x0100000000000000") 0 else 0x0100000000000000l) +
                               (if ((n & s64"0x0200000000000000") ≠ s64"0x0200000000000000") 0 else 0x0200000000000000l) +
                               (if ((n & s64"0x0400000000000000") ≠ s64"0x0400000000000000") 0 else 0x0400000000000000l) +
                               (if ((n & s64"0x0800000000000000") ≠ s64"0x0800000000000000") 0 else 0x0800000000000000l) +
                               (if ((n & s64"0x1000000000000000") ≠ s64"0x1000000000000000") 0 else 0x1000000000000000l) +
                               (if ((n & s64"0x2000000000000000") ≠ s64"0x2000000000000000") 0 else 0x2000000000000000l) +
                               (if ((n & s64"0x4000000000000000") ≠ s64"0x4000000000000000") 0 else 0x4000000000000000l)
                           else
                             -((if ((n & s64"0x0000000000000001") ≠ s64"0x0000000000000001") 0 else 0x0000000000000001l) +
                               (if ((n & s64"0x0000000000000002") ≠ s64"0x0000000000000002") 0 else 0x0000000000000002l) +
                               (if ((n & s64"0x0000000000000004") ≠ s64"0x0000000000000004") 0 else 0x0000000000000004l) +
                               (if ((n & s64"0x0000000000000008") ≠ s64"0x0000000000000008") 0 else 0x0000000000000008l) +
                               (if ((n & s64"0x0000000000000010") ≠ s64"0x0000000000000010") 0 else 0x0000000000000010l) +
                               (if ((n & s64"0x0000000000000020") ≠ s64"0x0000000000000020") 0 else 0x0000000000000020l) +
                               (if ((n & s64"0x0000000000000040") ≠ s64"0x0000000000000040") 0 else 0x0000000000000040l) +
                               (if ((n & s64"0x0000000000000080") ≠ s64"0x0000000000000080") 0 else 0x0000000000000080l) +
                               (if ((n & s64"0x0000000000000100") ≠ s64"0x0000000000000100") 0 else 0x0000000000000100l) +
                               (if ((n & s64"0x0000000000000200") ≠ s64"0x0000000000000200") 0 else 0x0000000000000200l) +
                               (if ((n & s64"0x0000000000000400") ≠ s64"0x0000000000000400") 0 else 0x0000000000000400l) +
                               (if ((n & s64"0x0000000000000800") ≠ s64"0x0000000000000800") 0 else 0x0000000000000800l) +
                               (if ((n & s64"0x0000000000001000") ≠ s64"0x0000000000001000") 0 else 0x0000000000001000l) +
                               (if ((n & s64"0x0000000000002000") ≠ s64"0x0000000000002000") 0 else 0x0000000000002000l) +
                               (if ((n & s64"0x0000000000004000") ≠ s64"0x0000000000004000") 0 else 0x0000000000004000l) +
                               (if ((n & s64"0x0000000000008000") ≠ s64"0x0000000000008000") 0 else 0x0000000000008000l) +
                               (if ((n & s64"0x0000000000010000") ≠ s64"0x0000000000010000") 0 else 0x0000000000010000l) +
                               (if ((n & s64"0x0000000000020000") ≠ s64"0x0000000000020000") 0 else 0x0000000000020000l) +
                               (if ((n & s64"0x0000000000040000") ≠ s64"0x0000000000040000") 0 else 0x0000000000040000l) +
                               (if ((n & s64"0x0000000000080000") ≠ s64"0x0000000000080000") 0 else 0x0000000000080000l) +
                               (if ((n & s64"0x0000000000100000") ≠ s64"0x0000000000100000") 0 else 0x0000000000100000l) +
                               (if ((n & s64"0x0000000000200000") ≠ s64"0x0000000000200000") 0 else 0x0000000000200000l) +
                               (if ((n & s64"0x0000000000400000") ≠ s64"0x0000000000400000") 0 else 0x0000000000400000l) +
                               (if ((n & s64"0x0000000000800000") ≠ s64"0x0000000000800000") 0 else 0x0000000000800000l) +
                               (if ((n & s64"0x0000000001000000") ≠ s64"0x0000000001000000") 0 else 0x0000000001000000l) +
                               (if ((n & s64"0x0000000002000000") ≠ s64"0x0000000002000000") 0 else 0x0000000002000000l) +
                               (if ((n & s64"0x0000000004000000") ≠ s64"0x0000000004000000") 0 else 0x0000000004000000l) +
                               (if ((n & s64"0x0000000008000000") ≠ s64"0x0000000008000000") 0 else 0x0000000008000000l) +
                               (if ((n & s64"0x0000000010000000") ≠ s64"0x0000000010000000") 0 else 0x0000000010000000l) +
                               (if ((n & s64"0x0000000020000000") ≠ s64"0x0000000020000000") 0 else 0x0000000020000000l) +
                               (if ((n & s64"0x0000000040000000") ≠ s64"0x0000000040000000") 0 else 0x0000000040000000l) +
                               (if ((n & s64"0x0000000080000000") ≠ s64"0x0000000080000000") 0 else 0x0000000080000000l) +
                               (if ((n & s64"0x0000000100000000") ≠ s64"0x0000000100000000") 0 else 0x0000000100000000l) +
                               (if ((n & s64"0x0000000200000000") ≠ s64"0x0000000200000000") 0 else 0x0000000200000000l) +
                               (if ((n & s64"0x0000000400000000") ≠ s64"0x0000000400000000") 0 else 0x0000000400000000l) +
                               (if ((n & s64"0x0000000800000000") ≠ s64"0x0000000800000000") 0 else 0x0000000800000000l) +
                               (if ((n & s64"0x0000001000000000") ≠ s64"0x0000001000000000") 0 else 0x0000001000000000l) +
                               (if ((n & s64"0x0000002000000000") ≠ s64"0x0000002000000000") 0 else 0x0000002000000000l) +
                               (if ((n & s64"0x0000004000000000") ≠ s64"0x0000004000000000") 0 else 0x0000004000000000l) +
                               (if ((n & s64"0x0000008000000000") ≠ s64"0x0000008000000000") 0 else 0x0000008000000000l) +
                               (if ((n & s64"0x0000010000000000") ≠ s64"0x0000010000000000") 0 else 0x0000010000000000l) +
                               (if ((n & s64"0x0000020000000000") ≠ s64"0x0000020000000000") 0 else 0x0000020000000000l) +
                               (if ((n & s64"0x0000040000000000") ≠ s64"0x0000040000000000") 0 else 0x0000040000000000l) +
                               (if ((n & s64"0x0000080000000000") ≠ s64"0x0000080000000000") 0 else 0x0000080000000000l) +
                               (if ((n & s64"0x0000100000000000") ≠ s64"0x0000100000000000") 0 else 0x0000100000000000l) +
                               (if ((n & s64"0x0000200000000000") ≠ s64"0x0000200000000000") 0 else 0x0000200000000000l) +
                               (if ((n & s64"0x0000400000000000") ≠ s64"0x0000400000000000") 0 else 0x0000400000000000l) +
                               (if ((n & s64"0x0000800000000000") ≠ s64"0x0000800000000000") 0 else 0x0000800000000000l) +
                               (if ((n & s64"0x0001000000000000") ≠ s64"0x0001000000000000") 0 else 0x0001000000000000l) +
                               (if ((n & s64"0x0002000000000000") ≠ s64"0x0002000000000000") 0 else 0x0002000000000000l) +
                               (if ((n & s64"0x0004000000000000") ≠ s64"0x0004000000000000") 0 else 0x0004000000000000l) +
                               (if ((n & s64"0x0008000000000000") ≠ s64"0x0008000000000000") 0 else 0x0008000000000000l) +
                               (if ((n & s64"0x0010000000000000") ≠ s64"0x0010000000000000") 0 else 0x0010000000000000l) +
                               (if ((n & s64"0x0020000000000000") ≠ s64"0x0020000000000000") 0 else 0x0020000000000000l) +
                               (if ((n & s64"0x0040000000000000") ≠ s64"0x0040000000000000") 0 else 0x0040000000000000l) +
                               (if ((n & s64"0x0080000000000000") ≠ s64"0x0080000000000000") 0 else 0x0080000000000000l) +
                               (if ((n & s64"0x0100000000000000") ≠ s64"0x0100000000000000") 0 else 0x0100000000000000l) +
                               (if ((n & s64"0x0200000000000000") ≠ s64"0x0200000000000000") 0 else 0x0200000000000000l) +
                               (if ((n & s64"0x0400000000000000") ≠ s64"0x0400000000000000") 0 else 0x0400000000000000l) +
                               (if ((n & s64"0x0800000000000000") ≠ s64"0x0800000000000000") 0 else 0x0800000000000000l) +
                               (if ((n & s64"0x1000000000000000") ≠ s64"0x1000000000000000") 0 else 0x1000000000000000l) +
                               (if ((n & s64"0x2000000000000000") ≠ s64"0x2000000000000000") 0 else 0x2000000000000000l) +
                               (if ((n & s64"0x4000000000000000") ≠ s64"0x4000000000000000") 0 else 0x4000000000000000l)) + 1) """

  @pure def toZ8(n: S64): Z8 =
    l""" requires s64"-128" ≤ n ∧ n ≤ s64"127"
         ensures  Z8.toZ(result) ≡ toZ(n)      """

  @pure def toZ16(n: S64): Z16 =
    l""" requires s64"-32768" ≤ n ∧ n ≤ s64"32767"
         ensures  Z16.toZ(result) ≡ toZ(n)         """

  @pure def toZ32(n: S64): Z32 =
    l""" requires s64"-2147483648" ≤ n ∧ n ≤ s64"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n)                   """

  @pure def toZ64(n: S64): Z64 =
    l""" ensures  Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: S64): N =
    l""" requires n ≥ s64"0"
         ensures  result ≡ (if ((n & s64"0x0000000000000001") ≠ s64"0x0000000000000001") 0 else 0x0000000000000001l) +
                           (if ((n & s64"0x0000000000000002") ≠ s64"0x0000000000000002") 0 else 0x0000000000000002l) +
                           (if ((n & s64"0x0000000000000004") ≠ s64"0x0000000000000004") 0 else 0x0000000000000004l) +
                           (if ((n & s64"0x0000000000000008") ≠ s64"0x0000000000000008") 0 else 0x0000000000000008l) +
                           (if ((n & s64"0x0000000000000010") ≠ s64"0x0000000000000010") 0 else 0x0000000000000010l) +
                           (if ((n & s64"0x0000000000000020") ≠ s64"0x0000000000000020") 0 else 0x0000000000000020l) +
                           (if ((n & s64"0x0000000000000040") ≠ s64"0x0000000000000040") 0 else 0x0000000000000040l) +
                           (if ((n & s64"0x0000000000000080") ≠ s64"0x0000000000000080") 0 else 0x0000000000000080l) +
                           (if ((n & s64"0x0000000000000100") ≠ s64"0x0000000000000100") 0 else 0x0000000000000100l) +
                           (if ((n & s64"0x0000000000000200") ≠ s64"0x0000000000000200") 0 else 0x0000000000000200l) +
                           (if ((n & s64"0x0000000000000400") ≠ s64"0x0000000000000400") 0 else 0x0000000000000400l) +
                           (if ((n & s64"0x0000000000000800") ≠ s64"0x0000000000000800") 0 else 0x0000000000000800l) +
                           (if ((n & s64"0x0000000000001000") ≠ s64"0x0000000000001000") 0 else 0x0000000000001000l) +
                           (if ((n & s64"0x0000000000002000") ≠ s64"0x0000000000002000") 0 else 0x0000000000002000l) +
                           (if ((n & s64"0x0000000000004000") ≠ s64"0x0000000000004000") 0 else 0x0000000000004000l) +
                           (if ((n & s64"0x0000000000008000") ≠ s64"0x0000000000008000") 0 else 0x0000000000008000l) +
                           (if ((n & s64"0x0000000000010000") ≠ s64"0x0000000000010000") 0 else 0x0000000000010000l) +
                           (if ((n & s64"0x0000000000020000") ≠ s64"0x0000000000020000") 0 else 0x0000000000020000l) +
                           (if ((n & s64"0x0000000000040000") ≠ s64"0x0000000000040000") 0 else 0x0000000000040000l) +
                           (if ((n & s64"0x0000000000080000") ≠ s64"0x0000000000080000") 0 else 0x0000000000080000l) +
                           (if ((n & s64"0x0000000000100000") ≠ s64"0x0000000000100000") 0 else 0x0000000000100000l) +
                           (if ((n & s64"0x0000000000200000") ≠ s64"0x0000000000200000") 0 else 0x0000000000200000l) +
                           (if ((n & s64"0x0000000000400000") ≠ s64"0x0000000000400000") 0 else 0x0000000000400000l) +
                           (if ((n & s64"0x0000000000800000") ≠ s64"0x0000000000800000") 0 else 0x0000000000800000l) +
                           (if ((n & s64"0x0000000001000000") ≠ s64"0x0000000001000000") 0 else 0x0000000001000000l) +
                           (if ((n & s64"0x0000000002000000") ≠ s64"0x0000000002000000") 0 else 0x0000000002000000l) +
                           (if ((n & s64"0x0000000004000000") ≠ s64"0x0000000004000000") 0 else 0x0000000004000000l) +
                           (if ((n & s64"0x0000000008000000") ≠ s64"0x0000000008000000") 0 else 0x0000000008000000l) +
                           (if ((n & s64"0x0000000010000000") ≠ s64"0x0000000010000000") 0 else 0x0000000010000000l) +
                           (if ((n & s64"0x0000000020000000") ≠ s64"0x0000000020000000") 0 else 0x0000000020000000l) +
                           (if ((n & s64"0x0000000040000000") ≠ s64"0x0000000040000000") 0 else 0x0000000040000000l) +
                           (if ((n & s64"0x0000000080000000") ≠ s64"0x0000000080000000") 0 else 0x0000000080000000l) +
                           (if ((n & s64"0x0000000100000000") ≠ s64"0x0000000100000000") 0 else 0x0000000100000000l) +
                           (if ((n & s64"0x0000000200000000") ≠ s64"0x0000000200000000") 0 else 0x0000000200000000l) +
                           (if ((n & s64"0x0000000400000000") ≠ s64"0x0000000400000000") 0 else 0x0000000400000000l) +
                           (if ((n & s64"0x0000000800000000") ≠ s64"0x0000000800000000") 0 else 0x0000000800000000l) +
                           (if ((n & s64"0x0000001000000000") ≠ s64"0x0000001000000000") 0 else 0x0000001000000000l) +
                           (if ((n & s64"0x0000002000000000") ≠ s64"0x0000002000000000") 0 else 0x0000002000000000l) +
                           (if ((n & s64"0x0000004000000000") ≠ s64"0x0000004000000000") 0 else 0x0000004000000000l) +
                           (if ((n & s64"0x0000008000000000") ≠ s64"0x0000008000000000") 0 else 0x0000008000000000l) +
                           (if ((n & s64"0x0000010000000000") ≠ s64"0x0000010000000000") 0 else 0x0000010000000000l) +
                           (if ((n & s64"0x0000020000000000") ≠ s64"0x0000020000000000") 0 else 0x0000020000000000l) +
                           (if ((n & s64"0x0000040000000000") ≠ s64"0x0000040000000000") 0 else 0x0000040000000000l) +
                           (if ((n & s64"0x0000080000000000") ≠ s64"0x0000080000000000") 0 else 0x0000080000000000l) +
                           (if ((n & s64"0x0000100000000000") ≠ s64"0x0000100000000000") 0 else 0x0000100000000000l) +
                           (if ((n & s64"0x0000200000000000") ≠ s64"0x0000200000000000") 0 else 0x0000200000000000l) +
                           (if ((n & s64"0x0000400000000000") ≠ s64"0x0000400000000000") 0 else 0x0000400000000000l) +
                           (if ((n & s64"0x0000800000000000") ≠ s64"0x0000800000000000") 0 else 0x0000800000000000l) +
                           (if ((n & s64"0x0001000000000000") ≠ s64"0x0001000000000000") 0 else 0x0001000000000000l) +
                           (if ((n & s64"0x0002000000000000") ≠ s64"0x0002000000000000") 0 else 0x0002000000000000l) +
                           (if ((n & s64"0x0004000000000000") ≠ s64"0x0004000000000000") 0 else 0x0004000000000000l) +
                           (if ((n & s64"0x0008000000000000") ≠ s64"0x0008000000000000") 0 else 0x0008000000000000l) +
                           (if ((n & s64"0x0010000000000000") ≠ s64"0x0010000000000000") 0 else 0x0010000000000000l) +
                           (if ((n & s64"0x0020000000000000") ≠ s64"0x0020000000000000") 0 else 0x0020000000000000l) +
                           (if ((n & s64"0x0040000000000000") ≠ s64"0x0040000000000000") 0 else 0x0040000000000000l) +
                           (if ((n & s64"0x0080000000000000") ≠ s64"0x0080000000000000") 0 else 0x0080000000000000l) +
                           (if ((n & s64"0x0100000000000000") ≠ s64"0x0100000000000000") 0 else 0x0100000000000000l) +
                           (if ((n & s64"0x0200000000000000") ≠ s64"0x0200000000000000") 0 else 0x0200000000000000l) +
                           (if ((n & s64"0x0400000000000000") ≠ s64"0x0400000000000000") 0 else 0x0400000000000000l) +
                           (if ((n & s64"0x0800000000000000") ≠ s64"0x0800000000000000") 0 else 0x0800000000000000l) +
                           (if ((n & s64"0x1000000000000000") ≠ s64"0x1000000000000000") 0 else 0x1000000000000000l) +
                           (if ((n & s64"0x2000000000000000") ≠ s64"0x2000000000000000") 0 else 0x2000000000000000l) +
                           (if ((n & s64"0x4000000000000000") ≠ s64"0x4000000000000000") 0 else 0x4000000000000000l)   """

  @pure def toN8(n: S64): N8 =
    l""" requires s64"0" ≤ n ∧ n ≤ s64"255"
         ensures  N8.toZ(result) ≡ toZ(n)   """

  @pure def toN16(n: S64): N16 =
    l""" requires s64"0" ≤ n ∧ n ≤ s64"65535"
         ensures  N16.toZ(result) ≡ toZ(n)    """

  @pure def toN32(n: S64): N32 =
    l""" requires s64"0" ≤ n ∧ n ≤ s64"4294967295"
         ensures  N32.toZ(result) ≡ toZ(n)         """

  @pure def toN64(n: S64): N64 =
    l""" ensures N64.toZ(result) ≡ toZ(n) """

  /* @first */
  @pure def toS8(n: S64): S8 =
  l""" requires s64"-128" ≤ n ∧ n ≤ s64"127" """

  /* @first */
  @pure def toS16(n: S64): S16 =
  l""" requires s64"-32768" ≤ n ∧ n ≤ s64"32767" """

  /* @first */
  @pure def toS32(n: S64): S32 =
  l""" requires s64"-2147483648" ≤ n ∧ n ≤ s64"2147483647" """

  @pure def toS64(n: S64): S64 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toU8(n: S64): U8 =
  l""" requires s64"0" ≤ n ∧ n ≤ s64"255" """

  /* @first */
  @pure def toU16(n: S64): U16 =
  l""" requires s64"0" ≤ n ∧ n ≤ s64"65535" """

  /* @first */
  @pure def toU32(n: S64): U32 =
  l""" requires s64"0" ≤ n ∧ n ≤ s64"4294967295" """

  /* @first */
  @pure def toU64(n: S64): U64 =
  l""" requires n ≥ s64"0" """

  /* @first */
  @pure def toRawU64(n: S64): U64 = $
}


@ext object U8 {
  val Max: U8 = u8"255"

  l""" invariant Max ≡ u8"255" """

  def random: U8 = $

  @pure def toB(n: U8): B =
    l""" ensures result ≡ (n ≠ u8"0") """

  @pure def toZ(n: U8): Z =
    l""" ensures result ≡ (if ((n & u8"0x01") ≠ u8"0x01") 0 else 0x01) +
                          (if ((n & u8"0x02") ≠ u8"0x02") 0 else 0x02) +
                          (if ((n & u8"0x04") ≠ u8"0x04") 0 else 0x04) +
                          (if ((n & u8"0x08") ≠ u8"0x08") 0 else 0x08) +
                          (if ((n & u8"0x10") ≠ u8"0x10") 0 else 0x10) +
                          (if ((n & u8"0x20") ≠ u8"0x20") 0 else 0x20) +
                          (if ((n & u8"0x40") ≠ u8"0x40") 0 else 0x40) +
                          (if ((n & u8"0x80") ≠ u8"0x80") 0 else 0x80)   """

  @pure def toZ8(n: U8): Z8 =
    l""" requires n ≤ u8"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: U8): Z16 =
    l""" ensures Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: U8): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: U8): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: U8): N =
    l""" ensures result ≡ (if ((n & u8"0x01") ≠ u8"0x01") n"0" else n"0x01") +
                          (if ((n & u8"0x02") ≠ u8"0x02") n"0" else n"0x02") +
                          (if ((n & u8"0x04") ≠ u8"0x04") n"0" else n"0x04") +
                          (if ((n & u8"0x08") ≠ u8"0x08") n"0" else n"0x08") +
                          (if ((n & u8"0x10") ≠ u8"0x10") n"0" else n"0x10") +
                          (if ((n & u8"0x20") ≠ u8"0x20") n"0" else n"0x20") +
                          (if ((n & u8"0x40") ≠ u8"0x40") n"0" else n"0x40") +
                          (if ((n & u8"0x80") ≠ u8"0x80") n"0" else n"0x80")   """

  @pure def toN8(n: U8): N8 =
    l""" requires n ≤ u8"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: U8): N16 =
    l""" ensures N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: U8): N32 =
    l""" ensures N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: U8): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  /* @first */
  @pure def toS8(n: U8): S8 =
  l""" requires n ≤ u8"127" """

  /* @first */
  @pure def toRawS8(n: U8): S8 = $

  /* @first */
  @pure def toS16(n: U8): S16 = $

  /* @first */
  @pure def toS32(n: U8): S32 = $

  /* @first */
  @pure def toS64(n: U8): S64 = $

  @pure def toU8(n: U8): U8 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toU16(n: U8): U16 = $

  /* @first */
  @pure def toU32(n: U8): U32 = $

  /* @first */
  @pure def toU64(n: U8): U64 = $
}


@ext object U16 {
  val Max: U16 = u16"65535"

  l""" invariant Max ≡ u16"65535" """

  def random: U16 = $

  @pure def toB(n: U16): B =
    l""" ensures result ≡ (n ≠ u16"0") """

  @pure def toZ(n: U16): Z =
    l""" ensures result ≡ (if ((n & u16"0x0001") ≠ u16"0x0001") 0 else 0x0001) +
                          (if ((n & u16"0x0002") ≠ u16"0x0002") 0 else 0x0002) +
                          (if ((n & u16"0x0004") ≠ u16"0x0004") 0 else 0x0004) +
                          (if ((n & u16"0x0008") ≠ u16"0x0008") 0 else 0x0008) +
                          (if ((n & u16"0x0010") ≠ u16"0x0010") 0 else 0x0010) +
                          (if ((n & u16"0x0020") ≠ u16"0x0020") 0 else 0x0020) +
                          (if ((n & u16"0x0040") ≠ u16"0x0040") 0 else 0x0040) +
                          (if ((n & u16"0x0080") ≠ u16"0x0080") 0 else 0x0080) +
                          (if ((n & u16"0x0100") ≠ u16"0x0100") 0 else 0x0100) +
                          (if ((n & u16"0x0200") ≠ u16"0x0200") 0 else 0x0200) +
                          (if ((n & u16"0x0400") ≠ u16"0x0400") 0 else 0x0400) +
                          (if ((n & u16"0x0800") ≠ u16"0x0800") 0 else 0x0800) +
                          (if ((n & u16"0x1000") ≠ u16"0x1000") 0 else 0x1000) +
                          (if ((n & u16"0x2000") ≠ u16"0x2000") 0 else 0x2000) +
                          (if ((n & u16"0x4000") ≠ u16"0x4000") 0 else 0x4000) +
                          (if ((n & u16"0x8000") ≠ u16"0x8000") 0 else 0x8000)   """

  @pure def toZ8(n: U16): Z8 =
    l""" requires n ≤ u16"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: U16): Z16 =
    l""" requires n ≤ u16"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: U16): Z32 =
    l""" ensures Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: U16): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: U16): N =
    l""" ensures result ≡ (if ((n & u16"0x0001") ≠ u16"0x0001") n"0" else n"0x0001") +
                          (if ((n & u16"0x0002") ≠ u16"0x0002") n"0" else n"0x0002") +
                          (if ((n & u16"0x0004") ≠ u16"0x0004") n"0" else n"0x0004") +
                          (if ((n & u16"0x0008") ≠ u16"0x0008") n"0" else n"0x0008") +
                          (if ((n & u16"0x0010") ≠ u16"0x0010") n"0" else n"0x0010") +
                          (if ((n & u16"0x0020") ≠ u16"0x0020") n"0" else n"0x0020") +
                          (if ((n & u16"0x0040") ≠ u16"0x0040") n"0" else n"0x0040") +
                          (if ((n & u16"0x0080") ≠ u16"0x0080") n"0" else n"0x0080") +
                          (if ((n & u16"0x0100") ≠ u16"0x0100") n"0" else n"0x0100") +
                          (if ((n & u16"0x0200") ≠ u16"0x0200") n"0" else n"0x0200") +
                          (if ((n & u16"0x0400") ≠ u16"0x0400") n"0" else n"0x0400") +
                          (if ((n & u16"0x0800") ≠ u16"0x0800") n"0" else n"0x0800") +
                          (if ((n & u16"0x1000") ≠ u16"0x1000") n"0" else n"0x1000") +
                          (if ((n & u16"0x2000") ≠ u16"0x2000") n"0" else n"0x2000") +
                          (if ((n & u16"0x4000") ≠ u16"0x4000") n"0" else n"0x4000") +
                          (if ((n & u16"0x8000") ≠ u16"0x8000") n"0" else n"0x8000")   """

  @pure def toN8(n: U16): N8 =
    l""" requires n ≤ u16"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: U16): N16 =
    l""" requires n ≤ u16"65535"
         ensures  N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: U16): N32 =
    l""" ensures N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: U16): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  /* @first */
  @pure def toS8(n: U16): S8 =
  l""" requires n ≤ u16"127" """

  /* @first */
  @pure def toS16(n: U16): S16 =
  l""" requires n ≤ u16"32767" """

  /* @first */
  @pure def toRawS16(n: U16): S16 = $

  /* @first */
  @pure def toS32(n: U16): S32 = $

  /* @first */
  @pure def toS64(n: U16): S64 = $

  /* @first */
  @pure def toU8(n: U16): U8 = $

  @pure def toU16(n: U16): U16 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toU32(n: U16): U32 = $

  /* @first */
  @pure def toU64(n: U16): U64 = $
}


@ext object U32 {
  val Max: U32 = u32"4294967295"

  l""" invariant Max ≡ u32"4294967295" """

  def random: U32 = $

  @pure def toB(n: U32): B =
    l""" ensures result ≡ (n ≠ u32"0") """

  @pure def toZ(n: U32): Z =
    l""" ensures result ≡ (if ((n & u32"0x00000001") ≠ u32"0x00000001") 0 else 0x00000001) +
                          (if ((n & u32"0x00000002") ≠ u32"0x00000002") 0 else 0x00000002) +
                          (if ((n & u32"0x00000004") ≠ u32"0x00000004") 0 else 0x00000004) +
                          (if ((n & u32"0x00000008") ≠ u32"0x00000008") 0 else 0x00000008) +
                          (if ((n & u32"0x00000010") ≠ u32"0x00000010") 0 else 0x00000010) +
                          (if ((n & u32"0x00000020") ≠ u32"0x00000020") 0 else 0x00000020) +
                          (if ((n & u32"0x00000040") ≠ u32"0x00000040") 0 else 0x00000040) +
                          (if ((n & u32"0x00000080") ≠ u32"0x00000080") 0 else 0x00000080) +
                          (if ((n & u32"0x00000100") ≠ u32"0x00000100") 0 else 0x00000100) +
                          (if ((n & u32"0x00000200") ≠ u32"0x00000200") 0 else 0x00000200) +
                          (if ((n & u32"0x00000400") ≠ u32"0x00000400") 0 else 0x00000400) +
                          (if ((n & u32"0x00000800") ≠ u32"0x00000800") 0 else 0x00000800) +
                          (if ((n & u32"0x00001000") ≠ u32"0x00001000") 0 else 0x00001000) +
                          (if ((n & u32"0x00002000") ≠ u32"0x00002000") 0 else 0x00002000) +
                          (if ((n & u32"0x00004000") ≠ u32"0x00004000") 0 else 0x00004000) +
                          (if ((n & u32"0x00008000") ≠ u32"0x00008000") 0 else 0x00008000) +
                          (if ((n & u32"0x00010000") ≠ u32"0x00010000") 0 else 0x00010000) +
                          (if ((n & u32"0x00020000") ≠ u32"0x00020000") 0 else 0x00020000) +
                          (if ((n & u32"0x00040000") ≠ u32"0x00040000") 0 else 0x00040000) +
                          (if ((n & u32"0x00080000") ≠ u32"0x00080000") 0 else 0x00080000) +
                          (if ((n & u32"0x00100000") ≠ u32"0x00100000") 0 else 0x00100000) +
                          (if ((n & u32"0x00200000") ≠ u32"0x00200000") 0 else 0x00200000) +
                          (if ((n & u32"0x00400000") ≠ u32"0x00400000") 0 else 0x00400000) +
                          (if ((n & u32"0x00800000") ≠ u32"0x00800000") 0 else 0x00800000) +
                          (if ((n & u32"0x01000000") ≠ u32"0x01000000") 0 else 0x01000000) +
                          (if ((n & u32"0x02000000") ≠ u32"0x02000000") 0 else 0x02000000) +
                          (if ((n & u32"0x04000000") ≠ u32"0x04000000") 0 else 0x04000000) +
                          (if ((n & u32"0x08000000") ≠ u32"0x08000000") 0 else 0x08000000) +
                          (if ((n & u32"0x10000000") ≠ u32"0x10000000") 0 else 0x10000000) +
                          (if ((n & u32"0x20000000") ≠ u32"0x20000000") 0 else 0x20000000) +
                          (if ((n & u32"0x40000000") ≠ u32"0x40000000") 0 else 0x40000000) +
                          (if ((n & u32"0x80000000") ≠ u32"0x80000000") 0 else 0x80000000)   """

  @pure def toZ8(n: U32): Z8 =
    l""" requires n ≤ u32"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: U32): Z16 =
    l""" requires n ≤ u32"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: U32): Z32 =
    l""" requires n ≤ u32"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: U32): Z64 =
    l""" ensures Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: U32): N =
    l""" ensures result ≡ (if ((n & u32"0x00000001") ≠ u32"0x00000001") n"0" else n"0x00000001") +
                          (if ((n & u32"0x00000002") ≠ u32"0x00000002") n"0" else n"0x00000002") +
                          (if ((n & u32"0x00000004") ≠ u32"0x00000004") n"0" else n"0x00000004") +
                          (if ((n & u32"0x00000008") ≠ u32"0x00000008") n"0" else n"0x00000008") +
                          (if ((n & u32"0x00000010") ≠ u32"0x00000010") n"0" else n"0x00000010") +
                          (if ((n & u32"0x00000020") ≠ u32"0x00000020") n"0" else n"0x00000020") +
                          (if ((n & u32"0x00000040") ≠ u32"0x00000040") n"0" else n"0x00000040") +
                          (if ((n & u32"0x00000080") ≠ u32"0x00000080") n"0" else n"0x00000080") +
                          (if ((n & u32"0x00000100") ≠ u32"0x00000100") n"0" else n"0x00000100") +
                          (if ((n & u32"0x00000200") ≠ u32"0x00000200") n"0" else n"0x00000200") +
                          (if ((n & u32"0x00000400") ≠ u32"0x00000400") n"0" else n"0x00000400") +
                          (if ((n & u32"0x00000800") ≠ u32"0x00000800") n"0" else n"0x00000800") +
                          (if ((n & u32"0x00001000") ≠ u32"0x00001000") n"0" else n"0x00001000") +
                          (if ((n & u32"0x00002000") ≠ u32"0x00002000") n"0" else n"0x00002000") +
                          (if ((n & u32"0x00004000") ≠ u32"0x00004000") n"0" else n"0x00004000") +
                          (if ((n & u32"0x00008000") ≠ u32"0x00008000") n"0" else n"0x00008000") +
                          (if ((n & u32"0x00010000") ≠ u32"0x00010000") n"0" else n"0x00010000") +
                          (if ((n & u32"0x00020000") ≠ u32"0x00020000") n"0" else n"0x00020000") +
                          (if ((n & u32"0x00040000") ≠ u32"0x00040000") n"0" else n"0x00040000") +
                          (if ((n & u32"0x00080000") ≠ u32"0x00080000") n"0" else n"0x00080000") +
                          (if ((n & u32"0x00100000") ≠ u32"0x00100000") n"0" else n"0x00100000") +
                          (if ((n & u32"0x00200000") ≠ u32"0x00200000") n"0" else n"0x00200000") +
                          (if ((n & u32"0x00400000") ≠ u32"0x00400000") n"0" else n"0x00400000") +
                          (if ((n & u32"0x00800000") ≠ u32"0x00800000") n"0" else n"0x00800000") +
                          (if ((n & u32"0x01000000") ≠ u32"0x01000000") n"0" else n"0x01000000") +
                          (if ((n & u32"0x02000000") ≠ u32"0x02000000") n"0" else n"0x02000000") +
                          (if ((n & u32"0x04000000") ≠ u32"0x04000000") n"0" else n"0x04000000") +
                          (if ((n & u32"0x08000000") ≠ u32"0x08000000") n"0" else n"0x08000000") +
                          (if ((n & u32"0x10000000") ≠ u32"0x10000000") n"0" else n"0x10000000") +
                          (if ((n & u32"0x20000000") ≠ u32"0x20000000") n"0" else n"0x20000000") +
                          (if ((n & u32"0x40000000") ≠ u32"0x40000000") n"0" else n"0x40000000") +
                          (if ((n & u32"0x80000000") ≠ u32"0x80000000") n"0" else n"0x80000000")   """

  @pure def toN8(n: U32): N8 =
    l""" requires n ≤ u32"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: U32): N16 =
    l""" requires n ≤ u32"65535"
         ensures  N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: U32): N32 =
    l""" ensures N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: U32): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  /* @first */
  @pure def toS8(n: U32): S8 =
  l""" requires n ≤ u32"127" """

  /* @first */
  @pure def toS16(n: U32): S16 =
  l""" requires n ≤ u32"32767" """

  /* @first */
  @pure def toS32(n: U32): S32 =
  l""" requires n ≤ u32"2147483647" """

  /* @first */
  @pure def toRawS32(n: U32): S32 = $

  /* @first */
  @pure def toS64(n: U32): S64 = $

  /* @first */
  @pure def toU8(n: U32): U8 =
  l""" requires n ≤ u32"255" """

  /* @first */
  @pure def toU16(n: U32): U16 =
  l""" requires n ≤ u32"65535" """

  @pure def toU32(n: U32): U32 =
    l""" ensures result ≡ n """

  /* @first */
  @pure def toU64(n: U32): U64 = $

  /* @first */
  @pure def toRawF32(n: U32): F32 = $
}


@ext object U64 {
  val Max: U64 = u64"18446744073709551615"

  l""" invariant Max ≡ u64"18446744073709551615" """

  def random: U64 = $

  @pure def toB(n: U64): B =
    l""" ensures result ≡ (n ≠ u64"0") """

  @pure def toZ(n: U64): Z =
    l""" ensures result ≡ (if ((n & u64"0x0000000000000001") ≠ u64"0x0000000000000001") 0 else 0x0000000000000001l) +
                          (if ((n & u64"0x0000000000000002") ≠ u64"0x0000000000000002") 0 else 0x0000000000000002l) +
                          (if ((n & u64"0x0000000000000004") ≠ u64"0x0000000000000004") 0 else 0x0000000000000004l) +
                          (if ((n & u64"0x0000000000000008") ≠ u64"0x0000000000000008") 0 else 0x0000000000000008l) +
                          (if ((n & u64"0x0000000000000010") ≠ u64"0x0000000000000010") 0 else 0x0000000000000010l) +
                          (if ((n & u64"0x0000000000000020") ≠ u64"0x0000000000000020") 0 else 0x0000000000000020l) +
                          (if ((n & u64"0x0000000000000040") ≠ u64"0x0000000000000040") 0 else 0x0000000000000040l) +
                          (if ((n & u64"0x0000000000000080") ≠ u64"0x0000000000000080") 0 else 0x0000000000000080l) +
                          (if ((n & u64"0x0000000000000100") ≠ u64"0x0000000000000100") 0 else 0x0000000000000100l) +
                          (if ((n & u64"0x0000000000000200") ≠ u64"0x0000000000000200") 0 else 0x0000000000000200l) +
                          (if ((n & u64"0x0000000000000400") ≠ u64"0x0000000000000400") 0 else 0x0000000000000400l) +
                          (if ((n & u64"0x0000000000000800") ≠ u64"0x0000000000000800") 0 else 0x0000000000000800l) +
                          (if ((n & u64"0x0000000000001000") ≠ u64"0x0000000000001000") 0 else 0x0000000000001000l) +
                          (if ((n & u64"0x0000000000002000") ≠ u64"0x0000000000002000") 0 else 0x0000000000002000l) +
                          (if ((n & u64"0x0000000000004000") ≠ u64"0x0000000000004000") 0 else 0x0000000000004000l) +
                          (if ((n & u64"0x0000000000008000") ≠ u64"0x0000000000008000") 0 else 0x0000000000008000l) +
                          (if ((n & u64"0x0000000000010000") ≠ u64"0x0000000000010000") 0 else 0x0000000000010000l) +
                          (if ((n & u64"0x0000000000020000") ≠ u64"0x0000000000020000") 0 else 0x0000000000020000l) +
                          (if ((n & u64"0x0000000000040000") ≠ u64"0x0000000000040000") 0 else 0x0000000000040000l) +
                          (if ((n & u64"0x0000000000080000") ≠ u64"0x0000000000080000") 0 else 0x0000000000080000l) +
                          (if ((n & u64"0x0000000000100000") ≠ u64"0x0000000000100000") 0 else 0x0000000000100000l) +
                          (if ((n & u64"0x0000000000200000") ≠ u64"0x0000000000200000") 0 else 0x0000000000200000l) +
                          (if ((n & u64"0x0000000000400000") ≠ u64"0x0000000000400000") 0 else 0x0000000000400000l) +
                          (if ((n & u64"0x0000000000800000") ≠ u64"0x0000000000800000") 0 else 0x0000000000800000l) +
                          (if ((n & u64"0x0000000001000000") ≠ u64"0x0000000001000000") 0 else 0x0000000001000000l) +
                          (if ((n & u64"0x0000000002000000") ≠ u64"0x0000000002000000") 0 else 0x0000000002000000l) +
                          (if ((n & u64"0x0000000004000000") ≠ u64"0x0000000004000000") 0 else 0x0000000004000000l) +
                          (if ((n & u64"0x0000000008000000") ≠ u64"0x0000000008000000") 0 else 0x0000000008000000l) +
                          (if ((n & u64"0x0000000010000000") ≠ u64"0x0000000010000000") 0 else 0x0000000010000000l) +
                          (if ((n & u64"0x0000000020000000") ≠ u64"0x0000000020000000") 0 else 0x0000000020000000l) +
                          (if ((n & u64"0x0000000040000000") ≠ u64"0x0000000040000000") 0 else 0x0000000040000000l) +
                          (if ((n & u64"0x0000000080000000") ≠ u64"0x0000000080000000") 0 else 0x0000000080000000l) +
                          (if ((n & u64"0x0000000100000000") ≠ u64"0x0000000100000000") 0 else 0x0000000100000000l) +
                          (if ((n & u64"0x0000000200000000") ≠ u64"0x0000000200000000") 0 else 0x0000000200000000l) +
                          (if ((n & u64"0x0000000400000000") ≠ u64"0x0000000400000000") 0 else 0x0000000400000000l) +
                          (if ((n & u64"0x0000000800000000") ≠ u64"0x0000000800000000") 0 else 0x0000000800000000l) +
                          (if ((n & u64"0x0000001000000000") ≠ u64"0x0000001000000000") 0 else 0x0000001000000000l) +
                          (if ((n & u64"0x0000002000000000") ≠ u64"0x0000002000000000") 0 else 0x0000002000000000l) +
                          (if ((n & u64"0x0000004000000000") ≠ u64"0x0000004000000000") 0 else 0x0000004000000000l) +
                          (if ((n & u64"0x0000008000000000") ≠ u64"0x0000008000000000") 0 else 0x0000008000000000l) +
                          (if ((n & u64"0x0000010000000000") ≠ u64"0x0000010000000000") 0 else 0x0000010000000000l) +
                          (if ((n & u64"0x0000020000000000") ≠ u64"0x0000020000000000") 0 else 0x0000020000000000l) +
                          (if ((n & u64"0x0000040000000000") ≠ u64"0x0000040000000000") 0 else 0x0000040000000000l) +
                          (if ((n & u64"0x0000080000000000") ≠ u64"0x0000080000000000") 0 else 0x0000080000000000l) +
                          (if ((n & u64"0x0000100000000000") ≠ u64"0x0000100000000000") 0 else 0x0000100000000000l) +
                          (if ((n & u64"0x0000200000000000") ≠ u64"0x0000200000000000") 0 else 0x0000200000000000l) +
                          (if ((n & u64"0x0000400000000000") ≠ u64"0x0000400000000000") 0 else 0x0000400000000000l) +
                          (if ((n & u64"0x0000800000000000") ≠ u64"0x0000800000000000") 0 else 0x0000800000000000l) +
                          (if ((n & u64"0x0001000000000000") ≠ u64"0x0001000000000000") 0 else 0x0001000000000000l) +
                          (if ((n & u64"0x0002000000000000") ≠ u64"0x0002000000000000") 0 else 0x0002000000000000l) +
                          (if ((n & u64"0x0004000000000000") ≠ u64"0x0004000000000000") 0 else 0x0004000000000000l) +
                          (if ((n & u64"0x0008000000000000") ≠ u64"0x0008000000000000") 0 else 0x0008000000000000l) +
                          (if ((n & u64"0x0010000000000000") ≠ u64"0x0010000000000000") 0 else 0x0010000000000000l) +
                          (if ((n & u64"0x0020000000000000") ≠ u64"0x0020000000000000") 0 else 0x0020000000000000l) +
                          (if ((n & u64"0x0040000000000000") ≠ u64"0x0040000000000000") 0 else 0x0040000000000000l) +
                          (if ((n & u64"0x0080000000000000") ≠ u64"0x0080000000000000") 0 else 0x0080000000000000l) +
                          (if ((n & u64"0x0100000000000000") ≠ u64"0x0100000000000000") 0 else 0x0100000000000000l) +
                          (if ((n & u64"0x0200000000000000") ≠ u64"0x0200000000000000") 0 else 0x0200000000000000l) +
                          (if ((n & u64"0x0400000000000000") ≠ u64"0x0400000000000000") 0 else 0x0400000000000000l) +
                          (if ((n & u64"0x0800000000000000") ≠ u64"0x0800000000000000") 0 else 0x0800000000000000l) +
                          (if ((n & u64"0x1000000000000000") ≠ u64"0x1000000000000000") 0 else 0x1000000000000000l) +
                          (if ((n & u64"0x2000000000000000") ≠ u64"0x2000000000000000") 0 else 0x2000000000000000l) +
                          (if ((n & u64"0x4000000000000000") ≠ u64"0x4000000000000000") 0 else 0x4000000000000000l) +
                          (if ((n & u64"0x8000000000000000") ≠ u64"0x8000000000000000") 0 else 0x8000000000000000l)   """

  @pure def toZ8(n: U64): Z8 =
    l""" requires n ≤ u64"127"
         ensures  Z8.toZ(result) ≡ toZ(n) """

  @pure def toZ16(n: U64): Z16 =
    l""" requires n ≤ u64"32767"
         ensures  Z16.toZ(result) ≡ toZ(n) """

  @pure def toZ32(n: U64): Z32 =
    l""" requires n ≤ u64"2147483647"
         ensures  Z32.toZ(result) ≡ toZ(n) """

  @pure def toZ64(n: U64): Z64 =
    l""" requires n ≤ u64"9223372036854775807"
         ensures  Z64.toZ(result) ≡ toZ(n) """

  @pure def toN(n: U64): N =
    l""" ensures result ≡ (if ((n & u64"0x0000000000000001") ≠ u64"0x0000000000000001") n"0" else n"0x0000000000000001") +
                          (if ((n & u64"0x0000000000000002") ≠ u64"0x0000000000000002") n"0" else n"0x0000000000000002") +
                          (if ((n & u64"0x0000000000000004") ≠ u64"0x0000000000000004") n"0" else n"0x0000000000000004") +
                          (if ((n & u64"0x0000000000000008") ≠ u64"0x0000000000000008") n"0" else n"0x0000000000000008") +
                          (if ((n & u64"0x0000000000000010") ≠ u64"0x0000000000000010") n"0" else n"0x0000000000000010") +
                          (if ((n & u64"0x0000000000000020") ≠ u64"0x0000000000000020") n"0" else n"0x0000000000000020") +
                          (if ((n & u64"0x0000000000000040") ≠ u64"0x0000000000000040") n"0" else n"0x0000000000000040") +
                          (if ((n & u64"0x0000000000000080") ≠ u64"0x0000000000000080") n"0" else n"0x0000000000000080") +
                          (if ((n & u64"0x0000000000000100") ≠ u64"0x0000000000000100") n"0" else n"0x0000000000000100") +
                          (if ((n & u64"0x0000000000000200") ≠ u64"0x0000000000000200") n"0" else n"0x0000000000000200") +
                          (if ((n & u64"0x0000000000000400") ≠ u64"0x0000000000000400") n"0" else n"0x0000000000000400") +
                          (if ((n & u64"0x0000000000000800") ≠ u64"0x0000000000000800") n"0" else n"0x0000000000000800") +
                          (if ((n & u64"0x0000000000001000") ≠ u64"0x0000000000001000") n"0" else n"0x0000000000001000") +
                          (if ((n & u64"0x0000000000002000") ≠ u64"0x0000000000002000") n"0" else n"0x0000000000002000") +
                          (if ((n & u64"0x0000000000004000") ≠ u64"0x0000000000004000") n"0" else n"0x0000000000004000") +
                          (if ((n & u64"0x0000000000008000") ≠ u64"0x0000000000008000") n"0" else n"0x0000000000008000") +
                          (if ((n & u64"0x0000000000010000") ≠ u64"0x0000000000010000") n"0" else n"0x0000000000010000") +
                          (if ((n & u64"0x0000000000020000") ≠ u64"0x0000000000020000") n"0" else n"0x0000000000020000") +
                          (if ((n & u64"0x0000000000040000") ≠ u64"0x0000000000040000") n"0" else n"0x0000000000040000") +
                          (if ((n & u64"0x0000000000080000") ≠ u64"0x0000000000080000") n"0" else n"0x0000000000080000") +
                          (if ((n & u64"0x0000000000100000") ≠ u64"0x0000000000100000") n"0" else n"0x0000000000100000") +
                          (if ((n & u64"0x0000000000200000") ≠ u64"0x0000000000200000") n"0" else n"0x0000000000200000") +
                          (if ((n & u64"0x0000000000400000") ≠ u64"0x0000000000400000") n"0" else n"0x0000000000400000") +
                          (if ((n & u64"0x0000000000800000") ≠ u64"0x0000000000800000") n"0" else n"0x0000000000800000") +
                          (if ((n & u64"0x0000000001000000") ≠ u64"0x0000000001000000") n"0" else n"0x0000000001000000") +
                          (if ((n & u64"0x0000000002000000") ≠ u64"0x0000000002000000") n"0" else n"0x0000000002000000") +
                          (if ((n & u64"0x0000000004000000") ≠ u64"0x0000000004000000") n"0" else n"0x0000000004000000") +
                          (if ((n & u64"0x0000000008000000") ≠ u64"0x0000000008000000") n"0" else n"0x0000000008000000") +
                          (if ((n & u64"0x0000000010000000") ≠ u64"0x0000000010000000") n"0" else n"0x0000000010000000") +
                          (if ((n & u64"0x0000000020000000") ≠ u64"0x0000000020000000") n"0" else n"0x0000000020000000") +
                          (if ((n & u64"0x0000000040000000") ≠ u64"0x0000000040000000") n"0" else n"0x0000000040000000") +
                          (if ((n & u64"0x0000000080000000") ≠ u64"0x0000000080000000") n"0" else n"0x0000000080000000") +
                          (if ((n & u64"0x0000000100000000") ≠ u64"0x0000000100000000") n"0" else n"0x0000000100000000") +
                          (if ((n & u64"0x0000000200000000") ≠ u64"0x0000000200000000") n"0" else n"0x0000000200000000") +
                          (if ((n & u64"0x0000000400000000") ≠ u64"0x0000000400000000") n"0" else n"0x0000000400000000") +
                          (if ((n & u64"0x0000000800000000") ≠ u64"0x0000000800000000") n"0" else n"0x0000000800000000") +
                          (if ((n & u64"0x0000001000000000") ≠ u64"0x0000001000000000") n"0" else n"0x0000001000000000") +
                          (if ((n & u64"0x0000002000000000") ≠ u64"0x0000002000000000") n"0" else n"0x0000002000000000") +
                          (if ((n & u64"0x0000004000000000") ≠ u64"0x0000004000000000") n"0" else n"0x0000004000000000") +
                          (if ((n & u64"0x0000008000000000") ≠ u64"0x0000008000000000") n"0" else n"0x0000008000000000") +
                          (if ((n & u64"0x0000010000000000") ≠ u64"0x0000010000000000") n"0" else n"0x0000010000000000") +
                          (if ((n & u64"0x0000020000000000") ≠ u64"0x0000020000000000") n"0" else n"0x0000020000000000") +
                          (if ((n & u64"0x0000040000000000") ≠ u64"0x0000040000000000") n"0" else n"0x0000040000000000") +
                          (if ((n & u64"0x0000080000000000") ≠ u64"0x0000080000000000") n"0" else n"0x0000080000000000") +
                          (if ((n & u64"0x0000100000000000") ≠ u64"0x0000100000000000") n"0" else n"0x0000100000000000") +
                          (if ((n & u64"0x0000200000000000") ≠ u64"0x0000200000000000") n"0" else n"0x0000200000000000") +
                          (if ((n & u64"0x0000400000000000") ≠ u64"0x0000400000000000") n"0" else n"0x0000400000000000") +
                          (if ((n & u64"0x0000800000000000") ≠ u64"0x0000800000000000") n"0" else n"0x0000800000000000") +
                          (if ((n & u64"0x0001000000000000") ≠ u64"0x0001000000000000") n"0" else n"0x0001000000000000") +
                          (if ((n & u64"0x0002000000000000") ≠ u64"0x0002000000000000") n"0" else n"0x0002000000000000") +
                          (if ((n & u64"0x0004000000000000") ≠ u64"0x0004000000000000") n"0" else n"0x0004000000000000") +
                          (if ((n & u64"0x0008000000000000") ≠ u64"0x0008000000000000") n"0" else n"0x0008000000000000") +
                          (if ((n & u64"0x0010000000000000") ≠ u64"0x0010000000000000") n"0" else n"0x0010000000000000") +
                          (if ((n & u64"0x0020000000000000") ≠ u64"0x0020000000000000") n"0" else n"0x0020000000000000") +
                          (if ((n & u64"0x0040000000000000") ≠ u64"0x0040000000000000") n"0" else n"0x0040000000000000") +
                          (if ((n & u64"0x0080000000000000") ≠ u64"0x0080000000000000") n"0" else n"0x0080000000000000") +
                          (if ((n & u64"0x0100000000000000") ≠ u64"0x0100000000000000") n"0" else n"0x0100000000000000") +
                          (if ((n & u64"0x0200000000000000") ≠ u64"0x0200000000000000") n"0" else n"0x0200000000000000") +
                          (if ((n & u64"0x0400000000000000") ≠ u64"0x0400000000000000") n"0" else n"0x0400000000000000") +
                          (if ((n & u64"0x0800000000000000") ≠ u64"0x0800000000000000") n"0" else n"0x0800000000000000") +
                          (if ((n & u64"0x1000000000000000") ≠ u64"0x1000000000000000") n"0" else n"0x1000000000000000") +
                          (if ((n & u64"0x2000000000000000") ≠ u64"0x2000000000000000") n"0" else n"0x2000000000000000") +
                          (if ((n & u64"0x4000000000000000") ≠ u64"0x4000000000000000") n"0" else n"0x4000000000000000") +
                          (if ((n & u64"0x8000000000000000") ≠ u64"0x8000000000000000") n"0" else n"0x8000000000000000")   """

  @pure def toN8(n: U64): N8 =
    l""" requires n ≤ u64"255"
         ensures  N8.toN(result) ≡ toN(n) """

  @pure def toN16(n: U64): N16 =
    l""" requires n ≤ u64"65535"
         ensures  N16.toN(result) ≡ toN(n) """

  @pure def toN32(n: U64): N32 =
    l""" requires n ≤ u64"4294967295"
         ensures  N32.toN(result) ≡ toN(n) """

  @pure def toN64(n: U64): N64 =
    l""" ensures N64.toN(result) ≡ toN(n) """

  /* @first */
  @pure def toS8(n: U64): S8 =
  l""" requires n ≤ u64"127" """

  /* @first */
  @pure def toS16(n: U64): S16 =
  l""" requires n ≤ u64"32767" """

  /* @first */
  @pure def toS32(n: U64): S32 =
  l""" requires n ≤ u64"2147483647" """

  /* @first */
  @pure def toS64(n: U64): S64 =
  l""" requires n ≤ u64"9223372036854775807" """

  /* @first */
  @pure def toRawS64(n: U64): S64 = $

  /* @first */
  @pure def toU8(n: U64): U8 =
  l""" requires n ≤ u64"255" """

  /* @first */
  @pure def toU16(n: U64): U16 =
  l""" requires n ≤ u64"65535" """

  /* @first */
  @pure def toU32(n: U64): U32 =
  l""" requires n ≤ u64"4294967295" """

  /* @first */
  @pure def toU64(n: U64): U64 =
  l""" ensures result ≡ n """

  /* @first */
  @pure def toRawF64(n: U64): F64 = $
}


@ext object F32 {
  def random: F32 = $

  @pure def toB(n: F32): B =
    l""" ensures result ≡ (n ≠ f32"0.0") """

  /* @first */
  @pure def toRawU32(n: F32): U32 = $

  @pure def toF32(n: F32): F32 =
    l""" ensures result ≡ n """
}


@ext object F64 {
  def random: F64 = $

  @pure def toB(n: F64): B =
    l""" ensures result ≡ (n ≠ f64"0.0") """

  /* @first */
  @pure def toRawU64(n: F64): U64 = $

  @pure def toF64(n: F64): F64 =
    l""" ensures result ≡ n """
}


@ext object R {
  def random: R = $

  @pure def toB(n: R): B =
    l""" ensures result ≡ (n ≠ r"0.0") """

  /* @first */
  @pure def toZ(n: R): Z = $

  /* @first */
  @pure def toN(n: R): N =
  l""" requires n ≥ r"0.0" """

  @pure def toR(n: R): R =
    l""" ensures result ≡ n """
}

@ext object SI {

  @pure def append[I, E](s: IS[I, E], e: E): IS[I, E] =
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [0, result.size)  result(i) ≡ s(i)
                 result(result.size - 1) ≡ e            """

  @pure def prepend[I, E](s: IS[I, E], e: E): IS[I, E] =
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [1, result.size)  result(i) ≡ s(i - 1)
                 result(0) ≡ e                              """

  @pure def appends[I, E](s1: IS[I, E], s2: IS[I, E]): IS[I, E] =
    l""" ensures result.size ≡ s1.size + s2.size
                 ∀i: [0, s1.size)  result(i) ≡ s1(i)
                 ∀i: [0, s2.size)  result(s1.size + i) ≡ s2(i) """

  @pure def toMS[I, E](s: IS[I, E]): MS[I, E] =
    l""" ensures result.size ≡ s.size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

  @pure def chunk[I, E](s: IS[I, E], size: I): IS[I, IS[I, E]] =
    l""" requires s.size % size ≡ 0
         ensures  result.size * size ≡ s.size
                  ∀i: [0, result.size)  result(i).size ≡ size
                  ∀i: [0, result.size)
                    ∀j: [0, size)
                      s(i * result.size + j) ≡ result(i)(j)   """

  @pure def drop[I, E](s: IS[I, E], size: I): IS[I, E] =
    l""" requires s.size ≥ size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(size + i) """

  @pure def dropRight[I, E](s: IS[I, E], size: I): IS[I, E] =
    l""" requires s.size ≥ size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(i) """

  @spec def foldLeftSpec[I, E, R](s: IS[I, E], f: (R, E) => R, init: R, i: I): R =
    l""" = base:  init,                                      if i ≡ 0
         = rec:   f(foldLeftSpec(s, f, init, i - 1), s(i)),  if 0 < i ∧ i < s.size """

  /* s = [0, 1, 2, 3]
     foldLeftSpec(s, f, init, 3) ≡ f(foldLeftSpec(s, f, init, 2), s(3))
                                 = f(f(foldLeftSpec(s, f, init, 1), s(2)), s(3))
                                 = f(f(f(foldLeftSpec(s, f, init, 0), s(i1), s(2)), s(3))
                                 = f(f(f(init, s(i1)), s(2)), s(3)) */

  @pure def foldLeft[I, E, R](s: IS[I, E], f: (R, E) => R, init: R): R =
    l""" ensures result ≡ foldLeftSpec(s, f, init, s.size - 1) """

  @spec def foldRightSpec[I, E, R](s: IS[I, E], f: (R, E) => R, init: R, i: I): R =
    l""" = base:  f(init, s(s.size - 1)),                                  if i ≡ 0
         = rec:   f(foldRightSpec(s, f, init, i - 1), s(s.size - i - 1)),  if 1 < i ∧ i ≤ s.size """

  /* s = [0, 1, 2, 3]
     foldRightSpec(s, f, init, 3) ≡ f(foldRightSpec(s, f, init, 2))), s(0))
                                  = f(f(foldRightSpec(s, f, init, 1)), s(1)), s(0))
                                  = f(f(f(foldRightSpec(s, f, init, 0), s(2)), s(1)), s(0))
                                  = f(f(f(f(init, s(3)), s(2)), s(1)), s(0)) */

  @pure def foldRight[I, E, R](s: IS[I, E], f: (R, E) => R, init: R): R =
    l""" ensures result ≡ foldRightSpec(s, f, init, s.size - 1) """

  @pure def map[I, E1, E2](s: IS[I, E1], f: E1 => E2): IS[I, E2] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ f(s(i)) """

  @pure def take[I, E](s: IS[I, E], size: I): IS[I, E] =
    l""" requires s.size ≥ size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

  @pure def sortWith[I, E](s: IS[I, E], lt: (E, E) => B): IS[I, E] = $

  @pure def fromU8[I](s: IS[I, B], n: U8): IS[I, B] =
    l""" ensures result.size ≡ s.size + 8
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size    ) ≡ ((n & u8"0x01") ≠ u8"0x01") ∧
                 result(s.size + 1) ≡ ((n & u8"0x02") ≠ u8"0x02") ∧
                 result(s.size + 2) ≡ ((n & u8"0x04") ≠ u8"0x04") ∧
                 result(s.size + 3) ≡ ((n & u8"0x08") ≠ u8"0x08") ∧
                 result(s.size + 4) ≡ ((n & u8"0x10") ≠ u8"0x10") ∧
                 result(s.size + 5) ≡ ((n & u8"0x20") ≠ u8"0x20") ∧
                 result(s.size + 6) ≡ ((n & u8"0x40") ≠ u8"0x40") ∧
                 result(s.size + 7) ≡ ((n & u8"0x80") ≠ u8"0x80")   """

  @pure def fromU16[I](s: IS[I, B], n: U16): IS[I, B] =
    l""" ensures result.size ≡ s.size + 16
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u16"0x0001") ≠ u16"0x0001") ∧
                 result(s.size +  1) ≡ ((n & u16"0x0002") ≠ u16"0x0002") ∧
                 result(s.size +  2) ≡ ((n & u16"0x0004") ≠ u16"0x0004") ∧
                 result(s.size +  3) ≡ ((n & u16"0x0008") ≠ u16"0x0008") ∧
                 result(s.size +  4) ≡ ((n & u16"0x0010") ≠ u16"0x0010") ∧
                 result(s.size +  5) ≡ ((n & u16"0x0020") ≠ u16"0x0020") ∧
                 result(s.size +  6) ≡ ((n & u16"0x0040") ≠ u16"0x0040") ∧
                 result(s.size +  7) ≡ ((n & u16"0x0080") ≠ u16"0x0080") ∧
                 result(s.size +  8) ≡ ((n & u16"0x0100") ≠ u16"0x0100") ∧
                 result(s.size +  9) ≡ ((n & u16"0x0200") ≠ u16"0x0200") ∧
                 result(s.size + 10) ≡ ((n & u16"0x0400") ≠ u16"0x0400") ∧
                 result(s.size + 11) ≡ ((n & u16"0x0800") ≠ u16"0x0800") ∧
                 result(s.size + 12) ≡ ((n & u16"0x1000") ≠ u16"0x1000") ∧
                 result(s.size + 13) ≡ ((n & u16"0x2000") ≠ u16"0x2000") ∧
                 result(s.size + 14) ≡ ((n & u16"0x4000") ≠ u16"0x4000") ∧
                 result(s.size + 15) ≡ ((n & u16"0x8000") ≠ u16"0x8000")   """

  @pure def fromU32[I](s: IS[I, B], n: U32): IS[I, B] =
    l""" ensures result.size ≡ s.size + 32
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u32"0x00000001") ≠ u32"0x00000001") ∧
                 result(s.size +  1) ≡ ((n & u32"0x00000002") ≠ u32"0x00000002") ∧
                 result(s.size +  2) ≡ ((n & u32"0x00000004") ≠ u32"0x00000004") ∧
                 result(s.size +  3) ≡ ((n & u32"0x00000008") ≠ u32"0x00000008") ∧
                 result(s.size +  4) ≡ ((n & u32"0x00000010") ≠ u32"0x00000010") ∧
                 result(s.size +  5) ≡ ((n & u32"0x00000020") ≠ u32"0x00000020") ∧
                 result(s.size +  6) ≡ ((n & u32"0x00000040") ≠ u32"0x00000040") ∧
                 result(s.size +  7) ≡ ((n & u32"0x00000080") ≠ u32"0x00000080") ∧
                 result(s.size +  8) ≡ ((n & u32"0x00000100") ≠ u32"0x00000100") ∧
                 result(s.size +  9) ≡ ((n & u32"0x00000200") ≠ u32"0x00000200") ∧
                 result(s.size + 10) ≡ ((n & u32"0x00000400") ≠ u32"0x00000400") ∧
                 result(s.size + 11) ≡ ((n & u32"0x00000800") ≠ u32"0x00000800") ∧
                 result(s.size + 12) ≡ ((n & u32"0x00001000") ≠ u32"0x00001000") ∧
                 result(s.size + 13) ≡ ((n & u32"0x00002000") ≠ u32"0x00002000") ∧
                 result(s.size + 14) ≡ ((n & u32"0x00004000") ≠ u32"0x00004000") ∧
                 result(s.size + 15) ≡ ((n & u32"0x00008000") ≠ u32"0x00008000") ∧
                 result(s.size + 16) ≡ ((n & u32"0x00010000") ≠ u32"0x00010000") ∧
                 result(s.size + 17) ≡ ((n & u32"0x00020000") ≠ u32"0x00020000") ∧
                 result(s.size + 18) ≡ ((n & u32"0x00040000") ≠ u32"0x00040000") ∧
                 result(s.size + 19) ≡ ((n & u32"0x00080000") ≠ u32"0x00080000") ∧
                 result(s.size + 20) ≡ ((n & u32"0x00100000") ≠ u32"0x00100000") ∧
                 result(s.size + 21) ≡ ((n & u32"0x00200000") ≠ u32"0x00200000") ∧
                 result(s.size + 22) ≡ ((n & u32"0x00400000") ≠ u32"0x00400000") ∧
                 result(s.size + 23) ≡ ((n & u32"0x00800000") ≠ u32"0x00800000") ∧
                 result(s.size + 24) ≡ ((n & u32"0x01000000") ≠ u32"0x01000000") ∧
                 result(s.size + 25) ≡ ((n & u32"0x02000000") ≠ u32"0x02000000") ∧
                 result(s.size + 26) ≡ ((n & u32"0x04000000") ≠ u32"0x04000000") ∧
                 result(s.size + 27) ≡ ((n & u32"0x08000000") ≠ u32"0x08000000") ∧
                 result(s.size + 28) ≡ ((n & u32"0x10000000") ≠ u32"0x10000000") ∧
                 result(s.size + 29) ≡ ((n & u32"0x20000000") ≠ u32"0x20000000") ∧
                 result(s.size + 30) ≡ ((n & u32"0x40000000") ≠ u32"0x40000000") ∧
                 result(s.size + 31) ≡ ((n & u32"0x80000000") ≠ u32"0x80000000")   """

  @pure def fromU64[I](s: IS[I, B], n: U64): IS[I, B] =
    l""" ensures result.size ≡ s.size + 64
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u64"0x0000000000000001") ≠ u64"0x0000000000000001") ∧
                 result(s.size +  1) ≡ ((n & u64"0x0000000000000002") ≠ u64"0x0000000000000002") ∧
                 result(s.size +  2) ≡ ((n & u64"0x0000000000000004") ≠ u64"0x0000000000000004") ∧
                 result(s.size +  3) ≡ ((n & u64"0x0000000000000008") ≠ u64"0x0000000000000008") ∧
                 result(s.size +  4) ≡ ((n & u64"0x0000000000000010") ≠ u64"0x0000000000000010") ∧
                 result(s.size +  5) ≡ ((n & u64"0x0000000000000020") ≠ u64"0x0000000000000020") ∧
                 result(s.size +  6) ≡ ((n & u64"0x0000000000000040") ≠ u64"0x0000000000000040") ∧
                 result(s.size +  7) ≡ ((n & u64"0x0000000000000080") ≠ u64"0x0000000000000080") ∧
                 result(s.size +  8) ≡ ((n & u64"0x0000000000000100") ≠ u64"0x0000000000000100") ∧
                 result(s.size +  9) ≡ ((n & u64"0x0000000000000200") ≠ u64"0x0000000000000200") ∧
                 result(s.size + 10) ≡ ((n & u64"0x0000000000000400") ≠ u64"0x0000000000000400") ∧
                 result(s.size + 11) ≡ ((n & u64"0x0000000000000800") ≠ u64"0x0000000000000800") ∧
                 result(s.size + 12) ≡ ((n & u64"0x0000000000001000") ≠ u64"0x0000000000001000") ∧
                 result(s.size + 13) ≡ ((n & u64"0x0000000000002000") ≠ u64"0x0000000000002000") ∧
                 result(s.size + 14) ≡ ((n & u64"0x0000000000004000") ≠ u64"0x0000000000004000") ∧
                 result(s.size + 15) ≡ ((n & u64"0x0000000000008000") ≠ u64"0x0000000000008000") ∧
                 result(s.size + 16) ≡ ((n & u64"0x0000000000010000") ≠ u64"0x0000000000010000") ∧
                 result(s.size + 17) ≡ ((n & u64"0x0000000000020000") ≠ u64"0x0000000000020000") ∧
                 result(s.size + 18) ≡ ((n & u64"0x0000000000040000") ≠ u64"0x0000000000040000") ∧
                 result(s.size + 19) ≡ ((n & u64"0x0000000000080000") ≠ u64"0x0000000000080000") ∧
                 result(s.size + 20) ≡ ((n & u64"0x0000000000100000") ≠ u64"0x0000000000100000") ∧
                 result(s.size + 21) ≡ ((n & u64"0x0000000000200000") ≠ u64"0x0000000000200000") ∧
                 result(s.size + 22) ≡ ((n & u64"0x0000000000400000") ≠ u64"0x0000000000400000") ∧
                 result(s.size + 23) ≡ ((n & u64"0x0000000000800000") ≠ u64"0x0000000000800000") ∧
                 result(s.size + 24) ≡ ((n & u64"0x0000000001000000") ≠ u64"0x0000000001000000") ∧
                 result(s.size + 25) ≡ ((n & u64"0x0000000002000000") ≠ u64"0x0000000002000000") ∧
                 result(s.size + 26) ≡ ((n & u64"0x0000000004000000") ≠ u64"0x0000000004000000") ∧
                 result(s.size + 27) ≡ ((n & u64"0x0000000008000000") ≠ u64"0x0000000008000000") ∧
                 result(s.size + 28) ≡ ((n & u64"0x0000000010000000") ≠ u64"0x0000000010000000") ∧
                 result(s.size + 29) ≡ ((n & u64"0x0000000020000000") ≠ u64"0x0000000020000000") ∧
                 result(s.size + 30) ≡ ((n & u64"0x0000000040000000") ≠ u64"0x0000000040000000") ∧
                 result(s.size + 31) ≡ ((n & u64"0x0000000080000000") ≠ u64"0x0000000080000000") ∧
                 result(s.size + 32) ≡ ((n & u64"0x0000000100000000") ≠ u64"0x0000000100000000") ∧
                 result(s.size + 33) ≡ ((n & u64"0x0000000200000000") ≠ u64"0x0000000200000000") ∧
                 result(s.size + 34) ≡ ((n & u64"0x0000000400000000") ≠ u64"0x0000000400000000") ∧
                 result(s.size + 35) ≡ ((n & u64"0x0000000800000000") ≠ u64"0x0000000800000000") ∧
                 result(s.size + 36) ≡ ((n & u64"0x0000001000000000") ≠ u64"0x0000001000000000") ∧
                 result(s.size + 37) ≡ ((n & u64"0x0000002000000000") ≠ u64"0x0000002000000000") ∧
                 result(s.size + 38) ≡ ((n & u64"0x0000004000000000") ≠ u64"0x0000004000000000") ∧
                 result(s.size + 39) ≡ ((n & u64"0x0000008000000000") ≠ u64"0x0000008000000000") ∧
                 result(s.size + 40) ≡ ((n & u64"0x0000010000000000") ≠ u64"0x0000010000000000") ∧
                 result(s.size + 41) ≡ ((n & u64"0x0000020000000000") ≠ u64"0x0000020000000000") ∧
                 result(s.size + 42) ≡ ((n & u64"0x0000040000000000") ≠ u64"0x0000040000000000") ∧
                 result(s.size + 43) ≡ ((n & u64"0x0000080000000000") ≠ u64"0x0000080000000000") ∧
                 result(s.size + 44) ≡ ((n & u64"0x0000100000000000") ≠ u64"0x0000100000000000") ∧
                 result(s.size + 45) ≡ ((n & u64"0x0000200000000000") ≠ u64"0x0000200000000000") ∧
                 result(s.size + 46) ≡ ((n & u64"0x0000400000000000") ≠ u64"0x0000400000000000") ∧
                 result(s.size + 47) ≡ ((n & u64"0x0000800000000000") ≠ u64"0x0000800000000000") ∧
                 result(s.size + 48) ≡ ((n & u64"0x0001000000000000") ≠ u64"0x0001000000000000") ∧
                 result(s.size + 49) ≡ ((n & u64"0x0002000000000000") ≠ u64"0x0002000000000000") ∧
                 result(s.size + 50) ≡ ((n & u64"0x0004000000000000") ≠ u64"0x0004000000000000") ∧
                 result(s.size + 51) ≡ ((n & u64"0x0008000000000000") ≠ u64"0x0008000000000000") ∧
                 result(s.size + 52) ≡ ((n & u64"0x0010000000000000") ≠ u64"0x0010000000000000") ∧
                 result(s.size + 53) ≡ ((n & u64"0x0020000000000000") ≠ u64"0x0020000000000000") ∧
                 result(s.size + 54) ≡ ((n & u64"0x0040000000000000") ≠ u64"0x0040000000000000") ∧
                 result(s.size + 55) ≡ ((n & u64"0x0080000000000000") ≠ u64"0x0080000000000000") ∧
                 result(s.size + 56) ≡ ((n & u64"0x0100000000000000") ≠ u64"0x0100000000000000") ∧
                 result(s.size + 57) ≡ ((n & u64"0x0200000000000000") ≠ u64"0x0200000000000000") ∧
                 result(s.size + 58) ≡ ((n & u64"0x0400000000000000") ≠ u64"0x0400000000000000") ∧
                 result(s.size + 59) ≡ ((n & u64"0x0800000000000000") ≠ u64"0x0800000000000000") ∧
                 result(s.size + 60) ≡ ((n & u64"0x1000000000000000") ≠ u64"0x1000000000000000") ∧
                 result(s.size + 61) ≡ ((n & u64"0x2000000000000000") ≠ u64"0x2000000000000000") ∧
                 result(s.size + 62) ≡ ((n & u64"0x4000000000000000") ≠ u64"0x4000000000000000") ∧
                 result(s.size + 63) ≡ ((n & u64"0x8000000000000000") ≠ u64"0x8000000000000000")   """

  @pure def toU8[I](s: IS[I, B]): U8 =
    l""" requires s.size ≡ 8
         ensures  fromU8(result) ≡ s """

  @pure def toU16[I](s: IS[I, B]): U16 =
    l""" requires s.size ≡ 16
         ensures  fromU16(result) ≡ s """

  @pure def toU32[I](s: IS[I, B]): U32 =
    l""" requires s.size ≡ 32
         ensures  fromU32(result) ≡ s """

  @pure def toU64[I](s: IS[I, B]): U64 =
    l""" requires s.size ≡ 64
         ensures  fromU64(result) ≡ s """
}


@ext object SM {

  @pure def append[I, E](s: MS[I, E], e: E): MS[I, E] =
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [0, result.size)  result(i) ≡ s(i)
                 result(result.size - 1) ≡ e            """

  @pure def prepend[I, E](s: MS[I, E], e: E): MS[I, E] =
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [1, result.size)  result(i) ≡ s(i - 1)
                 result(0) ≡ e                              """

  @pure def appends[I, E](s1: MS[I, E], s2: MS[I, E]): MS[I, E] =
    l""" ensures result.size ≡ s1.size + s2.size
                 ∀i: [0, s1.size)  result(i) ≡ s1(i)
                 ∀i: [0, s2.size)  result(s1.size + i) ≡ s2(i) """

  @pure def toIS[I, E](s: MS[I, E]): IS[I, E] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """

  @pure def chunk[I, E](s: MS[I, E], size: I): MS[I, MS[I, E]] =
    l""" requires s.size % size ≡ 0
          ensures result.size * size ≡ s.size
                  ∀i: [0, result.size)  result(i).size ≡ size
                  ∀i: [0, result.size)
                    ∀j: [0, size)
                      s(i * result.size + j) ≡ result(i)(j) """

  @pure def drop[I, E](s: MS[I, E], size: I): MS[I, E] =
    l""" requires s.size ≥ size
          ensures result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(size + i) """

  @pure def dropRight[I, E](s: MS[I, E], size: I): MS[I, E] =
    l""" requires s.size ≥ size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(i) """

  @spec def foldLeftSpec[I, E, R](s: MS[I, E], f: (R, E) => R, init: R, i: I): R =
    l""" = base:  init,                                      if i ≡ 0
         = rec:   f(foldLeftSpec(s, f, init, i - 1), s(i)),  if 0 < i ∧ i < s.size """

  @pure def foldLeft[I, E, R](s: MS[I, E], f: (R, E) => R, init: R): R =
    l""" ensures result ≡ foldLeftSpec(s, f, init, s.size - 1) """

  @spec def foldRightSpec[I, E, R](s: MS[I, E], f: (R, E) => R, init: R, i: I): R =
    l""" = base:  f(init, s(s.size - 1)),                                  if i ≡ 0
         = rec:   f(foldRightSpec(s, f, init, i - 1), s(s.size - i - 1)),  if 1 < i ∧ i ≤ s.size """

  @pure def foldRight[I, E, R](s: MS[I, E], f: (R, E) => R, init: R): R =
    l""" ensures result ≡ foldRightSpec(s, f, init, s.size - 1) """

  @pure def map[I, E1, E2](s: MS[I, E1], f: E1 => E2): MS[I, E2] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ f(s(i)) """

  def transform[I, E](s: MS[I, E], f: E => E): Unit =
    l""" modifies s
          ensures s.size ≡ s_in.size
                  ∀i: [0, s.size)  s(i) ≡ f(s_in(i)) """

  @pure def take[I, E](s: MS[I, E], size: I): MS[I, E] =
    l""" requires s.size ≥ size
          ensures result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

  def sortWith[I, E](s: MS[I, E], @pure lt: (E, E) => B): Unit = $

  @pure def fromU8[I](s: MS[I, B], n: U8): MS[I, B] =
    l""" ensures result.size ≡ s.size + 8
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size    ) ≡ ((n & u8"0x01") ≠ u8"0x01") ∧
                 result(s.size + 1) ≡ ((n & u8"0x02") ≠ u8"0x02") ∧
                 result(s.size + 2) ≡ ((n & u8"0x04") ≠ u8"0x04") ∧
                 result(s.size + 3) ≡ ((n & u8"0x08") ≠ u8"0x08") ∧
                 result(s.size + 4) ≡ ((n & u8"0x10") ≠ u8"0x10") ∧
                 result(s.size + 5) ≡ ((n & u8"0x20") ≠ u8"0x20") ∧
                 result(s.size + 6) ≡ ((n & u8"0x40") ≠ u8"0x40") ∧
                 result(s.size + 7) ≡ ((n & u8"0x80") ≠ u8"0x80")   """

  @pure def fromU16[I](s: MS[I, B], n: U16): MS[I, B] =
    l""" ensures result.size ≡ s.size + 16
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u16"0x0001") ≠ u16"0x0001") ∧
                 result(s.size +  1) ≡ ((n & u16"0x0002") ≠ u16"0x0002") ∧
                 result(s.size +  2) ≡ ((n & u16"0x0004") ≠ u16"0x0004") ∧
                 result(s.size +  3) ≡ ((n & u16"0x0008") ≠ u16"0x0008") ∧
                 result(s.size +  4) ≡ ((n & u16"0x0010") ≠ u16"0x0010") ∧
                 result(s.size +  5) ≡ ((n & u16"0x0020") ≠ u16"0x0020") ∧
                 result(s.size +  6) ≡ ((n & u16"0x0040") ≠ u16"0x0040") ∧
                 result(s.size +  7) ≡ ((n & u16"0x0080") ≠ u16"0x0080") ∧
                 result(s.size +  8) ≡ ((n & u16"0x0100") ≠ u16"0x0100") ∧
                 result(s.size +  9) ≡ ((n & u16"0x0200") ≠ u16"0x0200") ∧
                 result(s.size + 10) ≡ ((n & u16"0x0400") ≠ u16"0x0400") ∧
                 result(s.size + 11) ≡ ((n & u16"0x0800") ≠ u16"0x0800") ∧
                 result(s.size + 12) ≡ ((n & u16"0x1000") ≠ u16"0x1000") ∧
                 result(s.size + 13) ≡ ((n & u16"0x2000") ≠ u16"0x2000") ∧
                 result(s.size + 14) ≡ ((n & u16"0x4000") ≠ u16"0x4000") ∧
                 result(s.size + 15) ≡ ((n & u16"0x8000") ≠ u16"0x8000")   """

  @pure def fromU32[I](s: MS[I, B], n: U32): MS[I, B] =
    l""" ensures result.size ≡ s.size + 32
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u32"0x00000001") ≠ u32"0x00000001") ∧
                 result(s.size +  1) ≡ ((n & u32"0x00000002") ≠ u32"0x00000002") ∧
                 result(s.size +  2) ≡ ((n & u32"0x00000004") ≠ u32"0x00000004") ∧
                 result(s.size +  3) ≡ ((n & u32"0x00000008") ≠ u32"0x00000008") ∧
                 result(s.size +  4) ≡ ((n & u32"0x00000010") ≠ u32"0x00000010") ∧
                 result(s.size +  5) ≡ ((n & u32"0x00000020") ≠ u32"0x00000020") ∧
                 result(s.size +  6) ≡ ((n & u32"0x00000040") ≠ u32"0x00000040") ∧
                 result(s.size +  7) ≡ ((n & u32"0x00000080") ≠ u32"0x00000080") ∧
                 result(s.size +  8) ≡ ((n & u32"0x00000100") ≠ u32"0x00000100") ∧
                 result(s.size +  9) ≡ ((n & u32"0x00000200") ≠ u32"0x00000200") ∧
                 result(s.size + 10) ≡ ((n & u32"0x00000400") ≠ u32"0x00000400") ∧
                 result(s.size + 11) ≡ ((n & u32"0x00000800") ≠ u32"0x00000800") ∧
                 result(s.size + 12) ≡ ((n & u32"0x00001000") ≠ u32"0x00001000") ∧
                 result(s.size + 13) ≡ ((n & u32"0x00002000") ≠ u32"0x00002000") ∧
                 result(s.size + 14) ≡ ((n & u32"0x00004000") ≠ u32"0x00004000") ∧
                 result(s.size + 15) ≡ ((n & u32"0x00008000") ≠ u32"0x00008000") ∧
                 result(s.size + 16) ≡ ((n & u32"0x00010000") ≠ u32"0x00010000") ∧
                 result(s.size + 17) ≡ ((n & u32"0x00020000") ≠ u32"0x00020000") ∧
                 result(s.size + 18) ≡ ((n & u32"0x00040000") ≠ u32"0x00040000") ∧
                 result(s.size + 19) ≡ ((n & u32"0x00080000") ≠ u32"0x00080000") ∧
                 result(s.size + 20) ≡ ((n & u32"0x00100000") ≠ u32"0x00100000") ∧
                 result(s.size + 21) ≡ ((n & u32"0x00200000") ≠ u32"0x00200000") ∧
                 result(s.size + 22) ≡ ((n & u32"0x00400000") ≠ u32"0x00400000") ∧
                 result(s.size + 23) ≡ ((n & u32"0x00800000") ≠ u32"0x00800000") ∧
                 result(s.size + 24) ≡ ((n & u32"0x01000000") ≠ u32"0x01000000") ∧
                 result(s.size + 25) ≡ ((n & u32"0x02000000") ≠ u32"0x02000000") ∧
                 result(s.size + 26) ≡ ((n & u32"0x04000000") ≠ u32"0x04000000") ∧
                 result(s.size + 27) ≡ ((n & u32"0x08000000") ≠ u32"0x08000000") ∧
                 result(s.size + 28) ≡ ((n & u32"0x10000000") ≠ u32"0x10000000") ∧
                 result(s.size + 29) ≡ ((n & u32"0x20000000") ≠ u32"0x20000000") ∧
                 result(s.size + 30) ≡ ((n & u32"0x40000000") ≠ u32"0x40000000") ∧
                 result(s.size + 31) ≡ ((n & u32"0x80000000") ≠ u32"0x80000000")   """

  @pure def fromU64[I](s: MS[I, B], n: U64): MS[I, B] =
    l""" ensures result.size ≡ s.size + 64
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 result(s.size     ) ≡ ((n & u64"0x0000000000000001") ≠ u64"0x0000000000000001") ∧
                 result(s.size +  1) ≡ ((n & u64"0x0000000000000002") ≠ u64"0x0000000000000002") ∧
                 result(s.size +  2) ≡ ((n & u64"0x0000000000000004") ≠ u64"0x0000000000000004") ∧
                 result(s.size +  3) ≡ ((n & u64"0x0000000000000008") ≠ u64"0x0000000000000008") ∧
                 result(s.size +  4) ≡ ((n & u64"0x0000000000000010") ≠ u64"0x0000000000000010") ∧
                 result(s.size +  5) ≡ ((n & u64"0x0000000000000020") ≠ u64"0x0000000000000020") ∧
                 result(s.size +  6) ≡ ((n & u64"0x0000000000000040") ≠ u64"0x0000000000000040") ∧
                 result(s.size +  7) ≡ ((n & u64"0x0000000000000080") ≠ u64"0x0000000000000080") ∧
                 result(s.size +  8) ≡ ((n & u64"0x0000000000000100") ≠ u64"0x0000000000000100") ∧
                 result(s.size +  9) ≡ ((n & u64"0x0000000000000200") ≠ u64"0x0000000000000200") ∧
                 result(s.size + 10) ≡ ((n & u64"0x0000000000000400") ≠ u64"0x0000000000000400") ∧
                 result(s.size + 11) ≡ ((n & u64"0x0000000000000800") ≠ u64"0x0000000000000800") ∧
                 result(s.size + 12) ≡ ((n & u64"0x0000000000001000") ≠ u64"0x0000000000001000") ∧
                 result(s.size + 13) ≡ ((n & u64"0x0000000000002000") ≠ u64"0x0000000000002000") ∧
                 result(s.size + 14) ≡ ((n & u64"0x0000000000004000") ≠ u64"0x0000000000004000") ∧
                 result(s.size + 15) ≡ ((n & u64"0x0000000000008000") ≠ u64"0x0000000000008000") ∧
                 result(s.size + 16) ≡ ((n & u64"0x0000000000010000") ≠ u64"0x0000000000010000") ∧
                 result(s.size + 17) ≡ ((n & u64"0x0000000000020000") ≠ u64"0x0000000000020000") ∧
                 result(s.size + 18) ≡ ((n & u64"0x0000000000040000") ≠ u64"0x0000000000040000") ∧
                 result(s.size + 19) ≡ ((n & u64"0x0000000000080000") ≠ u64"0x0000000000080000") ∧
                 result(s.size + 20) ≡ ((n & u64"0x0000000000100000") ≠ u64"0x0000000000100000") ∧
                 result(s.size + 21) ≡ ((n & u64"0x0000000000200000") ≠ u64"0x0000000000200000") ∧
                 result(s.size + 22) ≡ ((n & u64"0x0000000000400000") ≠ u64"0x0000000000400000") ∧
                 result(s.size + 23) ≡ ((n & u64"0x0000000000800000") ≠ u64"0x0000000000800000") ∧
                 result(s.size + 24) ≡ ((n & u64"0x0000000001000000") ≠ u64"0x0000000001000000") ∧
                 result(s.size + 25) ≡ ((n & u64"0x0000000002000000") ≠ u64"0x0000000002000000") ∧
                 result(s.size + 26) ≡ ((n & u64"0x0000000004000000") ≠ u64"0x0000000004000000") ∧
                 result(s.size + 27) ≡ ((n & u64"0x0000000008000000") ≠ u64"0x0000000008000000") ∧
                 result(s.size + 28) ≡ ((n & u64"0x0000000010000000") ≠ u64"0x0000000010000000") ∧
                 result(s.size + 29) ≡ ((n & u64"0x0000000020000000") ≠ u64"0x0000000020000000") ∧
                 result(s.size + 30) ≡ ((n & u64"0x0000000040000000") ≠ u64"0x0000000040000000") ∧
                 result(s.size + 31) ≡ ((n & u64"0x0000000080000000") ≠ u64"0x0000000080000000") ∧
                 result(s.size + 32) ≡ ((n & u64"0x0000000100000000") ≠ u64"0x0000000100000000") ∧
                 result(s.size + 33) ≡ ((n & u64"0x0000000200000000") ≠ u64"0x0000000200000000") ∧
                 result(s.size + 34) ≡ ((n & u64"0x0000000400000000") ≠ u64"0x0000000400000000") ∧
                 result(s.size + 35) ≡ ((n & u64"0x0000000800000000") ≠ u64"0x0000000800000000") ∧
                 result(s.size + 36) ≡ ((n & u64"0x0000001000000000") ≠ u64"0x0000001000000000") ∧
                 result(s.size + 37) ≡ ((n & u64"0x0000002000000000") ≠ u64"0x0000002000000000") ∧
                 result(s.size + 38) ≡ ((n & u64"0x0000004000000000") ≠ u64"0x0000004000000000") ∧
                 result(s.size + 39) ≡ ((n & u64"0x0000008000000000") ≠ u64"0x0000008000000000") ∧
                 result(s.size + 40) ≡ ((n & u64"0x0000010000000000") ≠ u64"0x0000010000000000") ∧
                 result(s.size + 41) ≡ ((n & u64"0x0000020000000000") ≠ u64"0x0000020000000000") ∧
                 result(s.size + 42) ≡ ((n & u64"0x0000040000000000") ≠ u64"0x0000040000000000") ∧
                 result(s.size + 43) ≡ ((n & u64"0x0000080000000000") ≠ u64"0x0000080000000000") ∧
                 result(s.size + 44) ≡ ((n & u64"0x0000100000000000") ≠ u64"0x0000100000000000") ∧
                 result(s.size + 45) ≡ ((n & u64"0x0000200000000000") ≠ u64"0x0000200000000000") ∧
                 result(s.size + 46) ≡ ((n & u64"0x0000400000000000") ≠ u64"0x0000400000000000") ∧
                 result(s.size + 47) ≡ ((n & u64"0x0000800000000000") ≠ u64"0x0000800000000000") ∧
                 result(s.size + 48) ≡ ((n & u64"0x0001000000000000") ≠ u64"0x0001000000000000") ∧
                 result(s.size + 49) ≡ ((n & u64"0x0002000000000000") ≠ u64"0x0002000000000000") ∧
                 result(s.size + 50) ≡ ((n & u64"0x0004000000000000") ≠ u64"0x0004000000000000") ∧
                 result(s.size + 51) ≡ ((n & u64"0x0008000000000000") ≠ u64"0x0008000000000000") ∧
                 result(s.size + 52) ≡ ((n & u64"0x0010000000000000") ≠ u64"0x0010000000000000") ∧
                 result(s.size + 53) ≡ ((n & u64"0x0020000000000000") ≠ u64"0x0020000000000000") ∧
                 result(s.size + 54) ≡ ((n & u64"0x0040000000000000") ≠ u64"0x0040000000000000") ∧
                 result(s.size + 55) ≡ ((n & u64"0x0080000000000000") ≠ u64"0x0080000000000000") ∧
                 result(s.size + 56) ≡ ((n & u64"0x0100000000000000") ≠ u64"0x0100000000000000") ∧
                 result(s.size + 57) ≡ ((n & u64"0x0200000000000000") ≠ u64"0x0200000000000000") ∧
                 result(s.size + 58) ≡ ((n & u64"0x0400000000000000") ≠ u64"0x0400000000000000") ∧
                 result(s.size + 59) ≡ ((n & u64"0x0800000000000000") ≠ u64"0x0800000000000000") ∧
                 result(s.size + 60) ≡ ((n & u64"0x1000000000000000") ≠ u64"0x1000000000000000") ∧
                 result(s.size + 61) ≡ ((n & u64"0x2000000000000000") ≠ u64"0x2000000000000000") ∧
                 result(s.size + 62) ≡ ((n & u64"0x4000000000000000") ≠ u64"0x4000000000000000") ∧
                 result(s.size + 63) ≡ ((n & u64"0x8000000000000000") ≠ u64"0x8000000000000000")   """

  @pure def toU8[I](s: MS[I, B]): U8 =
    l""" requires s.size ≡ 8
          ensures  fromU8(result) ≡ s """

  @pure def toU16[I](s: MS[I, B]): U16 =
    l""" requires s.size ≡ 16
          ensures  fromU16(result) ≡ s """

  @pure def toU32[I](s: MS[I, B]): U32 =
    l""" requires s.size ≡ 32
          ensures  fromU32(result) ≡ s """

  @pure def toU64[I](s: MS[I, B]): U64 =
    l""" requires s.size ≡ 64
          ensures  fromU64(result) ≡ s """
}

@ext object String {

  @pure def fromB(b: B): String = $

  @pure def fromC(c: C): String = $

  @pure def fromHexC(c: C): String = $

  @pure def fromZ(n: Z): String = $

  @pure def fromZ8(n: Z8): String = $

  @pure def fromZ16(n: Z16): String = $

  @pure def fromZ32(n: Z32): String = $

  @pure def fromZ64(n: Z64): String = $

  @pure def fromN(n: N): String = $

  @pure def fromN8(n: N8): String = $

  @pure def fromN16(n: N16): String = $

  @pure def fromN32(n: N32): String = $

  @pure def fromN64(n: N64): String = $

  @pure def fromS8(n: S8): String = $

  @pure def fromS16(n: S16): String = $

  @pure def fromS32(n: S32): String = $

  @pure def fromS64(n: S64): String = $

  @pure def fromU8(n: U8): String = $

  @pure def fromU16(n: U16): String = $

  @pure def fromU32(n: U32): String = $

  @pure def fromU64(n: U64): String = $

  @pure def fromF32(n: F32): String = $

  @pure def fromF64(n: F64): String = $

  @pure def fromR(n: R): String = $

  @pure def fromIS[I, V](s: IS[I, V]): String = $

  @pure def fromMS[I, V](s: MS[I, V]): String = $

  @pure def fromValues(s: ISZ[C]): String = $

  @pure def toB(s: String): Option[B] = $

  @pure def toC(s: String): Option[C] = $

  @pure def toHexC(s: String): Option[C] = $

  @pure def toZ(s: String): Option[Z] = $

  @pure def toZ8(s: String): Option[Z8] = $

  @pure def toZ16(s: String): Option[Z16] = $

  @pure def toZ32(s: String): Option[Z32] = $

  @pure def toZ64(s: String): Option[Z64] = $

  @pure def toN(s: String): Option[N] = $

  @pure def toN8(s: String): Option[N8] = $

  @pure def toN16(s: String): Option[N16] = $

  @pure def toN32(s: String): Option[N32] = $

  @pure def toN64(s: String): Option[N64] = $

  @pure def toS8(s: String): Option[S8] = $

  @pure def toS16(s: String): Option[S16] = $

  @pure def toS32(s: String): Option[S32] = $

  @pure def toS64(s: String): Option[S64] = $

  @pure def toU8(s: String): Option[U8] = $

  @pure def toU16(s: String): Option[U16] = $

  @pure def toU32(s: String): Option[U32] = $

  @pure def toU64(s: String): Option[U64] = $

  @pure def toF32(s: String): Option[F32] = $

  @pure def toF64(s: String): Option[F64] = $

  @pure def toR(s: String): Option[R] = $

  @pure def toValues(s: String): ISZ[C] = $

}
*/