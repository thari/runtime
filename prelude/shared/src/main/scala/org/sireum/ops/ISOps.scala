// #Sireum
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

package org.sireum.ops

import org.sireum._

object ISZO {
  @spec def foldLeftSpec[T, R](s: IS[Z, T], f: (R, T) => R, init: R, i: Z): R =
    l""" = base:  init,                                      if i ≡ 0
         = rec:   f(foldLeftSpec(s, f, init, i - 1), s(i)),  if 0 < i ∧ i < s.size """

  /* s = [0, 1, 2, 3]
     foldLeftSpec(s, f, init, 3) ≡ f(foldLeftSpec(s, f, init, 2), s(3))
                                 = f(f(foldLeftSpec(s, f, init, 1), s(2)), s(3))
                                 = f(f(f(foldLeftSpec(s, f, init, 0), s(i1), s(2)), s(3))
                                 = f(f(f(init, s(i1)), s(2)), s(3)) */

  @spec def foldRightSpec[T, R](s: IS[Z, T], f: (R, T) => R, init: R, i: Z): R =
    l""" = base:  f(init, s(s.size - 1)),                                  if i ≡ 0
         = rec:   f(foldRightSpec(s, f, init, i - 1), s(s.size - i - 1)),  if 1 < i ∧ i ≤ s.size """

  /* s = [0, 1, 2, 3]
     foldRightSpec(s, f, init, 3) ≡ f(foldRightSpec(s, f, init, 2))), s(0))
                                  = f(f(foldRightSpec(s, f, init, 1)), s(1)), s(0))
                                  = f(f(f(foldRightSpec(s, f, init, 0), s(2)), s(1)), s(0))
                                  = f(f(f(f(init, s(3)), s(2)), s(1)), s(0)) */

  @spec def countRec[T](s: IS[Z, T], e: T, i: Z): Z =
    l""" = base:   0, if i ≡ s.size
         = recEq:  1 + count(s, i + 1), if 0 ≤ i ∧ i < s.size ∧ s(i) ≡ e
         = recNe:  count(s, i + 1), if 0 ≤ i ∧ i < s.size ∧ s(i) ≠ e     """

  @spec def count[T](s: IS[Z, T], e: T): Z = l""" = countRect(s, e, 0) """

  @spec def isPermutationRec[T](s1: IS[Z, T], s2: IS[Z, T], i: Z): B =
    l""" = baseT:  T,                                if i ≡ s1.size ∧ eqSize
         = baseF:  F,                                if iInRange ∧ eqSize ∧ ¬countEq
         = recOk:  isPermutationRec(s1, s2, i + 1),  if iInRange ∧ eqSize ∧ countEq

         where val eqSize: B = s1.size ≡ s2.size
               val iInRange: B = 0 ≤ i ∧ i < s1.size
               val countEq: B = count(s1, s1(i)) ≡ count(s1, s2(i))                   """

  @spec def isPermutation[T](s1: IS[Z, T], s2: IS[Z, T]): B = l""" = isPermutationRec(s1, s2) """
}

@rich class ISZOps[T](s: IS[Z, T]) extends SO[Z, T] {

  @pure def :+(e: T): IS[Z, T] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [0, result.size)  result(i) ≡ s(i)
                 result(result.size - 1) ≡ e            """

    return s :+ e
  }

  @pure def +:(e: T): IS[Z, T] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [1, result.size)  result(i) ≡ s(i - 1)
                 result(0) ≡ e                              """

    return e +: s
  }

  @pure def ++(other: IS[Z, T]): IS[Z, T] = {
    l""" ensures result.size ≡ s.size + other.size
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 ∀i: [0, other.size)  result(s.size + i) ≡ other(i) """

    return s ++ other
  }

  @pure def chunk(size: Z): IS[Z, IS[Z, T]] = {
    l""" requires 0 < size
                  size <= s.size
         ensures  if (s.size % size ≡ 0) result.size * size ≡ s.size
                    else (result.size - 1) * size + s.size % size ≡ s.size
                  if (s.size % size ≡ 0) ∀i: [0, result.size)  result(i).size ≡ size
                    else ∀i: [0, result.size - 1)  result(i).size ≡ size
                  s.size % size ≠ 0 → result(result.size - 1).size ≡ s.size % size
                  ∀i: [0, result.size)
                    ∀j: [0, result(i).size)
                      s(i * result.size + j) ≡ result(i)(j)                          """

    var r = IS[Z, IS[Z, T]]()
    var chunk = IS[Z, T]()
    for (e <- s) {
      if (chunk.size == size) {
        r = r :+ chunk
        chunk = IS[Z, T]()
      }
      chunk = chunk :+ e
    }
    if (chunk.nonEmpty) {
      r = r :+ chunk
    }
    return r
  }

  @pure def contains(e: T): B = {
    l""" ensures result ≡ (∃i: [0, s.size) s(i) ≡ e) """

    for (v <- s if e == v) {
      return T
    }
    return F
  }

  @pure def drop(size: Z): IS[Z, T] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(size + i) """

    return laxSlice(size, s.size)
  }

  @pure def dropRight(size: Z): IS[Z, T] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(i) """

    return laxSlice(0, s.size - size)
  }

  @pure def exists(p: T => B): B = {
    l""" ensures result ≡ (∃i: [0, s.size) p(i)) """

    for (e <- s if p(e)) {
      return T
    }
    return F
  }

  @pure def first: T = {
    l""" requires s.size > 0
         ensures  result ≡ s(0) """
    return s(0)
  }

  @pure def forall(p: T => B): B = {
    l""" ensures result ≡ (∀i: [0, s.size) p(i)) """

    for (e <- s if !p(e)) {
      return F
    }
    return T
  }

  @pure def foldLeft[R](f: (R, T) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldLeftSpec(s, f, init, s.size - 1) """

    var r = init
    for (e <- s) {
      r = f(r, e)
    }
    return r
  }

  @pure def foldRight[R](f: (R, T) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldRightSpec(s, f, init, s.size - 1) """

    var r = init
    for (e <- s.reverse) {
      r = f(r, e)
    }
    return r
  }

  @pure def insert(i: Z, e: T): IS[Z, T] = {
    l""" requires 0 ≤ i
                  i <= s.size
         ensures  result.size ≡ s.size + 1
                  ∀j: [0, i) result(j) ≡ s(j)
                  result(i) ≡ e
                  ∀j: [j, s.size) result(j + 1) ≡ s(j) """
    return (laxSlice(0, i) :+ e) ++ laxSlice(i, s.size)
  }

  @pure def last: T = {
    l""" requires s.size > 0
         ensures  result ≡ s(s.size - 1) """

    return s(s.size - 1)
  }

  @pure def indexOf(e: T): Z = {
    l""" ensures (0 ≤ result ∧ result < s.size) → s(result) ≡ e
                 (result ≡ s.size) ≡ (∀i: [0, s.size) s(i) ≠ e)
                 ∃i: [0, s.size) s(i) ≡ e ∧ (∀j: [0, i) s(j) ≠ e) → result ≡ i
                 0 ≤ result
                 result ≤ s.size                                """
    for (i <- 0 until s.size if e == s(i)) {
      return i
    }
    return s.size
  }

  @pure def laxSlice(from: Z, til: Z): IS[Z, T] = {
    l""" ensures if (til > from) result.size ≡ NO(til).min(s.size) - NO(0).max(from)
                   else result.size ≡ 0
                 ∀i: [i, result.size) result(i) ≡ s(NO(0).max(from) + i)               """

    var r = IS[Z, T]()
    for (i <- from until til if 0 <= i && i < s.size) {
      r = r :+ s(i)
    }
    return r
  }

  @pure def map[U](f: T => U): IS[Z, U] = {
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ f(s(i)) """

    return s.map(f)
  }

  @pure def remove(i: Z): IS[Z, T] = {
    l""" requires 0 ≤ i
                  i < s.size
         ensures  result.size ≡ s.size - 1
                  ∀j: [0, i)  result(j) ≡ s(j)
                  ∀j: [i, result.size)  result(j) ≡ s(j + 1)
     """
    return laxSlice(0, i) ++ laxSlice(i + 1, s.size)
  }

  @pure def slice(from: Z, til: Z): IS[Z, T] = {
    l""" requires 0 ≤ from
                  from < s.size
                  0 ≤ til
                  til ≤ s.size
                  from ≤ til
         ensures  result.size ≡ til - from
                  ∀i: [0, result.size) result(i) ≡ s(from + i) """

    return laxSlice(from, til)
  }

  @pure def sortWith(lt: (T, T) => B): IS[Z, T] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size - 1) lt(i, i + 1)
                 SOps.isPermutation(s, result)         """

  @pure def tail: IS[Z, T] = {
    l""" requires s.size > 0
         ensures  result.size ≡ s.size - 1
                  ∀i: [0, result.size)  result(i) ≡ s(i + 1) """
    return drop(1)
  }

  @pure def take(size: Z): IS[Z, T] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

    return laxSlice(0, size)
  }

  @pure def takeRight(size: Z): IS[Z, T] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(s.size - size + i) """

    return laxSlice(s.size - size, s.size)
  }

  @pure def toMS: MS[Z, T] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """
}

object ISZBOps {

  @pure def fromU8(n: U8): IS[Z, B] = {
    l""" ensures result.size ≡ 8
                 result(0) ≡ ((n & u8"0x01") ≠ u8"0x01") ∧
                 result(1) ≡ ((n & u8"0x02") ≠ u8"0x02") ∧
                 result(2) ≡ ((n & u8"0x04") ≠ u8"0x04") ∧
                 result(3) ≡ ((n & u8"0x08") ≠ u8"0x08") ∧
                 result(4) ≡ ((n & u8"0x10") ≠ u8"0x10") ∧
                 result(5) ≡ ((n & u8"0x20") ≠ u8"0x20") ∧
                 result(6) ≡ ((n & u8"0x40") ≠ u8"0x40") ∧
                 result(7) ≡ ((n & u8"0x80") ≠ u8"0x80")   """
    return IS[Z, B](
      (n & u8"0x01") != u8"0x01",
      (n & u8"0x02") != u8"0x02",
      (n & u8"0x04") != u8"0x04",
      (n & u8"0x08") != u8"0x08",
      (n & u8"0x10") != u8"0x10",
      (n & u8"0x20") != u8"0x20",
      (n & u8"0x40") != u8"0x40",
      (n & u8"0x80") != u8"0x80"
    )
  }

  @pure def fromU16[I](n: U16): IS[I, B] = {
    l""" ensures result.size ≡ 16
                 result( 0) ≡ ((n & u16"0x0001") ≠ u16"0x0001") ∧
                 result( 1) ≡ ((n & u16"0x0002") ≠ u16"0x0002") ∧
                 result( 2) ≡ ((n & u16"0x0004") ≠ u16"0x0004") ∧
                 result( 3) ≡ ((n & u16"0x0008") ≠ u16"0x0008") ∧
                 result( 4) ≡ ((n & u16"0x0010") ≠ u16"0x0010") ∧
                 result( 5) ≡ ((n & u16"0x0020") ≠ u16"0x0020") ∧
                 result( 6) ≡ ((n & u16"0x0040") ≠ u16"0x0040") ∧
                 result( 7) ≡ ((n & u16"0x0080") ≠ u16"0x0080") ∧
                 result( 8) ≡ ((n & u16"0x0100") ≠ u16"0x0100") ∧
                 result( 9) ≡ ((n & u16"0x0200") ≠ u16"0x0200") ∧
                 result(10) ≡ ((n & u16"0x0400") ≠ u16"0x0400") ∧
                 result(11) ≡ ((n & u16"0x0800") ≠ u16"0x0800") ∧
                 result(12) ≡ ((n & u16"0x1000") ≠ u16"0x1000") ∧
                 result(13) ≡ ((n & u16"0x2000") ≠ u16"0x2000") ∧
                 result(14) ≡ ((n & u16"0x4000") ≠ u16"0x4000") ∧
                 result(15) ≡ ((n & u16"0x8000") ≠ u16"0x8000")   """
    return IS[Z, B](
      (n & u16"0x0001") != u16"0x0001",
      (n & u16"0x0002") != u16"0x0002",
      (n & u16"0x0004") != u16"0x0004",
      (n & u16"0x0008") != u16"0x0008",
      (n & u16"0x0010") != u16"0x0010",
      (n & u16"0x0020") != u16"0x0020",
      (n & u16"0x0040") != u16"0x0040",
      (n & u16"0x0080") != u16"0x0080",
      (n & u16"0x0100") != u16"0x0100",
      (n & u16"0x0200") != u16"0x0200",
      (n & u16"0x0400") != u16"0x0400",
      (n & u16"0x0800") != u16"0x0800",
      (n & u16"0x1000") != u16"0x1000",
      (n & u16"0x2000") != u16"0x2000",
      (n & u16"0x4000") != u16"0x4000",
      (n & u16"0x8000") != u16"0x8000"
    )
  }

  @pure def fromU32(s: IS[Z, B], n: U32): IS[Z, B] = {
    l""" ensures result.size ≡ 32
                 result( 0) ≡ ((n & u32"0x00000001") ≠ u32"0x00000001") ∧
                 result( 1) ≡ ((n & u32"0x00000002") ≠ u32"0x00000002") ∧
                 result( 2) ≡ ((n & u32"0x00000004") ≠ u32"0x00000004") ∧
                 result( 3) ≡ ((n & u32"0x00000008") ≠ u32"0x00000008") ∧
                 result( 4) ≡ ((n & u32"0x00000010") ≠ u32"0x00000010") ∧
                 result( 5) ≡ ((n & u32"0x00000020") ≠ u32"0x00000020") ∧
                 result( 6) ≡ ((n & u32"0x00000040") ≠ u32"0x00000040") ∧
                 result( 7) ≡ ((n & u32"0x00000080") ≠ u32"0x00000080") ∧
                 result( 8) ≡ ((n & u32"0x00000100") ≠ u32"0x00000100") ∧
                 result( 9) ≡ ((n & u32"0x00000200") ≠ u32"0x00000200") ∧
                 result(10) ≡ ((n & u32"0x00000400") ≠ u32"0x00000400") ∧
                 result(11) ≡ ((n & u32"0x00000800") ≠ u32"0x00000800") ∧
                 result(12) ≡ ((n & u32"0x00001000") ≠ u32"0x00001000") ∧
                 result(13) ≡ ((n & u32"0x00002000") ≠ u32"0x00002000") ∧
                 result(14) ≡ ((n & u32"0x00004000") ≠ u32"0x00004000") ∧
                 result(15) ≡ ((n & u32"0x00008000") ≠ u32"0x00008000") ∧
                 result(16) ≡ ((n & u32"0x00010000") ≠ u32"0x00010000") ∧
                 result(17) ≡ ((n & u32"0x00020000") ≠ u32"0x00020000") ∧
                 result(18) ≡ ((n & u32"0x00040000") ≠ u32"0x00040000") ∧
                 result(19) ≡ ((n & u32"0x00080000") ≠ u32"0x00080000") ∧
                 result(20) ≡ ((n & u32"0x00100000") ≠ u32"0x00100000") ∧
                 result(21) ≡ ((n & u32"0x00200000") ≠ u32"0x00200000") ∧
                 result(22) ≡ ((n & u32"0x00400000") ≠ u32"0x00400000") ∧
                 result(23) ≡ ((n & u32"0x00800000") ≠ u32"0x00800000") ∧
                 result(24) ≡ ((n & u32"0x01000000") ≠ u32"0x01000000") ∧
                 result(25) ≡ ((n & u32"0x02000000") ≠ u32"0x02000000") ∧
                 result(26) ≡ ((n & u32"0x04000000") ≠ u32"0x04000000") ∧
                 result(27) ≡ ((n & u32"0x08000000") ≠ u32"0x08000000") ∧
                 result(28) ≡ ((n & u32"0x10000000") ≠ u32"0x10000000") ∧
                 result(29) ≡ ((n & u32"0x20000000") ≠ u32"0x20000000") ∧
                 result(30) ≡ ((n & u32"0x40000000") ≠ u32"0x40000000") ∧
                 result(31) ≡ ((n & u32"0x80000000") ≠ u32"0x80000000")   """

    return IS[Z, B](
      (n & u32"0x00000001") != u32"0x00000001",
      (n & u32"0x00000002") != u32"0x00000002",
      (n & u32"0x00000004") != u32"0x00000004",
      (n & u32"0x00000008") != u32"0x00000008",
      (n & u32"0x00000010") != u32"0x00000010",
      (n & u32"0x00000020") != u32"0x00000020",
      (n & u32"0x00000040") != u32"0x00000040",
      (n & u32"0x00000080") != u32"0x00000080",
      (n & u32"0x00000100") != u32"0x00000100",
      (n & u32"0x00000200") != u32"0x00000200",
      (n & u32"0x00000400") != u32"0x00000400",
      (n & u32"0x00000800") != u32"0x00000800",
      (n & u32"0x00001000") != u32"0x00001000",
      (n & u32"0x00002000") != u32"0x00002000",
      (n & u32"0x00004000") != u32"0x00004000",
      (n & u32"0x00008000") != u32"0x00008000",
      (n & u32"0x00010000") != u32"0x00010000",
      (n & u32"0x00020000") != u32"0x00020000",
      (n & u32"0x00040000") != u32"0x00040000",
      (n & u32"0x00080000") != u32"0x00080000",
      (n & u32"0x00100000") != u32"0x00100000",
      (n & u32"0x00200000") != u32"0x00200000",
      (n & u32"0x00400000") != u32"0x00400000",
      (n & u32"0x00800000") != u32"0x00800000",
      (n & u32"0x01000000") != u32"0x01000000",
      (n & u32"0x02000000") != u32"0x02000000",
      (n & u32"0x04000000") != u32"0x04000000",
      (n & u32"0x08000000") != u32"0x08000000",
      (n & u32"0x10000000") != u32"0x10000000",
      (n & u32"0x20000000") != u32"0x20000000",
      (n & u32"0x40000000") != u32"0x40000000",
      (n & u32"0x80000000") != u32"0x80000000"
    )
  }

  @pure def fromU64(s: IS[Z, B], n: U64): IS[Z, B] = {
    l""" ensures result.size ≡ 64
                 result(  ) ≡ ((n & u64"0x0000000000000001") ≠ u64"0x0000000000000001") ∧
                 result( 1) ≡ ((n & u64"0x0000000000000002") ≠ u64"0x0000000000000002") ∧
                 result( 2) ≡ ((n & u64"0x0000000000000004") ≠ u64"0x0000000000000004") ∧
                 result( 3) ≡ ((n & u64"0x0000000000000008") ≠ u64"0x0000000000000008") ∧
                 result( 4) ≡ ((n & u64"0x0000000000000010") ≠ u64"0x0000000000000010") ∧
                 result( 5) ≡ ((n & u64"0x0000000000000020") ≠ u64"0x0000000000000020") ∧
                 result( 6) ≡ ((n & u64"0x0000000000000040") ≠ u64"0x0000000000000040") ∧
                 result( 7) ≡ ((n & u64"0x0000000000000080") ≠ u64"0x0000000000000080") ∧
                 result( 8) ≡ ((n & u64"0x0000000000000100") ≠ u64"0x0000000000000100") ∧
                 result( 9) ≡ ((n & u64"0x0000000000000200") ≠ u64"0x0000000000000200") ∧
                 result(10) ≡ ((n & u64"0x0000000000000400") ≠ u64"0x0000000000000400") ∧
                 result(11) ≡ ((n & u64"0x0000000000000800") ≠ u64"0x0000000000000800") ∧
                 result(12) ≡ ((n & u64"0x0000000000001000") ≠ u64"0x0000000000001000") ∧
                 result(13) ≡ ((n & u64"0x0000000000002000") ≠ u64"0x0000000000002000") ∧
                 result(14) ≡ ((n & u64"0x0000000000004000") ≠ u64"0x0000000000004000") ∧
                 result(15) ≡ ((n & u64"0x0000000000008000") ≠ u64"0x0000000000008000") ∧
                 result(16) ≡ ((n & u64"0x0000000000010000") ≠ u64"0x0000000000010000") ∧
                 result(17) ≡ ((n & u64"0x0000000000020000") ≠ u64"0x0000000000020000") ∧
                 result(18) ≡ ((n & u64"0x0000000000040000") ≠ u64"0x0000000000040000") ∧
                 result(19) ≡ ((n & u64"0x0000000000080000") ≠ u64"0x0000000000080000") ∧
                 result(20) ≡ ((n & u64"0x0000000000100000") ≠ u64"0x0000000000100000") ∧
                 result(21) ≡ ((n & u64"0x0000000000200000") ≠ u64"0x0000000000200000") ∧
                 result(22) ≡ ((n & u64"0x0000000000400000") ≠ u64"0x0000000000400000") ∧
                 result(23) ≡ ((n & u64"0x0000000000800000") ≠ u64"0x0000000000800000") ∧
                 result(24) ≡ ((n & u64"0x0000000001000000") ≠ u64"0x0000000001000000") ∧
                 result(25) ≡ ((n & u64"0x0000000002000000") ≠ u64"0x0000000002000000") ∧
                 result(26) ≡ ((n & u64"0x0000000004000000") ≠ u64"0x0000000004000000") ∧
                 result(27) ≡ ((n & u64"0x0000000008000000") ≠ u64"0x0000000008000000") ∧
                 result(28) ≡ ((n & u64"0x0000000010000000") ≠ u64"0x0000000010000000") ∧
                 result(29) ≡ ((n & u64"0x0000000020000000") ≠ u64"0x0000000020000000") ∧
                 result(30) ≡ ((n & u64"0x0000000040000000") ≠ u64"0x0000000040000000") ∧
                 result(31) ≡ ((n & u64"0x0000000080000000") ≠ u64"0x0000000080000000") ∧
                 result(32) ≡ ((n & u64"0x0000000100000000") ≠ u64"0x0000000100000000") ∧
                 result(33) ≡ ((n & u64"0x0000000200000000") ≠ u64"0x0000000200000000") ∧
                 result(34) ≡ ((n & u64"0x0000000400000000") ≠ u64"0x0000000400000000") ∧
                 result(35) ≡ ((n & u64"0x0000000800000000") ≠ u64"0x0000000800000000") ∧
                 result(36) ≡ ((n & u64"0x0000001000000000") ≠ u64"0x0000001000000000") ∧
                 result(37) ≡ ((n & u64"0x0000002000000000") ≠ u64"0x0000002000000000") ∧
                 result(38) ≡ ((n & u64"0x0000004000000000") ≠ u64"0x0000004000000000") ∧
                 result(39) ≡ ((n & u64"0x0000008000000000") ≠ u64"0x0000008000000000") ∧
                 result(40) ≡ ((n & u64"0x0000010000000000") ≠ u64"0x0000010000000000") ∧
                 result(41) ≡ ((n & u64"0x0000020000000000") ≠ u64"0x0000020000000000") ∧
                 result(42) ≡ ((n & u64"0x0000040000000000") ≠ u64"0x0000040000000000") ∧
                 result(43) ≡ ((n & u64"0x0000080000000000") ≠ u64"0x0000080000000000") ∧
                 result(44) ≡ ((n & u64"0x0000100000000000") ≠ u64"0x0000100000000000") ∧
                 result(45) ≡ ((n & u64"0x0000200000000000") ≠ u64"0x0000200000000000") ∧
                 result(46) ≡ ((n & u64"0x0000400000000000") ≠ u64"0x0000400000000000") ∧
                 result(47) ≡ ((n & u64"0x0000800000000000") ≠ u64"0x0000800000000000") ∧
                 result(48) ≡ ((n & u64"0x0001000000000000") ≠ u64"0x0001000000000000") ∧
                 result(49) ≡ ((n & u64"0x0002000000000000") ≠ u64"0x0002000000000000") ∧
                 result(50) ≡ ((n & u64"0x0004000000000000") ≠ u64"0x0004000000000000") ∧
                 result(51) ≡ ((n & u64"0x0008000000000000") ≠ u64"0x0008000000000000") ∧
                 result(52) ≡ ((n & u64"0x0010000000000000") ≠ u64"0x0010000000000000") ∧
                 result(53) ≡ ((n & u64"0x0020000000000000") ≠ u64"0x0020000000000000") ∧
                 result(54) ≡ ((n & u64"0x0040000000000000") ≠ u64"0x0040000000000000") ∧
                 result(55) ≡ ((n & u64"0x0080000000000000") ≠ u64"0x0080000000000000") ∧
                 result(56) ≡ ((n & u64"0x0100000000000000") ≠ u64"0x0100000000000000") ∧
                 result(57) ≡ ((n & u64"0x0200000000000000") ≠ u64"0x0200000000000000") ∧
                 result(58) ≡ ((n & u64"0x0400000000000000") ≠ u64"0x0400000000000000") ∧
                 result(59) ≡ ((n & u64"0x0800000000000000") ≠ u64"0x0800000000000000") ∧
                 result(60) ≡ ((n & u64"0x1000000000000000") ≠ u64"0x1000000000000000") ∧
                 result(61) ≡ ((n & u64"0x2000000000000000") ≠ u64"0x2000000000000000") ∧
                 result(62) ≡ ((n & u64"0x4000000000000000") ≠ u64"0x4000000000000000") ∧
                 result(63) ≡ ((n & u64"0x8000000000000000") ≠ u64"0x8000000000000000")   """

    IS[Z, B](
      (n & u64"0x0000000000000001") != u64"0x0000000000000001",
      (n & u64"0x0000000000000002") != u64"0x0000000000000002",
      (n & u64"0x0000000000000004") != u64"0x0000000000000004",
      (n & u64"0x0000000000000008") != u64"0x0000000000000008",
      (n & u64"0x0000000000000010") != u64"0x0000000000000010",
      (n & u64"0x0000000000000020") != u64"0x0000000000000020",
      (n & u64"0x0000000000000040") != u64"0x0000000000000040",
      (n & u64"0x0000000000000080") != u64"0x0000000000000080",
      (n & u64"0x0000000000000100") != u64"0x0000000000000100",
      (n & u64"0x0000000000000200") != u64"0x0000000000000200",
      (n & u64"0x0000000000000400") != u64"0x0000000000000400",
      (n & u64"0x0000000000000800") != u64"0x0000000000000800",
      (n & u64"0x0000000000001000") != u64"0x0000000000001000",
      (n & u64"0x0000000000002000") != u64"0x0000000000002000",
      (n & u64"0x0000000000004000") != u64"0x0000000000004000",
      (n & u64"0x0000000000008000") != u64"0x0000000000008000",
      (n & u64"0x0000000000010000") != u64"0x0000000000010000",
      (n & u64"0x0000000000020000") != u64"0x0000000000020000",
      (n & u64"0x0000000000040000") != u64"0x0000000000040000",
      (n & u64"0x0000000000080000") != u64"0x0000000000080000",
      (n & u64"0x0000000000100000") != u64"0x0000000000100000",
      (n & u64"0x0000000000200000") != u64"0x0000000000200000",
      (n & u64"0x0000000000400000") != u64"0x0000000000400000",
      (n & u64"0x0000000000800000") != u64"0x0000000000800000",
      (n & u64"0x0000000001000000") != u64"0x0000000001000000",
      (n & u64"0x0000000002000000") != u64"0x0000000002000000",
      (n & u64"0x0000000004000000") != u64"0x0000000004000000",
      (n & u64"0x0000000008000000") != u64"0x0000000008000000",
      (n & u64"0x0000000010000000") != u64"0x0000000010000000",
      (n & u64"0x0000000020000000") != u64"0x0000000020000000",
      (n & u64"0x0000000040000000") != u64"0x0000000040000000",
      (n & u64"0x0000000080000000") != u64"0x0000000080000000",
      (n & u64"0x0000000100000000") != u64"0x0000000100000000",
      (n & u64"0x0000000200000000") != u64"0x0000000200000000",
      (n & u64"0x0000000400000000") != u64"0x0000000400000000",
      (n & u64"0x0000000800000000") != u64"0x0000000800000000",
      (n & u64"0x0000001000000000") != u64"0x0000001000000000",
      (n & u64"0x0000002000000000") != u64"0x0000002000000000",
      (n & u64"0x0000004000000000") != u64"0x0000004000000000",
      (n & u64"0x0000008000000000") != u64"0x0000008000000000",
      (n & u64"0x0000010000000000") != u64"0x0000010000000000",
      (n & u64"0x0000020000000000") != u64"0x0000020000000000",
      (n & u64"0x0000040000000000") != u64"0x0000040000000000",
      (n & u64"0x0000080000000000") != u64"0x0000080000000000",
      (n & u64"0x0000100000000000") != u64"0x0000100000000000",
      (n & u64"0x0000200000000000") != u64"0x0000200000000000",
      (n & u64"0x0000400000000000") != u64"0x0000400000000000",
      (n & u64"0x0000800000000000") != u64"0x0000800000000000",
      (n & u64"0x0001000000000000") != u64"0x0001000000000000",
      (n & u64"0x0002000000000000") != u64"0x0002000000000000",
      (n & u64"0x0004000000000000") != u64"0x0004000000000000",
      (n & u64"0x0008000000000000") != u64"0x0008000000000000",
      (n & u64"0x0010000000000000") != u64"0x0010000000000000",
      (n & u64"0x0020000000000000") != u64"0x0020000000000000",
      (n & u64"0x0040000000000000") != u64"0x0040000000000000",
      (n & u64"0x0080000000000000") != u64"0x0080000000000000",
      (n & u64"0x0100000000000000") != u64"0x0100000000000000",
      (n & u64"0x0200000000000000") != u64"0x0200000000000000",
      (n & u64"0x0400000000000000") != u64"0x0400000000000000",
      (n & u64"0x0800000000000000") != u64"0x0800000000000000",
      (n & u64"0x1000000000000000") != u64"0x1000000000000000",
      (n & u64"0x2000000000000000") != u64"0x2000000000000000",
      (n & u64"0x4000000000000000") != u64"0x4000000000000000",
      (n & u64"0x8000000000000000") != u64"0x8000000000000000"
    )
  }
}

@rich class ISZBOps(s: IS[Z, B]) extends SBO[Z] {

  @pure def toU8: U8 = {
    l""" requires s.size ≡ 8
         ensures  fromU8(result) ≡ s """

    var r = u8"0"
    var mask = u8"1"
    for (i <- 0 until 8) {
      if (s(i)) {
        r = r | mask
      }
      mask = mask << u8"1"
    }
    return r
  }

  @pure def toU16: U16 = {
    l""" requires s.size ≡ 16
         ensures  fromU16(result) ≡ s """

    var r = u16"0"
    var mask = u16"1"
    for (i <- 0 until 16) {
      if (s(i)) {
        r = r | mask
      }
      mask = mask << u16"1"
    }
    return r
  }

  @pure def toU32: U32 = {
    l""" requires s.size ≡ 32
         ensures  fromU32(result) ≡ s """

    var r = u32"0"
    var mask = u32"1"
    for (i <- 0 until 32) {
      if (s(i)) {
        r = r | mask
      }
      mask = mask << u32"1"
    }
    return r
  }

  @pure def toU64: U64 = {
    l""" requires s.size ≡ 64
         ensures  fromU64(result) ≡ s """

    var r = u64"0"
    var mask = u64"1"
    for (i <- 0 until 64) {
      if (s(i)) {
        r = r | mask
      }
      mask = mask << u64"1"
    }
    return r
  }

}
