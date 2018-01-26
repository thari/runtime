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

@sig trait SOps[I, V] {

  @pure def contains(e: V): B

  @pure def exists(p: V => B): B

  @pure def first: V

  @pure def foldLeft[R](f: (R, V) => R, init: R): R

  @pure def foldRight[R](f: (R, V) => R, init: R): R

  @pure def parMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R

  def mParMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R

  @pure def parMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R

  def mParMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R

  @pure def forall(p: V => B): B

  @pure def indexOf(e: V): I

  @pure def last: V

}

@sig trait ISOps[I, V] {

  @pure def :+(e: V): IS[I, V]

  @pure def +:(e: V): IS[I, V]

  @pure def ++(other: IS[I, V]): IS[I, V]

  @pure def chunk(size: Z): IS[Z, IS[I, V]]

  @pure def drop(size: Z): IS[I, V]

  @pure def dropRight(size: Z): IS[I, V]

  @pure def insert(i: I, e: V): IS[I, V]

  @pure def laxSlice(from: I, til: I): IS[I, V]

  @pure def map[U](f: V => U): IS[I, U]

  @pure def remove(i: I): IS[I, V]

  @pure def reverse: IS[I, V]

  @pure def slice(from: I, til: I): IS[I, V]

  @pure def sortWith(lt: (V, V) => B): IS[I, V]

  @pure def tail: IS[I, V]

  @pure def take(size: Z): IS[I, V]

  @pure def takeRight(size: Z): IS[Z, V]

}

@sig trait MSOps[I, V] {

  @pure def :+(e: V): MS[I, V]

  @pure def +:(e: V): MS[I, V]

  @pure def ++(other: MS[I, V]): MS[I, V]

  @pure def chunk(size: Z): MS[Z, MS[I, V]]

  @pure def drop(size: Z): MS[I, V]

  @pure def dropRight(size: Z): MS[I, V]

  @pure def insert(i: I, e: V): MS[I, V]

  @pure def laxSlice(from: I, til: I): MS[I, V]

  @pure def map[U](f: V => U): MS[I, U]

  @pure def remove(i: I): MS[I, V]

  @pure def reverse: MS[I, V]

  @pure def slice(from: I, til: I): MS[I, V]

  @pure def sortWith(lt: (V, V) => B): MS[I, V]

  @pure def tail: MS[I, V]

  @pure def take(size: Z): MS[I, V]

  @pure def takeRight(size: Z): MS[Z, V]

}

@ext object ISZOpsUtil {
  @pure def parMapFoldLeft[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R =
    l""" ensures ISZOps(s.map(f)).foldLeft(g, init) """

  def mParMapFoldLeft[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = $

  @pure def parMapFoldRight[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R =
    l""" ensures ISZOps(s.map(f)).foldLeft(g, init) """

  def mParMapFoldRight[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = $

  @pure def sortWith[V](s: IS[Z, V], lt: (V, V) => B): IS[Z, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size - 1) lt(i, i + 1)
                 SOps.isPermutation(s, result)         """
}

@datatype class ISZOps[V](s: IS[Z, V]) extends SOps[Z, V] with ISOps[Z, V] {

  @pure def :+(e: V): IS[Z, V] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [0, result.size)  result(i) ≡ s(i)
                 result(result.size - 1) ≡ e            """

    return s :+ e
  }

  @pure def +:(e: V): IS[Z, V] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [1, result.size)  result(i) ≡ s(i - 1)
                 result(0) ≡ e                              """

    return e +: s
  }

  @pure def ++(other: IS[Z, V]): IS[Z, V] = {
    l""" ensures result.size ≡ s.size + other.size
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 ∀i: [0, other.size)  result(s.size + i) ≡ other(i) """

    return s ++ other
  }

  @pure def chunk(size: Z): IS[Z, IS[Z, V]] = {
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

    var r = IS[Z, IS[Z, V]]()
    var chunk = IS[Z, V]()
    for (e <- s) {
      if (chunk.size == size) {
        r = r :+ chunk
        chunk = IS[Z, V]()
      }
      chunk = chunk :+ e
    }
    if (chunk.nonEmpty) {
      r = r :+ chunk
    }
    return r
  }

  @pure def contains(e: V): B = {
    l""" ensures result ≡ (∃i: [0, s.size) s(i) ≡ e) """

    return exists((x: V) => x == e)
  }

  @pure def drop(size: Z): IS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(size + i) """

    return laxSlice(size, s.size)
  }

  @pure def dropRight(size: Z): IS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(i) """

    return laxSlice(0, s.size - size)
  }

  @pure def exists(p: V => B): B = {
    l""" ensures result ≡ (∃i: [0, s.size) p(i)) """

    for (e <- s) {
      if (p(e)) {
        return T
      }
    }
    return F
  }

  @pure def first: V = {
    l""" requires s.size > 0
         ensures  result ≡ s(0) """

    return s(0)
  }

  @pure def forall(p: V => B): B = {
    l""" ensures result ≡ (∀i: [0, s.size) p(i)) """

    for (e <- s) {
      if (!p(e)) {
        return F
      }
    }
    return T
  }

  @pure def foldLeft[R](f: (R, V) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldLeftSpec(s, f, init, s.size - 1) """

    var r = init
    for (e <- s) {
      r = f(r, e)
    }
    return r
  }

  @pure def foldRight[R](f: (R, V) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldRightSpec(s, f, init, s.size - 1) """

    var r = init
    for (i <- s.indices.reverse) {
      r = f(r, s(i))
    }
    return r
  }

  @pure def insert(i: Z, e: V): IS[Z, V] = {
    l""" requires 0 ≤ i
                  i <= s.size
         ensures  result.size ≡ s.size + 1
                  ∀j: [0, i) result(j) ≡ s(j)
                  result(i) ≡ e
                  ∀j: [j, s.size) result(j + 1) ≡ s(j) """
    return (laxSlice(0, i) :+ e) ++ laxSlice(i, s.size)
  }

  @pure def last: V = {
    l""" requires s.size > 0
         ensures  result ≡ s(s.size - 1) """

    return s(s.size - 1)
  }

  @pure def indexOf(e: V): Z = {
    l""" ensures (0 ≤ result ∧ result < s.size) → s(result) ≡ e
                 (result ≡ s.size) ≡ (∀i: [0, s.size) s(i) ≠ e)
                 ∃i: [0, s.size) s(i) ≡ e ∧ (∀j: [0, i) s(j) ≠ e) → result ≡ i
                 0 ≤ result
                 result ≤ s.size                                """
    for (i <- z"0" until s.size if e == s(i)) {
      return i
    }
    return s.size
  }

  @pure def laxSlice(from: Z, til: Z): IS[Z, V] = {
    l""" ensures if (til > from) result.size ≡ NO(til).min(s.size) - NO(0).max(from)
                   else result.size ≡ 0
                 ∀i: [i, result.size) result(i) ≡ s(NO(0).max(from) + i)               """

    var r = IS[Z, V]()
    for (i <- from until til if 0 <= i && i < s.size) {
      r = r :+ s(i)
    }
    return r
  }

  @pure def map[U](f: V => U): IS[Z, U] = {
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ f(s(i)) """

    return s.map(f)
  }

  @pure def parMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = ISZOpsUtil.parMapFoldLeft(s, f, g, init)
    return r
  }

  def mParMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = ISZOpsUtil.mParMapFoldLeft(s, f, g, init)
    return r
  }

  @pure def parMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = ISZOpsUtil.parMapFoldRight(s, f, g, init)
    return r
  }

  def mParMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = ISZOpsUtil.mParMapFoldRight(s, f, g, init)
    return r
  }

  @pure def remove(i: Z): IS[Z, V] = {
    l""" requires 0 ≤ i
                  i < s.size
         ensures  result.size ≡ s.size - 1
                  ∀j: [0, i)  result(j) ≡ s(j)
                  ∀j: [i, result.size)  result(j) ≡ s(j + 1)
     """
    return laxSlice(0, i) ++ laxSlice(i + 1, s.size)
  }

  @pure def reverse: IS[Z, V] = {
    l""" ensures  result.size ≡ s.size
                  ∀i: [0, s.size)  result(i) ≡ s(s.size - 1 - i)
     """

    return for (i <- s.indices.reverse) yield s(i)
  }


  @pure def slice(from: Z, til: Z): IS[Z, V] = {
    l""" requires 0 ≤ from
                  from < s.size
                  0 ≤ til
                  til ≤ s.size
                  from ≤ til
         ensures  result.size ≡ til - from
                  ∀i: [0, result.size) result(i) ≡ s(from + i) """

    return laxSlice(from, til)
  }

  @pure def sortWith(lt: (V, V) => B): IS[Z, V] = {
    return ISZOpsUtil.sortWith(s, lt)
  }

  @pure def tail: IS[Z, V] = {
    l""" requires s.size > 0
         ensures  result.size ≡ s.size - 1
                  ∀i: [0, result.size)  result(i) ≡ s(i + 1) """
    return drop(1)
  }

  @pure def take(size: Z): IS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

    return laxSlice(0, size)
  }

  @pure def takeRight(size: Z): IS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(s.size - size + i) """

    return laxSlice(s.size - size, s.size)
  }
}


@ext object MSZOpsUtil {
  @pure def parMapFoldLeft[V, U, R](s: MS[Z, V], f: V => U, g: (R, U) => R, init: R): R =
    l""" ensures MSZOps(s.map(f)).foldLeft(g, init) """

  def mParMapFoldLeft[V, U, R](s: MS[Z, V], f: V => U, g: (R, U) => R, init: R): R = $

  @pure def parMapFoldRight[V, U, R](s: MS[Z, V], f: V => U, g: (R, U) => R, init: R): R =
    l""" ensures MSZOps(s.map(f)).foldLeft(g, init) """

  def mParMapFoldRight[V, U, R](s: MS[Z, V], f: V => U, g: (R, U) => R, init: R): R = $

  @pure def sortWith[V](s: MS[Z, V], lt: (V, V) => B): MS[Z, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size - 1) lt(i, i + 1)
                 SOps.isPermutation(s, result)         """
}

@datatype class MSZOps[V](s: MS[Z, V]) extends SOps[Z, V] with MSOps[Z, V] {

  @pure def :+(e: V): MS[Z, V] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [0, result.size)  result(i) ≡ s(i)
                 result(result.size - 1) ≡ e            """

    return s :+ e
  }

  @pure def +:(e: V): MS[Z, V] = {
    l""" ensures result.size ≡ s.size + 1
                 ∀i: [1, result.size)  result(i) ≡ s(i - 1)
                 result(0) ≡ e                              """

    return e +: s
  }

  @pure def ++(other: MS[Z, V]): MS[Z, V] = {
    l""" ensures result.size ≡ s.size + other.size
                 ∀i: [0, s.size)  result(i) ≡ s(i)
                 ∀i: [0, other.size)  result(s.size + i) ≡ other(i) """

    return s ++ other
  }

  @pure def chunk(size: Z): MS[Z, MS[Z, V]] = {
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

    var r = MS[Z, MS[Z, V]]()
    var chunk = MS[Z, V]()
    for (e <- s) {
      if (chunk.size == size) {
        r = r :+ chunk
        chunk = MS[Z, V]()
      }
      chunk = chunk :+ e
    }
    if (chunk.nonEmpty) {
      r = r :+ chunk
    }
    return r
  }

  @pure def contains(e: V): B = {
    l""" ensures result ≡ (∃i: [0, s.size) s(i) ≡ e) """

    return exists((x: V) => x == e)
  }

  @pure def drop(size: Z): MS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(size + i) """

    return laxSlice(size, s.size)
  }

  @pure def dropRight(size: Z): MS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ s.size - size
                  ∀i: [0, s.size - size)  result(i) ≡ s(i) """

    return laxSlice(0, s.size - size)
  }

  @pure def exists(p: V => B): B = {
    l""" ensures result ≡ (∃i: [0, s.size) p(i)) """

    for (e <- s) {
      if (p(e)) {
        return T
      }
    }
    return F
  }

  @pure def first: V = {
    l""" requires s.size > 0
         ensures  result ≡ s(0) """

    return s(0)
  }

  @pure def forall(p: V => B): B = {
    l""" ensures result ≡ (∀i: [0, s.size) p(i)) """

    for (e <- s) {
      if (!p(e)) {
        return F
      }
    }
    return T
  }

  @pure def foldLeft[R](f: (R, V) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldLeftSpec(s, f, init, s.size - 1) """

    var r = init
    for (e <- s) {
      r = f(r, e)
    }
    return r
  }

  @pure def foldRight[R](f: (R, V) => R, init: R): R = {
    l""" ensures result ≡ ISOps.foldRightSpec(s, f, init, s.size - 1) """

    var r = init
    for (i <- s.indices.reverse) {
      r = f(r, s(i))
    }
    return r
  }

  @pure def insert(i: Z, e: V): MS[Z, V] = {
    l""" requires 0 ≤ i
                  i <= s.size
         ensures  result.size ≡ s.size + 1
                  ∀j: [0, i) result(j) ≡ s(j)
                  result(i) ≡ e
                  ∀j: [j, s.size) result(j + 1) ≡ s(j) """
    return (laxSlice(0, i) :+ e) ++ laxSlice(i, s.size)
  }

  @pure def last: V = {
    l""" requires s.size > 0
         ensures  result ≡ s(s.size - 1) """

    return s(s.size - 1)
  }

  @pure def indexOf(e: V): Z = {
    l""" ensures (0 ≤ result ∧ result < s.size) → s(result) ≡ e
                 (result ≡ s.size) ≡ (∀i: [0, s.size) s(i) ≠ e)
                 ∃i: [0, s.size) s(i) ≡ e ∧ (∀j: [0, i) s(j) ≠ e) → result ≡ i
                 0 ≤ result
                 result ≤ s.size                                """
    for (i <- z"0" until s.size if e == s(i)) {
      return i
    }
    return s.size
  }

  @pure def laxSlice(from: Z, til: Z): MS[Z, V] = {
    l""" ensures if (til > from) result.size ≡ NO(til).min(s.size) - NO(0).max(from)
                   else result.size ≡ 0
                 ∀i: [i, result.size) result(i) ≡ s(NO(0).max(from) + i)               """

    var r = MS[Z, V]()
    for (i <- from until til if 0 <= i && i < s.size) {
      r = r :+ s(i)
    }
    return r
  }

  @pure def map[U](f: V => U): MS[Z, U] = {
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ f(s(i)) """

    return s.map(f)
  }

  @pure def parMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = MSZOpsUtil.parMapFoldLeft(s, f, g, init)
    return r
  }

  def mParMapFoldLeft[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = MSZOpsUtil.mParMapFoldLeft(s, f, g, init)
    return r
  }

  @pure def parMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = MSZOpsUtil.parMapFoldRight(s, f, g, init)
    return r
  }

  def mParMapFoldRight[U, R](f: V => U, g: (R, U) => R, init: R): R = {
    val r = MSZOpsUtil.mParMapFoldRight(s, f, g, init)
    return r
  }

  @pure def remove(i: Z): MS[Z, V] = {
    l""" requires 0 ≤ i
                  i < s.size
         ensures  result.size ≡ s.size - 1
                  ∀j: [0, i)  result(j) ≡ s(j)
                  ∀j: [i, result.size)  result(j) ≡ s(j + 1)
     """
    return laxSlice(0, i) ++ laxSlice(i + 1, s.size)
  }

  @pure def reverse: MS[Z, V] = {
    l""" ensures  result.size ≡ s.size
                  ∀i: [0, s.size)  result(i) ≡ s(s.size - 1 - i)
     """

    val r = s
    var i = 0
    var j = s.size - 1
    val half = s.size / 2
    while (i < half) {
      val t = r(i)
      r(i) = r(j)
      r(j) = t
      i = i + 1
      j = j - 1
    }
    return r
  }


  @pure def slice(from: Z, til: Z): MS[Z, V] = {
    l""" requires 0 ≤ from
                  from < s.size
                  0 ≤ til
                  til ≤ s.size
                  from ≤ til
         ensures  result.size ≡ til - from
                  ∀i: [0, result.size) result(i) ≡ s(from + i) """

    return laxSlice(from, til)
  }

  @pure def sortWith(lt: (V, V) => B): MS[Z, V] = {
    return MSZOpsUtil.sortWith(s, lt)
  }

  @pure def tail: MS[Z, V] = {
    l""" requires s.size > 0
         ensures  result.size ≡ s.size - 1
                  ∀i: [0, result.size)  result(i) ≡ s(i + 1) """
    return drop(1)
  }

  @pure def take(size: Z): MS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(i) """

    return laxSlice(0, size)
  }

  @pure def takeRight(size: Z): MS[Z, V] = {
    l""" requires 0 ≤ size
                  size ≤ s.size
         ensures  result.size ≡ size
                  ∀i: [0, result.size)  result(i) ≡ s(s.size - size + i) """

    return laxSlice(s.size - size, s.size)
  }
}

@sig trait SBOps[I] {

  @pure def toU8: U8

  @pure def toU16: U16

  @pure def toU32: U32

  @pure def toU64: U64
}

import org.sireum.U8._
import org.sireum.U16._
import org.sireum.U32._
import org.sireum.U64._

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

  @pure def fromU16(n: U16): IS[Z, B] = {
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

@datatype class ISZBOps(s: IS[Z, B]) extends SBOps[Z] {

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
