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

package org.sireum

object HashSSet {

  @pure def empty[T]: HashSSet[T] = {
    return HashSSet(HashSMap.empty)
  }

  @pure def emptyInit[T](initialCapacity: Z): HashSSet[T] = {
    return HashSSet(HashSMap.emptyInit(initialCapacity))
  }

  @pure def ++[I, T](s: IS[I, T]): HashSSet[T] = {
    return HashSSet.emptyInit[T](s.zize) ++ s
  }
}

@datatype class HashSSet[T](map: HashSMap[T, B]) {

  @pure def +(e: T): HashSSet[T] = {
    return HashSSet(map + e ~> T)
  }

  @pure def ++[I](is: IS[I, T]): HashSSet[T] = {
    var r = this
    for (e <- is) {
      r = r + e
    }
    return r
  }

  @pure def -(e: T): HashSSet[T] = {
    return HashSSet(map - e ~> T)
  }

  @pure def --[I](is: IS[I, T]): HashSSet[T] = {
    var r = this
    for (e <- is) {
      r = r - e
    }
    return r
  }

  @pure def contains(e: T): B = {
    return map.contains(e)
  }

  @pure def union(other: HashSSet[T]): HashSSet[T] = {
    return this ∪ other
  }

  @pure def ∪(other: HashSSet[T]): HashSSet[T] = {
    return this ++ other.elements
  }

  @pure def intersect(other: HashSSet[T]): HashSSet[T] = {
    return this ∩ other
  }

  @pure def ∩(other: HashSSet[T]): HashSSet[T] = {
    var r = HashSSet.emptyInit[T](size)
    for (e <- other.map.keys.elements) {
      if (contains(e)) {
        r = r + e
      }
    }
    return r
  }

  @pure def \(other: HashSSet[T]): HashSSet[T] = {
    return this -- other.elements
  }

  @pure def isEqual(other: HashSSet[T]): B = {
    return map.isEqual(other.map)
  }

  @pure override def hash: Z = {
    return map.hash
  }

  @pure def isEmpty: B = {
    return size == z"0"
  }

  @pure def nonEmpty: B = {
    return size != z"0"
  }

  @pure def size: Z = {
    return map.size
  }

  @pure def elements: ISZ[T] = {
    return map.keys.elements
  }

  @pure def string: String = {
    val r =
      st"""{
      |  ${(elements, ",\n")}
      |}"""
    return r.render
  }
}
