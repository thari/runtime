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
}

@datatype class HashSSet[T](map: HashSMap[T, B]) {

  @pure def add(e: T): HashSSet[T] = {
    return HashSSet(map.put(e, T))
  }

  @pure def addAll(is: ISZ[T]): HashSSet[T] = {
    var r = this
    for (e <- is) {
      r = r.add(e)
    }
    return r
  }

  @pure def remove(e: T): HashSSet[T] = {
    return HashSSet(map.remove(e, T))
  }

  @pure def removeAll(is: ISZ[T]): HashSSet[T] = {
    var r = this
    for (e <- is) {
      r = r.remove(e)
    }
    return r
  }

  @pure def contains(e: T): B = {
    return map.contains(e)
  }

  @pure def union(other: HashSSet[T]): HashSSet[T] = {
    var r = this
    for (e <- other.map.keys) {
      r = r.add(e)
    }
    return r
  }

  @pure def intersect(other: HashSSet[T]): HashSSet[T] = {
    var r = HashSSet.emptyInit[T](size)
    for (e <- other.map.keys) {
      if (contains(e)) {
        r = r.add(e)
      }
    }
    return r
  }

  @pure def substract(other: HashSSet[T]): HashSSet[T] = {
    var r = this
    for (e <- other.map.keys) {
      r = r.remove(e)
    }
    return r
  }

  @pure def isEqual(other: HashSSet[T]): B = {
    return map.isEqual(other.map)
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
    return map.keys
  }
}
