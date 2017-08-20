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

package org.sireumproto

object Set {
  @pure def empty[T]: Set[T] = {
    return Set(Map.empty[T, B])
  }
}

@datatype class Set[T](map: Map[T, B]) {

  @pure def add(e: T): Set[T] = {
    return Set(map.put(e, T))
  }

  @pure def addAll(is: ISZ[T]): Set[T] = {
    var r = this
    for (e <- is) {
      r = r.add(e)
    }
    return r
  }

  @pure def remove(e: T): Set[T] = {
    return Set(map.remove(e, T))
  }

  @pure def removeAll(is: ISZ[T]): Set[T] = {
    var r = this
    for (e <- is) {
      r = r.remove(e)
    }
    return r
  }

  @pure def contains(e: T): B = {
    return map.contains(e)
  }

  @pure def union(other: Set[T]): Set[T] = {
    var r = this
    for (p <- other.map.entries) {
      r = r.add(p._1)
    }
    return r
  }

  @pure def intersect(other: Set[T]): Set[T] = {
    var r = Set.empty[T]
    for (p <- other.map.entries) {
      val e = p._1
      if (contains(e)) {
        r = r.add(e)
      }
    }
    return r
  }

  @pure def substract(other: Set[T]): Set[T] = {
    var r = this
    for (p <- other.map.entries) {
      r = r.remove(p._1)
    }
    return r
  }

  @pure def isEqual(other: Set[T]): B = {
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
