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

object Map {
  @sig trait Eq[K, V] {
    @pure def keyEqual(k1: K, k2: K): B
    @pure def valueEqual(v1: V, v2: V): B
  }

  @datatype class DefaultEq[K, V] extends Eq[K, V] {
    @pure def keyEqual(k1: K, k2: K): B = {
      return k1 == k2
    }

    @pure def valueEqual(v1: V, v2: V): B = {
      return v1 == v2
    }
  }

  def empty[K, V]: Map[K, V] = {
    return emptyEq[K, V](DefaultEq())
  }

  def emptyEq[K, V](eq: Eq[K, V]): Map[K, V] = {
    return Map[K, V](ISZ[(K, V)](), eq)
  }
}

@datatype class Map[K, V](entries: ISZ[(K, V)],
                          @hidden eq: Map.Eq[K, V]) {

  @pure def put(key: K, value: V): Map[K, V] = {
    val index = indexOf(key)
    val newEntries: ISZ[(K, V)] =
      if (index < 0) entries :+ ((key, value))
      else entries((index, (key, value)))
    return Map(newEntries, eq)
  }

  @pure def get(key: K): Option[V] = {
    val index = indexOf(key)
    return if (index < 0) None[V]() else Some(entries(index)._2)
  }

  @pure def indexOf(key: K): Z = {
    var index = z"-1"
    for (i <- entries.indices if index == -1) {
      if (eq.keyEqual(entries(i)._1, key)) {
        index = i
      }
    }
    return index
  }

  @pure def removeAll(keys: ISZ[K]): Map[K, V] = {
    var deletedMappings = ISZ[(K, V)]()
    for (key <- keys) {
      get(key) match {
        case Some(value) => deletedMappings = deletedMappings :+ ((key, value))
        case _ =>
      }
    }
    if (deletedMappings.nonEmpty) {
      return Map(entries -- deletedMappings, eq)
    } else {
      return this
    }
  }

  @pure def remove(key: K, value: V): Map[K, V] = {
    return Map(entries - ((key, value)), eq)
  }

  @pure def isEqual(other: Map[K, V]): B = {
    if (size != other.size) {
      return F
    }
    val eq2 = other.eq
    for (otherKV <- other.entries) {
      val otherK = otherKV._1
      val otherV = otherKV._2
      val i = indexOf(otherK)
      if (i < 0) {
        return F
      }
      val kv = entries(i)
      val k = kv._1
      val v = kv._2
      if (!eq2.keyEqual(k, otherK) | !eq.valueEqual(v, otherV) | !eq2.valueEqual(v, otherV)) {
        return F
      }
    }
    return T
  }

  @pure def contains(key: K): B = {
    return indexOf(key) >= 0
  }

  @pure def isEmpty: B = {
    return size == z"0"
  }

  @pure def nonEmpty: B = {
    return size != z"0"
  }

  @pure def size: Z = {
    return entries.size
  }
}
