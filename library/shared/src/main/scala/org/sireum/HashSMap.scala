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

object HashSMap {

  @pure def empty[K, V]: HashSMap[K, V] = {
    return HashSMap(HashMap.empty, Set.empty)
  }

  @pure def emptyInit[K, V](initialCapacity: Z): HashSMap[K, V] = {
    return HashSMap(HashMap.emptyInit(initialCapacity), Set.empty)
  }

  @pure def ++[I, K, V](s: IS[I, (K, V)]): HashSMap[K, V] = {
    return HashSMap.emptyInit[K, V](s.zize) ++ s
  }

}

@datatype class HashSMap[K, V](map: HashMap[K, V], keys: Set[K]) {

  @pure def size: Z = {
    return keys.size
  }

  @pure def entries: ISZ[(K, V)] = {
    var r = ISZ[(K, V)]()
    for (k <- keys.elements) {
      map.get(k) match {
        case Some(v) => r = r :+ ((k, v))
        case _ =>
      }
    }
    return r
  }

  @pure def values: ISZ[V] = {
    return map.values
  }

  @pure def keySet: Set[K] = {
    return keys
  }

  @pure def valueSet: Set[V] = {
    return Set.empty[V] ++ values
  }

  @pure def +(p: (K, V)): HashSMap[K, V] = {
    val newMap = map + p
    return HashSMap(newMap, keys + p._1)
  }

  @pure def ++[I](entries: IS[I, (K, V)]): HashSMap[K, V] = {
    if (entries.isEmpty) {
      return this
    }
    val newMap = map ++ entries
    var newKeys = keys
    for (kv <- entries) {
      newKeys = newKeys + kv._1
    }
    return HashSMap(newMap, newKeys)
  }

  @pure def get(key: K): Option[V] = {
    return map.get(key)
  }

  @pure def entry(key: K): Option[(K, V)] = {
    return map.entry(key)
  }

  @pure def --(keys: ISZ[K]): HashSMap[K, V] = {
    return HashSMap(map -- keys, this.keys -- keys)
  }

  @pure def -(p: (K, V)): HashSMap[K, V] = {
    return HashSMap(map - p, keys - p._1)
  }

  @pure def contains(key: K): B = {
    return map.contains(key)
  }

  @pure def isEmpty: B = {
    return size == z"0"
  }

  @pure def nonEmpty: B = {
    return size != z"0"
  }

  @pure override def string: String = {
    val r =
      st"""{
      |  ${(for (e <- entries) yield st"${e._1} -> ${e._2}", ",\n")}
      |}"""
    return r.render
  }

  @pure override def hash: Z = {
    return map.hash
  }

  @pure def isEqual(other: HashSMap[K, V]): B = {
    return map.isEqual(other.map)
  }

}
