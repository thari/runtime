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

object HashMap {

  @pure def empty[K, V]: HashMap[K, V] = {
    return emptyInit[K, V](12)
  }

  @pure def emptyInit[K, V](initialCapacity: Z): HashMap[K, V] = {
    val sz: Z = if (initialCapacity <= 0) 4 else initialCapacity * 4 / 3 + 1
    return HashMap[K, V](ISZ.create(sz, Map.empty), 0)
  }

  @pure def ++[I, K, V](s: IS[I, (K, V)]): HashMap[K, V] = {
    return HashMap.emptyInit[K, V](s.zize) ++ s
  }

}

@datatype class HashMap[K, V](mapEntries: ISZ[Map[K, V]], size: Z) {

  @pure def entries: ISZ[(K, V)] = {
    var r = ISZ[(K, V)]()
    for (ms <- mapEntries) {
      if (ms.nonEmpty) {
        r = r ++ ms.entries
      }
    }
    return r
  }

  @pure def keys: ISZ[K] = {
    var r = ISZ[K]()
    for (ms <- mapEntries) {
      if (ms.nonEmpty) {
        r = r ++ ms.keys
      }
    }
    return r
  }

  @pure def values: ISZ[V] = {
    var r = ISZ[V]()
    for (ms <- mapEntries) {
      if (ms.nonEmpty) {
        r = r ++ ms.values
      }
    }
    return r
  }

  @pure def keySet: Set[K] = {
    return Set.empty[K] ++ keys
  }

  @pure def valueSet: Set[V] = {
    return Set.empty[V] ++ values
  }

  @pure def +(p: (K, V)): HashMap[K, V] = {
    val (key, value) = p
    val r = ensureCapacity(size + 1)
    val i = r.hashIndex(key)
    val m = r.mapEntries(i)
    val newSize: Z = if (m.contains(key)) size else size + 1
    return r(mapEntries = r.mapEntries(i ~> (m + key ~> value)), size = newSize)
  }

  @pure def ++[I](entries: IS[I, (K, V)]): HashMap[K, V] = {
    if (entries.isEmpty) {
      return this
    }
    var r = ensureCapacity(size + entries.zize)
    for (kv <- entries) {
      r = r + kv._1 ~> kv._2
    }
    return r
  }

  @pure def ensureCapacity(sz: Z): HashMap[K, V] = {
    if (mapEntries.size * 3 / 4 >= sz) {
      return this
    }
    val init = sz * 2
    var r = HashMap.emptyInit[K, V](init)
    for (ms <- mapEntries) {
      for (kv <- ms.entries) {
        r = r + kv._1 ~> kv._2
      }
    }
    return r
  }

  @pure def hashIndex(key: K): Z = {
    val sz = mapEntries.size
    val i = key.hash % sz
    return if (i < 0) i + sz else i
  }

  @pure def get(key: K): Option[V] = {
    val m = mapEntries(hashIndex(key))
    return m.get(key)
  }

  @pure def entry(key: K): Option[(K, V)] = {
    val m = mapEntries(hashIndex(key))
    return m.entry(key)
  }

  @pure def --[I](keys: IS[I, K]): HashMap[K, V] = {
    var r = this
    for (k <- keys) {
      r.get(k) match {
        case Some(v) => r = r - k ~> v
        case _ =>
      }
    }
    return r
  }

  @pure def -(p: (K, V)): HashMap[K, V] = {
    val (key, value) = p
    val i = hashIndex(key)
    return this(mapEntries(i ~> (mapEntries(i) - key ~> value)), size - 1)
  }

  @pure def contains(key: K): B = {
    return get(key).nonEmpty
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
    return size
  }

  @pure def isEqual(other: HashMap[K, V]): B = {
    if (size != other.size) {
      return F
    }

    var comparedKeys = ISZ[K]()
    for (ms <- mapEntries) {
      for (kv <- ms.entries) {
        val k = kv._1
        comparedKeys = comparedKeys :+ k
        other.get(k) match {
          case Some(v) =>
            if (kv._2 != v) {
              return F
            }
          case _ => return F
        }
      }
    }
    for (ms <- (other -- comparedKeys).mapEntries) {
      for (kv <- ms.entries) {
        val k = kv._1
        get(k) match {
          case Some(v) =>
            if (kv._2 != v) {
              return F
            }
          case _ => return F
        }
      }
    }

    return T
  }
}
