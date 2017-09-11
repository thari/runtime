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
    return emptyInit[K, V](16)
  }

  @pure def emptyInit[K, V](initialCapacity: Z): HashMap[K, V] = {
    val sz: Z = if (initialCapacity <= 0) 4 else initialCapacity * 4 / 3 + 1
    return HashMap[K, V](ISZ.create(sz, Map.empty), 0)
  }

}

@datatype class HashMap[K, V](mapEntries: ISZ[Map[K, V]], size: Z) {
  @pure def hash: Z = {
    return size
  }

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
    return Set.empty[K].addAll(keys)
  }

  @pure def valueSet: Set[V] = {
    return Set.empty[V].addAll(values)
  }

  @pure def put(key: K, value: V): HashMap[K, V] = {
    val r = ensureCapacity(size + 1)
    val i = hashIndex(key)
    val m = mapEntries(i)
    val newSize: Z = if (m.contains(key)) size else size + 1
    return r(mapEntries = mapEntries(i -> mapEntries(i).put(key, value)), size = newSize)
  }

  @pure def putAll(entries: ISZ[(K, V)]): HashMap[K, V] = {
    if (entries.isEmpty) {
      return this
    }
    var r = ensureCapacity(size + entries.size)
    for (kv <- entries) {
      r = r.put(kv._1, kv._2)
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
        r = r.put(kv._1, kv._2)
      }
    }
    return r
  }

  @pure def hashIndex(key: K): Z = {
    val sz = mapEntries.size
    val i = key.hashCode % sz
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

  @pure def removeAll(keys: ISZ[K]): HashMap[K, V] = {
    var r = this
    for (k <- keys) {
      r.get(k) match {
        case Some(v) => r = r.remove(k, v)
        case _ =>
      }
    }
    return r
  }

  @pure def remove(key: K, value: V): HashMap[K, V] = {
    val i = hashIndex(key)
    return this(mapEntries = mapEntries(i -> mapEntries(i).remove(key, value)), size = size - 1)
  }

  @pure def isEqual(other: HashMap[K, V]): B = {
    if (size != other.size) {
      return F
    }
    var seen = HashSet.emptyInit[K]((size + 1) * 3 / 4)
    for (ms <- mapEntries) {
      for (kv <- ms.entries) {
        val k = kv._1
        seen = seen.add(k)
        other.get(k) match {
          case Some(v) =>
            if (kv._2 != v) {
              return F
            }
          case _ => return F
        }
      }
    }
    for (ms <- other.mapEntries) {
      for (kv <- ms.entries) {
        val k = kv._1
        if (!seen.contains(k)) {
          get(k) match {
            case Some(v) =>
              if (kv._2 != v) {
                return F
              }
            case _ => return F
          }
        }
      }
    }

    return T
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
}
