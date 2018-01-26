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
  @pure def empty[K, V]: Map[K, V] = {
    return Map[K, V](ISZ[(K, V)]())
  }
}

@datatype class Map[K, V](entries: ISZ[(K, V)]) {

  @pure def keys: ISZ[K] = {
    var r = ISZ[K]()
    for (kv <- entries) {
      r = r :+ kv._1
    }
    return r
  }

  @pure def values: ISZ[V] = {
    var r = ISZ[V]()
    for (kv <- entries) {
      r = r :+ kv._2
    }
    return r
  }

  @pure def keySet: Set[K] = {
    return Set.empty[K].addAll(keys)
  }

  @pure def valueSet: Set[V] = {
    return Set.empty[V].addAll(values)
  }

  @pure def put(key: K, value: V): Map[K, V] = {
    val index = indexOf(key)
    val newEntries: ISZ[(K, V)] =
      if (index < 0) entries :+ ((key, value))
      else entries((index, (key, value)))
    return Map(newEntries)
  }

  @pure def get(key: K): Option[V] = {
    val index = indexOf(key)
    return if (index < 0) None[V]() else Some(entries(index)._2)
  }

  @pure def entry(key: K): Option[(K, V)] = {
    val index = indexOf(key)
    return if (index < 0) None[(K, V)]() else Some(entries(index))
  }

  @pure def indexOf(key: K): Z = {
    var index = z"-1"
    for (i <- entries.indices if index == z"-1") {
      if (entries(i)._1 == key) {
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
      return Map(entries -- deletedMappings)
    } else {
      return this
    }
  }

  @pure def remove(key: K, value: V): Map[K, V] = {
    return Map(entries - ((key, value)))
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

  @pure def toHashMap: HashMap[K, V] = {
    var r = HashMap.emptyInit[K, V](size)
    for (kv <- entries) {
      r = r.put(kv._1, kv._2)
    }
    return r
  }

  @pure def toHashSMap: HashSMap[K, V] = {
    var r = HashSMap.emptyInit[K, V](size)
    for (kv <- entries) {
      r = r.put(kv._1, kv._2)
    }
    return r
  }

  @pure def string: String = {
    val r = st"""{
                |  ${(for (e <- entries) yield st"${e._1} -> ${e._2}", ",\n")}
                |}"""
    return r.render
  }

  @pure override def hash: Z = {
    return entries.size
  }

  @pure def isEqual(other: Map[K, V]): B = {
    if (size != other.size) {
      return F
    }

    var seen = Set.empty[K]
    for (kv <- entries) {
      val k = kv._1
      seen = seen.add(k)
      other.get(k) match {
        case Some(v) =>
          if (v != kv._2) {
            return F
          }
        case _ => return F
      }
    }
    for (kv <- other.entries) {
      val k = kv._1
      if (!seen.contains(k)) {
        get(k) match {
          case Some(v) =>
            if (v != kv._2) {
              return F
            }
          case _ => return F
        }
      }
    }

    return T
  }
}
