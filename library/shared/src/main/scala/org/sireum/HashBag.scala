// #Sireum
/*
 Copyright (c) 2018, Robby, Kansas State University
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

object HashBag {

  @pure def empty[T]: HashBag[T] = {
    return HashBag(HashMap.empty)
  }

  @pure def emptyInit[T](initialCapacity: Z): HashBag[T] = {
    return HashBag(HashMap.emptyInit(initialCapacity))
  }

}

@datatype class HashBag[T](val map: HashMap[T, Z]) {

  @pure def size: Z = {
    var r = z"0"
    for (n <- map.values) {
      r = r + n
    }
    return r
  }

  @pure def elements: ISZ[T] = {
    var r = ISZ[T]()
    for (entry <- entries) {
      val (e, size) = entry
      r = r ++ (for (_ <- z"0" until size) yield e)
    }
    return r
  }

  @pure def isEmpty: B = {
    return size == 0
  }

  @pure def nonEmpty: B = {
    return !isEmpty
  }

  @pure def count(e: T): Z = {
    map.get(e) match {
      case Some(n) => return n
      case _ => return 0
    }
  }

  @pure def contains(e: T): B = {
    return count(e) > 0
  }

  @pure def +(e: T): HashBag[T] = {
    return addN(e, 1)
  }

  @pure def +#(p: (T, Z)): HashBag[T] = {
    return addN(p._1, p._2)
  }

  @pure def addN(e: T, n: Z): HashBag[T] = {
    if (n <= 0) {
      return this
    }
    return this(map + e ~> (count(e) + n))
  }

  @pure def ++[I](es: IS[I, T]): HashBag[T] = {
    var r = this
    for (e <- es) {
      r = r + e
    }
    return r
  }

  @pure def -(e: T): HashBag[T] = {
    return removeN(e, 1)
  }

  @pure def --[I](s: IS[I, T]): HashBag[T] = {
    var r = this
    for (e <- s) {
      r = r - e
    }
    return r
  }

  @pure def -#(p: (T, Z)): HashBag[T] = {
    return removeN(p._1, p._2)
  }

  @pure def removeN(e: T, n: Z): HashBag[T] = {
    val current = count(e)
    val newN = current - n
    if (newN <= 0) {
      return this(map - e ~> current)
    } else {
      return this(map + e ~> newN)
    }
  }

  @pure def \(other: HashBag[T]): HashBag[T] = {
    return this -- other.elements
  }

  @pure def entries: ISZ[(T, Z)] = {
    return map.entries
  }

  @pure def union(other: HashBag[T]): HashBag[T] = {
    return this ∪ other
  }

  @pure def ∪(other: HashBag[T]): HashBag[T] = {
    return this ++ other.elements
  }

  @pure def intersect(other: HashBag[T]): HashBag[T] = {
    return this ∩ other
  }

  @pure def ∩(other: HashBag[T]): HashBag[T] = {
    var r = HashBag.empty[T]
    for (e <- entries) {
      val n = e._2
      val m = other.count(e._1)
      if (n < m) {
        r = r.addN(e._1, n)
      } else {
        r = r.addN(e._1, m)
      }
    }
    return r
  }

  @pure override def string: String = {
    return map.string
  }
}
