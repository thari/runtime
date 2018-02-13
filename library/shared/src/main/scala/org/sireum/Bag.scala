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

object Bag {

  @pure def empty[T]: Bag[T] = {
    return Bag(Map.empty)
  }

  @pure def ++[I, T](s: IS[I, T]): Bag[T] = {
    return Bag.empty[T] ++ s
  }

}

@datatype class Bag[T](val map: Map[T, Z]) {

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

  @pure def +(e: T): Bag[T] = {
    return addN(e, 1)
  }

  @pure def addN(e: T, n: Z): Bag[T] = {
    if (n <= 0) {
      return this
    }
    return this(map + e ~> (count(e) + n))
  }

  @pure def +#(p: (T, Z)): Bag[T] = {
    val (e, n) = p
    return addN(e, n)
  }

  @pure def ++[I](es: IS[I, T]): Bag[T] = {
    var r = this
    for (e <- es) {
      r = r + e
    }
    return r
  }

  @pure def -(e: T): Bag[T] = {
    return removeN(e, 1)
  }

  @pure def --[I](es: IS[I, T]): Bag[T] = {
    var r = this
    for (e <- es) {
      r = r - e
    }
    return r
  }

  @pure def \(other: Bag[T]): Bag[T] = {
    return this -- other.elements
  }

  @pure def -#(p: (T, Z)): Bag[T] = {
    val (e, n) = p
    return removeN(e, n)
  }

  @pure def removeN(e: T, n: Z): Bag[T] = {
    val current = count(e)
    val newN = current - n
    if (newN <= 0) {
      return this(map - e ~> current)
    } else {
      return this(map + e ~> newN)
    }
  }

  @pure def entries: ISZ[(T, Z)] = {
    return map.entries
  }

  @pure def union(other: Bag[T]): Bag[T] = {
    return this ∪ other
  }

  @pure def ∪(other: Bag[T]): Bag[T] = {
    var r = this
    for (e <- other.entries) {
      r = r +# e._1 ~> e._2
    }
    return r
  }

  @pure def intersect(other: Bag[T]): Bag[T] = {
    return this ∩ other
  }

  @pure def ∩(other: Bag[T]): Bag[T] = {
    var r = Bag.empty[T]
    for (e <- entries) {
      val n = e._2
      val m = other.count(e._1)
      if (n < m) {
        r = r +# e._1 ~> n
      } else {
        r = r +# e._1 ~> m
      }
    }
    return r
  }

  @pure def toHashBag: HashBag[T] = {
    return HashBag(map.toHashMap)
  }

  @pure override def string: String = {
    return map.string
  }
}
