// #Sireum
package org.sireum
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

object UnionFind {

  type Index = Z

  object Internal {

    @pure def find[T](ds: UnionFind[T], e: Index): Index = {
      var root = e
      while (ds.parentOf(root) != root) {
        root = ds.parentOf(root)
      }
      return root
    }

    @pure def findCompress[T](ds: UnionFind[T], e: Index): (UnionFind[T], Index) = {
      var root = e
      var newParentOf = ds.parentOf
      while (newParentOf(root) != root) {
        newParentOf = newParentOf(root ~> newParentOf(newParentOf(root)))
        root = newParentOf(root)
      }
      return (ds(parentOf = newParentOf), root)
    }

    @pure def merge[T](ds: UnionFind[T], e1: Index, e2: Index): UnionFind[T] = {
      var newDs = ds
      val rootN: Index = {
        val pe1 = findCompress(newDs, e1)
        newDs = pe1._1
        pe1._2
      }
      val rootM: Index = {
        val pe2 = findCompress(newDs, e2)
        newDs = pe2._1
        pe2._2
      }
      val (rep, other): (Index, Index) =
        if (newDs.sizeOf(rootM) > newDs.sizeOf(rootN)) (rootM, rootN) else (rootN, rootM)
      return newDs(
        parentOf = newDs.parentOf(other ~> rep),
        sizeOf = newDs.sizeOf(rep ~> (newDs.sizeOf(rep) + newDs.sizeOf(other)))
      )
    }
  }

  @pure def create[T](elements: ISZ[T]): UnionFind[T] = {
    val size = elements.size
    var es = HashMap.emptyInit[T, Index](size)
    for (e <- elements) {
      es = es + e ~> es.size
    }
    val parentOf: IS[Index, Index] = for (i <- z"0" until size) yield i
    val sizeOf = IS.create[Index, Index](size, 1)
    return UnionFind(es, elements, parentOf, sizeOf)
  }
}

@datatype class UnionFind[T](
  elements: HashMap[T, UnionFind.Index],
  elementsInverse: IS[UnionFind.Index, T],
  parentOf: IS[UnionFind.Index, UnionFind.Index],
  sizeOf: IS[UnionFind.Index, UnionFind.Index]
) {

  @pure def size: Z = {
    return elements.size
  }

  @pure override def hash: Z = {
    return size
  }

  @pure def isEqual(other: UnionFind[T]): B = {
    if (elementsInverse.size != other.elementsInverse.size) {
      return F
    }
    if ((HashSet ++ elementsInverse) != (HashSet ++ other.elementsInverse)) {
      return F
    }
    var seen = HashSet.emptyInit[(T, T)](size)
    for (element1 <- elementsInverse; element2 <- elementsInverse if element1 != element2) {
      val p = (element1, element2)
      if (!seen.contains(p)) {
        seen = seen + p + ((element2, element1))
        if (inSameSet(element1, element2) != inSameSet(element1, element2)) {
          return F
        }
      }
    }
    return T
  }

  @pure def inSameSet(element1: T, element2: T): B = {
    return UnionFind.Internal.find(this, elements.get(element1).get) == UnionFind.Internal
      .find(this, elements.get(element2).get)
  }

  @pure def inSameSetCompress(element1: T, element2: T): (UnionFind[T], B) = {
    val e1 = elements.get(element1).get
    val e2 = elements.get(element2).get
    var newDs = this
    val rep1: UnionFind.Index = {
      val p1 = UnionFind.Internal.findCompress(newDs, e1)
      newDs = p1._1
      p1._2
    }
    val rep2: UnionFind.Index = {
      val p2 = UnionFind.Internal.findCompress(newDs, e2)
      newDs = p2._1
      p2._2
    }
    return (newDs, rep1 == rep2)
  }

  @pure def find(element: T): T = {
    val n = elements.get(element).get
    val rep = UnionFind.Internal.find(this, n)
    return elementsInverse(rep)
  }

  @pure def findCompress(element: T): (UnionFind[T], T) = {
    val n = elements.get(element).get
    val (newDs, rep) = UnionFind.Internal.findCompress(this, n)
    return (newDs, elementsInverse(rep))
  }

  @pure def merge(element1: T, element2: T): UnionFind[T] = {
    val e1 = elements.get(element1).get
    val e2 = elements.get(element2).get
    return UnionFind.Internal.merge(this, e1, e2)
  }

  @pure def toST(f: T => ST): ST = {
    var map = HashMap.emptyInit[UnionFind.Index, ISZ[ST]](size)
    for (element <- elementsInverse) {
      val rep = UnionFind.Internal.find(this, elements.get(element).get)
      map = map + rep ~> (map.get(rep).getOrElse(ISZ[ST]()) :+ f(element))
    }
    val sets: ISZ[ST] = for (sts <- map.values) yield st"""{
    |  ${(sts, ",\n")}
    |}"""
    val r =
      st"""{
      |  ${(sets, ",\n")}
      |}"""
    return r
  }

  @pure override def string: String = {
    return toST(e => st"$e").render
  }

}
