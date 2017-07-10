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

object Lattice {

  @pure def empty[T]: Lattice[T] = {
    return emptyEq(Set.DefaultEq[T]())
  }

  @pure def emptyEq[T](eq: Set.Eq[T]): Lattice[T] = {
    return Lattice[T](Map.emptyEq(DefaultEq[T](eq)), Map.emptyEq(DefaultEq[T](eq)), eq)
  }

  @datatype class DefaultEq[T](eq: Set.Eq[T]) extends Map.Eq[T, Set[T]] {
    @pure def keyEqual(k1: T, k2: T): B = {
      return eq.elementEqual(k1, k2)
    }

    @pure def valueEqual(v1: Set[T], v2: Set[T]): B = {
      return v1.isEqual(v2)
    }
  }
}

@datatype class Lattice[T](parents: Map[T, Set[T]],
                           children: Map[T, Set[T]],
                           @hidden eq: Set.Eq[T]) {
  val emptySet: Set[T] = Set.emptyEq(eq)

  @pure def isEqual(other: Lattice[T]): B = {
    if (!parents.isEqual(other.parents)) {
      return F
    }
    if (!children.isEqual(other.children)) {
      return F
    }
    return T
  }

  @pure def addNode(n: T): Lattice[T] = {
    parents.get(n) match {
      case Some(_) => return this
      case _ =>
        return Lattice(parents.put(n, emptySet), children.put(n, emptySet), eq)
    }
  }

  @pure def addParents(n: T, ns: ISZ[T]): Lattice[T] = {
    val newParents: Map[T, Set[T]] = parents.get(n) match {
      case Some(s) => parents.put(n, s.addAll(ns))
      case _ => parents.put(n, emptySet.addAll(ns))
    }
    var newChildren: Map[T, Set[T]] = children
    for (c <- ns) {
      newChildren = newChildren.get(c) match {
        case Some(s) => newChildren.put(c, s.add(n))
        case _ => newChildren.put(c, emptySet.add(n))
      }
    }
    return Lattice(newParents, newChildren, eq)
  }

  @pure def addChildren(n: T, ns: ISZ[T]): Lattice[T] = {
    val newChildren: Map[T, Set[T]] = children.get(n) match {
      case Some(s) => children.put(n, s.addAll(ns))
      case _ => children.put(n, emptySet.addAll(ns))
    }
    var newParents: Map[T, Set[T]] = parents
    for (c <- ns) {
      newParents = newParents.get(c) match {
        case Some(s) => newParents.put(c, s.add(n))
        case _ => newParents.put(c, emptySet.add(n))
      }
    }
    return Lattice(newParents, newChildren, eq)
  }

  @pure def childrenOf(n: T): Set[T] = {
    children.get(n) match {
      case Some(s) => return s
      case _ => return emptySet
    }
  }

  @pure def isChildOf(n: T, m: T): B = {
    return childrenOf(n).contains(m)
  }

  @pure def parentsOf(n: T): Set[T] = {
    parents.get(n) match {
      case Some(s) => return s
      case _ => return emptySet
    }
  }

  @pure def isParentOf(n: T, m: T): B = {
    return parentsOf(n).contains(m)
  }
}
