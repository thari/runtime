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

object Poset {

  @pure def empty[T]: Poset[T] = {
    return Poset[T](HashMap.empty, HashMap.empty)
  }
}

@datatype class Poset[T](parents: HashMap[T, HashSet[T]],
                         children: HashMap[T, HashSet[T]]) {
  val emptySet: HashSet[T] = HashSet.empty

  @pure def isEqual(other: Poset[T]): B = {
    if (!parents.isEqual(other.parents)) {
      return F
    }
    if (!children.isEqual(other.children)) {
      return F
    }
    return T
  }

  @pure def addNode(n: T): Poset[T] = {
    parents.get(n) match {
      case Some(_) => return this
      case _ =>
        return Poset(parents.put(n, emptySet), children.put(n, emptySet))
    }
  }

  @pure def addParents(n: T, ns: ISZ[T]): Poset[T] = {
    val newParents: HashMap[T, HashSet[T]] = parents.get(n) match {
      case Some(s) => parents.put(n, s.addAll(ns))
      case _ => parents.put(n, emptySet.addAll(ns))
    }
    var newChildren: HashMap[T, HashSet[T]] = children
    for (c <- ns) {
      newChildren = newChildren.get(c) match {
        case Some(s) => newChildren.put(c, s.add(n))
        case _ => newChildren.put(c, emptySet.add(n))
      }
    }
    return Poset(newParents, newChildren)
  }

  @pure def removeParent(n: T, parent: T): Poset[T] = {
    parents.get(n) match {
      case Some(s) =>
        return Poset(
          parents.put(n, s.remove(parent)),
          children.put(parent, children.get(parent).getOrElse(emptySet).remove(n)))
      case _ => return this
    }
  }

  @pure def removeChild(n: T, child: T): Poset[T] = {
    return removeParent(child, n)
  }

  @pure def addChildren(n: T, ns: ISZ[T]): Poset[T] = {
    val newChildren: HashMap[T, HashSet[T]] = children.get(n) match {
      case Some(s) => children.put(n, s.addAll(ns))
      case _ => children.put(n, emptySet.addAll(ns))
    }
    var newParents: HashMap[T, HashSet[T]] = parents
    for (c <- ns) {
      newParents = newParents.get(c) match {
        case Some(s) => newParents.put(c, s.add(n))
        case _ => newParents.put(c, emptySet.add(n))
      }
    }
    return Poset(newParents, newChildren)
  }

  @pure def childrenOf(n: T): HashSet[T] = {
    children.get(n) match {
      case Some(s) => return s
      case _ => return emptySet
    }
  }

  @pure def isChildOf(n: T, m: T): B = {
    return childrenOf(n).contains(m)
  }

  @pure def parentsOf(n: T): HashSet[T] = {
    parents.get(n) match {
      case Some(s) => return s
      case _ => return emptySet
    }
  }

  @pure def isParentOf(n: T, m: T): B = {
    return parentsOf(n).contains(m)
  }

  @pure def ancestorsOf(n: T): HashSet[T] = {
    return ancestorsCache(n, HashMap.empty)._1
  }

  @pure def ancestorsCache(n: T, acc: HashMap[T, HashSet[T]]): (HashSet[T], HashMap[T, HashSet[T]]) = {
    var mAcc = acc
    var r = emptySet
    for (nParent <- parentsOf(n).elements) {
      mAcc = ancestorsRec(nParent, mAcc)
      r = r.add(nParent).union(mAcc.get(nParent).getOrElse(emptySet))
    }
    return (r, mAcc)
  }

  @pure def ancestorsRec(m: T, acc: HashMap[T, HashSet[T]]): HashMap[T, HashSet[T]] = {
    if (acc.contains(m)) {
      return acc
    }
    val p = ancestorsCache(m, acc.put(m, emptySet))
    val mAncestors = p._1
    val mAcc = p._2
    return mAcc.put(m, mAncestors)
  }

  @pure def lub(ns: ISZ[T]): Option[T] = {
    if (ns.isEmpty) {
      return None()
    }
    val p0 = ancestorsCache(ns(0), HashMap.empty)
    var commons = p0._1.add(ns(0))
    var acc = p0._2
    for (i <- 1 until ns.size) {
      val p = ancestorsCache(ns(i), acc)
      acc = p._2
      commons = commons.intersect(p._1.add(ns(i)))
    }
    if (commons.isEmpty) {
      return None()
    }
    for (b1 <- commons.elements) {
      for (b2 <- commons.elements if b1 != b2) {
        if (ancestorsCache(b1, acc)._1.contains(b2)) {
          commons = commons.remove(b2)
        }
      }
    }
    if (commons.size == 1) {
      return Some(commons.elements(0))
    } else {
      return None()
    }
  }


  @pure def descendantsOf(n: T): HashSet[T] = {
    return descendantsCache(n, HashMap.empty)._1
  }

  @pure def descendantsCache(n: T, acc: HashMap[T, HashSet[T]]): (HashSet[T], HashMap[T, HashSet[T]]) = {
    var mAcc = acc
    var r = emptySet
    for (nChild <- childrenOf(n).elements) {
      mAcc = descendantsRec(nChild, mAcc)
      r = r.add(nChild).union(mAcc.get(nChild).getOrElse(emptySet))
    }
    return (r, mAcc)
  }

  @pure def descendantsRec(m: T, acc: HashMap[T, HashSet[T]]): HashMap[T, HashSet[T]] = {
    if (acc.contains(m)) {
      return acc
    }
    val p = descendantsCache(m, acc.put(m, emptySet))
    val mDescendants = p._1
    val mAcc = p._2
    return mAcc.put(m, mDescendants)
  }

  @pure def glb(ns: ISZ[T]): Option[T] = {
    if (ns.isEmpty) {
      return None()
    }
    val p0 = descendantsCache(ns(0), HashMap.empty)
    var commons = p0._1.add(ns(0))
    var acc = p0._2
    for (i <- 1 until ns.size) {
      val p = descendantsCache(ns(i), acc)
      acc = p._2
      commons = commons.intersect(p._1.add(ns(i)))
    }
    if (commons.isEmpty) {
      return None()
    }
    for (b1 <- commons.elements) {
      for (b2 <- commons.elements if b1 != b2) {
        if (descendantsCache(b1, acc)._1.contains(b2)) {
          commons = commons.remove(b2)
        }
      }
    }
    if (commons.size == 1) {
      return Some(commons.elements(0))
    } else {
      return None()
    }
  }
}
