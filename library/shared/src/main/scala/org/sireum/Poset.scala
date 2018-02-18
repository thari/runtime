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

  type Index = Z

  object Internal {

    val emptySet: HashSet[Poset.Index] = HashSet.empty

    @pure def addNode[T](poset: Poset[T], node: T): (Poset[T], Index) = {
      poset.nodes.get(node) match {
        case Some(n) => return (poset, n)
        case _ =>
          val n = poset.nodes.size
          return (
            poset(
              nodes = poset.nodes + node ~> n,
              nodesInverse = poset.nodesInverse :+ node,
              parents = poset.parents + n ~> emptySet,
              children = poset.children + n ~> emptySet
            ),
            n
          )
      }
    }

    @pure def addNodes[T](poset: Poset[T], nodes: ISZ[T]): (Poset[T], ISZ[Index]) = {
      var r = poset
      val s = ZS.create(nodes.size, 0)
      var i = 0
      for (nd <- nodes) {
        val p = addNode(r, nd)
        r = p._1
        s(i) = p._2
        i = i + 1
      }
      return (r, s.toIS)
    }

    @pure def addParents[T](poset: Poset[T], n: Index, ns: ISZ[Index]): Poset[T] = {
      var changed = F
      val newParents: HashMap[Index, HashSet[Index]] = {
        val s = poset.parents.get(n).get
        val newS = s ++ ns
        if (newS.size != s.size) {
          changed = T
          poset.parents + n ~> newS
        } else {
          poset.parents
        }
      }
      var newChildren: HashMap[Index, HashSet[Index]] = poset.children
      for (c <- ns) {
        newChildren = {
          val s = newChildren.get(c).get
          val newS = s + n
          if (newS.size != s.size) {
            changed = T
            newChildren + c ~> newS
          } else {
            newChildren
          }
        }
      }
      return if (changed) poset(parents = newParents, children = newChildren) else poset
    }

    @pure def removeParent[T](poset: Poset[T], n: Index, parent: Index): Poset[T] = {
      poset.parents.get(n) match {
        case Some(s) =>
          return poset(
            parents = poset.parents + n ~> (s - parent),
            children = poset.children + parent ~> (poset.children.get(parent).get - n)
          )
        case _ => return poset
      }
    }

    @pure def addChildren[T](poset: Poset[T], n: Index, ns: ISZ[Index]): Poset[T] = {
      var changed = F
      val newChildren: HashMap[Index, HashSet[Index]] = {
        val s = poset.children.get(n).get
        val newS = s ++ ns
        if (newS.size != s.size) {
          changed = T
          poset.children + n ~> newS
        } else {
          poset.children
        }
      }
      var newParents: HashMap[Index, HashSet[Index]] = poset.parents
      for (c <- ns) {
        newParents = {
          val s = newParents.get(c).get
          val newS = s + n
          if (newS.size != s.size) {
            changed = T
            newParents + c ~> newS
          } else {
            newParents
          }
        }
      }
      return if (changed) poset(parents = newParents, children = newChildren) else poset
    }

    @pure def childrenOf[T](poset: Poset[T], n: Index): HashSet[Index] = {
      poset.children.get(n) match {
        case Some(s) => return s
        case _ => return emptySet
      }
    }

    @pure def parentsOf[T](poset: Poset[T], n: Index): HashSet[Index] = {
      poset.parents.get(n) match {
        case Some(s) => return s
        case _ => return emptySet
      }
    }

    @pure def ancestorsOf[T](poset: Poset[T], n: Index): HashSet[Index] = {
      return ancestorsCache[T](poset, n, HashMap.empty)._1
    }

    @pure def ancestorsCache[T](
      poset: Poset[T],
      n: Index,
      acc: HashMap[Index, HashSet[Index]]
    ): (HashSet[Index], HashMap[Index, HashSet[Index]]) = {
      var mAcc = acc
      var r = emptySet
      for (nParent <- parentsOf(poset, n).elements) {
        mAcc = ancestorsRec(poset, nParent, mAcc)
        r = (r + nParent) ∪ mAcc.get(nParent).getOrElse(emptySet)
      }
      return (r, mAcc)
    }

    @pure def ancestorsRec[T](
      poset: Poset[T],
      m: Index,
      acc: HashMap[Index, HashSet[Index]]
    ): HashMap[Index, HashSet[Index]] = {
      if (acc.contains(m)) {
        return acc
      }
      val p = ancestorsCache(poset, m, acc + m ~> emptySet)
      val mAncestors = p._1
      val mAcc = p._2
      return mAcc + m ~> mAncestors
    }

    @pure def lub[T](poset: Poset[T], ns: ISZ[Index]): Option[Index] = {
      ns.size match {
        case z"0" => return None()
        case z"1" => return Some(ns(0))
        case _ =>
      }
      if ((HashSet ++ ns).size == 1) {
        return Some(ns(0))
      }
      val p0 = ancestorsCache[T](poset, ns(0), HashMap.empty)
      var commons = p0._1 + ns(0)
      var acc = p0._2
      for (i <- z"1" until ns.size) {
        val p = ancestorsCache(poset, ns(i), acc)
        acc = p._2
        commons = commons ∩ (p._1 + ns(i))
      }
      if (commons.isEmpty) {
        return None()
      }
      for (b1 <- commons.elements) {
        for (b2 <- commons.elements if b1 != b2) {
          if (ancestorsCache(poset, b1, acc)._1.contains(b2)) {
            commons = commons - b2
          }
        }
      }
      if (commons.size == 1) {
        return Some(commons.elements(0))
      } else {
        return None()
      }
    }

    @pure def descendantsOf[T](poset: Poset[T], n: Index): HashSet[Index] = {
      return descendantsCache[T](poset, n, HashMap.empty)._1
    }

    @pure def descendantsCache[T](
      poset: Poset[T],
      n: Index,
      acc: HashMap[Index, HashSet[Index]]
    ): (HashSet[Index], HashMap[Index, HashSet[Index]]) = {
      var mAcc = acc
      var r = emptySet
      for (nChild <- childrenOf(poset, n).elements) {
        mAcc = descendantsRec(poset, nChild, mAcc)
        r = (r + nChild) ∪ mAcc.get(nChild).getOrElse(emptySet)
      }
      return (r, mAcc)
    }

    @pure def descendantsRec[T](
      poset: Poset[T],
      m: Index,
      acc: HashMap[Index, HashSet[Index]]
    ): HashMap[Index, HashSet[Index]] = {
      if (acc.contains(m)) {
        return acc
      }
      val p = descendantsCache(poset, m, acc + m ~> emptySet)
      val mDescendants = p._1
      val mAcc = p._2
      return mAcc + m ~> mDescendants
    }

    @pure def glb[T](poset: Poset[T], ns: ISZ[Index]): Option[Index] = {
      ns.size match {
        case z"0" => return None()
        case z"1" => return Some(ns(0))
        case _ =>
      }
      if ((HashSet.empty[Index] ++ ns).size == 1) {
        return Some(ns(0))
      }
      val p0 = descendantsCache[T](poset, ns(0), HashMap.empty)
      var commons = p0._1 + ns(0)
      var acc = p0._2
      for (i <- z"1" until ns.size) {
        val p = descendantsCache(poset, ns(i), acc)
        acc = p._2
        commons = commons ∩ (p._1 + ns(i))
      }
      if (commons.isEmpty) {
        return None()
      }
      for (b1 <- commons.elements) {
        for (b2 <- commons.elements if b1 != b2) {
          if (descendantsCache(poset, b1, acc)._1.contains(b2)) {
            commons = commons - b2
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

  def empty[T]: Poset[T] = {
    return Poset[T](HashMap.empty, ISZ(), HashMap.empty, HashMap.empty)
  }
}

import Poset._

@datatype class Poset[T](
  nodes: HashMap[T, Z],
  nodesInverse: IS[Z, T],
  parents: HashMap[Z, HashSet[Z]],
  children: HashMap[Z, HashSet[Z]]
) {

  val emptySet: HashSet[T] = HashSet.empty

  @pure def size: Z = {
    return nodes.size
  }

  @pure override def hash: Z = {
    return size
  }

  @pure def isEqual(other: Poset[T]): B = {
    if (nodesInverse != other.nodesInverse) {
      return F
    }
    for (node <- nodes.keys) {
      val n = nodes.get(node).get
      val m = other.nodes.get(node).get
      val nParents = HashSet ++ parents.get(n).get.elements.map[T](np => nodesInverse(np))
      val mParents = HashSet ++ other.parents.get(m).get.elements.map[T](mp => other.nodesInverse(mp))
      if (nParents != mParents) {
        return F
      }
    }
    return T
  }

  @pure def addNode(node: T): Poset[T] = {
    return Poset.Internal.addNode(this, node)._1
  }

  @pure def rootNodes: ISZ[T] = {
    return for (e <- parents.entries if e._2.isEmpty) yield nodesInverse(e._1)
  }

  @pure def addParents(node: T, nds: ISZ[T]): Poset[T] = {
    var r = this
    val n: Index = {
      val p = Poset.Internal.addNode(r, node)
      r = p._1
      p._2
    }
    val ns: ISZ[Index] = {
      val p = Poset.Internal.addNodes(r, nds)
      r = p._1
      p._2
    }
    return Poset.Internal.addParents(r, n, ns)
  }

  @pure def removeParent(node: T, parent: T): Poset[T] = {
    (nodes.get(node), nodes.get(parent)) match {
      case (Some(n), Some(p)) => return Poset.Internal.removeParent(this, n, p)
      case _ => return this
    }
  }

  @pure def removeChild(n: T, child: T): Poset[T] = {
    return removeParent(child, n)
  }

  @pure def addChildren(node: T, nds: ISZ[T]): Poset[T] = {
    var r = this
    val n: Index = {
      val p = Poset.Internal.addNode(r, node)
      r = p._1
      p._2
    }
    val ns: ISZ[Index] = {
      val p = Poset.Internal.addNodes(r, nds)
      r = p._1
      p._2
    }
    return Poset.Internal.addChildren(r, n, ns)
  }

  @pure def childrenOf(node: T): HashSet[T] = {
    nodes.get(node) match {
      case Some(n) => HashSet ++ Poset.Internal.childrenOf(this, n).elements.map[T](n => nodesInverse(n))
      case _ => return emptySet
    }
  }

  @pure def isChildOf(node1: T, node2: T): B = {
    (nodes.get(node1), nodes.get(node2)) match {
      case (Some(n1), Some(n2)) => return Poset.Internal.childrenOf(this, n1).contains(n2)
      case _ => return F
    }
  }

  @pure def parentsOf(node: T): HashSet[T] = {
    nodes.get(node) match {
      case Some(n) => HashSet ++ Poset.Internal.parentsOf(this, n).elements.map[T](n => nodesInverse(n))
      case _ => return emptySet
    }
  }

  @pure def isParentOf(node1: T, node2: T): B = {
    return isChildOf(node2, node1)
  }

  @pure def ancestorsOf(node: T): HashSet[T] = {
    nodes.get(node) match {
      case Some(n) => HashSet ++ Poset.Internal.ancestorsOf(this, n).elements.map[T](n => nodesInverse(n))
      case _ => return emptySet
    }
  }

  @pure def lub(nds: ISZ[T]): Option[T] = {
    val ns: ISZ[Index] = for (node <- nds; n <- nodes.get(node).toIS) yield n
    return Poset.Internal.lub(this, ns).map(n => nodesInverse(n))
  }

  @pure def descendantsOf(node: T): HashSet[T] = {
    nodes.get(node) match {
      case Some(n) => HashSet ++ Poset.Internal.descendantsOf(this, n).elements.map[T](n => nodesInverse(n))
      case _ => return emptySet
    }
  }

  @pure def glb(nds: ISZ[T]): Option[T] = {
    val ns: ISZ[Index] = for (node <- nds; n <- nodes.get(node).toIS) yield n
    return Poset.Internal.glb(this, ns).map(n => nodesInverse(n))
  }

  @pure def toST(f: T => ST): ST = {
    val nodes: ISZ[ST] = for (e <- this.nodes.entries) yield st"n${e._2} ${f(e._1)}"
    val edges: ISZ[ST] = for (entry <- parents.entries; parent <- entry._2.elements) yield st"n${entry._1} -> n$parent"
    val r =
      st"""digraph G {
      |  rankdir="BT"
      |
      |  ${(nodes, "\n")}
      |
      |  ${(edges, "\n")}
      |}"""
    return r
  }

  @pure override def string: String = {
    return toST(node => st"""[label="$node"]""").render
  }
}
