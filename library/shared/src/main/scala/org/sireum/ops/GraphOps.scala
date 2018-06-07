// #Sireum
/*
 Copyright (c) 2017, Hariharan Thiagarajan, Kansas State University
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

package org.sireum.ops

import org.sireum.Graph
import org.sireum._

@sig trait GraphOps[V, E] {
  def getEdgeData(e: Graph.Edge[V, E]): Option[E]

  def getAllSuccessor(v: V): Set[V]

  def getAllPredecessor(v: V): Set[V]

  def getSCC: ISZ[HashSet[V]]

  def getCycles: ISZ[ISZ[V]]

  def forwardReach(criteria: ISZ[V]): ISZ[V]

  def backwardReach(criteria: ISZ[V]): ISZ[V]
}

object GraphOps {
  def apply[V, E](graph: Graph[V, E]): GraphOps[V, E] = new GraphOpsImpl(graph)
}

@datatype class GraphOpsImpl[V, E](graph: Graph[V, E]) extends GraphOps[V, E] {
  def getEdgeData(e: Graph.Edge[V, E]): Option[E] = {
    e match {
      case Graph.Edge.Data(s, d, ed) => Some[E](ed)
      case _ => None[E]()
    }
  }

  def getAllSuccessor(v: V): Set[V] = {
    if (graph.outgoingEdges.get(graph.nodes.get(v).get).nonEmpty) {
      Set.empty[V] ++ graph.outgoingEdges.get(graph.nodes.get(v).get).get.elements.map(es =>
        graph.nodesInverse(es.dest))
    } else {
      Set.empty[V]
    }
  }

  def getAllPredecessor(v: V): Set[V] = {
    if (graph.incomingEdges.get(graph.nodes.get(v).get).nonEmpty) {
      Set.empty[V] ++ graph.incomingEdges.get(graph.nodes.get(v).get).get.elements.map(es =>
        graph.nodesInverse(es.source))
    } else {
      Set.empty[V]
    }

  }

  def getSCC: ISZ[HashSet[V]] = {
    var result = ISZ[HashSet[V]]()
    var discoveryMap: HashMap[V, (B, B)] =
      HashMap ++ graph.nodes.keys.map(v => (v, (B.F, B.F)))

    def resetDiscoveryMap(): Unit = {
      discoveryMap = HashMap ++ discoveryMap.entries.map(e => (e._1, (B.F, B.F)))
    }

    def setDiscovered(v: V): B = {
      discoveryMap.get(v).exists { cf =>
        discoveryMap = discoveryMap + ((v, (B.T, cf._2)))
        B.T
      }
    }

    def setBoth(v: V): Unit = {
      discoveryMap = discoveryMap + ((v, (B.T, B.T)))
    }

    def isAllMySuccDiscovered(v: V): B = {
      ISZOps(getAllSuccessor(v).elements.map(discoveryMap.get)
        .flatMap(_.toIS.map(_._1))).foldLeft({ (c: B, n: B) => c & n }, B.T)
    }

    def dfs(v: V, isFirst: B): ISZ[V] = {
      var result = ISZ[V]()
      var stack = Stack.empty[V]
      stack = stack.push(v)

      while (stack.nonEmpty) {
        val current = stack.pop().get
        stack = current._2
        if (discoveryMap.get(current._1).nonEmpty
          && !discoveryMap.get(current._1).get._1) {
          setDiscovered(current._1)
          if (!isFirst) {
            result = result :+ current._1
          }
          setBoth(current._1)
          stack = stack.push(current._1)

          val nexts = if (isFirst) getAllSuccessor(current._1) else getAllPredecessor(current._1)

          nexts.elements.foreach(n => if (!discoveryMap.get(n).get._1) {
            stack = stack.push(n)
          })

        } else if (discoveryMap.get(current._1).get._2 && isFirst) {
          result = result.+:(current._1)
        }
      }
      result
    }

    var orderedNodes = ISZ[V]()

    graph.nodes.keys.foreach { k =>
      if (!discoveryMap.get(k).get._1)
        orderedNodes = dfs(k, isFirst = B.T) ++ orderedNodes
    }
    resetDiscoveryMap()
    orderedNodes.foreach(k =>
      if (!discoveryMap.get(k).get._1) {
        result = result :+ HashSet.empty ++ dfs(k, isFirst = B.F)
      }
    )
    result
  }

  def getCycles: ISZ[ISZ[V]] = {
    val sccs = getSCC
    var loops = ISZ[ISZ[V]]()
    var bSets = HashMap.empty[V, Set[V]]
    var stack = Stack.empty[V]
    var marked = Set.empty[V]
    var removed = HashMap.empty[V, Set[V]]
    var position = HashMap.empty[V, Z]
    var reach = HashMap.empty[V, B] ++ graph.nodes.keys.map(k => (k, B.F))

    def cycle(v: V, tq: Z): B = {
      var q = tq
      var foundCycle = B.F
      marked = marked + v
      stack = stack.push(v)
      val t = stack.size
      position = position + ((v, t))
      if (!reach.get(v).get) {
        q = t
      }
      val avRemoved = removed.get(v) match {
        case Some(r) => r
        case _ => Set.empty[V]
      }

      getAllSuccessor(v).elements.foreach { wV =>
        if (!avRemoved.contains(wV)) {
          if (!marked.contains(wV)) {
            val gotCycle = cycle(wV, q)
            if (gotCycle) {
              foundCycle = B.T
            } else {
              noCycle(v, wV)
            }
          } else if (position.get(wV).nonEmpty &&
            position.get(wV).get <= q) {
            foundCycle = B.T
            var cycle = ISZ[V]()
            val it = stack.elements.elements.iterator
            var current = stack.peek.get
            var break = B.T
            while (it.hasNext && break) {
              current = it.next()
              if (wV == current) {
                break = B.F
              }
            }
            cycle = cycle :+ wV
            break = B.T
            while (it.hasNext && break) {
              current = it.next()
              cycle = cycle :+ current
              if (current == v) {
                break = B.F
              }
            }
            loops = loops :+ cycle
          } else {
            noCycle(v, wV)
          }
        }
      }
      stack = stack.pop().get._2
      if (foundCycle) {
        unmark(v)
      }
      reach = reach + (v, B.T)
      position = position + (v, graph.nodes.size)
      foundCycle
    }

    def unmark(x: V): Unit = {
      marked = marked - x
      val temp = bSets.get(x) match {
        case Some(bsx) => bsx
        case _ => Set.empty[V]
      }

      temp.elements.foreach { y =>
        removed = removed + ((y, removed.get(y) match {
          case Some(ry) => ry - x
          case _ => Set.empty[V] - x
        }))
        if (marked.contains(y)) {
          unmark(y)
        }
      }
      bSets = bSets + (x, Set.empty[V])
    }


    def noCycle(x: V, y: V): Unit = {
      bSets = bSets + ((y, bSets.get(y) match {
        case Some(bs) => bs
        case _ => Set.empty[V]
      }))
      removed = removed + ((x, removed.get(x) match {
        case Some(rx) => rx + y
        case _ => Set.empty[V] + y
      }))
    }

    val startNodes = sccs.map { scc =>
      var max: Z = -1
      var startNode = scc.elements(0)
      scc.elements.foreach { node =>
        val inDegree = graph.incomingEdges.get(graph.nodes.get(node).get).get.size
        if (inDegree > max) {
          max = inDegree
          startNode = node
        }
      }
      startNode
    }

    startNodes.foreach(cycle(_, 0))
    loops
  }

  def forwardReach(criteria: ISZ[V]): ISZ[V] = reachable(criteria, B.T)

  def backwardReach(criteria: ISZ[V]): ISZ[V] = reachable(criteria, B.F)

  private def reachable(criteria: ISZ[V], isForward: B): ISZ[V] = {
    var workList = ISZ[V]()
    workList = workList ++ criteria
    var result = HashSet.empty[V]

    while (workList.nonEmpty) {
      val current = ISZOps(workList).first
      if (!result.contains(current)) {
        val next = if (isForward)
          getAllSuccessor(current) else getAllPredecessor(current)
        workList = workList ++ next.elements
        result = result + current
      }
      workList = ISZOps(workList).tail
    }
    result.elements
  }

}