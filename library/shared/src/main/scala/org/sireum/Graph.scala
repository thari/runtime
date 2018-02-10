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

object Graph {

  @datatype trait Edge[V, E] {
    def source: V
    def dest: V
  }

  object Edge {
    @datatype class Plain[V, E](val source: V, val dest: V) extends Edge[V, E]
    @datatype class Data[V, E](val source: V, val dest: V, val data: E) extends Edge[V, E]

    @datatype trait Internal[E] {
      def source: Z
      def dest: Z
      def toEdge[V](map: HashMap[Z, V]): Edge[V, E]
    }

    object Internal {

      @datatype class Plain[E](val source: Z, val dest: Z) extends Internal[E] {

        @pure def toEdge[V](map: HashMap[Z, V]): Edge[V, E] = {
          return Edge.Plain(map.get(source).get, map.get(dest).get)
        }
      }

      @datatype class Data[E](val source: Z, val dest: Z, val data: E) extends Internal[E] {

        @pure def toEdge[V](map: HashMap[Z, V]): Edge[V, E] = {
          return Edge.Data(map.get(source).get, map.get(dest).get, data)
        }
      }
    }

  }

  @pure def empty[V, E]: Graph[V, E] = {
    return Graph(HashMap.empty, HashMap.empty, HashMap.empty, 0)
  }
}

@datatype class Graph[V, E](
  val nodes: HashMap[V, Z],
  val incomingEdges: HashMap[Z, HashSet[Graph.Edge.Internal[E]]],
  val outgoingEdges: HashMap[Z, HashSet[Graph.Edge.Internal[E]]],
  nextNodeId: Z
) {

  @pure override def hash: Z = {
    return (nodes.size, incomingEdges.values.map(s => s.size)).hash
  }

  @pure def isEqual(other: Graph[V, E]): B = {
    if (nodes.size != other.nodes.size || incomingEdges.size != incomingEdges.size) {
      return F
    }
    if (nodes.keySet != other.nodes.keySet) {
      return F
    }
    val m = nodesInverse
    val thisEdges: ISZ[Graph.Edge[V, E]] = for (ess <- incomingEdges.values; es <- ess.elements) yield es.toEdge(m)
    val otherM = other.nodesInverse
    val otherEdges: ISZ[Graph.Edge[V, E]] = for (ess <- other.incomingEdges.values; es <- ess.elements)
      yield es.toEdge(otherM)
    return HashSet.empty[Graph.Edge[V, E]].addAll(thisEdges).addAll(otherEdges).size == thisEdges.size
  }

  @pure def addNode(node: V): Graph[V, E] = {
    nodes.get(node) match {
      case Some(_) => return this
      case _ => return this(nodes = nodes.put(node, nextNodeId), nextNodeId = nextNodeId + 1)
    }
  }

  @pure def add(edge: (V, V)): Graph[V, E] = {
    return addEdge(edge._1, edge._2)
  }

  @pure def addEdge(source: V, dest: V): Graph[V, E] = {
    var r = addNode(source)
    r = r.addNode(dest)
    val src = r.nodes.get(source).get
    val dst = r.nodes.get(dest).get
    val e = Graph.Edge.Internal.Plain[E](src, dst)
    return r(
      incomingEdges = r.incomingEdges.put(dst, r.incomingEdges.get(dst).getOrElse(HashSet.empty).add(e)),
      outgoingEdges = r.outgoingEdges.put(src, r.outgoingEdges.get(src).getOrElse(HashSet.empty).add(e))
    )
  }

  @pure def addDataEdge(data: E, source: V, dest: V): Graph[V, E] = {
    var r = addNode(source)
    r = addNode(dest)
    val src = nodes.get(source).get
    val dst = nodes.get(dest).get
    val e = Graph.Edge.Internal.Data(src, dst, data)
    return r(
      incomingEdges = r.incomingEdges.put(dst, r.incomingEdges.get(dst).getOrElse(HashSet.empty).add(e)),
      outgoingEdges = r.outgoingEdges.put(src, r.outgoingEdges.get(src).getOrElse(HashSet.empty).add(e))
    )
  }

  @pure def nodesInverse: HashMap[Z, V] = {
    var r = HashMap.emptyInit[Z, V](nodes.size)
    for (e <- nodes.entries) {
      r = r.put(e._2, e._1)
    }
    return r
  }

  @pure def edges(source: V, dest: V): ISZ[Graph.Edge[V, E]] = {
    val m = nodesInverse
    nodes.get(dest) match {
      case Some(d) => return outgoing(source).withFilter(e => e.dest == d)
      case _ => return ISZ()
    }
  }

  @pure def incoming(dest: V): ISZ[Graph.Edge[V, E]] = {
    val m = nodesInverse
    nodes.get(dest) match {
      case Some(d) =>
        incomingEdges.get(d) match {
          case Some(s) => return s.elements.map(e => e.toEdge(m))
          case _ =>
        }
      case _ =>
    }
    return ISZ()
  }

  @pure def outgoing(source: V): ISZ[Graph.Edge[V, E]] = {
    val m = nodesInverse
    nodes.get(source) match {
      case Some(d) =>
        outgoingEdges.get(d) match {
          case Some(s) => return s.elements.map(e => e.toEdge(m))
          case _ =>
        }
      case _ =>
    }
    return ISZ()
  }

  @pure def string: String = {
    @pure def e2st(e: Graph.Edge.Internal[E]): ST = {
      e match {
        case Graph.Edge.Internal.Data(source, dest, data) => return st"""n$source -> n$dest [label = "$data"]"""
        case Graph.Edge.Internal.Plain(source, dest) => return st"""n$source -> n$dest"""
      }
    }
    val nodes: ISZ[ST] = for (e <- this.nodes.entries) yield st"""n${e._2} [label="${e._1}"]"""
    val edges: ISZ[ST] = for (es <- this.incomingEdges.values; e <- es.elements) yield e2st(e)
    val r =
      st"""digraph G {
      |  ${(nodes, "\n")}
      |
      |  ${(edges, "\n")}
      |}"""
    return r.render
  }
}
