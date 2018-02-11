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
  
  type Index = Z

  @datatype trait Edge[V, E] {
    @pure def source: V
    @pure def dest: V
    @pure def toInternal(map: HashMap[V, Graph.Index]): Internal.Edge[E]
  }

  object Edge {

    @datatype class Plain[V, E](val source: V, val dest: V) extends Edge[V, E] {

      @pure override def toInternal(map: HashMap[V, Graph.Index]): Internal.Edge[E] = {
        return Internal.Edge.Plain(map.get(source).get, map.get(dest).get)
      }

    }

    @datatype class Data[V, E](val source: V, val dest: V, val data: E) extends Edge[V, E] {

      @pure override def toInternal(map: HashMap[V, Graph.Index]): Internal.Edge[E] = {
        return Internal.Edge.Data(map.get(source).get, map.get(dest).get, data)
      }

    }

  }

  object Internal {

    @datatype trait Edge[E] {
      @pure def source: Graph.Index
      @pure def dest: Graph.Index
      @pure def toEdge[V](map: ISZ[V]): Graph.Edge[V, E]
    }

    @datatype trait Edges[E] {
      @pure def elements: ISZ[Edge[E]]
      @pure def size: Z
      @pure def add(e: Edge[E]): Edges[E]
      @pure def addAll(es: ISZ[Edge[E]]): Edges[E]
      @pure def removeN(e: Edge[E], n: Z): Edges[E]
    }

    object Edges {

      @datatype class Set[E](set: HashSet[Edge[E]]) extends Edges[E] {

        @pure override def elements: ISZ[Edge[E]] = {
          return set.elements
        }

        @pure override def size: Z = {
          return set.size
        }

        @pure override def add(e: Edge[E]): Edges[E] = {
          return this(set = set.add(e))
        }

        @pure override def addAll(es: ISZ[Edge[E]]): Edges[E] = {
          return this(set = set.addAll(es))
        }

        @pure override def removeN(e: Edge[E], n: Z): Edges[E] = {
          return this(set = set.remove(e))
        }
      }

      @datatype class Bag[E](set: HashBag[Edge[E]]) extends Edges[E] {

        @pure override def elements: ISZ[Edge[E]] = {
          return set.elements
        }

        @pure override def size: Z = {
          return set.size
        }

        @pure override def add(e: Edge[E]): Edges[E] = {
          return this(set = set.add(e))
        }

        @pure override def addAll(es: ISZ[Edge[E]]): Edges[E] = {
          return this(set = set.addAll(es))
        }

        @pure override def removeN(e: Edge[E], n: Z): Edges[E] = {
          return this(set = set.removeN(e, n))
        }
      }

      @pure def empty[E](multi: B): Edges[E] = {
        return if (multi) Bag(HashBag.empty) else Set(HashSet.empty)
      }

    }

    object Edge {

      @datatype class Plain[E](val source: Graph.Index, val dest: Graph.Index) extends Edge[E] {

        @pure override def toEdge[V](map: ISZ[V]): Graph.Edge[V, E] = {
          return Graph.Edge.Plain(map(source), map(dest))
        }

      }

      @datatype class Data[E](val source: Graph.Index, val dest: Graph.Index, val data: E) extends Edge[E] {

        @pure override def toEdge[V](map: ISZ[V]): Graph.Edge[V, E] = {
          return Graph.Edge.Data(map(source), map(dest), data)
        }

      }

    }

    @pure def addEdge[V, E](g: Graph[V, E], e: Internal.Edge[E]): Graph[V, E] = {
      return g(
        incomingEdges = g.incomingEdges.put(e.dest, g.incomingEdges.get(e.dest).getOrElse(Edges.empty(g.multi)).add(e)),
        outgoingEdges = g.outgoingEdges.put(e.source, g.outgoingEdges.get(e.source).getOrElse(Edges.empty(g.multi)).add(e))
      )
    }

    @pure def addPlainEdge[V, E](g: Graph[V, E], src: Graph.Index, dst: Graph.Index): Graph[V, E] = {
      return addEdge(g, Graph.Internal.Edge.Plain[E](src, dst))
    }

    @pure def addDataEdge[V, E](g: Graph[V, E], data: E, src: Graph.Index, dst: Graph.Index): Graph[V, E] = {
      return addEdge(g, Graph.Internal.Edge.Data(src, dst, data))
    }

    @pure def removeEdge[V, E](g: Graph[V, E], e: Graph.Internal.Edge[E], n: Z): Graph[V, E] = {
      if (g.incomingEdges.get(e.dest).isEmpty) {
        return g
      }
      return g(
        incomingEdges = g.incomingEdges.put(e.dest, g.incomingEdges.get(e.dest).get.removeN(e, n)),
        outgoingEdges = g.outgoingEdges.put(e.source, g.outgoingEdges.get(e.source).get.removeN(e, n))
      )
    }

    @pure def incoming[V, E](g: Graph[V, E], dst: Graph.Index): ISZ[Graph.Internal.Edge[E]] = {
      g.incomingEdges.get(dst) match {
        case Some(s) => return s.elements
        case _ => return ISZ()
      }
    }

    @pure def outgoing[V, E](g: Graph[V, E], src: Graph.Index): ISZ[Graph.Internal.Edge[E]] = {
      g.outgoingEdges.get(src) match {
        case Some(s) => return s.elements
        case _ => return ISZ()
      }
    }

  }

  @pure def empty[V, E]: Graph[V, E] = {
    return Graph(HashMap.empty, ISZ(), HashMap.empty, HashMap.empty, 0, F)
  }

  @pure def emptyMulti[V, E]: Graph[V, E] = {
    return Graph(HashMap.empty, ISZ(), HashMap.empty, HashMap.empty, 0, T)
  }
}

@datatype class Graph[V, E](
  val nodes: HashMap[V, Graph.Index],
  val nodesInverse: ISZ[V],
  val incomingEdges: HashMap[Graph.Index, Graph.Internal.Edges[E]],
  val outgoingEdges: HashMap[Graph.Index, Graph.Internal.Edges[E]],
  val nextNodeId: Graph.Index,
  val multi: B
) {

  @pure def numOfNodes: Z = {
    return nodes.size
  }

  @pure def numOfEdges: Z = {
    var r = z"0"
    for (n <- incomingEdges.values.map(s => s.size)) {
      r = r + n
    }
    return r
  }

  @pure override def hash: Z = {
    return (numOfNodes, numOfEdges).hash
  }

  @pure def isEqual(other: Graph[V, E]): B = {
    if (nodes.size != other.nodes.size || incomingEdges.size != incomingEdges.size) {
      return F
    }
    if (nodes.keySet != other.nodes.keySet) {
      return F
    }
    val thisEdges: ISZ[Graph.Edge[V, E]] =
      for (ess <- incomingEdges.values; es <- ess.elements) yield es.toEdge(nodesInverse)
    val otherEdges: ISZ[Graph.Edge[V, E]] =
      for (ess <- other.incomingEdges.values; es <- ess.elements) yield es.toEdge(other.nodesInverse)
    return HashSet.empty[Graph.Edge[V, E]].addAll(thisEdges).addAll(otherEdges).size == thisEdges.size
  }

  @pure def deleteNodes(ns: ISZ[V]): Graph[V, E] = {
    var r: Graph[V, E] = if (multi) Graph.emptyMulti[V, E] else Graph.empty[V, E]
    val ins = HashSet.emptyInit[Graph.Index](ns.size).addAll(ns.map(n => nodes.get(n).get))
    for (es <- incomingEdges.values) {
      for (e <- es.elements) {
        if (ins.contains(e.source) && ins.contains(e.dest)) {
          r = r.addEdge(e.toEdge(nodesInverse))
        }
      }
    }
    return r
  }

  @pure def addNode(node: V): Graph[V, E] = {
    nodes.get(node) match {
      case Some(_) => return this
      case _ =>
        return this(
          nodes = nodes.put(node, nextNodeId),
          nodesInverse = nodesInverse :+ node,
          nextNodeId = nextNodeId + 1
        )
    }
  }

  @pure def add(edge: (V, V)): Graph[V, E] = {
    return addPlainEdge(edge._1, edge._2)
  }

  @pure def addData(edge: ((V, V), E)): Graph[V, E] = {
    return addDataEdge(edge._2, edge._1._1, edge._1._2)
  }

  @pure def addEdge(edge: Graph.Edge[V, E]): Graph[V, E] = {
    return Graph.Internal.addEdge(addNode(edge.source).addNode(edge.dest), edge.toInternal(nodes))
  }

  @pure def addPlainEdge(source: V, dest: V): Graph[V, E] = {
    val r = addNode(source).addNode(dest)
    return Graph.Internal.addPlainEdge(r, r.nodes.get(source).get, r.nodes.get(dest).get)
  }

  @pure def addDataEdge(data: E, source: V, dest: V): Graph[V, E] = {
    val r = addNode(source).addNode(dest)
    return Graph.Internal.addDataEdge(r, data, r.nodes.get(source).get, r.nodes.get(dest).get)
  }

  @pure def allEdges: ISZ[Graph.Edge[V, E]] = {
    return for (es <- incomingEdges.values; e <- es.elements) yield e.toEdge(nodesInverse)
  }

  @pure def removeEdges(edges: ISZ[Graph.Edge[V, E]]): Graph[V, E] = {
    var r = this
    for (e <- edges) {
      r = r.removeEdge(e)
    }
    return r
  }

  @pure def removeEdge(edge: Graph.Edge[V, E]): Graph[V, E] = {
    return removeEdgeN(edge, 1)
  }

  @pure def removeEdgeN(edge: Graph.Edge[V, E], n: Z): Graph[V, E] = {
    return Graph.Internal.removeEdge(this, edge.toInternal(nodes), n)
  }

  @pure def edges(source: V, dest: V): ISZ[Graph.Edge[V, E]] = {
    nodes.get(dest) match {
      case Some(d) => return outgoing(source).withFilter(e => e.dest == d)
      case _ => return ISZ()
    }
  }

  @pure def incoming(dest: V): ISZ[Graph.Edge[V, E]] = {
    nodes.get(dest) match {
      case Some(dst) => return Graph.Internal.incoming(this, dst).map(e => e.toEdge(nodesInverse))
      case _ => return ISZ()
    }
  }

  @pure def outgoing(source: V): ISZ[Graph.Edge[V, E]] = {
    nodes.get(source) match {
      case Some(src) => Graph.Internal.outgoing(this, src).map(e => e.toEdge(nodesInverse))
      case _ => return ISZ()
    }
  }

  @pure def toST(f: V => ST @pure, g: E => ST @pure): ST = {
    @pure def e2st(e: Graph.Internal.Edge[E]): ST = {
      e match {
        case Graph.Internal.Edge.Data(source, dest, data) =>  return st"""n$source -> n$dest [label = "${g(data)}"]"""
        case Graph.Internal.Edge.Plain(source, dest) => return st"""n$source -> n$dest"""
      }
    }
    val nodes: ISZ[ST] = for (e <- this.nodes.entries) yield st"""n${e._2} [label="${f(e._1)}"]"""
    val edges: ISZ[ST] = for (es <- incomingEdges.values; e <- es.elements) yield e2st(e)
    val r =
      st"""digraph G {
          |
          |  ${(nodes, "\n")}
          |
          |  ${(edges, "\n")}
          |
          |}"""
    return r

  }

  @pure override def string: String = {
    return toST(v => st"$v", e => st"$e").render
  }
}
