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

import org.sireum.ops.GraphOps
import org.sireum.test._

class GraphTest extends TestSuite {

  val tests = Tests {
    * - {
      val graph = Graph.empty[Z, String]
      val n1 = Z.random
      val n2 = differentRandom(n1)
      val g = graph +@ n1 ~> n2 ~> "out"
      assert(g.incoming(n1).isEmpty)
      assert(
        g.outgoing(n1)
          .elements
          .forall(e => e.source == n1 && e.dest == n2))
      assert(g.outgoing(n1).elements.map({ case Graph.Edge.Data(_, _, data) => data}) == Seq(string"out"))
      assert(g.outgoing(n2).isEmpty)
      assert(g.incoming(n1) == g.outgoing(n2 + 1))
    }

    * - {
      val graph = Graph.empty[Z, String]
      val n1 = Z.random
      val n2 = differentRandom(n1)
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      assert(g.incoming(n1).nonEmpty)
      assert(
        g.outgoing(n1)
          .elements
          .forall(e => e.source == n1 && e.dest == n2))
      assert(g.outgoing(n2).nonEmpty)
      assert(g.incoming(n1) == g.outgoing(n2))
      assert(g.outgoing(n1) == g.incoming(n2))
    }

    * - {
      val graph = Graph.empty[Z, String]
      val n1 = Z.random
      val n2 = differentRandom(n1)
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n2
      assert(g.incoming(n1).nonEmpty)
      assert(
        g.outgoing(n1)
          .elements
          .forall(e => e.source == n1 && e.dest == n2))
      assert(g.outgoing(n2).nonEmpty)
      assert(g.incoming(n1) == g.outgoing(n2))
      assert(g.outgoing(n1) == g.incoming(n2))
      assert(g.outgoing(n1).elements.size == 1)
    }

    * - {
      val graph = Graph.emptyMulti[Z, String]
      val n1 = Z.random
      val n2 = differentRandom(n1)
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n2

      assert(g.incoming(n1).nonEmpty)
      assert(
        g.outgoing(n1)
          .elements
          .forall(e => e.source == n1 && e.dest == n2))
      assert(g.outgoing(n2).nonEmpty)
      assert(g.incoming(n1) == g.outgoing(n2))
      assert(g.outgoing(n1) == g.incoming(n2))
      assert(g.outgoing(n1).elements.size == 2)
    }

    //GraphOps Tests : SCC smoke test
    * - {
      val graph = Graph.empty[Z, String]
      val n1: Z = 1
      val n2: Z = 2
      val n3: Z = 3
      val n4: Z = 4
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n3
      g = g + n3 ~> n1
      g = g + n1 ~> n4

      val scc = GraphOps[Z, String](g).getSCC
      assert(scc.nonEmpty)
      assert(scc.size == Z.apply(2))
      val entry1 = HashSet.empty ++ ISZ(n1, n2, n3)
      val entry2 = HashSet.empty ++ ISZ(n4)
      assert((Set.empty ++ scc).contains(entry1))
      assert((Set.empty ++ scc).contains(entry2))
    }

    //GraphOps Tests : Cycle smoke test
    * - {
      val graph = Graph.empty[Z, String]
      val n1: Z = 1
      val n2: Z = 2
      val n3: Z = 3
      val n4: Z = 4
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n3
      g = g + n3 ~> n1
      g = g + n1 ~> n4

      val cycle = GraphOps[Z, String](g).getCycles
      assert(cycle.nonEmpty)
      assert(cycle.size == Z.apply(2))

      val entry1 = HashSet.empty ++ ISZ(n1, n2)
      val entry2 = HashSet.empty ++ ISZ(n1, n3)
      val cycleSets = Set.empty ++ cycle.map(HashSet.empty ++ _)

      assert(cycleSets.contains(entry1))
      assert(cycleSets.contains(entry2))
    }

    //GraphOps Tests : Forward reach smoke test
    * - {
      val graph = Graph.empty[Z, String]
      val n1: Z = 1
      val n2: Z = 2
      val n3: Z = 3
      val n4: Z = 4
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n3
      g = g + n3 ~> n1
      g = g + n1 ~> n4

      val forwardFrom_n2 = GraphOps[Z, String](g).forwardReach(ISZ(n2))
      val forwardFrom_n4 = GraphOps[Z, String](g).forwardReach(ISZ(n4))

      assert((HashSet.empty ++ ISZ(n1, n2, n3, n4)) ==
        (HashSet.empty ++ forwardFrom_n2))

      assert(forwardFrom_n4.size == Z(1))
      assert((HashSet.empty ++ forwardFrom_n4).contains(n4))
    }

    //GraphOps Tests : Backward reach smoke test
    * - {
      val graph = Graph.empty[Z, String]
      val n1: Z = 1
      val n2: Z = 2
      val n3: Z = 3
      val n4: Z = 4
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n3
      g = g + n3 ~> n1
      g = g + n1 ~> n4

      val backwardFrom_n4 = GraphOps[Z, String](g).backwardReach(ISZ(n4))
      assert((HashSet.empty ++ ISZ(n1, n2, n3, n4)) ==
        (HashSet.empty ++ backwardFrom_n4))
    }
  }

  def differentRandom(n: Z): Z = {
    var r = Z.random
    while (r == n) {
      r = Z.random
    }
    r
  }
}
