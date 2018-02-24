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

import utest._
import org.sireum.test._

object GraphTest extends TestSuite {

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
      assert(g.outgoing(n2).isEmpty)
      assert(g.incoming(n1) == g.outgoing(1))
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
  }

  def differentRandom(n: Z): Z = {
    var r = Z.random
    while (r == n) {
      r = Z.random
    }
    r
  }
}
