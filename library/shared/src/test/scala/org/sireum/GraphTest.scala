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

import org.sireum.test._

class GraphTest extends SireumRuntimeSpec {

  {
    implicit val _spec: SireumSpec = this

    val graph = Graph.empty[Z, Unit]

    * {
      val n1 = Z.random
      val n2 = Z.random
      val g = graph.add(n1 ~> n2)
      g.incoming(n1).isEmpty && g.outgoing(n1).elements.exists(e => e.source == n1 && e.dest == n2) &&
        g.outgoing(n2).isEmpty && g.incoming(n1) == g.outgoing(1)
    }

    * {
      val n1 = Z.random
      val n2 = Z.random
      var g = graph.add(n1 ~> n2)
      g = g.add(n2 ~> n1)
      g.incoming(n1).nonEmpty && g.outgoing(n1).elements.exists(e => e.source == n1 && e.dest == n2) &&
        g.outgoing(n2).nonEmpty && g.incoming(n1) == g.outgoing(n2) && g.outgoing(n1) == g.incoming(n2)
    }
  }
}
