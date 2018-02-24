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

object UnionFindTest extends TestSuite {

  val tests = Tests {

    * - {

      var uf = UnionFind.create(ISZ(1, 2, 3, 4, 5))
      uf = uf.merge(1, 2)
      uf = uf.merge(3, 4)
      uf = uf.merge(4, 5)
      assert(uf.inSameSet(1, 1))
      assert(uf.inSameSet(2, 2))
      assert(uf.inSameSet(1, 2))
      assert(uf.inSameSet(2, 1))
      assert(uf.inSameSet(3, 3))
      assert(uf.inSameSet(4, 4))
      assert(uf.inSameSet(5, 5))
      assert(uf.inSameSet(3, 4))
      assert(uf.inSameSet(3, 5))
      assert(uf.inSameSet(4, 3))
      assert(uf.inSameSet(4, 5))
      assert(uf.inSameSet(5, 3))
      assert(uf.inSameSet(5, 4))
      assert(!uf.inSameSet(1, 3))
      assert(!uf.inSameSet(2, 3))
      assert(!uf.inSameSet(1, 4))
      assert(!uf.inSameSet(2, 4))
      assert(!uf.inSameSet(1, 5))
      assert(!uf.inSameSet(2, 5))
      assert(!uf.inSameSet(3, 1))
      assert(!uf.inSameSet(3, 2))
      assert(!uf.inSameSet(4, 1))
      assert(!uf.inSameSet(4, 2))
      assert(!uf.inSameSet(5, 1))
      assert(!uf.inSameSet(5, 2))

    }

  }

}
