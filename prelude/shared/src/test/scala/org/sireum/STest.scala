/*
 * Copyright (c) 2017, Robby, Kansas State University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum

import org.sireum.test.SireumRuntimeSpec

import ISZOps._
import ISZBOps._

class STest extends SireumRuntimeSpec {
  "MS[Z8, B]" - {

    * {
      val s1 = collection._MS[Z8, B](T, T)
      val s2 = collection._MS.create[Z8, B](z8"2", T)
      s1(0) == s2(0) && s1(1) == s2(1)
    }

    * {
      val s1 = collection._MS[Z8, B](F, T)
      val s2 = collection._MS.create[Z8, B](z8"2", T)
      s2(z8"0") = false
      s1 == s2
    }

  }

  "IS[U8, B]" - {

    * {
      val s1 = collection._MS[U8, B](T, T)
      val s2 = collection._MS.create[U8, B](u8"2", T)
      s1(0) == s2(0) && s1(1) == s2(1)
    }

    * {
      val s1 = collection._MS[U8, B](F, T)
      val s2 = collection._MS.create[U8, B](u8"2", T)
      s2(z8"0") = F
      s1 == s2
    }

  }

  "ISZOps" - {
    val s = ISZ[Z](Z.random, Z.random, Z.random, Z.random)

    *(ISZOps(s).contains(s(2)))

    *(ISZOps(s :+ 1).forall(_ >= 1))

    *(ISZOps(s :+ 2).exists(_ == 2))

    *(ISZOps(s) :+ 3 == s :+ 3)

    *(1 +: ISZOps(s) == 1 +: s)

    *(ISZOps(s) ++ s == s ++ s)

    *(ISZOps(s).toMS == MSZ(s.elements: _*))

    * {
      s == ISZOps(ISZOps(s).chunk(2)).
        foldLeft((r: ISZ[Z], t: ISZ[Z]) => r ++ t, ISZ[Z]())
    }
  }
}
