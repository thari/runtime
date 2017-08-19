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

package org.sireumproto

import org.sireum.test.SireumRuntimeSpec
import scala.collection.mutable.{BitSet => BS}

class ISTest extends SireumRuntimeSpec {

  "ISZ[B]" - {

    val empty = ISZ[B]()

    *(empty.data.isInstanceOf[Array[scala.Any]])

    *(empty.data.asInstanceOf[Array[scala.Any]].length == 0)

    *((empty :+ T).data.isInstanceOf[BS])

    *((ISZ[B](T, F, T, T, F) ++ ISZ[B](T, T, F, F, T)).toString == "[B64]")

  }

  "ISZ[String]" - {

    *(ISZ[String]("A", "B", "C\nD").toString == "[\"A\", \"B\", \"C\\nD\"]")

  }

  "ISZ[C]" - {

    *(ISZ[C]('A', 'B', '\u0010').toString == "['A', 'B', '\\u0010']")

  }

}
