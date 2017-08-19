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
import org.sireum.test.SireumSpec._

import scala.collection.mutable.{BitSet => BS}
import U16._
import N16._
import spire.math.Real

class STest extends SireumRuntimeSpec {

  "IS" - {

    *(ISZ[Z](z"1", z"2", z"3") equiv IS[Z, Z](z"1", z"2", z"3"))

    *(ISU16[U16](u16"-1", u16"-2", u16"-3") equiv IS[U16, U16](u16"-1", u16"-2", u16"-3"))

    *(ISZ[Z](z"1", z"2", z"3") != ISZ[U16](u16"-1", u16"-2", u16"-3"))

    "ISZ[B]" - {

      val empty = ISZ[B]()

      *(empty.data.isInstanceOf[scala.Array[scala.Any]])

      *(empty.data.asInstanceOf[scala.Array[scala.Any]].length equiv 0)

      *((empty :+ T).data.isInstanceOf[BS])

      *((ISZ[B](T, F, T, T, F) ++ ISZ[B](T, T, F, F, T)).toString equiv "[B64]")

    }

    "ISZ[String]" - {

      *(ISZ[String]("A", "B", "C\nD").toString equiv "[\"A\", \"B\", \"C\\nD\"]")

    }

    "ISZ[C]" - {

      *(ISZ[C]('A', 'B', '\u0010').toString equiv "['A', 'B', '\\020']")

    }

    "ISZ[Z]" - {

      *(ISZ[Z](1, 2, 3).data equiv scala.Array[scala.Any](Z.MP.Long(1), Z.MP.Long(2), Z.MP.Long(3)))

      *(ISZ[Z](z"10000000000000000000", z"20000000000000000000", z"30000000000000000000").data equiv
        scala.Array[scala.Any](scala.BigInt("10000000000000000000"),
          scala.BigInt("20000000000000000000"), scala.BigInt("30000000000000000000")))

      *(ISZ[Z](z"10000000000000000000", z"20000000000000000000", z"30000000000000000000")(1) equiv
        z"20000000000000000000")
    }

    "ISZ[U16]" - {

      *(ISZ[U16](u16"1", u16"2", u16"3").data equiv scala.Array[scala.Short](1, 2, 3))

      *(ISZ[U16](u16"1", u16"2", u16"3")(1) equiv U16(2))

    }

    "ISZ[N16]" - {

      *(ISZ[N16](n16"1", n16"2", n16"3").data equiv scala.Array[scala.Int](1, 2, 3))

      *(ISZ[N16](n16"1", n16"2", n16"3")(1) equiv N16(2))

    }

    "ISZ[R]" - {

      *(ISZ[R](r"1", r"2", r"3").data equiv scala.Array[Real](Real(1), Real(2), Real(3)))

      *(ISZ[R](r"1", r"2", r"3")(1) equiv R(2))

    }

  }
}