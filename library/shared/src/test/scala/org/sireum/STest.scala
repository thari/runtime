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

package org.sireum

import org.sireum.test._
import org.sireum.ops.ISZOps
import scala.collection.mutable.{BitSet => BS}
import U16._
import N16._
import Z8._
import U8._

class STest extends TestSuite {

  val tests = Tests {
    "IS" - {

      * - assert(ISZ[Z](z"1", z"2", z"3") =~= IS[Z, Z](z"1", z"2", z"3"))

      * - assert(
        ISZ[Z](z"1", z"2", z"3") !~= ISZ[U16](u16"-1", u16"-2", u16"-3"))

      "ISZ[B]" - {

        val empty = ISZ[B]()

        * - assert(empty.data.isInstanceOf[scala.Array[scala.Any]])

        * - assert(empty.data.asInstanceOf[scala.Array[scala.Any]].length =~= 0)

        * - assert((empty :+ T).data.isInstanceOf[BS])

        * - assert(
          (ISZ[B](T, F, T, T, F) ++ ISZ[B](T, T, F, F, T)).toString =~= "[B64]")

      }

      "ISZ[String]" - {

        * - assert(
          ISZ[String]("A", "B", "C\nD").toString =~= "[\"A\", \"B\", \"C\\nD\"]")

      }

      "ISZ[C]" - {

        * - assert(
          ISZ[C]('A', 'B', '\u0010').toString =~= "['A', 'B', '\\020']")

      }

      "ISZ[Z]" - {

        * - assert(
          ISZ[Z](1, 2, 3).data =~= scala
            .Array[scala.Any](Z.MP.Long(1), Z.MP.Long(2), Z.MP.Long(3)))

        * - assert(
          ISZ[Z](z"10000000000000000000",
                 z"20000000000000000000",
                 z"30000000000000000000").data =~=
            scala.Array[scala.Any](scala.BigInt("10000000000000000000"),
                                   scala.BigInt("20000000000000000000"),
                                   scala.BigInt("30000000000000000000")))

        * - assert(
          ISZ[Z](z"10000000000000000000",
                 z"20000000000000000000",
                 z"30000000000000000000")(1) =~=
            z"20000000000000000000")
      }

      "ISZ[U16]" - {

        * - assert(
          ISZ[U16](u16"1", u16"2", u16"3").data =~= scala
            .Array[scala.Short](1, 2, 3))

        * - assert(ISZ[U16](u16"1", u16"2", u16"3")(1) =~= U16(2))

      }

      "ISZ[N16]" - {

        * - assert(
          ISZ[N16](n16"1", n16"2", n16"3").data =~= scala
            .Array[scala.Int](1, 2, 3))

        * - assert(ISZ[N16](n16"1", n16"2", n16"3")(1) =~= N16(2))

      }

      "ISZ[R]" - {

        * - assert(
          ISZ[R](r"1", r"2", r"3").data =~= scala
            .Array[BigDecimal](BigDecimal(1), BigDecimal(2), BigDecimal(3)))

        * - assert(ISZ[R](r"1", r"2", r"3")(1) =~= R(2))

      }

    }

    "MS" - {

      "MS[Z8, B]" - {

        * - {
          val s1 = MS[Z8, B](T, T)
          val s2 = MS.create[Z8, B](z8"2", T)
          assert(s1(z8"0") =~= s2(z8"0"))
          assert(s1(z8"1") =~= s2(z8"1"))
        }

        * - {
          val s1 = MS[Z8, B](F, T)
          val s2 = MS.create[Z8, B](z8"2", T)
          s2(z8"0") = false
          assert(s1 =~= s2)
        }
      }

      "MS[U8, B]" - {

        * - {
          val s1 = MS[U8, B](T, T)
          val s2 = MS.create[U8, B](u8"2", T)
          assert(s1(u8"0") =~= s2(u8"0"))
          assert(s1(u8"1") =~= s2(u8"1"))
        }

        * - {
          val s1 = MS[U8, B](F, T)
          val s2 = MS.create[U8, B](u8"2", T)
          s2(u8"0") = F
          assert(s1 =~= s2)
        }

      }
    }

    "ISZOps" - {

      val s = ISZ[Z](Z.random, Z.random, Z.random, Z.random)

      * - assert(ISZOps(s).contains(s(2)))

      * - assert(ISZOps(ISZOps(s :+ 1).drop(4)).forall(_ >= z"1"))

      * - assert(ISZOps(s :+ 2).exists(_ == z"2"))

      * - assert(ISZOps(s) :+ 3 =~= s :+ 3)

      * - assert(1 +: ISZOps(s) =~= 1 +: s)

      * - assert(ISZOps(s) ++ s =~= s ++ s)

      * - assert(s.toMS =~= MSZ(s.elements: _*))

      * - {
        assert(
          s =~= ISZOps(ISZOps(s).chunk(2))
            .foldLeft((r: ISZ[Z], t: ISZ[Z]) => r ++ t, ISZ[Z]()))
      }

    }
  }
}
