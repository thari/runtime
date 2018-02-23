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

import scala.util.{Failure, Success, Try}

class RangeTest extends TestSuite {

  val numOfRandomTests = 64

  val tests = Tests {

    "N" - {

      * - assert(N.hasMin)

      * - assert(!N.hasMax)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          assert(N.random >= N(0))
        }
      }

    }

    "One10i" - {

      import One10i._

      * - assert(!One10i.isSigned)

      * - assert(!One10i.isBitVector)

      * - assert(One10i.hasMin)

      * - assert(One10i.hasMax)

      * - assert(One10i.Index =~= one10i"1")

      * - assert(One10i.Min =~= one10i"1")

      * - assert(One10i.Max =~= one10i"10")

      * - assert(One10i.Name =~= "One10i")

      val x = one10i"10"

      * - assert(x.toIndex =~= z"9")

      * - assert(!x.isSigned)

      * - assert(!x.isBitVector)

      * - assert(x.hasMin)

      * - assert(x.hasMax)

      * - assert(x.Index =~= one10i"1")

      * - assert(x.Min =~= one10i"1")

      * - assert(x.Max =~= one10i"10")

      * - assert(x.Name =~= "One10i")

      * - assert(x.value =~= z"10")

      * - assert(x - one10i"9" =~= One10i.Min)

      * - assert(x.decrease.increase =~= x)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          val v = One10i.random
          assert(One10i.Min <= v)
          assert(v <= One10i.Max)
        }
      }

      val random = new _root_.java.util.Random

      def rand(): Int = random.nextInt(10) + 1

      * - {
        for ((op, op1, op2) <- List[(Predef.String,
                                     One10i => One10i => One10i,
                                     Int => Int => Int)](("+", _.+, _.+),
                                                         ("-", _.-, _.-),
                                                         ("*", _.*, _.*),
                                                         ("/", _./, _./),
                                                         ("%", _.%, _.%))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            var m = rand()
            while (m == 0 && (op == "/" || op == "%")) m = rand()
            Try(op1(One10i(n))(One10i(m)).toBigInt.toInt) match {
              case Success(r) => assert(r =~= op2(n)(m))
              case Failure(_) =>
                val ir = op2(n)(m)
                assert(
                  !(One10i.Min.toBigInt <= ir && ir <= One10i.Max.toBigInt))
            }
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    One10i => One10i => B,
                                    Int => Int => scala.Boolean)](
               (">", _.>, _.>),
               (">=", _.>=, _.>=),
               ("<", _.<, _.<),
               ("<=", _.<=, _.<=),
               ("==", _.==, _.==),
               ("!=", _.!=, _.!=))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            assert(op1(One10i(n))(One10i(m)).value =~= op2(n)(m))
          }
        }
      }
    }

    "M1_16" - {

      import M1_16._

      * - assert(M1_16.isSigned)

      * - assert(!M1_16.isBitVector)

      * - assert(M1_16.hasMin)

      * - assert(M1_16.hasMax)

      * - assert(M1_16.Index =~= m1_16"0")

      * - assert(M1_16.Min =~= m1_16"-1")

      * - assert(M1_16.Max =~= m1_16"16")

      * - assert(M1_16.Name =~= "M1_16")

      val x = m1_16"10"

      * - assert(x.toIndex =~= z"10")

      * - assert(x.isSigned)

      * - assert(x.Index =~= m1_16"0")

      * - assert(x.Min =~= m1_16"-1")

      * - assert(x.Max =~= m1_16"16")

      * - assert(x.Name =~= "M1_16")

      * - assert(x.value =~= z"10")

      * - assert(x - m1_16"11" =~= M1_16.Min)

      * - assert(x.decrease.increase =~= x)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          val v = M1_16.random
          assert(M1_16.Min <= v)
          assert(v <= M1_16.Max)
        }
      }

      val random = new _root_.java.util.Random

      def rand(): Int = random.nextInt(18) - 1

      * - {
        for ((op, op1, op2) <- List[(Predef.String,
                                     M1_16 => M1_16 => M1_16,
                                     Int => Int => Int)](("+", _.+, _.+),
                                                         ("-", _.-, _.-),
                                                         ("*", _.*, _.*),
                                                         ("/", _./, _./),
                                                         ("%", _.%, _.%))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            var m = rand()
            while (m == 0 && (op == "/" || op == "%")) m = rand()
            Try(op1(M1_16(n))(M1_16(m)).toBigInt.toInt) match {
              case Success(r) => assert(r =~= op2(n)(m))
              case Failure(_) =>
                val ir = op2(n)(m)
                assert(!(M1_16.Min.toBigInt <= ir && ir <= M1_16.Max.toBigInt))
            }
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    M1_16 => M1_16 => B,
                                    Int => Int => scala.Boolean)](
               (">", _.>, _.>),
               (">=", _.>=, _.>=),
               ("<", _.<, _.<),
               ("<=", _.<=, _.<=),
               ("==", _.==, _.==),
               ("!=", _.!=, _.!=))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            assert(op1(M1_16(n))(M1_16(m)).value =~= op2(n)(m))
          }
        }
      }
    }
  }
}
