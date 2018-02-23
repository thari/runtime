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
import spire.math._
import scala.util.{Failure, Success, Try}

class BitsTest extends TestSuite {

  val numOfRandomTests = 64

  val tests = Tests {

    "S8" - {

      import S8._

      * - assert(S8.isSigned)

      * - assert(S8.isBitVector)

      * - assert(S8.hasMin)

      * - assert(S8.hasMax)

      * - assert(S8.isWrapped)

      * - assert(S8.BitWidth =~= 8)

      * - assert(S8.Index =~= s8"0")

      * - assert(S8.Min =~= s8"-128")

      * - assert(S8.Max =~= s8"127")

      * - assert(S8.Name =~= "S8")

      val x = s8"-114"

      * - assert(x.toIndex =~= z"-114")

      * - assert(x.isSigned)

      * - assert(x.isBitVector)

      * - assert(x.hasMin)

      * - assert(x.hasMax)

      * - assert(x.isWrapped)

      * - assert(x.BitWidth =~= 8)

      * - assert(x.Index =~= s8"0")

      * - assert(x.Min =~= s8"-128")

      * - assert(x.Max =~= s8"127")

      * - assert(x.Name =~= "S8")

      * - assert(x.value =~= -114)

      * - assert(x - s8"15" =~= S8.Max)

      * - assert(x + S8.Min * s8"-2" =~= x)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          val v = S8.random
          assert(S8.Min <= v)
          assert(v <= S8.Max)
        }
      }

      val random = new _root_.java.util.Random

      def rand(): Byte = random.nextInt.toByte

      * - {
        for ((op, op1, op2) <- List[(Predef.String,
                                     S8 => S8 => S8,
                                     Byte => Byte => Int)](("+", _.+, _.+),
                                                           ("-", _.-, _.-),
                                                           ("*", _.*, _.*),
                                                           ("/", _./, _./),
                                                           ("%", _.%, _.%))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            var m = rand()
            while (m == 0 && (op == "/" || op == "%")) m = rand()
            assert(
              op1(S8(n))(S8(m)).toBigInt =~= scala.BigInt(op2(n)(m).toByte))
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    S8 => S8 => S8,
                                    Byte => Int => Int)]((">>", _.>>, _.>>),
                                                         (">>>", _.>>>, _.>>>),
                                                         ("<<", _.<<, _.<<))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            assert(
              op1(S8(n))(S8(m)).toBigInt =~= scala.BigInt(
                op2(n)(m.toInt).toByte))
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    S8 => S8 => B,
                                    Byte => Byte => scala.Boolean)](
               (">", _.>, _.>),
               (">=", _.>=, _.>=),
               ("<", _.<, _.<),
               ("<=", _.<=, _.<=),
               ("==", _.==, _.==),
               ("!=", _.!=, _.!=))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            assert(op1(S8(n))(S8(m)).value =~= op2(n)(m))
          }
        }
      }

    }

    "U16" - {

      import U16._

      * - assert(!U16.isSigned)

      * - assert(U16.isBitVector)

      * - assert(U16.hasMin)

      * - assert(U16.hasMax)

      * - assert(U16.isWrapped)

      * - assert(U16.BitWidth =~= 16)

      * - assert(U16.Index =~= u16"0")

      * - assert(U16.Min =~= u16"0")

      * - assert(U16.Max =~= u16"65535")

      * - assert(U16.Name =~= "U16")

      val x = u16"14"

      * - assert(x.toIndex =~= z"14")

      * - assert(!x.isSigned)

      * - assert(x.isBitVector)

      * - assert(x.hasMin)

      * - assert(x.hasMax)

      * - assert(x.isWrapped)

      * - assert(x.BitWidth =~= 16)

      * - assert(x.Index =~= u16"0")

      * - assert(x.Min =~= u16"0")

      * - assert(x.Max =~= u16"65535")

      * - assert(x.Name =~= "U16")

      * - assert(x.value =~= 14l)

      * - assert(x - u16"15" =~= U16.Max)

      * - assert(x + U16.Max =~= x.decrease)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          val v = U16.random
          assert(U16.Min <= v)
          assert(v <= U16.Max)
        }
      }

      val random = new _root_.java.util.Random

      def rand(): Short = random.nextInt.toShort

      * - {
        for ((op, op1, op2) <- List[(Predef.String,
                                     U16 => U16 => U16,
                                     UShort => UShort => UShort)](
               ("+", _.+, _.+),
               ("-", _.-, _.-),
               ("*", _.*, _.*),
               ("/", _./, _./),
               ("%", _.%, _.%))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            var m = rand()
            while (m == 0 && (op == "/" || op == "%")) m = rand()
            val un = UShort(n)
            val um = UShort(m)
            assert(op1(U16(n))(U16(m)).toBigInt =~= op2(un)(um).toBigInt)
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    U16 => U16 => U16,
                                    UShort => Int => UShort)](
               (">>>", _.>>>, _.>>>),
               ("<<", _.<<, _.<<))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            val un = UShort(n)
            val um = m.toInt
            assert(op1(U16(n))(U16(m)).toBigInt =~= op2(un)(um).toBigInt)
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    U16 => U16 => B,
                                    UShort => UShort => scala.Boolean)](
               (">", _.>, _.>),
               (">=", _.>=, _.>=),
               ("<", _.<, _.<),
               ("<=", _.<=, _.<=),
               ("==", _.==, _.==),
               ("!=", _.!=, _.!=))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            val un = UShort(n)
            val um = UShort(m)
            assert(op1(U16(n))(U16(m)).value =~= op2(un)(um))
          }
        }
      }

    }

    "S16_m2" - {

      import S16_m2._

      * - assert(S16_m2.isSigned)

      * - assert(S16_m2.isBitVector)

      * - assert(S16_m2.hasMin)

      * - assert(S16_m2.hasMax)

      * - assert(!S16_m2.isWrapped)

      * - assert(S16_m2.BitWidth =~= 16)

      * - assert(S16_m2.Index =~= s16_m2"-2")

      * - assert(S16_m2.Min =~= s16_m2"-2")

      * - assert(S16_m2.Max =~= s16_m2"32767")

      * - assert(S16_m2.Name =~= "S16_m2")

      val x = s16_m2"14"

      * - assert(x.toIndex =~= z"16")

      * - assert(x.isSigned)

      * - assert(x.isBitVector)

      * - assert(x.hasMin)

      * - assert(x.hasMax)

      * - assert(!x.isWrapped)

      * - assert(x.BitWidth =~= 16)

      * - assert(x.Index =~= s16_m2"-2")

      * - assert(x.Min =~= s16_m2"-2")

      * - assert(x.Max =~= s16_m2"32767")

      * - assert(x.Name =~= "S16_m2")

      * - assert(x.value =~= 14l)

      * - assert(x - s16_m2"15" =~= s16_m2"-1")

      * - assert(Try(x + S16_m2.Max).isFailure)

      * - {
        for (_ <- 0 until numOfRandomTests) {
          val v = S16_m2.random
          assert(S16_m2.Min <= v)
          assert(v <= S16_m2.Max)
        }
      }

      val random = new _root_.java.util.Random

      def rand(): Short = (random.nextInt(16 + 3) - 2).toShort

      * - {
        for ((op, op1, op2) <- List[(Predef.String,
                                     S16_m2 => S16_m2 => S16_m2,
                                     Short => Short => Int)](("+", _.+, _.+),
                                                             ("-", _.-, _.-),
                                                             ("*", _.*, _.*),
                                                             ("/", _./, _./),
                                                             ("%", _.%, _.%))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            var m = rand()
            while (m == 0 && (op == "/" || op == "%")) m = rand()
            val br = op2(n)(m).toShort.toInt
            Try(op1(S16_m2(n))(S16_m2(m)).toBigInt.toShort) match {
              case Success(r) => assert(r =~= br)
              case Failure(_) =>
                assert(
                  !(S16_m2.Min.toBigInt <= br && br <= S16_m2.Max.toBigInt))
            }
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    S16_m2 => S16_m2 => S16_m2,
                                    Short => Int => Int)]((">>", _.>>, _.>>),
                                                          (">>>", _.>>>, _.>>>),
                                                          ("*", _.<<, _.<<))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            val br = op2(n)(m).toShort.toInt
            Try(op1(S16_m2(n))(S16_m2(m)).toBigInt.toShort) match {
              case Success(r) => assert(r =~= br)
              case Failure(_) =>
                assert(
                  !(S16_m2.Min.toBigInt <= br && br <= S16_m2.Max.toBigInt))
            }
          }
        }
      }

      * - {
        for ((_, op1, op2) <- List[(Predef.String,
                                    S16_m2 => S16_m2 => B,
                                    Short => Short => scala.Boolean)](
               (">", _.>, _.>),
               (">=", _.>=, _.>=),
               ("<", _.<, _.<),
               ("<=", _.<=, _.<=),
               ("==", _.==, _.==),
               ("!=", _.!=, _.!=))) {
          for (_ <- 0 until numOfRandomTests) {
            val n = rand()
            val m = rand()
            assert(op1(S16_m2(n))(S16_m2(m)).value =~= op2(n)(m))
          }
        }
      }

    }
  }
}
