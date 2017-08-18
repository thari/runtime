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
import spire.math._

import scala.util.{Failure, Success, Try}

@bits(signed = T, width = 16, min = -2, index = true) class S16_m2

class BitsTest extends SireumRuntimeSpec {

  val numOfRandomTests = 64

  "U16" - {

    import U16._

    *(!U16.isSigned)

    *(U16.isBitVector)

    *(U16.hasMin)

    *(U16.hasMax)

    *(U16.isWrapped)

    *(U16.BitWidth == 16)

    *(U16.Index == u16"0")

    *(U16.Min == u16"0")

    *(U16.Max == u16"65535")

    *(U16.Name == "U16")

    val x = u16"14"

    *(x.toIndex == z"14")

    *(!x.isSigned)

    *(x.isBitVector)

    *(x.hasMin)

    *(x.hasMax)

    *(x.isWrapped)

    *(x.BitWidth == 16)

    *(x.Index == u16"0")

    *(x.Min == u16"0")

    *(x.Max == u16"65535")

    *(x.Name == "U16")

    *(x.value == 14l)

    *(x - u16"15" == U16.Max)

    *(x + U16.Max == x.decrease)

    for (_ <- 0 until numOfRandomTests) {
      *("random"){
        val v = U16.random(System.currentTimeMillis)
        U16.Min <= v && v <= U16.Max
      }
    }

    val random = new java.util.Random
    def rand(): Short = random.nextInt.toShort

    for ((op, op1, op2) <- List[(Predef.String, U16 => Z => U16, UShort => UShort => UShort)](
      ("+", _.+, _.+), ("-", _.-, _.-), ("*", _.*, _.*), ("/", _./, _./), ("%", _.%, _.%))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        var m = rand()
        while (m == 0 && (op == "/" || op == "%")) m = rand()
        val un = UShort(n)
        val um = UShort(m)
        *(s"$un $op $um")(op1(U16(n))(U16(m)).toBigInt == op2(un)(um).toBigInt)
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, U16 => Z => U16, UShort => Int => UShort)](
      (">>>", _.>>>, _.>>>), ("<<", _.<<, _.<<))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        val un = UShort(n)
        val um = m.toInt
        *(s"$un $op $um")(op1(U16(n))(U16(m)).toBigInt == op2(un)(um).toBigInt)
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, U16 => Z => B, UShort => UShort => scala.Boolean)](
      (">", _.>, _.>), (">=", _.>=, _.>=), ("<", _.<, _.<), ("<=", _.<=, _.<=), ("==", _.==, _.==), ("!=", _.!=, _.!=))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        val un = UShort(n)
        val um = UShort(m)
        *(s"$un $op $um")(op1(U16(n))(U16(m)).value == op2(un)(um))
      }
    }

  }

  "S16_m2" - {

    import S16_m2._

    *(S16_m2.isSigned)

    *(S16_m2.isBitVector)

    *(S16_m2.hasMin)

    *(S16_m2.hasMax)

    *(!S16_m2.isWrapped)

    *(S16_m2.BitWidth == 16)

    *(S16_m2.Index == s16_m2"-2")

    *(S16_m2.Min == s16_m2"-2")

    *(S16_m2.Max == s16_m2"32767")

    *(S16_m2.Name == "S16_m2")

    val x = s16_m2"14"

    *(x.toIndex == z"16")

    *(x.isSigned)

    *(x.isBitVector)

    *(x.hasMin)

    *(x.hasMax)

    *(!x.isWrapped)

    *(x.BitWidth == 16)

    *(x.Index == s16_m2"-2")

    *(x.Min == s16_m2"-2")

    *(x.Max == s16_m2"32767")

    *(x.Name == "S16_m2")

    *(x.value == 14l)

    *(x - s16_m2"15" == s16_m2"-1")

    *(Try(x + S16_m2.Max).isFailure)

    for (_ <- 0 until numOfRandomTests) {
      *("random"){
        val v = S16_m2.random(System.currentTimeMillis)
        S16_m2.Min <= v && v <= S16_m2.Max
      }
    }

    val random = new java.util.Random
    def rand(): Short = (random.nextInt(16 + 3) - 2).toShort

    for ((op, op1, op2) <- List[(Predef.String, S16_m2 => Z => S16_m2, Short => Short => Int)](
      ("+", _.+, _.+), ("-", _.-, _.-), ("*", _.*, _.*), ("/", _./, _./), ("%", _.%, _.%))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        var m = rand()
        while (m == 0 && (op == "/" || op == "%")) m = rand()
        *(s"$n $op $m") {
          val br = op2(n)(m).toShort.toInt
          Try(op1(S16_m2(n))(S16_m2(m)).toBigInt.toShort) match {
            case Success(r) => r == br
            case Failure(_) => !(S16_m2.Min.toBigInt <= br && br <= S16_m2.Max.toBigInt)
          }
        }
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, S16_m2 => Z => S16_m2, Short => Int => Int)](
      (">>", _.>>, _.>>), (">>>", _.>>>, _.>>>), ("*", _.<<, _.<<))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        *(s"$n $op $m") {
          val br = op2(n)(m).toShort.toInt
          Try(op1(S16_m2(n))(S16_m2(m)).toBigInt.toShort) match {
            case Success(r) => r == br
            case Failure(_) => !(S16_m2.Min.toBigInt <= br && br <= S16_m2.Max.toBigInt)
          }
        }
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, S16_m2 => Z => B, Short => Short => scala.Boolean)](
      (">", _.>, _.>), (">=", _.>=, _.>=), ("<", _.<, _.<), ("<=", _.<=, _.<=), ("==", _.==, _.==), ("!=", _.!=, _.!=))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        *(s"$n $op $m")(op1(S16_m2(n))(S16_m2(m)).value == op2(n)(m))
      }
    }

  }
}
