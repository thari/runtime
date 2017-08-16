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

@range(min = 1, max = 10, index = T) class One10i

@range(min = -1, max = 16) class M1_16

class RangeTest extends SireumRuntimeSpec {

  val numOfRandomTests = 64

  "N" - {

    *(N.hasMin)

    *(!N.hasMax)

    for (_ <- 0 until numOfRandomTests) {
      *("random")(N.random >= N(0))
    }

  }

  "One10i" - {

    import One10i._

    *(!One10i.isSigned)

    *(!One10i.isBitVector)

    *(One10i.hasMin)

    *(One10i.hasMax)

    *(One10i.Index == one10i"1")

    *(One10i.Min == one10i"1")

    *(One10i.Max == one10i"10")

    *(One10i.Name == "One10i")

    val x = one10i"10"

    *(x.toIndex == z"9")

    *(!x.isSigned)

    *(!x.isBitVector)

    *(x.hasMin)

    *(x.hasMax)

    *(x.Index == one10i"1")

    *(x.Min == one10i"1")

    *(x.Max == one10i"10")

    *(x.Name == "One10i")

    *(x.value == z"10")

    *(x - one10i"9" == One10i.Min)

    *(x.decrease.increase == x)

    for (_ <- 0 until numOfRandomTests) {
      *("random"){
        val v = One10i.random
        val r = One10i.Min <= v && v <= One10i.Max
        if (!r) println(s"One10i.random = $v")
        r
      }
    }

    val random = new java.util.Random

    def rand(): Int = random.nextInt(10) + 1

    for ((op, op1, op2) <- List[(Predef.String, One10i => Z => One10i, Int => Int => Int)](
      ("+", _.+, _.+), ("-", _.-, _.-), ("*", _.*, _.*), ("/", _./, _./), ("%", _.%, _.%))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        var m = rand()
        while (m == 0 && (op == "/" || op == "%")) m = rand()
        *(s"$n $op $m")(Try(op1(One10i(n))(One10i(m)).toBigInt.toInt) match {
          case Success(r) => r == op2(n)(m)
          case Failure(_) =>
            val ir = op2(n)(m)
            !(One10i.Min.toBigInt <= ir && ir <= One10i.Max.toBigInt)
        })
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, One10i => Z => B, Int => Int => scala.Boolean)](
      (">", _.>, _.>), (">=", _.>=, _.>=), ("<", _.<, _.<), ("<=", _.<=, _.<=), ("==", _.==, _.==), ("!=", _.!=, _.!=))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        *(s"$n $op $m")(op1(One10i(n))(One10i(m)).value == op2(n)(m))
      }
    }
  }

  "M1_16" - {

    import M1_16._

    *(M1_16.isSigned)

    *(!M1_16.isBitVector)

    *(M1_16.hasMin)

    *(M1_16.hasMax)

    *(M1_16.Index == m1_16"0")

    *(M1_16.Min == m1_16"-1")

    *(M1_16.Max == m1_16"16")

    *(M1_16.Name == "M1_16")

    val x = m1_16"10"

    *(x.toIndex == z"10")

    *(x.isSigned)

    *(x.Index == m1_16"0")

    *(x.Min == m1_16"-1")

    *(x.Max == m1_16"16")

    *(x.Name == "M1_16")

    *(x.value == z"10")

    *(x - m1_16"11" == M1_16.Min)

    *(x.decrease.increase == x)

    for (_ <- 0 until numOfRandomTests) {
      *("random"){
        val v = M1_16.random
        M1_16.Min <= v && v <= M1_16.Max
      }
    }

    val random = new java.util.Random

    def rand(): Int = random.nextInt(18) - 1

    for ((op, op1, op2) <- List[(Predef.String, M1_16 => Z => M1_16, Int => Int => Int)](
      ("+", _.+, _.+), ("-", _.-, _.-), ("*", _.*, _.*), ("/", _./, _./), ("%", _.%, _.%))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        var m = rand()
        while (m == 0 && (op == "/" || op == "%")) m = rand()
        *(s"$n $op $m")(Try(op1(M1_16(n))(M1_16(m)).toBigInt.toInt) match {
          case Success(r) => r == op2(n)(m)
          case Failure(_) =>
            val ir = op2(n)(m)
            !(M1_16.Min.toBigInt <= ir && ir <= M1_16.Max.toBigInt)
        })
      }
    }

    for ((op, op1, op2) <- List[(Predef.String, M1_16 => Z => B, Int => Int => scala.Boolean)](
      (">", _.>, _.>), (">=", _.>=, _.>=), ("<", _.<, _.<), ("<=", _.<=, _.<=), ("==", _.==, _.==), ("!=", _.!=, _.!=))) {
      for (_ <- 0 until numOfRandomTests) {
        val n = rand()
        val m = rand()
        *(s"$n $op $m")(op1(M1_16(n))(M1_16(m)).value == op2(n)(m))
      }
    }
  }
}
