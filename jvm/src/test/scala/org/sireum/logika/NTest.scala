/*
 * Copyright (c) 2016, Robby, Kansas State University
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

package org.sireum.logika

import org.sireum.logika.test.LogikaSpec

class NTest extends LogikaSpec {
  final val n5 = N("5")
  final val bigVal = "10000000000000000000000000000000000000000000000000000000000"
  final val nBig = N(bigVal)
  final val size = 1024

  "eqs" - {
    * (true)
    * (!(5 == n5)) // does not support Int == Z
    * (5 != n5) // does not support Int != Z
    * (n5 == 5)
    * (n5 == java.lang.Byte.valueOf("5"))
    * (n5 == new java.lang.Character(5.toChar))
    * (n5 == java.lang.Short.valueOf("5"))
    * (n5 == java.lang.Integer.valueOf("5"))
    * (n5 == java.lang.Long.valueOf("5"))
    * (n5 == new java.math.BigInteger("5"))
    * (n5 == BigInt("5"))
    * (nBig == new java.math.BigInteger(bigVal))
    * (nBig == BigInt(bigVal))
  }

  "comps" - {
    * (n5 < n"6")
    * (n5 <= n"6")
    * (n5 <= n"5")
    * (!(n5 < n"5"))
    * (!(n5 <= n"4"))
    * (n5 > n"4")
    * (n5 >= n"4")
    * (!(n5 > n"5"))
    * (!(n5 >= n"6"))

    val zBig1 = nBig + n"1"
    val zBigM1 = nBig - n"1"

    * (nBig < zBig1)
    * (nBig <= zBig1)
    * (nBig <= nBig)
    * (!(nBig < nBig))
    * (!(nBig <= zBigM1))
    * (nBig > zBigM1)
    * (nBig >= zBigM1)
    * (!(nBig > nBig))
    * (!(nBig >= zBig1))
  }

  "randomOps" - {
    type I = BigInt
    val add = ("+", (n1: N, n2: N) => n1 + n2, (i1: I, i2: I) => i1 + i2)
    val sub = ("-", (n1: N, n2: N) => n1 - n2, (i1: I, i2: I) => i1 - i2)
    val mul = ("*", (n1: N, n2: N) => n1 * n2, (i1: I, i2: I) => i1 * i2)
    val zero = BigInt(0)
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(add, sub, mul)) {
        lazy val n1 = N(randomInt().toBigInt.abs)
        lazy val n2 = N(randomInt().toBigInt.abs)
        lazy val nr = nop(n1, n2)
        lazy val ir = zero.max(iop(n1.toBigInt, n2.toBigInt))

        * (nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }

    val div = ("/", (n1: N, n2: N) => n1 / n2, (i1: I, i2: I) => i1 / i2)
    val rem = ("%", (n1: N, n2: N) => n1 % n2, (i1: I, i2: I) => i1 % i2)
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(div, rem)) {
        lazy val n1 = N(randomInt().toBigInt.abs)
        lazy val n2 = {
          var r = N(randomInt().toBigInt.abs)

          while (r == math.N.zero) {
            r = N(randomInt().toBigInt.abs)
          }
          r
        }
        lazy val nr = nop(n1, n2)
        lazy val ir = zero.max(iop(n1.toBigInt, n2.toBigInt))
        * (nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }

    val eq = ("==", (n1: N, n2: N) => n1 == n2, (i1: I, i2: I) => i1 == i2)
    val ne = ("!=", (n1: N, n2: N) => n1 != n2, (i1: I, i2: I) => i1 != i2)
    val gt = (">", (n1: N, n2: N) => n1 > n2, (i1: I, i2: I) => B(i1 > i2))
    val ge = (">=", (n1: N, n2: N) => n1 >= n2, (i1: I, i2: I) => B(i1 >= i2))
    val lt = ("<", (n1: N, n2: N) => n1 < n2, (i1: I, i2: I) => B(i1 < i2))
    val le = ("<=", (n1: N, n2: N) => n1 <= n2, (i1: I, i2: I) => B(i1 <= i2))
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(eq, ne, gt, ge, lt, le)) {
        lazy val n1 = N(randomInt().toBigInt.abs)
        lazy val n2 = N(randomInt().toBigInt.abs)
        lazy val nr = nop(n1, n2)
        lazy val ir = iop(n1.toBigInt, n2.toBigInt)

        * (nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }
  }
}
