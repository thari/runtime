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

import org.junit.Test


class NTest {
  final val n5 = N("5")
  final val bigVal = "10000000000000000000000000000000000000000000000000000000000"
  final val nBig = N(bigVal)
  final val size = 1024

  @Test
  def eqs(): Unit = {
    assert(!(5 == n5)) // does not support Int == Z
    assert(5 != n5) // does not support Int != Z
    assert(n5 == 5)
    assert(n5 == java.lang.Byte.valueOf("5"))
    assert(n5 == new java.lang.Character(5.toChar))
    assert(n5 == java.lang.Short.valueOf("5"))
    assert(n5 == java.lang.Integer.valueOf("5"))
    assert(n5 == java.lang.Long.valueOf("5"))
    assert(n5 == new java.math.BigInteger("5"))
    assert(n5 == BigInt("5"))
    assert(nBig == new java.math.BigInteger(bigVal))
    assert(nBig == BigInt(bigVal))
  }

  @Test
  def comps(): Unit = {
    assert(n5 < 6)
    assert(n5 <= 6)
    assert(n5 <= 5)
    assert(!(n5 < 5))
    assert(!(n5 <= 4))
    assert(n5 > 4)
    assert(n5 >= 4)
    assert(!(n5 > 5))
    assert(!(n5 >= 6))

    val zBig1 = nBig + 1
    val zBigM1 = nBig - 1
    assert(nBig < zBig1)
    assert(nBig <= zBig1)
    assert(nBig <= nBig)
    assert(!(nBig < nBig))
    assert(!(nBig <= zBigM1))
    assert(nBig > zBigM1)
    assert(nBig >= zBigM1)
    assert(!(nBig > nBig))
    assert(!(nBig >= zBig1))
  }

  @Test
  def randomOps(): Unit = {
    type I = BigInt
    val add = ("+", (n1: N, n2: N) => n1 + n2, (i1: I, i2: I) => i1 + i2)
    val sub = ("-", (n1: N, n2: N) => n1 - n2, (i1: I, i2: I) => i1 - i2)
    val mul = ("*", (n1: N, n2: N) => n1 * n2, (i1: I, i2: I) => i1 * i2)
    val zero = BigInt(0)
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(add, sub, mul)) {
        val n1 = N(randomInt().toBigInt.abs)
        val n2 = N(randomInt().toBigInt.abs)
        val nr = nop(n1, n2)
        val ir = zero.max(iop(n1.toBigInt, n2.toBigInt))
        assert(nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }

    val div = ("/", (n1: N, n2: N) => n1 / n2, (i1: I, i2: I) => i1 / i2)
    val rem = ("%", (n1: N, n2: N) => n1 % n2, (i1: I, i2: I) => i1 % i2)
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(div, rem)) {
        val n1 = N(randomInt().toBigInt.abs)
        var n2 = N(randomInt().toBigInt.abs)
        while (n2 == math.N.zero) {
          n2 = N(randomInt().toBigInt.abs)
        }
        val nr = nop(n1, n2)
        val ir = zero.max(iop(n1.toBigInt, n2.toBigInt))
        assert(nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }

    val eq = ("==", (n1: N, n2: N) => n1 == n2, (i1: I, i2: I) => i1 == i2)
    val ne = ("!=", (n1: N, n2: N) => n1 != n2, (i1: I, i2: I) => i1 != i2)
    val gt = (">", (n1: N, n2: N) => n1 > n2, (i1: I, i2: I) => i1 > i2)
    val ge = (">=", (n1: N, n2: N) => n1 >= n2, (i1: I, i2: I) => i1 >= i2)
    val lt = ("<", (n1: N, n2: N) => n1 < n2, (i1: I, i2: I) => i1 < i2)
    val le = ("<=", (n1: N, n2: N) => n1 <= n2, (i1: I, i2: I) => i1 <= i2)
    for (i <- 0 until size)
      for ((op, nop, iop) <- Seq(eq, ne, gt, ge, lt, le)) {
        val n1 = N(randomInt().toBigInt.abs)
        val n2 = N(randomInt().toBigInt.abs)
        val nr = nop(n1, n2)
        val ir = iop(n1.toBigInt, n2.toBigInt)
        assert(nr == ir, s"$n1 $op $n2 ($nr != $ir)")
      }
  }
}
