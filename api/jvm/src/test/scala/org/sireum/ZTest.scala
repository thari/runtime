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

package org.sireum

import org.sireum.test.SireumSpec

import com.github.ghik.silencer.silent

class ZTest extends SireumSpec {
  final val z5 = z"5"
  final val bigVal = "10000000000000000000000000000000000000000000000000000000000"
  final val zBig = z"10000000000000000000000000000000000000000000000000000000000"
  final val size = 1024

  "eqs" - {
    *(!(5 == z5): @silent) // does not support Int == Z
    *(5 != z5: @silent) // does not support Int != Z
    *(z5 == 5: @silent)
    *(z5 == java.lang.Byte.valueOf("5"))
    *(z5 == new java.lang.Character(5.toChar))
    *(z5 == java.lang.Short.valueOf("5"))
    *(z5 == java.lang.Integer.valueOf("5"))
    *(z5 == java.lang.Long.valueOf("5"))
    *(z5 == new java.math.BigInteger("5"))
    *(z5 == BigInt("5"))
    *(zBig == new java.math.BigInteger(bigVal))
    *(zBig == BigInt(bigVal))
  }

  "comps" - {
    *(z5 < 6)
    *(z5 <= 6)
    *(z5 <= 5)
    *(!(z5 < 5))
    *(!(z5 <= 4))
    *(z5 > 4)
    *(z5 >= 4)
    *(!(z5 > 5))
    *(!(z5 >= 6))

    val zBig1 = zBig + 1
    val zBigM1 = zBig - 1

    *(zBig < zBig1)
    *(zBig <= zBig1)
    *(zBig <= zBig)
    *(!(zBig < zBig))
    *(!(zBig <= zBigM1))
    *(zBig > zBigM1)
    *(zBig >= zBigM1)
    *(!(zBig > zBig))
    *(!(zBig >= zBig1))
  }

  "randomOps" - {
    type I = BigInt
    val add = ("+", (z1: Z, z2: Z) => z1 + z2, (i1: I, i2: I) => i1 + i2)
    val sub = ("-", (z1: Z, z2: Z) => z1 - z2, (i1: I, i2: I) => i1 - i2)
    val mul = ("*", (z1: Z, z2: Z) => z1 * z2, (i1: I, i2: I) => i1 * i2)
    for (i <- 0 until size)
      for ((op, zop, iop) <- Seq(add, sub, mul)) {
        lazy val z1 = randomInt()
        lazy val z2 = randomInt()
        lazy val zr = zop(z1, z2)
        lazy val ir = iop(z1.toBigInt, z2.toBigInt)

        *(zr == ir, s"$z1 $op $z2 ($zr != $ir)")
      }

    val div = ("/", (z1: Z, z2: Z) => z1 / z2, (i1: I, i2: I) => i1 / i2)
    val rem = ("%", (z1: Z, z2: Z) => z1 % z2, (i1: I, i2: I) => i1 % i2)
    for (i <- 0 until size)
      for ((op, zop, iop) <- Seq(div, rem)) {
        lazy val z1 = randomInt()
        lazy val z2 = {
          var r = randomInt()
          while (r == math._Z.zero) {
            r = randomInt()
          }
          r
        }
        lazy val zr = zop(z1, z2)
        lazy val ir = iop(z1.toBigInt, z2.toBigInt)

        *(zr == ir, s"$z1 $op $z2 ($zr != $ir)")
      }

    val eq = ("==", (z1: Z, z2: Z) => z1 == z2, (i1: I, i2: I) => i1 == i2)
    val ne = ("!=", (z1: Z, z2: Z) => z1 != z2, (i1: I, i2: I) => i1 != i2)
    val gt = (">", (z1: Z, z2: Z) => z1 > z2, (i1: I, i2: I) => i1 > i2)
    val ge = (">=", (z1: Z, z2: Z) => z1 >= z2, (i1: I, i2: I) => i1 >= i2)
    val lt = ("<", (z1: Z, z2: Z) => z1 < z2, (i1: I, i2: I) => i1 < i2)
    val le = ("<=", (z1: Z, z2: Z) => z1 <= z2, (i1: I, i2: I) => i1 <= i2)
    for (i <- 0 until size)
      for ((op, zop, iop) <- Seq(eq, ne, gt, ge, lt, le)) {
        lazy val z1 = randomInt()
        lazy val z2 = randomInt()
        lazy val zr = zop(z1, z2)
        lazy val ir = iop(z1.toBigInt, z2.toBigInt)

        *(zr == ir, s"$z1 $op $z2 ($zr != $ir)")
      }
  }
}
