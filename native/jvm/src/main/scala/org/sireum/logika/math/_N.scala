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

package org.sireum.logika.math

import org.apfloat.Apint

import org.sireum.logika.{B, Z, N}

object _N {

  final val zero: N = _N(_Z.zero)

  final val one: N = _N(_Z.one)

  @inline
  final def apply(n: Int): N = apply(_Z(n))

  @inline
  final def apply(n: Long): N = apply(_Z(n))

  @inline
  final def apply(n: String): N = apply(_Z(n))

  @inline
  final def apply(n: BigInt): N = apply(_Z(n))

  @inline
  final def apply(n: java.math.BigInteger): N = apply(_Z(n))

  @inline
  final def apply(n: Apint): N = apply(_Z(n))

  @inline
  final def apply(z: _Z): N = {
    if (z < 0) println(z)
    require(z >= 0)
    new _N(z)
  }

  final def random: N = {
    val z = _Z.random
    if (z < _Z.zero) _N(-z) else _N(z)
  }
}

final class _N(val n: Z) extends AnyVal {
  def +(other: N): N = _N(toZ + other.toZ)

  def -(other: N): N = _N(toZ - other.toZ)

  def *(other: N): N = _N(toZ * other.toZ)

  def /(other: N): N = _N(toZ / other.toZ)

  def %(other: N): N = _N(toZ % other.toZ)

  def >(other: N): B = toZ > other.toZ

  def >=(other: N): B = toZ >= other.toZ

  def <(other: N): B = toZ < other.toZ

  def <=(other: N): B = toZ <= other.toZ

  def toZ: Z = n

  def toApint: Apint = toZ.toApint

  def toBigInt: BigInt = toZ.toBigInt

  override def toString: String = toZ.toString
}
