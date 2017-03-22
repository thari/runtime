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

import spire.math.Real

import scala.math.ScalaNumericConversions

import org.sireum.logika._

object _R extends LogikaNumberCompanion {

  @inline
  final def apply(r: String): _R = apply(Real(r.replaceAll(" ", "")))

  @inline
  final def apply(r: Real): _R = RImpl(r)

  final override def random: _R = _R(_Z.random.toString + "." + _N.random.toString)
}

sealed trait _R extends ScalaNumericConversions with Comparable[_R] with _LogikaNumber {
  final def +(other: _R): _R = _R(value + other.value)

  final def -(other: _R): _R = _R(value - other.value)

  final def *(other: _R): _R = _R(value * other.value)

  final def /(other: _R): _R = _R(value / other.value)

  final def %(other: _R): _R = _R(value % other.value)

  final def >(other: _R): B = value.compare(other.value) > 0

  final def >=(other: _R): B = value.compare(other.value) >= 0

  final def <(other: _R): B = value.compare(other.value) < 0

  final def <=(other: _R): B = value.compare(other.value) <= 0

  final def unary_-(): _R = _R(-value)

  final override def compareTo(other: _R): Int = value.compare(other.value)

  final override def doubleValue: Double = value.doubleValue

  final override def floatValue: Float = value.floatValue

  final override def intValue: Int = value.intValue

  final override def longValue: Long = value.longValue

  final override def underlying: Real = value

  final override def isWhole = false

  final override lazy val hashCode: Int = value.hashCode

  final override def equals(other: Any): Boolean = other match {
    case other: _R => (this eq other) || value.equals(other.value)
    case _ => false
  }

  final override def toString: String = value.toString

  def value: Real
}

final private case class RImpl(value: Real) extends _R
