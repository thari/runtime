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

object R extends LogikaNumberCompanion {

  @inline
  final def apply(r: String): R = apply(Real(r.replaceAll(" ", "")))

  @inline
  final def apply(r: Real): R = RImpl(r)

  final override def random: R = R(Z.random.toString + "." + N.random.toString)
}

sealed trait R extends ScalaNumericConversions with Comparable[R] with LogikaNumber {
  final def +(other: R): R = R(toReal + other.toReal)

  final def -(other: R): R = R(toReal - other.toReal)

  final def *(other: R): R = R(toReal * other.toReal)

  final def /(other: R): R = R(toReal / other.toReal)

  final def %(other: R): R = R(toReal % other.toReal)

  final def >(other: R): B = toReal.compare(other.toReal) > 0

  final def >=(other: R): B = toReal.compare(other.toReal) >= 0

  final def <(other: R): B = toReal.compare(other.toReal) < 0

  final def <=(other: R): B = toReal.compare(other.toReal) <= 0

  final def unary_-(): R = R(-toReal)

  final override def compareTo(other: R): Int = toReal.compare(other.toReal)

  final override def doubleValue: Double = toReal.doubleValue

  final override def floatValue: Float = toReal.floatValue

  final override def intValue: Int = toReal.intValue

  final override def longValue: Long = toReal.longValue

  final override def underlying: Real = toReal

  final override def isWhole = false

  final override lazy val hashCode: Int = toReal.hashCode

  final override def equals(other: Any): B = other match {
    case other: R => (this eq other) || toReal.equals(other.toReal)
    case _ => false
  }

  final override def toString: String = toReal.toString

  def toReal: Real
}

final private case class RImpl(value: Real) extends R {
  override def toReal: Real = value
}
