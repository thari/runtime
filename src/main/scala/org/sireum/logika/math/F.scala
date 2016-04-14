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

import org.sireum.logika._

import scala.math.ScalaNumericConversions
import scala.util.Random

sealed trait F {
  def bitWidth: Int

  sealed trait Value extends ScalaNumericConversions with Comparable[Value] with LogikaNumber {

    def +(other: Value): Value

    def -(other: Value): Value

    def *(other: Value): Value

    def /(other: Value): Value

    def %(other: Value): Value

    def >(other: Value): B

    def >=(other: Value): B

    def <(other: Value): B

    def <=(other: Value): B

    def unary_-(): Value

    def isNaN: B

    def isPosInfinity: B

    def isNegInfinity: B

    final override def isWhole: B = false
  }

}

object F32 extends F with LogikaNumberCompanion {

  @inline
  final def apply(value: Float): F32 = ValueImpl(value)

  @inline
  final def apply(s: String): F32 = ValueImpl(s.toFloat)

  final override def bitWidth = 32

  final override def random: F32 =
    F32.ValueImpl(new Random().nextFloat)

  private[math] final case class ValueImpl(value: Float) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def >(other: Value): B = other match {
      case other: ValueImpl => value > other.value
    }

    override def >=(other: Value): B = other match {
      case other: ValueImpl => value >= other.value
    }

    override def <(other: Value): B = other match {
      case other: ValueImpl => value < other.value
    }

    override def <=(other: Value): B = other match {
      case other: ValueImpl => value <= other.value
    }

    override def unary_-(): Value = ValueImpl(-value)

    override def isNaN: B = value.isNaN

    override def isPosInfinity: B = value.isPosInfinity

    override def isNegInfinity: B = value.isNegInfinity

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): B = other match {
      case other: ValueImpl => value == other.value
      case other: Float => value == other
      case other: Double => value.toDouble == other
      case _ => false
    }

    override def compareTo(other: Value): Int = other match {
      case other: ValueImpl => value.compareTo(other.value)
    }

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def floatValue: Float = value

    override def doubleValue: Double = value.toDouble

    override def underlying: Object = new java.lang.Float(value)

    override def toString: String = value.toString
  }

}


object F64 extends F with LogikaNumberCompanion {
  @inline
  final def apply(value: Double): F64 = ValueImpl(value)

  @inline
  final def apply(s: String): F64 = ValueImpl(s.toDouble)

  final override def bitWidth = 64

  final override def random: F64 =
    F64.ValueImpl(new Random().nextDouble)

  private[math] final case class ValueImpl(value: Double) extends Value {
    override def +(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value + other.value)
    }

    override def -(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value - other.value)
    }

    override def *(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value * other.value)
    }

    override def /(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value / other.value)
    }

    override def %(other: Value): Value = other match {
      case other: ValueImpl => ValueImpl(value % other.value)
    }

    override def >(other: Value): B = other match {
      case other: ValueImpl => value > other.value
    }

    override def >=(other: Value): B = other match {
      case other: ValueImpl => value >= other.value
    }

    override def <(other: Value): B = other match {
      case other: ValueImpl => value < other.value
    }

    override def <=(other: Value): B = other match {
      case other: ValueImpl => value <= other.value
    }

    override def unary_-(): Value = ValueImpl(-value)

    override def isNaN: B = value.isNaN

    override def isPosInfinity: B = value.isPosInfinity

    override def isNegInfinity: B = value.isNegInfinity

    override val hashCode: Int = value.hashCode

    override def equals(other: Any): B = other match {
      case other: ValueImpl => value == other.value
      case other: Float => value == other
      case other: Double => value.toDouble == other
      case _ => false
    }

    override def compareTo(other: Value): Int = other match {
      case other: ValueImpl => value.compareTo(other.value)
    }

    override def intValue: Int = value.toInt

    override def longValue: Long = value.toLong

    override def floatValue: Float = value.toFloat

    override def doubleValue: Double = value

    override def underlying: Object = new java.lang.Double(value)

    override def toString: String = value.toString
  }

}
