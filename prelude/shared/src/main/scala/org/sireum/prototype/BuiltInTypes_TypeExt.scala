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

package org.sireum.prototype

import scala.language.implicitConversions


trait Value extends Any {
  def string: String
}

trait Mutable extends Any with Value

trait Immutable extends Any with Value

trait Equal[E <: Equal[E]] extends Any {

  def isEqual(other: E): B

  def hash: Z

}

trait Ordered[O <: Ordered[O]] extends Any {

  def <(other: O): B

  def <=(other: O): B

  def >(other: O): B

  def >=(other: O): B

}

trait Number[N <: Number[N]] extends Any with Immutable with Equal[N] with Ordered[N] {

  def +(other: N): N

  def -(other: N): N

  def *(other: N): N

  def /(other: N): N

  def %(other: N): N

}

trait Index[I <: Index[I]] extends Any with Number[I]


object B {

  @inline implicit def _2B(b: Boolean): B = if (b) T else F

  @inline implicit def _4B(b: B): Boolean = if (b == T) true else false

}

sealed trait B extends Immutable with Equal[B] {

  def &(other: B): B

  def |(other: B): B

  def |^(other: B): B

  def &&(other: => B): B

  def ||(other: => B): B

  def unary_!(): B

  def unary_~(): B

  final override def equals(other: Any): Boolean = other match {
    case b: Boolean => isEqual(b)
    case b: B => isEqual(b)
    case b: java.lang.Boolean => isEqual(b.booleanValue)
  }

}


case object T extends B {

  final val string: String = new String("T")

  final override val hashCode: Int = true.hashCode

  @inline final def &(other: B): B = other

  @inline final def |(other: B): B = T

  @inline final def |^(other: B): B = !other

  @inline final def &&(other: => B): B = other

  @inline final def ||(other: => B): B = T

  @inline final def unary_!(): B = F

  @inline final def unary_~(): B = F

  @inline final def hash: Z = ???

  @inline final def isEqual(other: B): B = other == T

}


case object F extends B {

  final val string: String = new String("F")

  final override val hashCode: Int = false.hashCode

  @inline final def &(other: B): B = F

  @inline final def |(other: B): B = other

  @inline final def |^(other: B): B = other

  @inline final def &&(other: => B): B = F

  @inline final def ||(other: => B): B = other

  @inline final def unary_!(): B = T

  @inline final def unary_~(): B = T

  @inline final def hash: Z = ???

  @inline final def isEqual(other: B): B = other == F
}


object C {

  @inline implicit def _2C(c: Char): C = new C(c)

  @inline implicit def _4C(c: C): Char = c.value

}

final class C(val value: Char) extends AnyVal with Immutable with Equal[C] with Ordered[C] {

  @inline def <(other: C): B = value < other.value

  @inline def <=(other: C): B = value <= other.value

  @inline def >(other: C): B = value > other.value

  @inline def >=(other: C): B = value >= other.value

  @inline def hash: Z = value.hashCode

  @inline def isEqual(other: C): B = value == other.value

  @inline def string: String = new String(value.toString)

}



object Z {

  @inline implicit def _2Z(n: Int): Z = ???

  @inline implicit def _2lZ(n: Long): Z = ???

}

sealed trait Z extends Index[Z] {

  def BitWidthOpt: Option[Z] = ???

  def MinOpt: Option[Z] = ???

  def MaxOpt: Option[Z] = ???

  def Min: Z = ???

  def Max: Z = ???

  def BitWidth: Z = ???

  def >>(other: Z): Z = ???

  def >>>(other: Z): Z = ???

  def <<(other: Z): Z = ???

  def &(other: Z): Z = ???

  def |(other: Z): Z = ???

  def |^(other: Z): Z = ???

  def unary_~(): B = ???

}


sealed trait FloatingPoint extends Any with Number[FloatingPoint] {

  def BitWidth: Z

  def SignificandBitWidth: Z

  def ExponentBitWidth: Z

}

final class F32(val value: Float) extends AnyVal with FloatingPoint {

  def BitWidth: Z = 32

  def SignificandBitWidth: Z = 24

  def ExponentBitWidth: Z = 7

  def <(other: FloatingPoint): B = ???

  def <=(other: FloatingPoint): B = ???

  def >(other: FloatingPoint): B = ???

  def >=(other: FloatingPoint): B = ???

  def +(other: FloatingPoint): FloatingPoint = ???

  def -(other: FloatingPoint): FloatingPoint = ???

  def *(other: FloatingPoint): FloatingPoint = ???

  def /(other: FloatingPoint): FloatingPoint = ???

  def %(other: FloatingPoint): FloatingPoint = ???

  def hash: Z = value.hashCode

  def isEqual(other: FloatingPoint): B = other match {
    case other: F32 => value == other.value
    case _ => ???
  }

  def string: String = java.lang.Float.toString(value)
}

final class F64(val value: Double) extends AnyVal with FloatingPoint {

  def BitWidth: Z = 64

  def SignificandBitWidth: Z = 53

  def ExponentBitWidth: Z = 10

  def hash: Z = value.hashCode

  def <(other: FloatingPoint): B = ???

  def <=(other: FloatingPoint): B = ???

  def >(other: FloatingPoint): B = ???

  def >=(other: FloatingPoint): B = ???

  def +(other: FloatingPoint): FloatingPoint = ???

  def -(other: FloatingPoint): FloatingPoint = ???

  def *(other: FloatingPoint): FloatingPoint = ???

  def /(other: FloatingPoint): FloatingPoint = ???

  def %(other: FloatingPoint): FloatingPoint = ???

  def isEqual(other: FloatingPoint): B = other match {
    case other: F64 => value == other.value
    case _ => ???
  }

  def string: String = java.lang.Double.toString(value)

}


final class R extends Number[R] {

  def <(other: R): B = ???

  def <=(other: R): B = ???

  def >(other: R): B = ???

  def >=(other: R): B = ???

  def +(other: R): R = ???

  def -(other: R): R = ???

  def *(other: R): R = ???

  def /(other: R): R = ???

  def %(other: R): R = ???

  def hash: Z = ???

  def isEqual(other: R): B = ???

  def string: String = ???
}


object String {

  implicit def _2String(s: Predef.String): String = new String(s)

  implicit def _4String(s: String): Predef.String = s.value

}

final class String(val value: Predef.String) extends AnyVal with Immutable with Equal[String] {

  def hash: Z = value.hashCode

  def isEqual(other: String): B = ???

  def string: String = ???

}

sealed trait IS[I <: Index[I], V <: Immutable] extends Immutable {

}

sealed trait MS[I <: Index[I], V <: Value] extends Mutable {

}

trait Datatype[O <: Datatype[O]] extends Immutable with Equal[O] {

}

trait Record[O <: Datatype[O]] extends Mutable with Equal[O] {

}

trait Rich {

}