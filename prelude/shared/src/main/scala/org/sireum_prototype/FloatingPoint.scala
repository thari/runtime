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

package org.sireum_prototype


private[sireum_prototype] sealed trait FloatingPoint[F <: FloatingPoint[F]] extends Any with Number[F] {

  def BitWidth: Z

  def SignificandBitWidth: Z

  def ExponentBitWidth: Z

}

object F32 {

  def unapply(f: F32): scala.Option[scala.Float] = scala.Some(f.value)

  import scala.language.implicitConversions

  implicit def apply(f: scala.Float): F32 = new F32(f)

}

final class F32(val value: scala.Float) extends AnyVal with FloatingPoint[F32] {

  def BitWidth: Z = 32

  def SignificandBitWidth: Z = 24

  def ExponentBitWidth: Z = 7

  def <(other: F32): B = value < other.value

  def <=(other: F32): B = value <= other.value

  def >(other: F32): B = value > other.value

  def >=(other: F32): B = value >= other.value

  def +(other: F32): F32 = value + other.value

  def -(other: F32): F32 = value - other.value

  def *(other: F32): F32 = value * other.value

  def /(other: F32): F32 = value / other.value

  def %(other: F32): F32 = value % other.value

  def hash: Z = value.hashCode

  def isEqual(other: Immutable): B = this == other

  def string: String = java.lang.Float.toString(value)
}

object F64 {

  def unapply(d: F64): scala.Option[scala.Double] = scala.Some(d.value)

  import scala.language.implicitConversions

  implicit def apply(d: scala.Double): F64 = new F64(d)

}

final class F64(val value: scala.Double) extends AnyVal with FloatingPoint[F64] {

  def BitWidth: Z = 64

  def SignificandBitWidth: Z = 53

  def ExponentBitWidth: Z = 10

  def hash: Z = value.hashCode

  def <(other: F64): B = value < other.value

  def <=(other: F64): B = value <= other.value

  def >(other: F64): B = value > other.value

  def >=(other: F64): B = value >= other.value

  def +(other: F64): F64 = value + other.value

  def -(other: F64): F64 = value - other.value

  def *(other: F64): F64 = value * other.value

  def /(other: F64): F64 = value / other.value

  def %(other: F64): F64 = value % other.value

  def isEqual(other: Immutable): B = this == other

  def string: String = java.lang.Double.toString(value)

}
