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


object B {

  val T = new B(true)
  val F = new B(false)

  def unapply(b: B): scala.Option[scala.Boolean] = scala.Some(b.value)

  import scala.language.implicitConversions

  @inline implicit def apply(b: Boolean): B = if (b) T else F

  @inline implicit def $4B(b: B): Boolean = b.value

}

final class B(val value: Boolean) extends AnyVal with Immutable {

  @inline def &(other: B): B = value & other.value

  @inline def |(other: B): B = value | other.value

  @inline def |^(other: B): B = value ^ other.value

  @inline def &&(other: => B): B = value && other.value

  @inline def ||(other: => B): B = value && other.value

  @inline def unary_! : B = !value

  @inline def unary_~ : B = !value

  @inline def hash: Z = hashCode

  @inline def isEqual(other: Immutable): B = this == other

  @inline def string: String = toString

  @inline override def toString: Predef.String = value.toString

}
