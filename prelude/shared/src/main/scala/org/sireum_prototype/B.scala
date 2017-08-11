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

  import scala.language.implicitConversions

  @inline implicit def $2B(b: Boolean): B = if (b) T else F

  @inline implicit def $4B(b: B): Boolean = if (b == T) true else false

}

sealed trait B extends Immutable {

  @pure def &(other: B): B

  @pure def |(other: B): B

  @pure def |^(other: B): B

  @pure def &&(other: => B): B

  @pure def ||(other: => B): B

  @pure def unary_!(): B

  @pure def unary_~(): B

  @pure final def isEqual(other: Immutable): B = this == other

  final override def equals(other: Any): Boolean = other match {
    case b: Boolean => isEqual(b: B)
    case b: B => isEqual(b)
    case b: java.lang.Boolean => isEqual(b.booleanValue: B)
  }

}

case object T extends B {

  final val string: String = new String("T")

  final override val hashCode: Int = true.hashCode

  @pure final def &(other: B): B = other

  @pure final def |(other: B): B = T

  @pure final def |^(other: B): B = !other

  @pure final def &&(other: => B): B = other

  @pure final def ||(other: => B): B = T

  @pure final def unary_!(): B = F

  @pure final def unary_~(): B = F

  @pure final def hash: Z = hashCode

}

case object F extends B {

  final val string: String = new String("F")

  final override val hashCode: Int = false.hashCode

  @pure final def &(other: B): B = F

  @pure final def |(other: B): B = other

  @pure final def |^(other: B): B = other

  @pure final def &&(other: => B): B = F

  @pure final def ||(other: => B): B = other

  @pure final def unary_!(): B = T

  @pure final def unary_~(): B = T

  @pure final def hash: Z = hashCode

}

