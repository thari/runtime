// #Sireum
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

import org.sireum.ext

@ext trait Value {
  def string: String
}

@ext trait Mutable extends Value

@ext trait Immutable extends Value

@ext trait Equal[E <: Equal[E]] {

  def isEqual(other: E): B

  def hash: Z

}

@ext trait Ordered[O <: Ordered[O]] {

  def <(other: O): B

  def <=(other: O): B

  def >(other: O): B

  def >=(other: O): B

}


@ext trait Number[N <: Number[N]] extends Immutable with Equal[N] with Ordered[N] {

  def +(other: N): N

  def -(other: N): N

  def *(other: N): N

  def /(other: N): N

  def %(other: N): N

}


@ext trait Index[I <: Index[I]] extends Number[I]


@ext trait B extends Immutable with Equal[B] {

  def &(other: B): B

  def |(other: B): B

  def |^(other: B): B

  def &&(other: => B): B

  def ||(other: => B): B

  def unary_!(): B

  def unary_~(): B

}


@ext trait C extends Immutable with Equal[C] with Ordered[C] {
}


@ext trait Z extends Index[Z] {

  def BitWidthOpt: Option[Z]

  def MinOpt: Option[Z]

  def MaxOpt: Option[Z]

  def Min: Z

  def Max: Z

  def BitWidth: Z

  def >>(other: Z): Z

  def >>>(other: Z): Z

  def <<(other: Z): Z

  def &(other: Z): Z

  def |(other: Z): Z

  def |^(other: Z): Z

  def unary_~(): B
}


@ext trait FloatingPoint extends Number[FloatingPoint] {

  def BitWidth: Z

  def SignificandBitWidth: Z

  def DecimalBitWidth: Z

}


@ext trait R extends Number[R] {
  def hash: Z

  def isEqual(other: R): B

  def string: String
}


@ext trait String extends Immutable {
  def hash: Z

  def isEqual(other: String): B

  def string: String
}

@ext trait IS[I <: Index[I], V <: Immutable] extends Immutable {

}

@ext trait MS[I <: Index[I], V <: Value] extends Mutable {

}

@ext trait Datatype[O <: Datatype[O]] extends Immutable with Equal[O] {

}

@ext trait Record[O <: Datatype[O]] extends Mutable with Equal[O] {

}

@ext trait Rich {

}