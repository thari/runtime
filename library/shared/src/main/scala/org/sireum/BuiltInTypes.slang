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

package org.sireum


@ext trait Immutable {

  @pure def string: String

}


@ext trait B extends Immutable {

  @pure def &(other: B): B

  @pure def |(other: B): B

  @pure def |^(other: B): B

  @pure def &&(other: => B): B

  @pure def ||(other: => B): B

  @pure def unary_! : B

  @pure def unary_~ : B

}


@ext trait C extends Immutable {
  def <(other: C): B

  def <=(other: C): B

  def >(other: C): B

  def >=(other: C): B

  def >>>(other: C): C

  def <<(other: C): C

  def &(other: C): C

  def |(other: C): C

  def |^(other: C): C
}

@ext trait Number extends Immutable

@ext trait Z extends Number {

  @pure def <(other: Z): B

  @pure def <=(other: Z): B

  @pure def >(other: Z): B

  @pure def >=(other: Z): B

  @pure def +(other: Z): Z

  @pure def -(other: Z): Z

  @pure def *(other: Z): Z

  @pure def /(other: Z): Z

  @pure def %(other: Z): Z

  @pure def unary_- : Z

  @pure def >>(other: Z): Z

  @pure def >>>(other: Z): Z

  @pure def <<(other: Z): Z

  @pure def &(other: Z): Z

  @pure def |(other: Z): Z

  @pure def |^(other: Z): Z

  @pure def unary_~ : Z

  @pure def increase: Z

  @pure def decrease: Z

}


@ext trait FloatingPoint extends Number


@ext trait F32 extends FloatingPoint {

  @pure def <(other: F32): B

  @pure def <=(other: F32): B

  @pure def >(other: F32): B

  @pure def >=(other: F32): B

  @pure def +(other: F32): F32

  @pure def -(other: F32): F32

  @pure def *(other: F32): F32

  @pure def /(other: F32): F32

  @pure def %(other: F32): F32

  @pure def unary_- : F32

}


@ext trait F64 extends FloatingPoint {

  @pure def <(other: F64): B

  @pure def <=(other: F64): B

  @pure def >(other: F64): B

  @pure def >=(other: F64): B

  @pure def +(other: F64): F64

  @pure def -(other: F64): F64

  @pure def *(other: F64): F64

  @pure def /(other: F64): F64

  @pure def %(other: F64): F64

  @pure def unary_- : F64

}


@ext trait R extends Number {

  @pure def <(other: R): B

  @pure def <=(other: R): B

  @pure def >(other: R): B

  @pure def >=(other: R): B

  @pure def +(other: R): R

  @pure def -(other: R): R

  @pure def *(other: R): R

  @pure def /(other: R): R

  @pure def %(other: R): R

  @pure def unary_- : R

}


@ext trait String extends Immutable {

  def at(i: Z): C

  def size: Z

  def toCis: IS[Z, C]

  def toCms: MS[Z, C]

}

@ext trait ST extends Immutable {

  def render: String

  def renderCompact: String

}


@ext trait IS[I, V] extends Immutable {

  @pure def isEmpty: B

  @pure def nonEmpty: B

  @pure def :+(e: V): IS[I, V]

  @pure def +:(e: V): IS[I, V]

  @pure def ++(other: IS[I, V]): IS[I, V]

  @pure def --(other: IS[I, V]): IS[I, V]

  @pure def -(e: V): IS[I, V]

  @pure def map[V2](f: V => V2): IS[I, V2]

  @pure def flatMap[V2](f: V => IS[I, V2]): IS[I, V2]

  @pure def withFilter(p: V => B): IS[I, V]

  @pure def foreach(p: V => Unit): Unit

  @pure def size: I

  @pure def toMS: MS[I, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """

}

@ext trait EnumSig extends Immutable {
  def numOfElements: Z
}

@ext trait DatatypeSig extends Immutable {

  def hash: Z

}

@ext trait RichSig extends Immutable


@ext trait ST extends Immutable


@ext trait Mutable {

  @pure def string: String

}


@ext trait MS[I, V] extends Mutable {

  @pure def isEmpty: B

  @pure def nonEmpty: B

  @pure def :+(e: V): MS[I, V]

  @pure def +:(e: V): MS[I, V]

  @pure def ++(other: MS[I, V]): MS[I, V]

  @pure def --(other: MS[I, V]): MS[I, V]

  @pure def -(e: V): MS[I, V]

  @pure def map[V2](f: V => V2): MS[I, V2]

  @pure def flatMap[V2](f: V => MS[I, V2]): MS[I, V2]

  @pure def withFilter(p: V => B): MS[I, V]

  @pure def foreach(p: V => Unit): Unit

  @pure def size: I

  @pure def toIS: IS[I, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """

}


@ext trait RecordSig extends Mutable {
  def hash: Z
}
