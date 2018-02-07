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


object B {
}

@ext trait B {

  @pure def hash: Z

  @pure def string: String

  @pure def &(other: B): B

  @pure def |(other: B): B

  @pure def |^(other: B): B

  @pure def &&(other: => B): B

  @pure def ||(other: => B): B

  @pure def unary_! : B

  @pure def unary_~ : B

}


object C {
}

@ext trait C {

  @pure def hash: Z

  @pure def string: String

  @pure def <(other: C): B

  @pure def <=(other: C): B

  @pure def >(other: C): B

  @pure def >=(other: C): B

  @pure def >>>(other: C): C

  @pure def <<(other: C): C

  @pure def &(other: C): C

  @pure def |(other: C): C

  @pure def |^(other: C): C
}


object Z {
}

@ext trait Z {

  @pure def hash: Z

  @pure def string: String

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


object F32 {
}

@ext trait F32 {

  @pure def hash: Z

  @pure def string: String

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


object F64 {
}

@ext trait F64 {

  @pure def hash: Z

  @pure def string: String

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


object R {
}

@ext trait R {

  @pure def hash: Z

  @pure def string: String

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


@ext trait String {

  @pure def hash: Z

  @pure def string: String

  @pure def at(i: Z): C

  @pure def size: Z

  @pure def toCis: IS[Z, C]

  @pure def toCms: MS[Z, C]

}


@ext trait ST {

  @pure def hash: Z

  @pure def string: String

  @pure def render: String

  @pure def renderCompact: String

}


@ext object IS {
  def create[I, V](size: I, default: V): IS[I, V] = $
}

@ext trait IS[I, V] {

  @pure def hash: Z

  @pure def string: String

  @pure def isEmpty: B

  @pure def nonEmpty: B

  @pure def :+(e: V): IS[I, V]

  @pure def +:(e: V): IS[I, V]

  @pure def ++(other: IS[I, V]): IS[I, V]

  @pure def --(other: IS[I, V]): IS[I, V]

  @pure def -(e: V): IS[I, V]

  @pure def map[V2](f: V => V2 @pure): IS[I, V2]

  @pure def flatMap[V2](f: V => IS[I, V2] @pure): IS[I, V2]

  @pure def withFilter(p: V => B @pure): IS[I, V]

  @pure def foreach(p: V => Unit): Unit

  @pure def size: I

  @pure def toMS: MS[I, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """

}

@ext object ISZ {
  def create[V](size: Z, default: V): ISZ[V] =
    l""" requires size ≥ 0 """
}

@ext object MS {
  def create[I, V](size: I, default: V): MS[I, V] = $
}

@ext trait MS[I, V] {

  @pure def hash: Z

  @pure def string: String

  @pure def isEmpty: B

  @pure def nonEmpty: B

  @pure def :+(e: V): MS[I, V]

  @pure def +:(e: V): MS[I, V]

  @pure def ++(other: MS[I, V]): MS[I, V]

  @pure def --(other: MS[I, V]): MS[I, V]

  @pure def -(e: V): MS[I, V]

  @pure def map[V2](f: V => V2 @pure): MS[I, V2]

  @pure def flatMap[V2](f: V => MS[I, V2] @pure): MS[I, V2]

  @pure def withFilter(p: V => B @pure): MS[I, V]

  @pure def foreach(p: V => Unit): Unit

  @pure def size: I

  @pure def toIS: IS[I, V] =
    l""" ensures result.size ≡ s.size
                 ∀i: [0, result.size)  result(i) ≡ s(i) """

}

@ext object MSZ {
  def create[V](size: Z, default: V): MSZ[V] =
    l""" requires size ≥ 0 """
}

@ext object ZS {
  def create(size: Z, default: Z): ZS =
    l""" requires size ≥ 0 """
}