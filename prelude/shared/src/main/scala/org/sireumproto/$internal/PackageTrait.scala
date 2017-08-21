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

package org.sireumproto.$internal

import org.sireumproto.{$ZCompanion, Immutable, Z, IS, MS, helper}
import language.experimental.macros

trait PackageTrait {

  type ISZ[T] = IS[Z, T]
  type MSZ[T] = MS[Z, T]
  type ZS = MS[Z, Z]

  object up {
    def update[T](lhs: T, rhs: T): Unit = macro Macro.up
  }

  object pat {
    def update(args: Any*): Unit = macro Macro.pat
  }

  object ISZ {
    def apply[V](args: V*): ISZ[V] = IS[Z, V](args: _*)
    def create[V](size: Z, default: V): ISZ[V] = IS.create[Z, V](size, default)
  }

  object MSZ {
    def apply[V](args: V*): MSZ[V] = MS[Z, V](args: _*)
    def create[V](size: Z, default: V): MSZ[V] = MS.create[Z, V](size, default)
  }

  object ZS {
    def apply(args: Z*): ZS = MS[Z, Z](args: _*)
    def create(size: Z, default: Z): ZS = MS.create[Z, Z](size, default)
  }

  val T: org.sireumproto.B = true
  val F: org.sireumproto.B = false

  def halt(msg: Any): Nothing = helper.halt(msg)

  def $[T]: T = macro Macro.$[T]

  import language.implicitConversions

  @inline implicit def $2BigIntOpt(n: scala.Int): scala.Option[scala.BigInt] = scala.Some(scala.BigInt(n))

  @inline implicit def $2BigIntOpt(n: scala.Long): scala.Option[scala.BigInt] = scala.Some(scala.BigInt(n))

  @inline implicit def $2BigIntOpt(n: org.sireumproto.Z): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)

  @inline implicit val $ZCompanion: $ZCompanion[Z] = Z

}
