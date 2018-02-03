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

package org.sireum.$internal

import org.sireum.{$ZCompanion, B, Z, IS, MS, String, helper}
import language.experimental.macros

trait PackageTrait {

  type ISZ[T] = IS[Z, T]
  type MSZ[T] = MS[Z, T]
  type ZS = MS[Z, Z]

  object ISZ {
    def apply[V](args: V*): ISZ[V] = IS[Z, V](args: _*)
    def create[V](size: Z, default: V): ISZ[V] = IS.create(size, default)
  }

  object MSZ {
    def apply[V](args: V*): MSZ[V] = MS[Z, V](args: _*)
    def create[V](size: Z, default: V): MSZ[V] = MS.create(size, default)
  }

  object ZS {
    def apply(args: Z*): ZS = MS[Z, Z](args: _*)
    def create(size: Z, default: Z): ZS = MS.create(size, default)
  }

  val T: org.sireum.B = true
  val F: org.sireum.B = false

  final def randomInt(): Z = Z.random

  final def promptInt(msg: String): Z = {
    while (true) {
      Console.out.print(msg.value)
      Console.out.flush()
      val s = Console.in.readLine()
      try {
        return Z.$String(s)
      } catch {
        case _: Throwable =>
          Console.err.println(s"Invalid integer format: $s.")
          Console.err.flush()
      }
    }
    Z.MP.zero
  }

  final def readInt(): Z = promptInt("Enter an integer: ")

  def halt(msg: scala.Any): scala.Nothing = helper.halt(msg)

  def $[T]: T = macro Macro.$[T]

  final def cprintln(isErr: B, as: Any*): Unit = {
    if (isErr) eprintln(as: _*) else println(as: _*)
  }

  final def cprint(isErr: B, as: Any*): Unit = {
    if (isErr) eprint(as: _*) else print(as: _*)
  }

  final def println(as: Any*): Unit = {
    as.size match {
      case 0 =>
        Console.out.println()
        Console.out.flush()
      case 1 =>
        Console.out.println(as(0))
        Console.out.flush()
      case _ =>
        for (a <- as) Console.out.print(a)
        Console.out.println()
        Console.out.flush()
    }
  }

  final def print(as: Any*): Unit = {
    for (a <- as) {
      Console.out.print(a)
      Console.out.flush()
    }
  }

  final def eprintln(as: Any*): Unit = {
    as.size match {
      case 0 =>
        Console.err.println()
        Console.err.flush()
      case 1 =>
        Console.err.println(as(0))
        Console.err.flush()
      case _ =>
        for (a <- as) Console.err.print(a)
        Console.err.println()
        Console.err.flush()
    }
  }

  final def eprint(as: Any*): Unit = {
    for (a <- as) {
      Console.err.print(a)
      Console.err.flush()
    }
  }

  import language.implicitConversions

  @inline implicit def $2BigIntOpt(n: scala.Int): scala.Option[scala.BigInt] = scala.Some(scala.BigInt(n))

  @inline implicit def $2BigIntOpt(n: scala.Long): scala.Option[scala.BigInt] = scala.Some(scala.BigInt(n))

  @inline implicit def $2BigIntOpt(n: org.sireum.Z): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)

  @inline implicit val $ZCompanion: $ZCompanion[Z] = Z

}
