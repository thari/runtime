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

package org.sireum

package object logika {
  type Z = math.Z
  type Z8 = math.Z8.Value
  type Z16 = math.Z16.Value
  type Z32 = math.Z32.Value
  type Z64 = math.Z64.Value
  type S8 = math.S8.Value
  type S16 = math.S16.Value
  type S32 = math.S32.Value
  type S64 = math.S64.Value
  type N = math.N
  type N8 = math.N8.Value
  type N16 = math.N16.Value
  type N32 = math.N32.Value
  type N64 = math.N64.Value
  type U8 = math.U8.Value
  type U16 = math.U16.Value
  type U32 = math.U32.Value
  type U64 = math.U64.Value
  type R = math.R
  type F32 = math.F32.Value
  type F64 = math.F64.Value

  type S[I <: math.LogikaIntegralNumber, V] = collection.MS[I, V]
  type IS[I <: math.LogikaIntegralNumber, V] = collection.IS[I, V]

  type ZS = collection.MS[Z, Z]

  final val Z = math.Z
  final val Z8 = math.Z8
  final val Z16 = math.Z16
  final val Z32 = math.Z32
  final val Z64 = math.Z64
  final val S8 = math.S8
  final val S16 = math.S16
  final val S32 = math.S32
  final val S64 = math.S64
  final val N = math.N
  final val N8 = math.N8
  final val N16 = math.N16
  final val N32 = math.N32
  final val N64 = math.N64
  final val U8 = math.U8
  final val U16 = math.U16
  final val U32 = math.U32
  final val U64 = math.U64
  final val R = math.R
  final val F32 = math.F32
  final val F64 = math.F64

  final val MS = collection.MS
  final val IS = collection.IS

  object ZS {
    def apply(values: Z*): ZS = MS.apply[Z, Z](values: _*)
    def create(size: Z, default: Z): ZS = MS.create[Z, Z](size, default)
  }

  final def readInt(msg: String = "Enter an integer: "): Z = {
    while (true) {
      Console.out.print(msg)
      Console.out.flush()
      val s = Console.in.readLine()
      try {
        return Z(s)
      } catch {
        case _: Throwable =>
          Console.err.println(s"Invalid integer format: $s.")
          Console.err.flush()
      }
    }
    Z.zero
  }

  final def println(as: Any*): Unit = {
    print(as: _*)
    scala.Predef.println()
  }

  final def print(as: Any*): Unit =
    for (a <- as) scala.Predef.print(a)

  final def randomInt(): Z = Z.random

  final class helper extends scala.annotation.Annotation

  final class pure extends scala.annotation.Annotation

  final class native extends scala.annotation.Annotation

  import scala.language.implicitConversions
  final implicit def _Z(n: Int): Z = Z(n)

  final implicit def _2B(b: Boolean): B = B(b)

  final implicit def _2Boolean(b: B): Boolean = b.value

  import scala.language.experimental.macros

  final implicit class _Clonable[T](val o: T) extends AnyVal {
    def clone: T = o match {
      case o: Clonable => o.clone.asInstanceOf[T]
      case _ => o
    }
  }

  final implicit class _Logika(val sc: StringContext) extends AnyVal {

    def z(args: Any*): Z = math.Z(sc.parts.mkString(""))

    def z8(args: Any*): Z8 = z(args: _*).toZ8

    def z16(args: Any*): Z16 = z(args: _*).toZ16

    def z32(args: Any*): Z32 = z(args: _*).toZ32

    def z64(args: Any*): Z64 = z(args: _*).toZ64

    def n(args: Any*): N = z(args: _*).toN

    def n8(args: Any*): N8 = z(args: _*).toN8

    def n16(args: Any*): N16 = z(args: _*).toN16

    def n32(args: Any*): N32 = z(args: _*).toN32

    def n64(args: Any*): N64 = z(args: _*).toN64

    def s8(args: Any*): S8 = z(args: _*).toS8

    def s16(args: Any*): S16 = z(args: _*).toS16

    def s32(args: Any*): S32 = z(args: _*).toS32

    def s64(args: Any*): S64 = z(args: _*).toS64

    def u8(args: Any*): U8 = z(args: _*).toU8

    def u16(args: Any*): U16 = z(args: _*).toU16

    def u32(args: Any*): U32 = z(args: _*).toU32

    def u64(args: Any*): U64 = z(args: _*).toU64

    def r(args: Any*): R = math.R(sc.raw(args))

    def l(args: Any*): Unit = macro _macro.lImpl

    def c[T](args: Any*): T = macro _macro.cImpl[T]
  }

  @scala.annotation.compileTimeOnly("Immutable Record")
  final class irecord extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.irecordImpl
  }

  @scala.annotation.compileTimeOnly("Record")
  final class record extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.recordImpl
  }
}
