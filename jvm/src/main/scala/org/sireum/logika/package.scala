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
  type TT[T] = scala.reflect.runtime.universe.TypeTag[T]

  type Z = math._Z
  type Z8 = math._Z8.Value
  type Z16 = math._Z16.Value
  type Z32 = math._Z32.Value
  type Z64 = math._Z64.Value
  type S8 = math._S8.Value
  type S16 = math._S16.Value
  type S32 = math._S32.Value
  type S64 = math._S64.Value
  type N = math._N
  type N8 = math._N8.Value
  type N16 = math._N16.Value
  type N32 = math._N32.Value
  type N64 = math._N64.Value
  type U8 = math._U8.Value
  type U16 = math._U16.Value
  type U32 = math._U32.Value
  type U64 = math._U64.Value
  type R = math._R
  type F32 = math._F32.Value
  type F64 = math._F64.Value

  type MS[I, V] = collection._MS[I, V]
  type IS[I, V] = collection._IS[I, V]

  type ZS = collection._MS[Z, Z]

  object ZS {
    def apply(values: Z*): ZS = collection._MS.apply[Z, Z](values: _*)
    def create(size: Z, default: Z): ZS = collection._MS.create[Z, Z](size, default)
  }

  /* deprecated: begin */
  type BS = MS[Z, B]
  type Z8S = MS[Z, Z8]
  type Z16S = MS[Z, Z16]
  type Z32S = MS[Z, Z32]
  type Z64S = MS[Z, Z64]
  type NS = MS[Z, N]
  type N8S = MS[Z, N8]
  type N16S = MS[Z, N16]
  type N32S = MS[Z, N32]
  type N64S = MS[Z, N64]
  type S8S = MS[Z, S8]
  type S16S = MS[Z, S16]
  type S32S = MS[Z, S32]
  type S64S = MS[Z, S64]
  type U8S = MS[Z, U8]
  type U16S = MS[Z, U16]
  type U32S = MS[Z, U32]
  type U64S = MS[Z, U64]
  type F32S = MS[Z, F32]
  type F64S = MS[Z, F64]
  type RS = MS[Z, R]

  object BS {
    def apply(values: B*): BS = collection._MS.apply[Z, B](values: _*)
    def create(size: Z, default: B): BS = collection._MS.create[Z, B](size, default)
  }

  object Z8S {
    def apply(values: Z8*): Z8S = collection._MS.apply[Z, Z8](values: _*)
    def create(size: Z, default: Z8): Z8S = collection._MS.create[Z, Z8](size, default)
  }

  object Z16S {
    def apply(values: Z16*): Z16S = collection._MS.apply[Z, Z16](values: _*)
    def create(size: Z, default: Z16): Z16S = collection._MS.create[Z, Z16](size, default)
  }

  object Z32S {
    def apply(values: Z32*): Z32S = collection._MS.apply[Z, Z32](values: _*)
    def create(size: Z, default: Z32): Z32S = collection._MS.create[Z, Z32](size, default)
  }

  object Z64S {
    def apply(values: Z64*): Z64S = collection._MS.apply[Z, Z64](values: _*)
    def create(size: Z, default: Z64): Z64S = collection._MS.create[Z, Z64](size, default)
  }

  object NS {
    def apply(values: N*): NS = collection._MS.apply[Z, N](values: _*)
    def create(size: Z, default: N): NS = collection._MS.create[Z, N](size, default)
  }

  object N8S {
    def apply(values: N8*): N8S = collection._MS.apply[Z, N8](values: _*)
    def create(size: Z, default: N8): N8S = collection._MS.create[Z, N8](size, default)
  }

  object N16S {
    def apply(values: N16*): N16S = collection._MS.apply[Z, N16](values: _*)
    def create(size: Z, default: N16): N16S = collection._MS.create[Z, N16](size, default)
  }

  object N32S {
    def apply(values: N32*): N32S = collection._MS.apply[Z, N32](values: _*)
    def create(size: Z, default: N32): N32S = collection._MS.create[Z, N32](size, default)
  }

  object N64S {
    def apply(values: N64*): N64S = collection._MS.apply[Z, N64](values: _*)
    def create(size: Z, default: N64): N64S = collection._MS.create[Z, N64](size, default)
  }

  object S8S {
    def apply(values: S8*): S8S = collection._MS.apply[Z, S8](values: _*)
    def create(size: Z, default: S8): S8S = collection._MS.create[Z, S8](size, default)
  }

  object S16S {
    def apply(values: S16*): S16S = collection._MS.apply[Z, S16](values: _*)
    def create(size: Z, default: S16): S16S = collection._MS.create[Z, S16](size, default)
  }

  object S32S {
    def apply(values: S32*): S32S = collection._MS.apply[Z, S32](values: _*)
    def create(size: Z, default: S32): S32S = collection._MS.create[Z, S32](size, default)
  }

  object S64S {
    def apply(values: S64*): S64S = collection._MS.apply[Z, S64](values: _*)
    def create(size: Z, default: S64): S64S = collection._MS.create[Z, S64](size, default)
  }

  object U8S {
    def apply(values: U8*): U8S = collection._MS.apply[Z, U8](values: _*)
    def create(size: Z, default: U8): U8S = collection._MS.create[Z, U8](size, default)
  }

  object U16S {
    def apply(values: U16*): U16S = collection._MS.apply[Z, U16](values: _*)
    def create(size: Z, default: U16): U16S = collection._MS.create[Z, U16](size, default)
  }

  object U32S {
    def apply(values: U32*): U32S = collection._MS.apply[Z, U32](values: _*)
    def create(size: Z, default: U32): U32S = collection._MS.create[Z, U32](size, default)
  }

  object U64S {
    def apply(values: U64*): U64S = collection._MS.apply[Z, U64](values: _*)
    def create(size: Z, default: U64): U64S = collection._MS.create[Z, U64](size, default)
  }

  object F32S {
    def apply(values: F32*): F32S = collection._MS.apply[Z, F32](values: _*)
    def create(size: Z, default: F32): F32S = collection._MS.create[Z, F32](size, default)
  }

  object F64S {
    def apply(values: F64*): F64S = collection._MS.apply[Z, F64](values: _*)
    def create(size: Z, default: F64): F64S = collection._MS.create[Z, F64](size, default)
  }

  object RS {
    def apply(values: R*): RS = collection._MS.apply[Z, R](values: _*)
    def create(size: Z, default: R): RS = collection._MS.create[Z, R](size, default)
  }
  /* deprecated: end */

  final def readInt(msg: String = "Enter an integer: "): Z = {
    while (true) {
      Console.out.print(msg)
      Console.out.flush()
      val s = Console.in.readLine()
      try {
        return math._Z(s)
      } catch {
        case _: Throwable =>
          Console.err.println(s"Invalid integer format: $s.")
          Console.err.flush()
      }
    }
    math._Z.zero
  }

  final def println(as: Any*): Unit = {
    print(as: _*)
    scala.Predef.println()
  }

  final def print(as: Any*): Unit =
    for (a <- as) scala.Predef.print(a)

  final def randomInt(): Z = math._Z.random

  final class helper extends scala.annotation.StaticAnnotation

  final class pure extends scala.annotation.StaticAnnotation

  final class part extends scala.annotation.StaticAnnotation

  import scala.language.implicitConversions
  final implicit def _Z(n: Int): Z = math._Z(n)

  final implicit def _2B(b: Boolean): B = if (b) T else F

  final implicit def _2Boolean(b: B): Boolean = b.value

  import scala.language.experimental.macros

  final implicit class _Clonable[T](val o: T) extends AnyVal {
    def clone: T = o match {
      case o: Clonable => o.clone.asInstanceOf[T]
      case _ => o
    }
  }

  final implicit class _Logika(val sc: StringContext) extends AnyVal {

    def z(args: Any*): Z = math._Z(sc.parts.mkString(""))

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

    def f32(args: Any*): F32 = math._F32(sc.parts.mkString("").toFloat)

    def f64(args: Any*): F64 = math._F64((sc.parts.mkString("") + "d").toDouble)

    def r(args: Any*): R = math._R(sc.raw(args))

    def l[T : TT](args: Any*): T = $

    def c[T](args: Any*): Any => T = macro _macro.cImpl[T]
  }

  @scala.annotation.compileTimeOnly("Immutable Record")
  final class irecord extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.irecordImpl
  }

  @scala.annotation.compileTimeOnly("Record")
  final class record extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.recordImpl
  }

  @scala.annotation.compileTimeOnly("Enum")
  final class enum extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.enumImpl
  }

  @scala.annotation.compileTimeOnly("Enum")
  final class native extends scala.annotation.StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro _macro.nativeImpl
  }

  import scala.reflect.runtime.universe._

  val _unitType: Type = typeOf[Unit]

  def $[T: TT]: T =
    if (typeOf[T] =:= _unitType) ().asInstanceOf[T]
    else throw new NotImplementedError
}
