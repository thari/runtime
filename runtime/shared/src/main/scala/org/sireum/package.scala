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

package org

package object sireum {
  type B = _Type.Alias.B
  type C = _Type.Alias.C
  type Z = _Type.Alias.Z
  type Z8 = _Type.Alias.Z8
  type Z16 = _Type.Alias.Z16
  type Z32 = _Type.Alias.Z32
  type Z64 = _Type.Alias.Z64
  type S8 = _Type.Alias.S8
  type S16 = _Type.Alias.S16
  type S32 = _Type.Alias.S32
  type S64 = _Type.Alias.S64
  type N = _Type.Alias.N
  type N8 = _Type.Alias.N8
  type N16 = _Type.Alias.N16
  type N32 = _Type.Alias.N32
  type N64 = _Type.Alias.N64
  type U8 = _Type.Alias.U8
  type U16 = _Type.Alias.U16
  type U32 = _Type.Alias.U32
  type U64 = _Type.Alias.U64
  type R = _Type.Alias.R
  type F32 = _Type.Alias.F32
  type F64 = _Type.Alias.F64
  type String = _Type.Alias.String
  type ST = _Type.Alias.ST

  type MS[I, V] = _Type.Alias.MS[I, V]
  type MSZ[V] = _Type.Alias.MS[Z, V]
  type MSZ8[V] = _Type.Alias.MS[Z8, V]
  type MSZ16[V] = _Type.Alias.MS[Z16, V]
  type MSZ32[V] = _Type.Alias.MS[Z32, V]
  type MSZ64[V] = _Type.Alias.MS[Z64, V]
  type MSS8[V] = _Type.Alias.MS[S8, V]
  type MSS16[V] = _Type.Alias.MS[S16, V]
  type MSS32[V] = _Type.Alias.MS[S32, V]
  type MSS64[V] = _Type.Alias.MS[S64, V]
  type MSN[V] = _Type.Alias.MS[N, V]
  type MSN8[V] = _Type.Alias.MS[N8, V]
  type MSN16[V] = _Type.Alias.MS[N16, V]
  type MSN32[V] = _Type.Alias.MS[N32, V]
  type MSN64[V] = _Type.Alias.MS[N64, V]
  type MSU8[V] = _Type.Alias.MS[U8, V]
  type MSU16[V] = _Type.Alias.MS[U16, V]
  type MSU32[V] = _Type.Alias.MS[U32, V]
  type MSU64[V] = _Type.Alias.MS[U64, V]

  type IS[I, V] = _Type.Alias.IS[I, V]
  type ISZ[V] = _Type.Alias.IS[Z, V]
  type ISZ8[V] = _Type.Alias.IS[Z8, V]
  type ISZ16[V] = _Type.Alias.IS[Z16, V]
  type ISZ32[V] = _Type.Alias.IS[Z32, V]
  type ISZ64[V] = _Type.Alias.IS[Z64, V]
  type ISS8[V] = _Type.Alias.IS[S8, V]
  type ISS16[V] = _Type.Alias.IS[S16, V]
  type ISS32[V] = _Type.Alias.IS[S32, V]
  type ISS64[V] = _Type.Alias.IS[S64, V]
  type ISN[V] = _Type.Alias.IS[N, V]
  type ISN8[V] = _Type.Alias.IS[N8, V]
  type ISN16[V] = _Type.Alias.IS[N16, V]
  type ISN32[V] = _Type.Alias.IS[N32, V]
  type ISN64[V] = _Type.Alias.IS[N64, V]
  type ISU8[V] = _Type.Alias.IS[U8, V]
  type ISU16[V] = _Type.Alias.IS[U16, V]
  type ISU32[V] = _Type.Alias.IS[U32, V]
  type ISU64[V] = _Type.Alias.IS[U64, V]

  type ZS = _Type.Alias.MS[Z, Z]

  val T: B = true
  val F: B = false
  val ST: _Template.type = _Template
  val _XBoolean: _Extractors.Boolean.type = _Extractors.Boolean
  val _XChar: _Extractors.Char.type = _Extractors.Char
  val _XInt: _Extractors.Int.type = _Extractors.Int
  val _XLong: _Extractors.Long.type = _Extractors.Long
  val _XFLoat: _Extractors.Float.type = _Extractors.Float
  val _XDouble: _Extractors.Double.type = _Extractors.Double
  val _XString: _Extractors.String.type = _Extractors.String

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

  final def cprintln(isErr: B, as: Any*): Unit = {
    if (isErr) eprintln(as: _*) else println(as: _*)
  }

  final def cprint(isErr: B, as: Any*): Unit = {
    if (isErr) eprint(as: _*) else print(as: _*)
  }

  final def println(as: Any*): Unit = {
    print(as: _*)
    Console.out.println()
    Console.out.flush()
  }

  final def print(as: Any*): Unit = {
    for (a <- as) {
      Console.out.print(a)
      Console.out.flush()
    }
  }

  final def eprintln(as: Any*): Unit = {
    print(as: _*)
    Console.err.println()
    Console.err.flush()
  }

  final def eprint(as: Any*): Unit = {
    for (a <- as) {
      Console.err.print(a)
      Console.err.flush()
    }
  }

  final def randomInt(): Z = math._Z.random

  import scala.language.experimental.macros

  object MS {
    def apply[I, V](values: V*): MS[I, V] = macro _macro.msApplyImpl[I, V]

    def create[I, V](size: I, default: V): MS[I, V] = macro _macro.msCreateImpl[I, V]
  }

  object MSZ {
    def apply[V](values: V*): MSZ[V] = collection._MS.apply[Z, V](values: _*)

    def create[V](size: Z, default: V): MSZ[V] = collection._MS.create[Z, V](size, default)
  }

  object MSZ8 {
    def apply[V](values: V*): MSZ8[V] = collection._MS.apply[Z8, V](values: _*)

    def create[V](size: Z8, default: V): MSZ8[V] = collection._MS.create[Z8, V](size, default)
  }

  object MSZ16 {
    def apply[V](values: V*): MSZ16[V] = collection._MS.apply[Z16, V](values: _*)

    def create[V](size: Z16, default: V): MSZ16[V] = collection._MS.create[Z16, V](size, default)
  }

  object MSZ32 {
    def apply[V](values: V*): MSZ32[V] = collection._MS.apply[Z32, V](values: _*)

    def create[V](size: Z32, default: V): MSZ32[V] = collection._MS.create[Z32, V](size, default)
  }

  object MSZ64 {
    def apply[V](values: V*): MSZ64[V] = collection._MS.apply[Z64, V](values: _*)

    def create[V](size: Z64, default: V): MSZ64[V] = collection._MS.create[Z64, V](size, default)
  }

  object MSS8 {
    def apply[V](values: V*): MSS8[V] = collection._MS.apply[S8, V](values: _*)

    def create[V](size: S8, default: V): MSS8[V] = collection._MS.create[S8, V](size, default)
  }

  object MSS16 {
    def apply[V](values: V*): MSS16[V] = collection._MS.apply[S16, V](values: _*)

    def create[V](size: S16, default: V): MSS16[V] = collection._MS.create[S16, V](size, default)
  }

  object MSS32 {
    def apply[V](values: V*): MSS32[V] = collection._MS.apply[S32, V](values: _*)

    def create[V](size: S32, default: V): MSS32[V] = collection._MS.create[S32, V](size, default)
  }

  object MSS64 {
    def apply[V](values: V*): MSS64[V] = collection._MS.apply[S64, V](values: _*)

    def create[V](size: S64, default: V): MSS64[V] = collection._MS.create[S64, V](size, default)
  }

  object MSN {
    def apply[V](values: V*): MSN[V] = collection._MS.apply[N, V](values: _*)

    def create[V](size: N, default: V): MSN[V] = collection._MS.create[N, V](size, default)
  }

  object MSN8 {
    def apply[V](values: V*): MSN8[V] = collection._MS.apply[N8, V](values: _*)

    def create[V](size: N8, default: V): MSN8[V] = collection._MS.create[N8, V](size, default)
  }

  object MSN16 {
    def apply[V](values: V*): MSN16[V] = collection._MS.apply[N16, V](values: _*)

    def create[V](size: N16, default: V): MSN16[V] = collection._MS.create[N16, V](size, default)
  }

  object MSN32 {
    def apply[V](values: V*): MSN32[V] = collection._MS.apply[N32, V](values: _*)

    def create[V](size: N32, default: V): MSN32[V] = collection._MS.create[N32, V](size, default)
  }

  object MSN64 {
    def apply[V](values: V*): MSN64[V] = collection._MS.apply[N64, V](values: _*)

    def create[V](size: N64, default: V): MSN64[V] = collection._MS.create[N64, V](size, default)
  }

  object MSU8 {
    def apply[V](values: V*): MSU8[V] = collection._MS.apply[U8, V](values: _*)

    def create[V](size: U8, default: V): MSU8[V] = collection._MS.create[U8, V](size, default)
  }

  object MSU16 {
    def apply[V](values: V*): MSU16[V] = collection._MS.apply[U16, V](values: _*)

    def create[V](size: U16, default: V): MSU16[V] = collection._MS.create[U16, V](size, default)
  }

  object MSU32 {
    def apply[V](values: V*): MSU32[V] = collection._MS.apply[U32, V](values: _*)

    def create[V](size: U32, default: V): MSU32[V] = collection._MS.create[U32, V](size, default)
  }

  object MSU64 {
    def apply[V](values: V*): MSU64[V] = collection._MS.apply[U64, V](values: _*)

    def create[V](size: U64, default: V): MSU64[V] = collection._MS.create[U64, V](size, default)
  }

  object IS {
    def apply[I, V](values: V*): IS[I, V] = macro _macro.isApplyImpl[I, V]

    def create[I, V](size: I, default: V): IS[I, V] = macro _macro.isCreateImpl[I, V]
  }

  object ISZ {
    def apply[V](values: V*): ISZ[V] = collection._IS.apply[Z, V](values: _*)

    def create[V](size: Z, default: V): ISZ[V] = collection._IS.create[Z, V](size, default)
  }

  object ISZ8 {
    def apply[V](values: V*): ISZ8[V] = collection._IS.apply[Z8, V](values: _*)

    def create[V](size: Z8, default: V): ISZ8[V] = collection._IS.create[Z8, V](size, default)
  }

  object ISZ16 {
    def apply[V](values: V*): ISZ16[V] = collection._IS.apply[Z16, V](values: _*)

    def create[V](size: Z16, default: V): ISZ16[V] = collection._IS.create[Z16, V](size, default)
  }

  object ISZ32 {
    def apply[V](values: V*): ISZ32[V] = collection._IS.apply[Z32, V](values: _*)

    def create[V](size: Z32, default: V): ISZ32[V] = collection._IS.create[Z32, V](size, default)
  }

  object ISZ64 {
    def apply[V](values: V*): ISZ64[V] = collection._IS.apply[Z64, V](values: _*)

    def create[V](size: Z64, default: V): ISZ64[V] = collection._IS.create[Z64, V](size, default)
  }

  object ISS8 {
    def apply[V](values: V*): ISS8[V] = collection._IS.apply[S8, V](values: _*)

    def create[V](size: S8, default: V): ISS8[V] = collection._IS.create[S8, V](size, default)
  }

  object ISS16 {
    def apply[V](values: V*): ISS16[V] = collection._IS.apply[S16, V](values: _*)

    def create[V](size: S16, default: V): ISS16[V] = collection._IS.create[S16, V](size, default)
  }

  object ISS32 {
    def apply[V](values: V*): ISS32[V] = collection._IS.apply[S32, V](values: _*)

    def create[V](size: S32, default: V): ISS32[V] = collection._IS.create[S32, V](size, default)
  }

  object ISS64 {
    def apply[V](values: V*): ISS64[V] = collection._IS.apply[S64, V](values: _*)

    def create[V](size: S64, default: V): ISS64[V] = collection._IS.create[S64, V](size, default)
  }

  object ISN {
    def apply[V](values: V*): ISN[V] = collection._IS.apply[N, V](values: _*)

    def create[V](size: N, default: V): ISN[V] = collection._IS.create[N, V](size, default)
  }

  object ISN8 {
    def apply[V](values: V*): ISN8[V] = collection._IS.apply[N8, V](values: _*)

    def create[V](size: N8, default: V): ISN8[V] = collection._IS.create[N8, V](size, default)
  }

  object ISN16 {
    def apply[V](values: V*): ISN16[V] = collection._IS.apply[N16, V](values: _*)

    def create[V](size: N16, default: V): ISN16[V] = collection._IS.create[N16, V](size, default)
  }

  object ISN32 {
    def apply[V](values: V*): ISN32[V] = collection._IS.apply[N32, V](values: _*)

    def create[V](size: N32, default: V): ISN32[V] = collection._IS.create[N32, V](size, default)
  }

  object ISN64 {
    def apply[V](values: V*): ISN64[V] = collection._IS.apply[N64, V](values: _*)

    def create[V](size: N64, default: V): ISN64[V] = collection._IS.create[N64, V](size, default)
  }

  object ISU8 {
    def apply[V](values: V*): ISU8[V] = collection._IS.apply[U8, V](values: _*)

    def create[V](size: U8, default: V): ISU8[V] = collection._IS.create[U8, V](size, default)
  }

  object ISU16 {
    def apply[V](values: V*): ISU16[V] = collection._IS.apply[U16, V](values: _*)

    def create[V](size: U16, default: V): ISU16[V] = collection._IS.create[U16, V](size, default)
  }

  object ISU32 {
    def apply[V](values: V*): ISU32[V] = collection._IS.apply[U32, V](values: _*)

    def create[V](size: U32, default: V): ISU32[V] = collection._IS.create[U32, V](size, default)
  }

  object ISU64 {
    def apply[V](values: V*): ISU64[V] = collection._IS.apply[U64, V](values: _*)

    def create[V](size: U64, default: V): ISU64[V] = collection._IS.create[U64, V](size, default)
  }

  object ZS {
    def apply(values: Z*): ZS = collection._MS.apply[Z, Z](values: _*)

    def create(size: Z, default: Z): ZS = collection._MS.create[Z, Z](size, default)
  }

  def $[T]: T = macro _macro.$Impl[T]

  val up = _Helper.Up

  val pat = _Helper.Pat

  def _assign[T](arg: T): T = macro _macro._assignImpl

  def __assign[T](arg: T): T = {
    arg match {
      case x: _Mutable => (if (x.owned) x.clone.owned = true else x.owned = true).asInstanceOf[T]
      case _ => arg
    }
  }

  import scala.language.implicitConversions

  final implicit class _Slang(val sc: StringContext) extends AnyVal {

    def z(args: Any*): Z = math._Z(sc.parts.mkString(""))

    def z8(args: Any*): Z8 = Z_Ext.toZ8(z(args: _*))

    def z16(args: Any*): Z16 = Z_Ext.toZ16(z(args: _*))

    def z32(args: Any*): Z32 = Z_Ext.toZ32(z(args: _*))

    def z64(args: Any*): Z64 = Z_Ext.toZ64(z(args: _*))

    def n(args: Any*): N = Z_Ext.toN(z(args: _*))

    def n8(args: Any*): N8 = Z_Ext.toN8(z(args: _*))

    def n16(args: Any*): N16 = Z_Ext.toN16(z(args: _*))

    def n32(args: Any*): N32 = Z_Ext.toN32(z(args: _*))

    def n64(args: Any*): N64 = Z_Ext.toN64(z(args: _*))

    def s8(args: Any*): S8 = Z_Ext.toS8(z(args: _*))

    def s16(args: Any*): S16 = Z_Ext.toS16(z(args: _*))

    def s32(args: Any*): S32 = Z_Ext.toS32(z(args: _*))

    def s64(args: Any*): S64 = Z_Ext.toS64(z(args: _*))

    def u8(args: Any*): U8 = Z_Ext.toU8(z(args: _*))

    def u16(args: Any*): U16 = Z_Ext.toU16(z(args: _*))

    def u32(args: Any*): U32 = Z_Ext.toU32(z(args: _*))

    def u64(args: Any*): U64 = Z_Ext.toU64(z(args: _*))

    def f32(args: Any*): F32 = math.Numbers.toF32(sc.parts.mkString("").toFloat)

    def f64(args: Any*): F64 = math.Numbers.toF64((sc.parts.mkString("") + "d").toDouble)

    def r(args: Any*): R = _Helper.R(sc.raw(args))

    def l[T](args: Any*): T = throw new NotImplementedError

    def lUnit(args: Any*): Unit = {}

    def lDef[T](args: Any*): T = throw new NotImplementedError

    def st(args: Any*): ST = macro _macro.st
  }

  final implicit def _Z(n: Int): Z = math._Z(n)

  final implicit def _Z(n: Long): Z = math._Z(n)

  final implicit def _Z(n: BigInt): Z = math._Z(n)

  final implicit def _2B(b: Boolean): B = new _B(b)

  final implicit def _2Boolean(b: B): Boolean = b.value

  final implicit def _2C(c: Char): C = new _C(c)

  final implicit def _2F32(f: Float): F32 = new math._F32(f)

  final implicit def _2F64(d: Double): F64 = new math._F64(d)

  final implicit def _2String(s: Predef.String): String = new _String(s)
}
