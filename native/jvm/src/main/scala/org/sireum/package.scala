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

  type MS[I, V] = _Type.Alias.MS[I, V]
  type MSZ[V] = _Type.Alias.MS[Z, V]
  type IS[I, V] = _Type.Alias.IS[I, V]
  type ISZ[V] = _Type.Alias.IS[Z, V]

  type ZS = _Type.Alias.MS[Z, Z]

  val T: B = true
  val F: B = false

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

  import scala.language.experimental.macros

  object MS {
    def apply[I, V](values: V*): MS[I, V] = macro _macro.msApplyImpl[I, V]

    def create[I, V](size: I, default: V): MS[I, V] = macro _macro.msCreateImpl[I, V]
  }

  object IS {
    def apply[I, V](values: V*): IS[I, V] = macro _macro.isApplyImpl[I, V]

    def create[I, V](size: I, default: V): IS[I, V] = macro _macro.isCreateImpl[I, V]
  }

  object MSZ {
    def apply[V](values: V*): MSZ[V] = macro _macro.msApplyImpl[Z, V]

    def create[V](size: Z, default: V): MSZ[V] = macro _macro.msCreateImpl[Z, V]
  }

  object ISZ {
    def apply[V](values: V*): ISZ[V] = macro _macro.isApplyImpl[Z, V]

    def create[V](size: Z, default: V): ISZ[V] = macro _macro.isCreateImpl[Z, V]
  }

  object ZS {
    def apply(values: Z*): ZS = macro _macro.msApplyImpl[Z, Z]

    def create(size: Z, default: Z): ZS = macro _macro.msCreateImpl[Z, Z]
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

    def l[T](args: Any*): T = ??? //macro _macro.lDefImpl[T]

    def lUnit(args: Any*): Unit = {} //macro _macro.lUnitImpl

    def lDef[T](args: Any*): T = ??? //macro _macro.lDefImpl[T]
  }

  final implicit def _Z(n: Int): Z = math.Numbers.toZ(n)

  final implicit def _Z(n: Long): Z = math.Numbers.toZ(n)

  final implicit def _Z(n: BigInt): Z = math.Numbers.toZ(n)

  final implicit def _2B(b: Boolean): B = new _B(b)

  final implicit def _2Boolean(b: B): Boolean = b.value
}
