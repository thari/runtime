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
  type B = org.sireum.B
  type Z = org.sireum.Z
  type Z8 = org.sireum.Z8
  type Z16 = org.sireum.Z16
  type Z32 = org.sireum.Z32
  type Z64 = org.sireum.Z64
  type S8 = org.sireum.S8
  type S16 = org.sireum.S16
  type S32 = org.sireum.S32
  type S64 = org.sireum.S64
  type N = org.sireum.N
  type N8 = org.sireum.N8
  type N16 = org.sireum.N16
  type N32 = org.sireum.N32
  type N64 = org.sireum.N64
  type U8 = org.sireum.U8
  type U16 = org.sireum.U16
  type U32 = org.sireum.U32
  type U64 = org.sireum.U64
  type R = org.sireum.R
  type F32 = org.sireum.F32
  type F64 = org.sireum.F64

  type ZS = collection._MS[Z, Z]

  val T: B = org.sireum.T
  val F: B = org.sireum.F

  final def readInt(msg: String = "Enter an integer: "): Z = org.sireum.readInt(msg)

  final def println(as: Any*): Unit = org.sireum.println(as: _*)

  final def print(as: Any*): Unit = org.sireum.print(as:_ *)

  final def randomInt(): Z = org.sireum.randomInt()

  type BS = org.sireum.MS[Z, B]
  type Z8S = org.sireum.MS[Z, Z8]
  type Z16S = org.sireum.MS[Z, Z16]
  type Z32S = org.sireum.MS[Z, Z32]
  type Z64S = org.sireum.MS[Z, Z64]
  type NS = org.sireum.MS[Z, N]
  type N8S = org.sireum.MS[Z, N8]
  type N16S = org.sireum.MS[Z, N16]
  type N32S = org.sireum.MS[Z, N32]
  type N64S = org.sireum.MS[Z, N64]
  type S8S = org.sireum.MS[Z, S8]
  type S16S = org.sireum.MS[Z, S16]
  type S32S = org.sireum.MS[Z, S32]
  type S64S = org.sireum.MS[Z, S64]
  type U8S = org.sireum.MS[Z, U8]
  type U16S = org.sireum.MS[Z, U16]
  type U32S = org.sireum.MS[Z, U32]
  type U64S = org.sireum.MS[Z, U64]
  type F32S = org.sireum.MS[Z, F32]
  type F64S = org.sireum.MS[Z, F64]
  type RS = org.sireum.MS[Z, R]

  import scala.language.experimental.macros

  object BS {
    def apply(values: B*): BS = macro _macro.msApplyImpl[Z, B]

    def create(size: Z, default: B): BS = macro _macro.msCreateImpl[Z, B]
  }

  val ZS = org.sireum.ZS

  object Z8S {
    def apply(values: Z8*): Z8S = macro _macro.msApplyImpl[Z, Z8]

    def create(size: Z, default: Z8): Z8S = macro _macro.msCreateImpl[Z, Z8]
  }

  object Z16S {
    def apply(values: Z16*): Z16S = macro _macro.msApplyImpl[Z, Z16]

    def create(size: Z, default: Z16): Z16S = macro _macro.msCreateImpl[Z, Z16]
  }

  object Z32S {
    def apply(values: Z32*): Z32S = macro _macro.msApplyImpl[Z, Z32]

    def create(size: Z, default: Z32): Z32S = macro _macro.msCreateImpl[Z, Z32]
  }

  object Z64S {
    def apply(values: Z64*): Z64S = macro _macro.msApplyImpl[Z, Z64]

    def create(size: Z, default: Z64): Z64S = macro _macro.msCreateImpl[Z, Z64]
  }

  object NS {
    def apply(values: N*): NS = macro _macro.msApplyImpl[Z, N]

    def create(size: Z, default: N): NS = macro _macro.msCreateImpl[Z, N]
  }

  object N8S {
    def apply(values: N8*): N8S = macro _macro.msApplyImpl[Z, N8]

    def create(size: Z, default: N8): N8S = macro _macro.msCreateImpl[Z, N8]
  }

  object N16S {
    def apply(values: N16*): N16S = macro _macro.msApplyImpl[Z, N16]

    def create(size: Z, default: N16): N16S = macro _macro.msCreateImpl[Z, N16]
  }

  object N32S {
    def apply(values: N32*): N32S = macro _macro.msApplyImpl[Z, N32]

    def create(size: Z, default: N32): N32S = macro _macro.msCreateImpl[Z, N32]
  }

  object N64S {
    def apply(values: N64*): N64S = macro _macro.msApplyImpl[Z, N64]

    def create(size: Z, default: N64): N64S = macro _macro.msCreateImpl[Z, N64]
  }

  object S8S {
    def apply(values: S8*): S8S = macro _macro.msApplyImpl[Z, S8]

    def create(size: Z, default: S8): S8S = macro _macro.msCreateImpl[Z, S8]
  }

  object S16S {
    def apply(values: S16*): S16S = macro _macro.msApplyImpl[Z, S16]

    def create(size: Z, default: S16): S16S = macro _macro.msCreateImpl[Z, S16]
  }

  object S32S {
    def apply(values: S32*): S32S = macro _macro.msApplyImpl[Z, S32]

    def create(size: Z, default: S32): S32S = macro _macro.msCreateImpl[Z, S32]
  }

  object S64S {
    def apply(values: S64*): S64S = macro _macro.msApplyImpl[Z, S64]

    def create(size: Z, default: S64): S64S = macro _macro.msCreateImpl[Z, S64]
  }

  object U8S {
    def apply(values: U8*): U8S = macro _macro.msApplyImpl[Z, U8]

    def create(size: Z, default: U8): U8S = macro _macro.msCreateImpl[Z, U8]
  }

  object U16S {
    def apply(values: U16*): U16S = macro _macro.msApplyImpl[Z, U16]

    def create(size: Z, default: U16): U16S = macro _macro.msCreateImpl[Z, U16]
  }

  object U32S {
    def apply(values: U32*): U32S = macro _macro.msApplyImpl[Z, U32]

    def create(size: Z, default: U32): U32S = macro _macro.msCreateImpl[Z, U32]
  }

  object U64S {
    def apply(values: U64*): U64S = macro _macro.msApplyImpl[Z, U64]

    def create(size: Z, default: U64): U64S = macro _macro.msCreateImpl[Z, U64]
  }

  object F32S {
    def apply(values: F32*): F32S = macro _macro.msApplyImpl[Z, F32]

    def create(size: Z, default: F32): F32S = macro _macro.msCreateImpl[Z, F32]
  }

  object F64S {
    def apply(values: F64*): F64S = macro _macro.msApplyImpl[Z, F64]

    def create(size: Z, default: F64): F64S = macro _macro.msCreateImpl[Z, F64]
  }

  object RS {
    def apply(values: R*): RS = macro _macro.msApplyImpl[Z, R]

    def create(size: Z, default: R): RS = macro _macro.msCreateImpl[Z, R]
  }

  import scala.language.implicitConversions

  final implicit class _Slang(val sc: StringContext) extends AnyVal {

    def z(args: Any*): Z = org.sireum.math._Z(sc.parts.mkString(""))

    def z8(args: Any*): Z8 = org.sireum.Z_Ext.toZ8(z(args: _*))

    def z16(args: Any*): Z16 = org.sireum.Z_Ext.toZ16(z(args: _*))

    def z32(args: Any*): Z32 = org.sireum.Z_Ext.toZ32(z(args: _*))

    def z64(args: Any*): Z64 = org.sireum.Z_Ext.toZ64(z(args: _*))

    def n(args: Any*): N = org.sireum.Z_Ext.toN(z(args: _*))

    def n8(args: Any*): N8 = org.sireum.Z_Ext.toN8(z(args: _*))

    def n16(args: Any*): N16 = org.sireum.Z_Ext.toN16(z(args: _*))

    def n32(args: Any*): N32 = org.sireum.Z_Ext.toN32(z(args: _*))

    def n64(args: Any*): N64 = org.sireum.Z_Ext.toN64(z(args: _*))

    def s8(args: Any*): S8 = org.sireum.Z_Ext.toS8(z(args: _*))

    def s16(args: Any*): S16 = org.sireum.Z_Ext.toS16(z(args: _*))

    def s32(args: Any*): S32 = org.sireum.Z_Ext.toS32(z(args: _*))

    def s64(args: Any*): S64 = org.sireum.Z_Ext.toS64(z(args: _*))

    def u8(args: Any*): U8 = org.sireum.Z_Ext.toU8(z(args: _*))

    def u16(args: Any*): U16 = org.sireum.Z_Ext.toU16(z(args: _*))

    def u32(args: Any*): U32 = org.sireum.Z_Ext.toU32(z(args: _*))

    def u64(args: Any*): U64 = org.sireum.Z_Ext.toU64(z(args: _*))

    def f32(args: Any*): F32 = math.Numbers.toF32(sc.parts.mkString("").toFloat)

    def f64(args: Any*): F64 = math.Numbers.toF64((sc.parts.mkString("") + "d").toDouble)

    def r(args: Any*): R = org.sireum._Helper.R(sc.raw(args))

    def l(args: Any*): Unit = {}
  }

  final implicit def _Z(n: Int): Z = org.sireum._Z(n)

  final implicit def _Z(n: Long): Z = math._Z(n)

  final implicit def _Z(n: BigInt): Z = math._Z(n)

  final implicit def _2B(b: Boolean): B = new B(b)

  final implicit def _2Boolean(b: B): Boolean = b.value
}
