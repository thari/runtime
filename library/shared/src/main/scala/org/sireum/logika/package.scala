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
  type ZS = org.sireum.MS[Z, Z]
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
  type helper = org.sireum.helper
  type pure = org.sireum.pure

  val helper: org.sireum.helper.type = org.sireum.helper
  val ZS: org.sireum.ZS.type = org.sireum.ZS
  val T: B = org.sireum.T
  val F: B = org.sireum.F

  object BS {
    def apply(values: B*): BS = MSZ[B](values: _*)

    def create(size: Z, default: B): BS = MS.create[Z, B](size, default)
  }

  object Z8S {
    def apply(values: Z8*): Z8S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: Z8): Z8S = org.sireum.MSZ.create(size, default)
  }

  object Z16S {
    def apply(values: Z16*): Z16S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: Z16): Z16S = org.sireum.MSZ.create(size, default)
  }

  object Z32S {
    def apply(values: Z32*): Z32S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: Z32): Z32S = org.sireum.MSZ.create(size, default)
  }

  object Z64S {
    def apply(values: Z64*): Z64S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: Z64): Z64S = org.sireum.MSZ.create(size, default)
  }

  object NS {
    def apply(values: N*): NS = org.sireum.MSZ(values: _*)

    def create(size: Z, default: N): NS = org.sireum.MSZ.create(size, default)
  }

  object N8S {
    def apply(values: N8*): N8S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: N8): N8S = org.sireum.MSZ.create(size, default)
  }

  object N16S {
    def apply(values: N16*): N16S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: N16): N16S = org.sireum.MSZ.create(size, default)
  }

  object N32S {
    def apply(values: N32*): N32S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: N32): N32S = org.sireum.MSZ.create(size, default)
  }

  object N64S {
    def apply(values: N64*): N64S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: N64): N64S = org.sireum.MSZ.create(size, default)
  }

  object S8S {
    def apply(values: S8*): S8S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: S8): S8S = org.sireum.MSZ.create(size, default)
  }

  object S16S {
    def apply(values: S16*): S16S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: S16): S16S = org.sireum.MSZ.create(size, default)
  }

  object S32S {
    def apply(values: S32*): S32S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: S32): S32S = org.sireum.MSZ.create(size, default)
  }

  object S64S {
    def apply(values: S64*): S64S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: S64): S64S = org.sireum.MSZ.create(size, default)
  }

  object U8S {
    def apply(values: U8*): U8S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: U8): U8S = org.sireum.MSZ.create(size, default)
  }

  object U16S {
    def apply(values: U16*): U16S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: U16): U16S = org.sireum.MSZ.create(size, default)
  }

  object U32S {
    def apply(values: U32*): U32S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: U32): U32S = org.sireum.MSZ.create(size, default)
  }

  object U64S {
    def apply(values: U64*): U64S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: U64): U64S = org.sireum.MSZ.create(size, default)
  }

  object F32S {
    def apply(values: F32*): F32S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: F32): F32S = org.sireum.MSZ.create(size, default)
  }

  object F64S {
    def apply(values: F64*): F64S = org.sireum.MSZ(values: _*)

    def create(size: Z, default: F64): F64S = org.sireum.MSZ.create(size, default)
  }

  object RS {
    def apply(values: R*): RS = org.sireum.MSZ(values: _*)

    def create(size: Z, default: R): RS = org.sireum.MSZ.create(size, default)
  }

  final implicit class _MSClone[I, V](val ms: MS[I, V]) extends AnyVal {
    def clone: MS[I, V] = ms.$clone
  }

  final implicit class _Slang(val sc: StringContext) extends AnyVal {

    def z(args: scala.Any*): Z = org.sireum.Z.$String(sc.parts.mkString(""))

    def z8(args: scala.Any*): Z8 = org.sireum.Z8.$String(sc.parts.mkString(""))

    def z16(args: scala.Any*): Z16 = org.sireum.Z16.$String(sc.parts.mkString(""))

    def z32(args: scala.Any*): Z32 = org.sireum.Z32.$String(sc.parts.mkString(""))

    def z64(args: scala.Any*): Z64 = org.sireum.Z64.$String(sc.parts.mkString(""))

    def n(args: scala.Any*): N = org.sireum.N.$String(sc.parts.mkString(""))

    def n8(args: scala.Any*): N8 = org.sireum.N8.$String(sc.parts.mkString(""))

    def n16(args: scala.Any*): N16 = org.sireum.N16.$String(sc.parts.mkString(""))

    def n32(args: scala.Any*): N32 = org.sireum.N32.$String(sc.parts.mkString(""))

    def n64(args: scala.Any*): N64 = org.sireum.N64.$String(sc.parts.mkString(""))

    def s8(args: scala.Any*): S8 = org.sireum.S8.$String(sc.parts.mkString(""))

    def s16(args: scala.Any*): S16 = org.sireum.S16.$String(sc.parts.mkString(""))

    def s32(args: scala.Any*): S32 = org.sireum.S32.$String(sc.parts.mkString(""))

    def s64(args: scala.Any*): S64 = org.sireum.S64.$String(sc.parts.mkString(""))

    def u8(args: scala.Any*): U8 = org.sireum.U8.$String(sc.parts.mkString(""))

    def u16(args: scala.Any*): U16 = org.sireum.U16.$String(sc.parts.mkString(""))

    def u32(args: scala.Any*): U32 = org.sireum.U32.$String(sc.parts.mkString(""))

    def u64(args: scala.Any*): U64 = org.sireum.U64.$String(sc.parts.mkString(""))

    def f32(args: scala.Any*): F32 = org.sireum.F32.$String(sc.parts.mkString(""))

    def f64(args: scala.Any*): F64 = org.sireum.F64.$String(sc.parts.mkString(""))

    def r(args: scala.Any*): R = org.sireum.R.$String(sc.parts.mkString(""))

    import scala.language.experimental.macros

    def l(args: scala.Any*): Unit = macro org.sireum.$internal.Macro.lUnit
  }

  final def readInt(msg: String = "Enter an integer: "): Z = org.sireum.promptInt(msg)

  final def println(as: Any*): Unit = org.sireum.println(as: _*)

  final def print(as: Any*): Unit = org.sireum.print(as:_ *)

  final def randomInt(): Z = org.sireum.randomInt()

  final def Z(n: scala.Int): Z = org.sireum.Z(n)

  final def Z(n: scala.Long): Z = org.sireum.Z(n)

  final def Z(n: scala.BigInt): Z = org.sireum.Z(n)

  final def String(s: Predef.String): String = org.sireum.String(s)
}
