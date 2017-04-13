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
  type TT[T] = scala.reflect.runtime.universe.TypeTag[T]

  type B = scala.Boolean
  type Z = math._Z
  type Z8 = scala.Byte
  type Z16 = scala.Short
  type Z32 = scala.Int
  type Z64 = scala.Long
  type S8 = scala.Byte
  type S16 = scala.Short
  type S32 = scala.Int
  type S64 = scala.Long
  type N = math._N
  type N8 = spire.math.UByte
  type N16 = spire.math.UShort
  type N32 = spire.math.UInt
  type N64 = spire.math.ULong
  type U8 = spire.math.UByte
  type U16 = spire.math.UShort
  type U32 = spire.math.UInt
  type U64 = spire.math.ULong
  type R = spire.math.Real
  type F32 = scala.Float
  type F64 = scala.Double

  type MS[I, V] = collection._MS[I, V]
  type MSZ[V] = collection._MS[Z, V]
  type IS[I, V] = collection._IS[I, V]
  type ISZ[V] = collection._IS[Z, V]

  type ZS = collection._MS[Z, Z]

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

  object _R {

    final def apply(r: String): R = spire.math.Real(r.replaceAllLiterally(" ", ""))

    final def random: R = apply(math._Z.random.toString + "." + math._N.random.toString)
  }

  def _clone[T](o: T): T = o match {
    case o: IS[_, _] => o.clone.asInstanceOf[T]
    case o: MS[_, _] => o.clone.asInstanceOf[T]
    case o: _Clonable => o.clone.asInstanceOf[T]
    case x => x
  }

  def _quote(s: String): String = {
    def escape(s: String): String = s.flatMap(escapedChar)

    def escapedChar(ch: Char): String = ch match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"' => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _ => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else String.valueOf(ch)
    }

    "\"" + escape(s) + "\""
  }

  def _append(sb: StringBuilder, x: Any): Unit = x match {
    case x: String => sb.append(_quote(x))
    case _ => sb.append(x)
  }

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

  final class _Up[T] {
    def update(lhs: T, rhs: T): Unit = macro _macro.up[T]
  }

  def up[T]: _Up[T] = new _Up[T]

  def _assign[T](arg: T): T = macro _macro._assign[T]

  def __assign[T](arg: T): T = arg match {
    case x: _Record => (if (x.owned) x.clone.owned = true else x.owned = true).asInstanceOf[T]
    case x: MS[_, _] => x.clone.asInstanceOf[T]
    case _ => arg
  }

  import scala.language.implicitConversions

  final implicit class _Logika(val sc: StringContext) extends AnyVal {

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

    def f32(args: Any*): F32 = sc.parts.mkString("").toFloat

    def f64(args: Any*): F64 = (sc.parts.mkString("") + "d").toDouble

    def r(args: Any*): R = _R(sc.raw(args))

    def l(args: Any*): Unit = macro _macro.lImpl

    def c[T](args: Any*): T = macro _macro.cImpl[T]
  }

  final implicit def _Z(n: Int): Z = math._Z(n)

  final implicit def _Z(n: Long): Z = math._Z(n)

  final implicit def _Z(n: BigInt): Z = math._Z(n)

  final implicit class _2B(val x: Boolean) extends AnyVal {
    def ^|(other: B): B = x != other
  }

  final implicit class _2R(val n: R) extends AnyVal {
    def <(other: R): B = n.compare(other) < 0

    def >(other: R): B = n.compare(other) > 0

    def <=(other: R): B = n.compare(other) <= 0

    def >=(other: R): B = n.compare(other) >= 0
  }

  final class helper extends scala.annotation.StaticAnnotation

  final class pure extends scala.annotation.StaticAnnotation

  final class hidden extends scala.annotation.StaticAnnotation

  final class part extends scala.annotation.StaticAnnotation
}
