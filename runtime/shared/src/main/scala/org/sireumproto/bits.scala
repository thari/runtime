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

package org.sireumproto

import scala.meta._

class bits(signed: Boolean,
           width: Int,
           min: Option[BigInt] = None,
           max: Option[BigInt] = None,
           index: Boolean = false) extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case q"class $tname" =>
        val q"new bits(..$args)" = this
        var width = 0
        var signed = false
        var indexB = false
        var minOpt: Option[BigInt] = None
        var maxOpt: Option[BigInt] = None
        val bi8 = scala.BigInt(8)
        val bi16 = scala.BigInt(16)
        val bi32 = scala.BigInt(32)
        val bi64 = scala.BigInt(64)
        for (arg <- args) {
          arg match {
            case arg"signed = ${exp: Term}" =>
              helper.extractBoolean(exp) match {
                case Some(b) => signed = b
                case _ => abort(arg.pos, s"Invalid Slang @bits signed argument: ${arg.syntax}")
              }
            case arg"width = ${exp: Term}" =>
              val nOpt = helper.extractInt(exp)
              nOpt match {
                case Some(`bi8`) | Some(`bi16`) | Some(`bi32`) | Some(`bi64`) => width = nOpt.get.toInt
                case _ => abort(arg.pos, s"Invalid Slang @bits width argument: ${arg.syntax} (only 8, 16, 32, or 64 are currently supported)")
              }
            case arg"min = ${exp: Term}" => minOpt = helper.extractInt(exp)
            case arg"max = ${exp: Term}" => maxOpt = helper.extractInt(exp)
            case arg"index = ${exp: Term}" =>
              helper.extractBoolean(exp) match {
                case Some(b) => indexB = b
                case _ => abort(arg.pos, s"Invalid Slang @bits index argument: ${arg.syntax}")
              }
            case _ => abort(arg.pos, s"Invalid Slang @bits argument: ${arg.syntax}")
          }
        }
        val (wMin, wMax) =
          if (signed) (BigInt(-2).pow(width - 1), BigInt(2).pow(width - 1) - 1)
          else (BigInt(0), BigInt(2).pow(width) - 1)
        if (indexB && minOpt.isEmpty) abort(tree.pos, s"Slang @bits ${tname.value}'s min should specified when index is enabled.")
        val min = minOpt.getOrElse(wMin)
        val max = maxOpt.getOrElse(wMax)
        val index = if (indexB) min else BigInt(0)
        if (min > max) abort(tree.pos, s"Slang @bits ${tname.value}'s min ($min) should not be greater than its max ($max).")
        val signedString = if (signed) "signed" else "unsigned"
        if (min < wMin) abort(tree.pos, s"Slang @bits ${tname.value}'s min ($min) should not be less than its $signedString bit-width minimum ($wMin).")
        if (max > wMax) abort(tree.pos, s"Slang @bits ${tname.value}'s max ($max) should not be greater than its $signedString bit-width maximum ($wMax).")
        //if (index < min) abort(tree.pos, s"Slang @bits ${tname.value}'s index ($index) should not be less than its min ($min).")
        //if (index > max) abort(tree.pos, s"Slang @bits ${tname.value}'s index ($index) should not be less than its max ($max).")
        val wrapped = min == wMin && max == wMax
        val result = bits.q(signed, width, wrapped, min, max, index.toLong, tname.value)
        //println(result)
        result
      case _ => abort(tree.pos, s"Invalid Slang @bits on: ${tree.structure}")
    }
  }
}

object bits {
  def q(signed: Boolean, width: Int, wrapped: Boolean, min: BigInt, max: BigInt, index: Long, name: String): Term.Block = {
    val typeName = Type.Name(name)
    val termName = Term.Name(name)
    val iTermName = helper.zCompanionName(name)
    val (isTermName, isTypeName) = helper.iSName(name)
    val (msTermName, msTypeName) = helper.mSName(name)
    val lowerTermName = Term.Name(name.toLowerCase)
    val ctorName = Ctor.Name(name)
    val scTypeName = helper.scName(name)
    val nameStr = Lit.String(name)
    val isZeroIndex = Lit.Boolean(index == 0)
    val minErrorMessage = Lit.String(s" is less than $name.Min ($min)")
    val maxErrorMessage = Lit.String(s" is greater than $name.Max ($max)")
    val (valueTypeName, bvType, minLit, maxLit, indexLit, randomSeed, apply, intObject, longObject, bigIntObject) = width match {
      case 8 => (
        t"scala.Byte",
        ctor"Z.BV.Byte[$typeName]",
        q"(${Lit.Int(min.toInt)}).toByte",
        q"(${Lit.Int(max.toInt)}).toByte",
        q"(${Lit.Int(index.toInt)}).toByte",
        q"new $ctorName((n + zMin).toBigInt.toByte)",
        q"""def apply(n: Z): $typeName = n match {
              case n: Z.MP.Long =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toByte)
              case n: Z.MP.BigInt =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toByte)
              case _ => halt(s"Unsupported $$Name creation from $${n.Name}.")
            }""",
        q"""object Int extends $$ZCompanionInt[$typeName] {
              def apply(n: scala.Int): $typeName = if (isWrapped) new $ctorName(n.toByte) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Int] = scala.Some(n.toMP.toIntOpt.get)
            }""",
        q"""object Long extends $$ZCompanionLong[$typeName] {
              def apply(n: scala.Long): $typeName = if (isWrapped) new $ctorName(n.toByte) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Long] = scala.Some(n.toMP.toLongOpt.get)
            }""",
        q"""object BigInt extends $$ZCompanionBigInt[$typeName] {
              def apply(n: scala.BigInt): $typeName = if (isWrapped) new $ctorName(n.toByte) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)
            }"""
      )
      case 16 => (
        t"scala.Short",
        ctor"Z.BV.Short[$typeName]",
        q"(${Lit.Int(min.toInt)}).toShort",
        q"(${Lit.Int(max.toInt)}).toShort",
        q"(${Lit.Int(index.toInt)}).toShort",
        q"new $ctorName((n + zMin).toBigInt.toShort)",
        q"""def apply(n: Z): $typeName = n match {
              case n: Z.MP.Long =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toShort)
              case n: Z.MP.BigInt =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toShort)
              case _ => halt(s"Unsupported $$Name creation from $${n.Name}.")
            }""",
        q"""object Int extends $$ZCompanionInt[$typeName] {
              def apply(n: scala.Int): $typeName = if (isWrapped) new $ctorName(n.toShort) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Int] = scala.Some(n.toMP.toIntOpt.get)
            }""",
        q"""object Long extends $$ZCompanionLong[$typeName] {
              def apply(n: scala.Long): $typeName = if (isWrapped) new $ctorName(n.toShort) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Long] = scala.Some(n.toMP.toLongOpt.get)
            }""",
        q"""object BigInt extends $$ZCompanionBigInt[$typeName] {
              def apply(n: scala.BigInt): $typeName = if (isWrapped) new $ctorName(n.toShort) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)
            }"""
      )
      case 32 => (
        t"scala.Int",
        ctor"Z.BV.Int[$typeName]",
        Lit.Int(min.toInt),
        Lit.Int(max.toInt),
        Lit.Int(index.toInt),
        q"new $ctorName((n + zMin).toBigInt.toInt)",
        q"""def apply(n: Z): $typeName = n match {
              case n: Z.MP.Long =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toInt)
              case n: Z.MP.BigInt =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toInt)
              case _ => halt(s"Unsupported $$Name creation from $${n.Name}.")
            }""",
        q"""object Int extends $$ZCompanionInt[$typeName] {
              def apply(n: scala.Int): $typeName = if (isWrapped) new $ctorName(n) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Int] = {
                val v = n.toMP
                if (scala.Int.MinValue <= v && v <= scala.Int.MaxValue) scala.Some(v.toIntOpt.get)
                else scala.None
              }
            }""",
        q"""object Long extends $$ZCompanionLong[$typeName] {
              def apply(n: scala.Long): $typeName = if (isWrapped) new $ctorName(n.toInt) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Long] = scala.Some(n.toMP.toLongOpt.get)
            }""",
        q"""object BigInt extends $$ZCompanionBigInt[$typeName] {
              def apply(n: scala.BigInt): $typeName = if (isWrapped) new $ctorName(n.toInt) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)
            }"""
      )
      case 64 => (
        t"scala.Long",
        ctor"Z.BV.Long[$typeName]",
        Lit.Long(min.toLong),
        Lit.Long(max.toLong),
        Lit.Long(index),
        q"new $ctorName((n + zMin).toBigInt.toLong)",
        q"""def apply(n: Z): $typeName = n match {
              case n: Z.MP.Long =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value)
              case n: Z.MP.BigInt =>
                if (!isWrapped) {
                  assert(Min.toMP <= n, n + $minErrorMessage)
                  assert(n <= Max.toMP, n + $maxErrorMessage)
                }
                new $ctorName(n.value.toLong)
              case _ => halt(s"Unsupported $$Name creation from $${n.Name}.")
            }""",
        q"""object Int extends $$ZCompanionInt[$typeName] {
              def apply(n: scala.Int): $typeName = if (isWrapped) new $ctorName(n) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Int] = {
                val v = n.toMP
                if (scala.Int.MinValue <= v && v <= scala.Int.MaxValue) scala.Some(v.toIntOpt.get)
                else scala.None
              }
            }""",
        q"""object Long extends $$ZCompanionLong[$typeName] {
              def apply(n: scala.Long): $typeName = if (isWrapped) new $ctorName(n) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.Long] = {
                val v = n.toMP
                if (scala.Long.MinValue <= v && v <= scala.Long.MaxValue) scala.Some(v.toLongOpt.get)
                else scala.None
              }
            }""",
        q"""object BigInt extends $$ZCompanionBigInt[$typeName] {
              def apply(n: scala.BigInt): $typeName = if (isWrapped) new $ctorName(n.toLong) else $termName(Z.MP(n))
              def unapply(n: $typeName): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)
            }"""
      )
    }

    Term.Block(List(
      q"""final class $typeName(val value: $valueTypeName) extends AnyVal with $bvType {
            @inline def Name: Predef.String = $termName.Name
            @inline def BitWidth: scala.Int = $termName.BitWidth
            @inline def Min: $typeName = $termName.Min
            @inline def Max: $typeName = $termName.Max
            @inline def Index: $typeName = $termName.Index
            @inline def isZeroIndex: scala.Boolean = $termName.isZeroIndex
            @inline def isSigned: scala.Boolean = $termName.isSigned
            @inline def isWrapped: scala.Boolean = $termName.isWrapped
            def make(v: $valueTypeName): $typeName = $termName(v)
          }""",
      q"""object $termName extends $$ZCompanion[$typeName] {
            type $isTypeName[T <: Immutable] = IS[$typeName, T]
            type $msTypeName[T] = MS[$typeName, T]
            val Name: Predef.String = $nameStr
            val BitWidth: scala.Int = ${Lit.Int(width)}
            val Min: $typeName = new $ctorName($minLit)
            val Max: $typeName = new $ctorName($maxLit)
            val Index: $typeName = new $ctorName($indexLit)
            val isZeroIndex: scala.Boolean = $isZeroIndex
            val isSigned: scala.Boolean = ${Lit.Boolean(signed)}
            val isWrapped: scala.Boolean = ${Lit.Boolean(wrapped)}
            val isBitVector: scala.Boolean = true
            val hasMin: scala.Boolean = true
            val hasMax: scala.Boolean = true
            def random: $typeName = randomSeed(System.currentTimeMillis)
            def randomSeed(seed: Z): $typeName = {
              val zMin = Z(Min.toBigInt)
              val d = Z(Max.toBigInt) - zMin + Z.MP.one
              val n = Z.randomSeed(seed) % d
              $randomSeed
            }
            $apply;
            def unapply(n: $typeName): scala.Option[Z] = scala.Some(n.toMP);
            $intObject;
            $longObject;
            object String extends $$ZCompanionString[$typeName] {
              def apply(s: Predef.String): $typeName = BigInt(scala.BigInt(helper.normNum(s)))
              def unapply(n: $typeName): scala.Option[Predef.String] = scala.Some(n.toBigInt.toString)
            };
            $bigIntObject;
            object $isTermName {
              def apply[V <: Immutable](args: V*): $isTypeName[V] = IS[$typeName, V](args: _*)
              def create[V <: Immutable](size: Z, default: V): $isTypeName[V] = IS.create[$typeName, V](size, default)
            }
            object $msTermName {
              def apply[V](args: V*): $msTypeName[V] = MS[$typeName, V](args: _*)
              def create[V](size: Z, default: V): $msTypeName[V] = MS.create[$typeName, V](size, default)
            }
            implicit class $scTypeName(val sc: StringContext) {
              object $lowerTermName {
                def apply(args: Any*): $typeName = {
                  assume(args.isEmpty && sc.parts.length == 1)
                  String(sc.parts.head)
                }
                def unapply(n: $typeName): scala.Boolean = {
                  assume(sc.parts.length == 1)
                  n == String(sc.parts.head)
                }
              }
            }
            import scala.language.implicitConversions
            implicit val $iTermName: $$ZCompanion[$typeName] = this
          }"""
    ))
  }
}

