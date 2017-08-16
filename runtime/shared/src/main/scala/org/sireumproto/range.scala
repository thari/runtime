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

class range(min: Option[BigInt] = None,
            max: Option[BigInt] = None,
            index: Boolean = false) extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case q"class $tname" =>
        val q"new range(..$args)" = this
        var minOpt: Option[BigInt] = None
        var maxOpt: Option[BigInt] = None
        var indexB = false
        for (arg <- args) {
          arg match {
            case arg"min = ${exp: Term}" => minOpt = helper.extractInt(exp)
            case arg"max = ${exp: Term}" => maxOpt = helper.extractInt(exp)
            case arg"index = ${exp: Term}" =>
              helper.extractBoolean(exp) match {
                case Some(b) => indexB = b
                case _ =>
              }
            case _ => abort(arg.pos, s"Invalid Slang @range ${tname.value} argument: ${arg.syntax}")
          }
        }
        //def checkIndexMin(n: BigInt): Unit = if (index < n) abort(tree.pos, s"Slang @range ${tname.value}'s index ($index) should not be less than its minimum ($n).")
        //def checkIndexMax(n: BigInt): Unit = if (index > n) abort(tree.pos, s"Slang @range ${tname.value}'s index ($index) should not be greater than its maximum ($n).")
        val index = (minOpt, maxOpt) match {
          case (Some(n), Some(m)) =>
            if (n > m) abort(tree.pos, s"Slang @range ${tname.value}'s min ($n) should not be greater than its max ($m).")
            //checkIndexMin(n)
            //checkIndexMax(m)
            if (indexB) n else BigInt(0)
          case (Some(n), _) =>
            //checkIndexMin(n)
            if (indexB) n else BigInt(0)
          case (_, Some(_)) if indexB =>
            //checkIndexMax(m)
            abort(tree.pos, s"Slang @range ${tname.value}'s min should specified when index is enabled.")
          case _ => abort(tree.pos, s"Slang @range ${tname.value} should have either a minimum, a maximum, or both.")
        }
        val result = range.q(index, minOpt, maxOpt, tname.value)
        //println(result)
        result
      case _ => abort(tree.pos, s"Invalid Slang @range on: ${tree.syntax}")
    }
  }
}

object range {
  def q(index: BigInt, minOpt: Option[BigInt], maxOpt: Option[BigInt], name: String): Term.Block = {
    def unsupported(op: Predef.String) = Lit.String(s"Unsupported $name operation '$op'")

    val typeName = Type.Name(name)
    val termName = Term.Name(name)
    val lowerTermName = Term.Name(name.toLowerCase)
    val ctorName = Ctor.Name(name)
    val nameStr = Lit.String(name)
    val signed = minOpt.forall(_ < 0)
    val scTypeName = Type.Name(name + "$Slang")

    def min = Lit.String(minOpt.map(_.toString).getOrElse("0"))

    def max = Lit.String(maxOpt.map(_.toString).getOrElse("0"))

    val isZeroIndex = Lit.Boolean(index == 0)

    val minUnsupported = unsupported("Min")
    val maxUnsupported = unsupported("Max")
    val minErrorMessage = Lit.String(s" is less than $name.Min (${min.value})")
    val maxErrorMessage = Lit.String(s" is greater than $name.Max (${max.value})")
    Term.Block(List(
      q"""final class $typeName(val value: Z.MP) extends AnyVal with Z.Range[$typeName] {
            @inline def Name: Predef.String = $termName.Name
            @inline def Min: $typeName = $termName.Min
            @inline def Max: $typeName = $termName.Max
            @inline def Index: $typeName = $termName.Index
            @inline def isZeroIndex: scala.Boolean = $termName.isZeroIndex
            @inline def isSigned: scala.Boolean = $termName.isSigned
            @inline def hasMin: scala.Boolean = $termName.hasMin
            @inline def hasMax: scala.Boolean = $termName.hasMax
            def make(v: Z.MP): $typeName = $termName(v)
          }""",
      q"""object $termName {
            val Name: Predef.String = $nameStr
            lazy val Min: $typeName = if (hasMin) new $ctorName(Z.MP($min)) else halt($minUnsupported)
            lazy val Max: $typeName = if (hasMax) new $ctorName(Z.MP($max)) else halt($maxUnsupported)
            val Index: $typeName = new $ctorName(Z.MP(${Lit.String(index.toString)}))
            def isZeroIndex: scala.Boolean = $isZeroIndex
            def isSigned: scala.Boolean = ${Lit.Boolean(signed)}
            def hasMin: scala.Boolean = ${Lit.Boolean(minOpt.nonEmpty)}
            def hasMax: scala.Boolean = ${Lit.Boolean(maxOpt.nonEmpty)}
            private def check(v: scala.BigInt): scala.BigInt = {
              if (hasMin) assert(Min.toBigInt <= v, v + $minErrorMessage)
              if (hasMax) assert(v <= Max.toBigInt, v + $maxErrorMessage)
              v
            }
            def apply(value: Z): $typeName = value match {
              case value: Z.MP => new $ctorName(value)
              case _ => halt(s"Unsupported $$Name creation from $${value.Name}.")
            }
            def unapply(n: $typeName): scala.Option[Z] = scala.Some(n.value)
            object Int {
              def apply(value: scala.Int): $typeName = {
                check(scala.BigInt(value))
                new $ctorName(Z.MP(value))
              }
              def unapply(n: $typeName): scala.Option[scala.Int] =
                if (scala.Int.MinValue <= n.value && n.value <= scala.Int.MaxValue) scala.Some(n.value.toBigInt.toInt)
                else scala.None
            }
            object Long {
              def apply(value: scala.Long): $typeName = {
                check(scala.BigInt(value))
                new $ctorName(Z.MP(value))
              }
              def unapply(n: $typeName): scala.Option[scala.Long] =
                if (scala.Long.MinValue <= n.value && n.value <= scala.Long.MaxValue) scala.Some(n.value.toBigInt.toLong)
                else scala.None
            }
            object String {
              def apply(value: Predef.String): $typeName = BigInt(scala.BigInt(value))
              def unapply(n: $typeName): scala.Option[Predef.String]= scala.Some(n.toBigInt.toString)
            }
            object BigInt {
              def apply(value: scala.BigInt): $typeName = new $ctorName(Z.MP(check(value)))
              def unapply(n: $typeName): scala.Option[scala.BigInt]= scala.Some(n.toBigInt)
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
            def apply(value: Z.MP): $typeName = BigInt(value.toBigInt)
          }"""
    ))
  }
}
