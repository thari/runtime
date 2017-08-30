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

package org.sireum

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("Enable scala.meta paradise to expand Slang macros")
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
            case arg"min = ${exp: Term}" =>
              helper.extractInt(exp) match {
                case Some(n) => minOpt = scala.Some(n)
                case _ => abort(arg.pos, s"Invalid Slang @range ${tname.value} min argument: ${arg.syntax}")
              }
            case arg"max = ${exp: Term}" =>
              helper.extractInt(exp) match {
                case Some(n) => maxOpt = scala.Some(n)
                case _ => abort(arg.pos, s"Invalid Slang @range ${tname.value} max argument: ${arg.syntax}")
              }
            case arg"index = ${exp: Term}" =>
              helper.extractBoolean(exp) match {
                case Some(b) => indexB = b
                case _ => abort(arg.pos, s"Invalid Slang @range ${tname.value} index argument: ${arg.syntax}")
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
          case (_, Some(_)) =>
            //checkIndexMax(m)
            if (indexB) abort(tree.pos, s"Slang @range ${tname.value}'s min should specified when index is enabled.") else BigInt(0)
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
    val iTermName = helper.zCompanionName(name)
    val (isTermName, isTypeName) = helper.iSName(name)
    val (msTermName, msTypeName) = helper.mSName(name)
    val lowerTermName = helper.scPrefix(name)
    val ctorName = Ctor.Name(name)
    val nameStr = Lit.String(name)
    val signed = minOpt.forall(_ < 0)
    val scTypeName = helper.scName(name)
    val (boxerTerm, boxerObject) = (minOpt, maxOpt) match {
      case (Some(min: BigInt), Some(max: BigInt)) =>
        if (scala.Byte.MinValue.toInt <= min && max.toInt <= scala.Byte.MaxValue)
          (q"$termName.Boxer", List[Stat](
            q"""object Boxer extends org.sireum.Z.Boxer.Byte {
                  def make(o: scala.Byte): $typeName = new $ctorName(org.sireum.Z.MP(o))
                }"""))
        else if (scala.Short.MinValue.toInt <= min && max.toInt <= scala.Short.MaxValue)
          (q"$termName.Boxer", List[Stat](
            q"""object Boxer extends org.sireum.Z.Boxer.Short {
                  def make(o: scala.Short): $typeName = new $ctorName(org.sireum.Z.MP(o))
                }"""))
        else if (scala.Int.MinValue <= min && max <= scala.Int.MaxValue)
          (q"$termName.Boxer", List[Stat](
            q"""object Boxer extends org.sireum.Z.Boxer.Int {
                  def make(o: scala.Int): $typeName = new $ctorName(org.sireum.Z.MP(o))
                }"""))
        else if (scala.Long.MinValue <= min && max <= scala.Long.MaxValue)
          (q"$termName.Boxer", List[Stat](
            q"""object Boxer extends org.sireum.Z.Boxer.Long {
                  def make(o: scala.Long): $typeName = new $ctorName(org.sireum.Z.MP(o))
                }"""))
        else (q"org.sireum.Z.Boxer.Z", List[Stat]())
      case _ => (q"org.sireum.Z.Boxer.Z", List[Stat]())
    }

    def min = Lit.String(minOpt.map(_.toString).getOrElse("0"))

    def max = Lit.String(maxOpt.map(_.toString).getOrElse("0"))

    val isZeroIndex = Lit.Boolean(index == 0)

    val minUnsupported = unsupported("Min")
    val maxUnsupported = unsupported("Max")
    val minErrorMessage = Lit.String(s" is less than $name.Min (${min.value})")
    val maxErrorMessage = Lit.String(s" is greater than $name.Max (${max.value})")
    Term.Block(List(
      q"""final class $typeName(val value: org.sireum.Z.MP) extends AnyVal with org.sireum.Z.Range[$typeName] {
            @inline def Name: Predef.String = $termName.Name
            @inline def Min: $typeName = $termName.Min
            @inline def Max: $typeName = $termName.Max
            @inline def Index: $typeName = $termName.Index
            @inline def isZeroIndex: scala.Boolean = $termName.isZeroIndex
            @inline def isSigned: scala.Boolean = $termName.isSigned
            @inline def hasMin: scala.Boolean = $termName.hasMin
            @inline def hasMax: scala.Boolean = $termName.hasMax
            def make(v: org.sireum.Z.MP): $typeName = $termName(v)
            def boxer = $boxerTerm
          }""",
      q"""object $termName extends org.sireum.$$ZCompanion[$typeName] {
            type $isTypeName[T <: org.sireum.Immutable] = org.sireum.IS[$typeName, T]
            type $msTypeName[T] = org.sireum.MS[$typeName, T];
            ..$boxerObject;
            val Name: Predef.String = $nameStr
            lazy val Min: $typeName = if (hasMin) new $ctorName(org.sireum.Z.MP($min)) else halt($minUnsupported)
            lazy val Max: $typeName = if (hasMax) new $ctorName(org.sireum.Z.MP($max)) else halt($maxUnsupported)
            val Index: $typeName = new $ctorName(org.sireum.Z.MP(${Lit.String(index.toString)}))
            val isZeroIndex: scala.Boolean = $isZeroIndex
            val isSigned: scala.Boolean = ${Lit.Boolean(signed)}
            val isBitVector: scala.Boolean = false
            val hasMin: scala.Boolean = ${Lit.Boolean(minOpt.nonEmpty)}
            val hasMax: scala.Boolean = ${Lit.Boolean(maxOpt.nonEmpty)}
            def BitWidth: scala.Int = halt(s"Unsupported $$Name operation 'BitWidth'")
            def random: $typeName = if (hasMax && hasMin) {
              val d = Max.value - Min.value + org.sireum.Z.MP.one
              val n = org.sireum.Z.random % d
              new $ctorName(n + Min.value)
            } else if (hasMax) {
              val n = org.sireum.Z.random
              new $ctorName(if (n > Max.value) Max.value - n else n)
            } else {
              val n = org.sireum.Z.random
              new $ctorName(if (n < Min.value) Min.value + n else n)
            }
            def randomSeed(seed: org.sireum.Z): $typeName = if (hasMax && hasMin) {
              val d = Max.value - Min.value + org.sireum.Z.MP.one
              val n = org.sireum.Z.randomSeed(seed) % d
              new $ctorName(n + Min.value)
            } else if (hasMax) {
              val n = org.sireum.Z.randomSeed(seed)
              new $ctorName(if (n > Max.value) Max.value - n else n)
            } else {
              val n = org.sireum.Z.randomSeed(seed)
              new $ctorName(if (n < Min.value) Min.value + n else n)
            }
            private def check(v: org.sireum.Z.MP): org.sireum.Z.MP = {
              if (hasMin) assert(Min.value <= v, v + $minErrorMessage)
              if (hasMax) assert(v <= Max.value, v + $maxErrorMessage)
              v
            }
            def apply(n: org.sireum.Z): $typeName = n match {
              case n: org.sireum.Z.MP => new $ctorName(n)
              case _ => halt(s"Unsupported $$Name creation from $${n.Name}.")
            }
            def apply(s: String): Option[$typeName] = try Some($termName.String(s.value)) catch { case _: Throwable => None[$typeName]() }
            def unapply(n: $typeName): scala.Option[org.sireum.Z] = scala.Some(n.value)
            object Int extends org.sireum.$$ZCompanionInt[$typeName] {
              def apply(n: scala.Int): $typeName = new $ctorName(check(org.sireum.Z.MP(n)))
              def unapply(n: $typeName): scala.Option[scala.Int] =
                if (scala.Int.MinValue <= n.value && n.value <= scala.Int.MaxValue) scala.Some(n.value.toBigInt.toInt)
                else scala.None
            }
            object Long extends org.sireum.$$ZCompanionLong[$typeName] {
              def apply(n: scala.Long): $typeName = new $ctorName(check(org.sireum.Z.MP(n)))
              def unapply(n: $typeName): scala.Option[scala.Long] =
                if (scala.Long.MinValue <= n.value && n.value <= scala.Long.MaxValue) scala.Some(n.value.toBigInt.toLong)
                else scala.None
            }
            object String extends org.sireum.$$ZCompanionString[$typeName] {
              def apply(s: Predef.String): $typeName = BigInt(org.sireum.Z.String(s).toBigInt)
              def unapply(n: $typeName): scala.Option[Predef.String] = scala.Some(n.toBigInt.toString)
            }
            object BigInt extends org.sireum.$$ZCompanionBigInt[$typeName] {
              def apply(n: scala.BigInt): $typeName = new $ctorName(check(org.sireum.Z.MP(n)))
              def unapply(n: $typeName): scala.Option[scala.BigInt] = scala.Some(n.toBigInt)
            }
            object $isTermName {
              def apply[V <: org.sireum.Immutable](args: V*): $isTypeName[V] = org.sireum.IS[$typeName, V](args: _*)
              def create[V <: org.sireum.Immutable](size: org.sireum.Z, default: V): $isTypeName[V] = org.sireum.IS.create[$typeName, V](size, default)
            }
            object $msTermName {
              def apply[V](args: V*): $msTypeName[V] = org.sireum.MS[$typeName, V](args: _*)
              def create[V](size: org.sireum.Z, default: V): $msTypeName[V] = org.sireum.MS.create[$typeName, V](size, default)
            }
            implicit class $scTypeName(val sc: scala.StringContext) {
              object $lowerTermName {
                def apply(args: scala.Any*): $typeName = {
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
            implicit val $iTermName: org.sireum.$$ZCompanion[$typeName] = this
          }"""
    ))
  }
}
