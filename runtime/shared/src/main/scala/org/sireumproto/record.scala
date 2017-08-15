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

// TODO: clean up quasiquotes due to IntelliJ's macro annotation inference workaround
object record {

  def transformTrait(tree: Defn.Trait): Defn.Trait = {
    val q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (mods.nonEmpty || estats.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @record traits have to be of the form '@record trait <id> ... { ... }'.")
    val tVars = tparams.map { tp =>
      val tparam"..$_ $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
      Type.Name(tparamname.value)
    }
    val tpe = if (tVars.isEmpty) tname else t"$tname[..$tVars]"
    val (hasHash, hasEqual) = helper.hasHashEquals(tpe, stats)
    val equals =
      if (hasEqual) {
        val eCases =
          Vector(if (tparams.isEmpty) p"case o: $tname => isEqual(o)"
          else p"case (o: $tname[..$tVars] @unchecked) => isEqual(o)",
            p"case _ => false")
        List(q"override def equals(o: scala.Any): scala.Boolean = if (this eq o.asInstanceOf[scala.AnyRef]) true else o match { ..case $eCases }")
      } else List()
    val hash = if (hasHash) List() else List(q"override def hash: Z = hashCode")
    q"sealed trait $tname[..$tparams] extends Record with ..$ctorcalls { $param => ..${hash ++ equals ++ stats} }"
  }

  def transformClass(tree: Defn.Class, o: Defn.Object): Term.Block = {
    val q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (mods.nonEmpty || ctorMods.nonEmpty || paramss.size > 1 ||
      estats.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @record classes have to be of the form '@record class <id> ... (...) ... { ... }'.")
    val tVars = tparams.map { tp =>
      val tparam"..$mods $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
      Type.Name(tparamname.value)
    }
    val tpe = if (tVars.isEmpty) tname else t"$tname[..$tVars]"
    val (hasHash, hasEqual) = helper.hasHashEquals(tpe, stats)
    val ctorName = Ctor.Name(tname.value)
    var inVars = Vector[Term.Assign]()
    for (stat <- stats) stat match {
      case stat: Defn.Var =>
        for (pat <- stat.pats) pat match {
          case q"${name: Pat.Var.Term}" =>
            val varName = name.name
            inVars :+= q"r.$varName = $varName"
          case _ => abort(stat.pos, s"Unsupported var definition form in Slang @record: ${pat.syntax}")
        }
      case _ =>
    }
    if (paramss.nonEmpty && paramss.head.nonEmpty) {
      var varNames: Vector[Term.Name] = Vector()
      var cparams: Vector[Term.Param] = Vector()
      var applyParams: Vector[Term.Param] = Vector()
      var oApplyParams: Vector[Term.Param] = Vector()
      var applyArgs: Vector[Term.Name] = Vector()
      var unapplyTypes: Vector[Type] = Vector()
      var unapplyArgs: Vector[Term.Name] = Vector()
      var vars: Vector[Stat] = Vector()
      for (param <- paramss.head) param match {
        case param"..$mods $paramname: ${atpeopt: Option[Type.Arg]} = $expropt" if (atpeopt match {
          case Some(targ"${tpe: Type}") => true
          case _ => false
        }) =>
          val varName = Term.Name("_" + paramname.value)
          val paramName = Term.Name(paramname.value)
          var hidden = false
          var isVar = false
          mods.foreach {
            case mod"@hidden" => hidden = true
            case mod"varparam" => isVar = true
            case _ => false
          }
          varNames :+= varName
          cparams :+= param"private var $varName: $atpeopt"
          vars :+= q"def $paramName = $varName"
          vars :+= q"def ${Term.Name(paramname.value + "_=")}($paramname: $atpeopt): this.type = { dirty = true; $varName = $paramName; this }"
          applyParams :+= param"$paramname: $atpeopt = this.$varName"
          oApplyParams :+= param"$paramname: $atpeopt"
          applyArgs :+= paramName
          if (!hidden) {
            val Some(targ"${tpe: Type}") = atpeopt
            unapplyTypes :+= tpe
            unapplyArgs :+= varName
          }
        case _ => abort(param.pos, "Unsupported Slang @record parameter form.")
      }
      val cls = {
        val clone = {
          val cloneNew = q"val r: $tpe = new $ctorName(..${applyArgs.map(arg => q"$$clone($arg)")})"
          q"override def $$clone: $tpe = { ..${cloneNew +: inVars :+ q"r"} }"
        }
        val hashCode =
          if (hasHash) q"override val hashCode: scala.Int = hash.toInt"
          else if (hasEqual) q"override def hashCode: scala.Int = 0"
          else {
            val hashCodeDirty = q"private var dirty: scala.Boolean = true"
            val hashCodeVar = q"private var _hashCode: scala.Int = _"
            val hashCodeDef = q"private def computeHashCode: scala.Int = scala.Seq(this.getClass, ..$unapplyArgs).hashCode"
            vars = (hashCodeDirty +: vars) :+ hashCodeVar :+ hashCodeDef
            q"override def hashCode: scala.Int = { if (dirty) { dirty = false; _hashCode = computeHashCode}; _hashCode }"
          }
        val equals =
          if (hasEqual) {
            val eCases =
              Vector(if (tparams.isEmpty) p"case o: $tname => isEqual(o)"
              else p"case (o: $tname[..$tVars] @unchecked) => isEqual(o)",
                p"case _ => false")
            q"override def equals(o: scala.Any): scala.Boolean = if (this eq o.asInstanceOf[scala.AnyRef]) true else o match { ..case $eCases }"
          } else {
            val eCaseEqs = unapplyArgs.map(arg => q"$arg == o.$arg")
            val eCaseExp = if (eCaseEqs.isEmpty) q"true" else eCaseEqs.tail.foldLeft(eCaseEqs.head)((t1, t2) => q"$t1 && $t2")
            val eCases =
              Vector(if (tparams.isEmpty) p"case o: $tname => if (this.hashCode != o.hashCode) false else $eCaseExp"
              else p"case (o: $tname[..$tVars] @unchecked) => if (this.hashCode != o.hashCode) false else $eCaseExp",
                p"case _ => false")
            q"override def equals(o: scala.Any): scala.Boolean = if (this eq o.asInstanceOf[scala.AnyRef]) true else o match { ..case $eCases }"
          }
        val hash = if (hasHash) List() else List(q"override def hash: Z = hashCode")
        val isEqual = if (hasEqual) List() else List(q"def isEqual(other: Mutable): B = this == other")
        val apply = q"def apply(..$applyParams): $tpe = new $ctorName(..${applyArgs.map(arg => q"$$assign($arg)")})"
        val toString = {
          var appends = applyArgs.map(arg => q"sb.append($arg.toString)")
          appends =
            if (appends.isEmpty) appends
            else appends.head +: appends.tail.flatMap(a => Vector( q"""sb.append(", ")""", a))
          Vector(
            q"""override def toString: java.lang.String = {
                    val sb = new java.lang.StringBuilder
                    sb.append(${Lit.String(tname.value)})
                    sb.append('(')
                    ..$appends
                    sb.append(')')
                    sb.toString
                  }""",
            q"override def string: String = toString"
          )
        }
//        val content = {
//          var fields = List[Term.Arg](q"(${Lit.String("type")}, ${Lit.String(tname.value)})")
//          for (x <- applyArgs) {
//            fields ::= q"(${Lit.String(x.value)}, $x)"
//          }
//          q"override def content: scala.Seq[(Predef.String, scala.Any)] = scala.Seq(..${fields.reverse})"
//        }
        q"final class $tname[..$tparams](...${Vector(cparams)}) extends Record with ..$ctorcalls { ..${vars ++ hash ++ isEqual ++ toString ++ Vector(hashCode, equals, clone, apply) ++ stats} }"
      }
      val companion = {
        val (apply, unapply) =
          if (tparams.isEmpty)
            (q"def apply(..$oApplyParams): $tpe = new $ctorName(..$applyArgs)",
              unapplyTypes.size match {
                case 0 => q"def unapply(o: $tpe): scala.Boolean = true"
                case 1 => q"def unapply(o: $tpe): scala.Option[${unapplyTypes.head}] = scala.Some($$clone(o.${unapplyArgs.head}))"
                case n if n <= 22 => q"def unapply(o: $tpe): scala.Option[(..$unapplyTypes)] = scala.Some((..${unapplyArgs.map(arg => q"$$clone(o.$arg)")}))"
                case _ =>
                  val unapplyTypess = unapplyTypes.grouped(22).map(types => t"(..$types)").toVector
                  val unapplyArgss = unapplyArgs.grouped(22).map(args => q"(..${args.map(a => q"$$clone(o.$a.clone)")})").toVector
                  q"def unapply(o: $tpe): scala.Option[(..$unapplyTypess)] = scala.Some((..$unapplyArgss))"
              })
          else
            (q"def apply[..$tparams](..$oApplyParams): $tpe = new $ctorName(..$applyArgs)",
              unapplyTypes.size match {
                case 0 => q"def unapply[..$tparams](o: $tpe): scala.Boolean = true"
                case 1 => q"def unapply[..$tparams](o: $tpe): scala.Option[${unapplyTypes.head}] = scala.Some(o.${unapplyArgs.head})"
                case n if n <= 22 => q"def unapply[..$tparams](o: $tpe): scala.Option[(..$unapplyTypes)] = scala.Some((..${unapplyArgs.map(arg => q"$$clone(o.$arg)")}))"
                case _ =>
                  val unapplyTypess = unapplyTypes.grouped(22).map(types => t"(..$types)").toVector
                  val unapplyArgss = unapplyArgs.grouped(22).map(args => q"(..${args.map(a => q"$$clone(o.$a)")})").toVector
                  q"def unapply[..$tparams](o: $tpe): scala.Option[(..$unapplyTypess)] = scala.Some((..$unapplyArgss))"
              })
        o.copy(templ = o.templ.copy(stats = Some(o.templ.stats.getOrElse(List()) ++ List(apply, unapply))))
      }
      Term.Block(Vector(cls, companion))
    } else {
      val cls = {
        val clone = if (inVars.nonEmpty) {
          val cloneNew = q"val r: $tpe = new $ctorName()"
          q"override def $$clone: $tpe = { ..${cloneNew +: inVars :+ q"r"} }"
        } else q"override def $$clone: $tpe = this"
        val hashCode =
          if (hasHash) q"override val hashCode: scala.Int = hash.toInt"
          else if (hasEqual) q"override val hashCode: scala.Int = 0"
          else q"override val hashCode: scala.Int = this.getClass.hashCode"
        val equals =
          if (hasEqual) {
            val eCases =
              Vector(if (tparams.isEmpty) p"case o: $tname => isEqual(o)"
              else p"case o: $tname[..$tVars] => isEqual(o)",
                p"case _ => false")
            q"override def equals(o: scala.Any): scala.Boolean = if (this eq o.asInstanceOf[scala.AnyRef]) true else o match { ..case $eCases }"
          } else {
            val eCases =
              Vector(if (tparams.isEmpty) p"case o: $tname => true"
              else p"case o: $tname[..$tVars] => true",
                p"case _ => false")
            q"override def equals(o: scala.Any): Boolean = if (this eq o.asInstanceOf[scala.AnyRef]) true else o match { ..case $eCases }"
          }
        val hash = if (hasHash) List() else List(q"override def hash: Z = hashCode")
        val isEqual = if (hasEqual) List() else List(q"def isEqual(other: Mutable): B = this == other")
        val toString = {
          val r = tname.value + "()"
          Vector(q"""override def toString: java.lang.String = ${Lit.String(r)}""", q"override def string: String = toString")
        }
//        val content = q"override def content: scala.Seq[(Predef.String, scala.Any)] = scala.Seq((${Lit.String("type")}, ${Lit.String(tname.value)}))"
        q"final class $tname[..$tparams](...$paramss) extends Record with ..$ctorcalls { ..${hash ++ isEqual ++ toString ++ Vector(hashCode, equals, clone) ++ stats} }"
      }
      val companion = {
        val (v, apply, unapply) =
          if (tparams.isEmpty)
            (q"private[this] val v: scala.AnyRef = new $ctorName()",
              q"def apply(): $tpe = v.asInstanceOf[$tpe]",
              q"def unapply(o: $tpe): scala.Boolean = true")
          else
            (q"private[this] val v: scala.AnyRef = new $ctorName[..${tparams.map(_ => t"Nothing")}]()",
              q"def apply[..$tparams](): $tpe = v.asInstanceOf[$tpe]",
              q"def unapply[..$tparams](o: $tpe): Boolean = true")
        o.copy(templ = o.templ.copy(stats = Some(o.templ.stats.getOrElse(List()) ++ List(v, apply, unapply))))
      }
      Term.Block(Vector(cls, companion))
    }
  }
}

class record extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case tree: Defn.Trait => record.transformTrait(tree)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(List[Stat](record.transformTrait(t), o))
      case tree: Defn.Class => record.transformClass(tree, q"object ${Term.Name(tree.name.value)} {}")
      case Term.Block(Seq(t: Defn.Class, o: Defn.Object)) => record.transformClass(t, o)
      case _ => abort(tree.pos, s"Invalid Slang @record on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}
