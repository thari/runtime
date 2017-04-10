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

package org.sireum.logika

import scala.meta._
import scala.meta.dialects.Scala212

// TODO: clean up quasiquotes due to IntelliJ's macro annotation inference workaround
class datatype extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
        if (mods.nonEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
          abort("Logika @datatype traits have to be of the form '@datatype trait <id> { ... }'.")
        q"sealed trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }"
      case Term.Block(Seq(q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }", o: Defn.Object)) =>
        if (mods.nonEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
          abort("Logika @datatype traits have to be of the form '@datatype trait <id> { ... }'.")
        Term.Block(Vector(q"sealed trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }", o))
      case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
        if (mods.nonEmpty || ctorMods.nonEmpty || paramss.size > 1 ||
          estats.nonEmpty || ctorcalls.size > 1 || !param.name.isInstanceOf[Name.Anonymous])
          abort("Logika @datatype classes have to be of the form '@datatype class <id>(...) { ... }'.")
        val tVars = tparams.map { tp =>
          val tparam"..$mods $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
          Type.Name(tparamname.value)
        }
        val tpe = {
          if (tVars.isEmpty) tname else t"$tname[..$tVars]"
        }
        val clone = q"override def clone: $tpe = this"
        val ctorName = Ctor.Name(tname.value)
        if (paramss.nonEmpty && paramss.head.nonEmpty) {
          var cparams: Vector[Term.Param] = Vector()
          var applyParams: Vector[Term.Param] = Vector()
          var oApplyParams: Vector[Term.Param] = Vector()
          var applyArgs: Vector[Term.Name] = Vector()
          var unapplyTypes: Vector[Type] = Vector()
          var unapplyArgs: Vector[Term.Name] = Vector()
          for (param <- paramss.head) param match {
            case param"..$mods $paramname: ${atpeopt: Option[Type.Arg]} = $expropt" if (atpeopt match {
              case Some(targ"${tpe: Type}") => true
              case _ => false
            }) =>
              val varName = Term.Name(paramname.value)
              val hidden = mods.exists({
                case mod"@hidden" => true
                case _ => false
              })
              cparams :+= param"val $paramname: $atpeopt"
              applyParams :+= param"$paramname: $atpeopt = this.$varName"
              oApplyParams :+= param"$paramname: $atpeopt"
              applyArgs :+= varName
              if (!hidden) {
                val Some(targ"${tpe: Type}") = atpeopt
                unapplyTypes :+= tpe
                unapplyArgs :+= varName
              }
            case _ => abort(param.pos, "Unsupported Logika @datatype parameter form.")
          }
          val cls = {
            val hashCode = q"override lazy val hashCode: Int = { (this.getClass, ..$unapplyArgs).hashCode }"
            val equals = {
              val eCaseEqs = unapplyArgs.map(arg => q"$arg == o.$arg")
              val eCaseExp = eCaseEqs.tail.foldLeft(eCaseEqs.head)((t1, t2) => q"$t1 && $t2")
              val eCases =
                Vector(if (tparams.isEmpty) p"case o: $tname => if (this.hashCode != o.hashCode) false else $eCaseExp"
                else p"case (o: $tname[..$tVars] @unchecked) => $eCaseExp",
                  p"case _ => false")
              q"override def equals(o: Any): Boolean = if (this eq o.asInstanceOf[AnyRef]) true else o match { ..case $eCases }"
            }
            val apply = q"def apply(..$applyParams): $tpe = new $ctorName(..$applyArgs)"
            val toString = {
              var appends = applyArgs.map(arg => q"org.sireum.logika._append(sb, $arg)")
              appends =
                if (appends.isEmpty) appends
                else appends.head +: appends.tail.flatMap(a => Vector(q"""sb.append(", ")""", a))
              q"""override def toString(): java.lang.String = {
                    val sb = new StringBuilder
                    sb.append(${Lit(tname.value)})
                    sb.append('(')
                    ..$appends
                    sb.append(')')
                    sb.toString
                  }"""
            }
            q"class $tname[..$tparams](...${Vector(cparams)}) extends {} with org.sireum.logika._Immutable with org.sireum.logika._Clonable with ..$ctorcalls { ..${Vector(hashCode, equals, clone, apply, toString) ++ stats} }"
          }
          val companion = {
            val (apply, unapply) =
              if (tparams.isEmpty)
                (q"def apply(..$oApplyParams): $tpe = new $ctorName(..$applyArgs)",
                  unapplyTypes.size match {
                    case 0 => q"def unapply(o: $tpe): Option[Unit] = scala.Some(())"
                    case 1 => q"def unapply(o: $tpe): Option[${unapplyTypes.head}] = scala.Some(o.${unapplyArgs.head})"
                    case _ => q"def unapply(o: $tpe): Option[(..$unapplyTypes)] = scala.Some((..${unapplyArgs.map(arg => q"o.$arg")}))"
                  })
              else
                (q"def apply[..$tparams](..$oApplyParams): $tpe = new $ctorName(..$applyArgs)",
                  unapplyTypes.size match {
                    case 0 => q"def unapply[..$tparams](o: $tpe): Option[Unit] = scala.Some(())"
                    case 1 => q"def unapply[..$tparams](o: $tpe): Option[${unapplyTypes.head}] = scala.Some(o.${unapplyArgs.head})"
                    case _ => q"def unapply[..$tparams](o: $tpe): Option[(..$unapplyTypes)] = scala.Some((..${unapplyArgs.map(arg => q"o.$arg")}))"
                  })
            q"object ${Term.Name(tname.value)} { ..${Vector(apply, unapply)} }"
          }
          Term.Block(Vector(cls, companion))
        } else {
          val cls = {
            val hashCode = q"override val hashCode: Int = this.getClass.hashCode"
            val equals = {
              val eCases =
                Vector(if (tparams.isEmpty) p"case o: $tname => true"
                else p"case o: $tname[..$tVars] => true",
                  p"case _ => false")
              q"override def equals(o: Any): Boolean = if (this eq o.asInstanceOf[AnyRef]) true else o match { ..case $eCases }"
            }
            val toString = {
              val r = tname.value + "()"
              q"""override def toString(): java.lang.String = ${Lit(r)}"""
            }
            q"class $tname[..$tparams](...$paramss) extends {} with org.sireum.logika._Immutable with org.sireum.logika._Clonable with ..$ctorcalls { ..${Vector(hashCode, equals, clone, toString) ++ stats} }"
          }
          val companion = {
            val (v, apply, unapply) =
              if (tparams.isEmpty)
                (q"private[this] val v: AnyRef = new $ctorName()",
                  q"def apply(): $tpe = v.asInstanceOf[$tpe]",
                  q"def unapply(o: $tpe): Option[Unit] = unv")
              else
                (q"private[this] val v: AnyRef = new $ctorName[..${tparams.map(_ => t"Nothing")}]()",
                  q"def apply[..$tparams](): $tpe = v.asInstanceOf[$tpe]",
                  q"def unapply[..$tparams](o: $tpe): Option[Unit] = unv")
            val unv = q"private[this] lazy val unv: Option[Unit] = scala.Some(())"
            q"object ${Term.Name(tname.value)} { ..${Vector(v, unv, apply, unapply)} }"
          }
          Term.Block(Vector(cls, companion))
        }
      case Term.Block(Seq(_: Defn.Class, _: Defn.Object)) =>
        abort(s"Cannot use Logika @datatype on a class with an existing companion object.")
      case _ =>
        abort(s"Invalid Logika @datatype on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}
