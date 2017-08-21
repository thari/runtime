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

import scala.meta._

object rich {
  def translateTrait(tree: Defn.Trait, o: Defn.Object): Term.Block = {
    val q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (mods.nonEmpty || tparams.isEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @rich traits have to be of the form '@rich trait <id>[...] { ... }'.")
    var tVars = Vector[Type.Name]()
    var tParams = Vector[Type.Param]()
    for (tp <- tparams) {
      val tparam"..$_ $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
      tVars :+= Type.Name(tparamname.value)
      tParams :+= tp
    }
    val tpe = t"$tname[..$tVars]"
    val traitDef = q"trait $tname[..$tparams] extends RichSig { ..$stats }"
    val objectDef = {
      val fParams = (if (tVars.size > 1) Vector(t"(..$tVars)") else tVars) :+ Type.Name("R_")
      val richF = q"trait Rich$$F[T, U] { def result: U }"
      val apply = q"def apply[..${tParams :+ tparam"R_ <: $tpe"}](f: Rich$$F[..$fParams]): R_ = f.result"
      o.copy(templ = o.templ.copy(stats = Some(o.templ.stats.getOrElse(List()) ++ List(richF, apply))))
    }
    Term.Block(Vector(traitDef, objectDef))
  }

  def translateClass(tree: Defn.Class, o: Defn.Object): Term.Block = {
    val q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (mods.nonEmpty || ctorMods.nonEmpty || paramss.isEmpty || paramss.size > 1 || paramss.head.isEmpty ||
      estats.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @rich classes have to be of the form '@rich class <id>(...) ... extends ... { ... }'.")
    val tVars = tparams.map { tp =>
      val tparam"..$mods $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
      Type.Name(tparamname.value)
    }
    val tpe = if (tVars.isEmpty) tname else t"$tname[..$tVars]"
    var varNames = Vector[Term.Name]()
    var varTypes = Vector[Type]()
    for (param <- paramss.head) param match {
      case param"..$mods $paramname: ${atpeopt: Option[Type.Arg]} = ${expropt: Option[Term]}" if mods.isEmpty &&
        expropt.isEmpty && (atpeopt match {
        case Some(targ"${_: Type}") => true
        case _ => false
      }) =>
        varNames :+= Term.Name(paramname.value)
        val Some(targ"${tpe: Type}") = atpeopt
        varTypes :+= tpe
      case _ => abort(param.pos, "Unsupported Slang @rich parameter form.")
    }
    val extName = Term.Name(tname.value + "_Ext")
    val dollar = Term.Name("$").structure
    val newStats = for (stat <- stats) yield stat match {
      case q"..$mods def $_[..$_](...$_): $_ = $_" if mods.exists { case mod"@spec" => true; case _ => false } => stat
      case q"..$_ def $name[..$tparams](...$paramss): ${tpeopt: Option[Type]} = $expr" =>
        val isExt = if (expr.structure == dollar) true else expr match {
          case Term.Apply(Term.Select(Term.Apply(Term.Name("StringContext"), _), Term.Name("lDef")), _) => true
          case expr: Term.Interpolate if expr.prefix.value == "lDef" => true
          case _ => false
        }
        if (isExt) {
          if (paramss.size > 1)
            abort(stat.pos, s"Slang @rich class extension methods should only have a list of parameters (instead of several lists of parameters).")
          if (tpeopt.isEmpty)
            abort(stat.pos, s"Slang @rich class extension methods should be explicitly typed.")
          val tVars = tparams.map { tp =>
            val tparam"..$mods $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
            Type.Name(tparamname.value)
          }
          val params = if (paramss.isEmpty) List() else paramss.head.map {
            case param"..$_ $paramname: ${atpeopt: Option[Type.Arg]} = $_" => atpeopt match {
              case Some(targ"${_: Type}") => arg"${Term.Name(paramname.value)}"
              case Some(_: Type.Arg.ByName) => arg"${Term.Name(paramname.value)}"
              case _ => abort(paramname.pos, "Unsupported Slang @rich class extension method parameter form.")
            }
          }
          if (tVars.isEmpty) q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name(..${varNames ++ params})"
          else q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name[..$tVars](..${varNames ++ params})"
        } else stat
    }
    val stats2 = List[Stat](
      q"""final override def string: String = halt("Slang @rich does not support operation 'string'.")""",
      q"""final override def hashCode: scala.Int = halt("Slang @rich does not support operation 'hashCode'.")""",
      q"""final override def equals(other: scala.Any): scala.Boolean = halt("Slang @rich does not support operation '=='.")"""
    )
    val classDef =
      if (tparams.isEmpty) q"final class $tname(...$paramss) extends RichSig with ..$ctorcalls { ..${newStats ++ stats2} }"
      else q"final class $tname[..$tparams](...$paramss) extends RichSig with ..$ctorcalls { ..${newStats ++ stats2} }"
    val objectDef = {
      val ctorName = Ctor.Name(tname.value)
      val apply =
        if (tparams.isEmpty) q"def apply(...$paramss): $tpe = new $ctorName(..$varNames)"
        else q"def apply[..$tparams](...$paramss): $tpe = new $ctorName(..$varNames)"
      if (ctorcalls.isEmpty) o.copy(templ = o.templ.copy(stats = Some(o.templ.stats.getOrElse(List()) :+ apply)))
      else {
        val impClasses: Vector[Option[Stat]] = for (ctorcall <- ctorcalls.toVector) yield ctorcall match {
          case ctor"$ref[..$atpesnel]()" if paramss.nonEmpty && paramss.head.nonEmpty =>
            val argType = if (atpesnel.size > 1) t"(..$atpesnel)" else atpesnel.head
            val impClass = {
              val (result, impParam) =
                if (varTypes.size > 1)
                  (q"lazy val result: $tpe = apply(..${varTypes.indices.map(i => q"arg.${Term.Name(s"_${i + 1}")}")})",
                    param"arg: (..$varTypes)")
                else (q"lazy val result: $tpe = apply(arg)", param"arg: ${varTypes.head}")
              val richF = ref match {
                case ctor"$ref.$ctorname" =>
                  val o = q"$ref.${Term.Name(ctorname.value)}"
                  ctor"$o.${Ctor.Name("Rich$F")}[$argType, $tpe]"
                case ref: Ctor.Name => ctor"${Term.Name(ref.value)}.${Ctor.Name("Rich$F")}[$argType, $tpe]"
              }
              val ctorname = ref.syntax.replaceAllLiterally(".", "_")
              if (tparams.nonEmpty)
                q"""implicit final class ${Type.Name(s"${tname.value}_${ctorname}_F")}[..$tparams]($impParam) extends $richF { $result }"""
              else
                q"""implicit final class ${Type.Name(s"${tname.value}_${ctorname}_F")}($impParam) extends $richF { $result }"""
            }
            Some(impClass)
          case _ => None
        }
        o.copy(templ = o.templ.copy(stats = Some(o.templ.stats.getOrElse(List()) ++ (impClasses.flatten :+ apply))))
      }
    }
    Term.Block(Vector(classDef, objectDef))
  }
}

final class rich extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case tree: Defn.Trait => rich.translateTrait(tree,  q"object ${Term.Name(tree.name.value)} {}")
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => rich.translateTrait(t, o)
      case tree: Defn.Class => rich.translateClass(tree,  q"object ${Term.Name(tree.name.value)} {}")
      case Term.Block(Seq(t: Defn.Class, o: Defn.Object)) => rich.translateClass(t, o)
      case _ => abort(tree.pos, s"Invalid Slang @rich on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}

