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

import scala.meta._

object ext {

  val extSuffix = "_Ext"
  val typeExtSuffix = "$Ext"

  def transformObject(o: Defn.Object): Defn.Object = {
    val q"..$mods object $name extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = o
    val dollar = Term.Name("$").structure
    if (mods.nonEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort(s"Invalid @ext form on an object; it has to be of the form '@ext object ${name.value} { ... }'.")
    val extName = Term.Name(name.value + extSuffix)
    val newStats = for (stat <- stats) yield stat match {
      case q"..$mods val ..$patsnel: ${tpeopt: Option[Type]} = $$" =>
        if (mods.nonEmpty || patsnel.size != 1 || !patsnel.head.isInstanceOf[Pat.Var.Term] || tpeopt.isEmpty)
          abort(stat.pos, s"Invalid Slang @ext on a val; it has to be of the form: '@ext val <id>: <type> = $$'")
        val varName = Term.Name(patsnel.head.asInstanceOf[Pat.Var.Term].name.value)
        q"..$mods val ${Pat.Var.Term(varName)}: $tpeopt = $extName.$varName"
      case q"..$mods var ..$patsnel: ${tpeopt: Option[Type]} = $$" =>
        if (mods.nonEmpty || patsnel.size != 1 || !patsnel.head.isInstanceOf[Pat.Var.Term] || tpeopt.isEmpty)
          abort(stat.pos, s"Invalid Slang @ext on a var; it has to be of the form: '@ext var <id>: <type> = $$'")
        val varName = Term.Name(patsnel.head.asInstanceOf[Pat.Var.Term].name.value)
        q"..$mods var ${Pat.Var.Term(varName)}: $tpeopt = $extName.$varName"
      case q"..$mods def $_[..$_](...$_): $_ = $_" if mods.exists { case mod"@spec" => true; case _ => false } => stat
      case q"..$mods def $name[..$tparams](...$paramss): ${tpeopt: Option[Type]} = $expr" =>

        if (paramss.size > 1)
          abort(stat.pos, s"Slang @ext object methods should only have a list of parameters (instead of several lists of parameters).")
        if (tpeopt.isEmpty)
          abort(stat.pos, s"Slang @ext object methods should be explicitly typed.")
        val tVars = tparams.map { tp =>
          val tparam"..$mods $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
          Type.Name(tparamname.value)
        }
        val params = if (paramss.isEmpty) List() else paramss.head.map {
          case param"..$_ $paramname: ${atpeopt: Option[Type.Arg]} = $_" => atpeopt match {
            case Some(targ"${_: Type}") => arg"${Term.Name(paramname.value)}"
            case Some(_: Type.Arg.ByName) => arg"${Term.Name(paramname.value)}"
            case _ => abort(paramname.pos, "Unsupported Slang @ext object method parameter form.")
          }
        }
        if (expr.structure == dollar) {
        } else expr match {
          case Term.Apply(Term.Select(Term.Apply(Term.Name("StringContext"), _), Term.Name("lDef")), _) =>
          case expr: Term.Interpolate if expr.prefix.value == "lDef" =>
          case _ => abort(stat.pos, "Invalid expression for Slang @ext object method; it should be either $ or l\"\"\"{ ... }\"\"\".")
        }
        if (tVars.isEmpty)
          if (paramss.isEmpty)
            q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name"
          else
            q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name(..$params)"
        else if (paramss.isEmpty)
          q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name[..$tVars]"
        else
          q"def $name[..$tparams](...$paramss): $tpeopt = $extName.$name[..$tVars](..$params)"
      case q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
        if (tparams.nonEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
          abort(stat.pos, s"Invalid Slang @ext on a trait; it has to be of the form: '@ext trait ${tname.value}'")
        q"type $tname = $extName.$tname"
      case Term.Apply(Term.Select(Term.Apply(Term.Name("StringContext"), _), Term.Name("lUnit")), _) => stat
      case expr: Term.Interpolate if expr.prefix.value == "lUnit" => stat
      case q"..$_ val ..$_: $_ = $_" => stat
      case q"..$_ var ..$_: $_ = $_" => stat
      case _ => abort(stat.pos, s"Invalid Slang @ext object member: ${stat.syntax}.")
    }
    q"object $name extends { } with ..$ctorcalls { ..$newStats }"
  }

  def transformTrait(tree: Defn.Trait): Term.Block = {
    val q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (mods.nonEmpty || estats.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @ext traits have to be of the form '@ext trait <id> ... { ... }'.")

    if (!tree.name.value.endsWith(typeExtSuffix))
      abort(s"Slang @ext traits have to be named ending with $typeExtSuffix.")

    val tVars = tparams.map { tp =>
      val tparam"..$_ $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
      Type.Name(tparamname.value)
    }

    val ev = {
      import scala.collection.immutable.Seq
      val tName = Type.Name(tname.value.substring(0, tname.value.length - typeExtSuffix.length))
      val traitType = if (tVars.isEmpty) tName else t"$tName[..$tVars]"
      val evStats: List[Stat] = for (stat <- stats.toList) yield {
        val (mods: Seq[Mod], name: Term.Name, tparams: Seq[Type.Param], paramss: Seq[Seq[Term.Param]], tpe: Type) = stat match {
          case q"..$mods def $name[..$tparams](...$paramss): ${Some(t)} = $$" => (mods, name, tparams, paramss, t)
          case q"..$mods def $name[..$tparams](...$paramss): $tpe" => (mods, name, tparams, paramss, tpe)
          case _ => abort(stat.pos, "Slang @ext traits can only have method declarations or method definitions with $ expression.")
        }
        paramss.length match {
          case 0 =>
            if (tparams.isEmpty) q"..$mods def $name: $tpe = (ev: $traitType).$name"
            else {
              val tParamVars = tparams.map { tp =>
                val tparam"..$_ $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
                Type.Name(tparamname.value)
              }
              q"..$mods def $name[..$tparams](...$paramss): $tpe = (ev: $traitType).$name[..$tParamVars]"
            }
          case 1 =>
            val params: List[Term.Name] = for (param <- paramss.head.toList) yield param match {
              case param"..$_ $paramname: $_ = $_" => Term.Name(paramname.value)
              case _ => abort(param.pos, "Unsupported parameter form for a Slang @ext method.")
            }
            if (tparams.isEmpty) q"..$mods def $name(...$paramss): $tpe = (ev: $traitType).$name(..$params)"
            else {
              val tParamVars = tparams.map { tp =>
                val tparam"..$_ $tparamname[..$_] >: $_ <: $_ <% ..$_ : ..$_" = tp
                Type.Name(tparamname.value)
              }
              q"..$mods def $name[..$tparams](...$paramss): $tpe = (ev: $traitType).$name[..$tParamVars](..$params)"
            }
          case _ => abort(stat.pos, "Slang @ext trait methods cannot have multiple parameter tuples.")
        }
      }
      if (tVars.isEmpty) q"def __ev(ev: $tName): Unit = { ..$evStats }"
      else q"def __ev[..$tparams](ev: $tName[..$tVars]): Unit = { ..$evStats }"
    }

    Term.Block(List(q"trait $tname", q"object ${Term.Name(tree.name.value)} { $ev }"))
  }
}

class ext extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case tree: Defn.Trait => ext.transformTrait(tree)
      case tree: Defn.Object => ext.transformObject(tree)
      case Term.Block(Seq(_: Defn.Trait, _: Defn.Object)) => abort(s"Slang @ext cannot used on a trait with a companion object.")
      case _ => abort(tree.pos, s"Slang @ext can only be used on an object or a trait.")
    }
    //println(result.syntax)
    result
  }
}

class mext extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case tree: Defn.Trait => ext.transformTrait(tree)
      case Term.Block(Seq(_: Defn.Trait, _: Defn.Object)) => abort(s"Slang @mext cannot used on a trait with a companion object.")
      case _ => abort(tree.pos, s"Slang @mext can only be used on a trait.")
    }
    //println(result.syntax)
    result
  }
}
