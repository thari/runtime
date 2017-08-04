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

final class rich extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
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
        val traitDef = q"trait $tname[..$tparams] extends org.sireum._Rich { ..$stats }"
        val objectDef = {
          val fParams = (if (tVars.size > 1) Vector(t"(..$tVars)") else tVars) :+ Type.Name("R_")
          val apply = q"def apply[..${tParams :+ tparam"R_ <: $tpe"}](f: org.sireum._RichF[..$fParams]): R_ = f.result"
          q"object ${Term.Name(tname.value)} { $apply }"
        }
        Term.Block(Vector(traitDef, objectDef))
      case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
        if (mods.nonEmpty || ctorMods.nonEmpty || paramss.isEmpty || paramss.size > 1 ||
          estats.nonEmpty || ctorcalls.size > 1 || !param.name.isInstanceOf[Name.Anonymous])
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
        val classDef =
          if (tparams.isEmpty) q"final class $tname(...$paramss) extends ..$ctorcalls { ..$stats }"
          else q"final class $tname[..$tparams](...$paramss) extends ..$ctorcalls { ..$stats }"
        val objectDef = {
          val ctorName = Ctor.Name(tname.value)
          val apply =
            if (tparams.isEmpty) q"def apply(...$paramss): $tpe = new $ctorName(..$varNames)"
            else q"def apply[..$tparams](...$paramss): $tpe = new $ctorName(..$varNames)"
          ctorcalls.headOption match {
            case Some(ctor"$_[..$atpesnel]()") if paramss.nonEmpty && paramss.head.nonEmpty =>
              val argType = if (atpesnel.size > 1) t"(..$atpesnel)" else atpesnel.head
              val impClass = {
                val (result, impParam) =
                  if (varTypes.size > 1)
                    (q"lazy val result: $tpe = apply(..${varTypes.indices.map(i => q"arg.${Term.Name(s"_${i + 1}")}")})",
                      param"arg: (..$varTypes)")
                  else (q"lazy val result: $tpe = apply(arg)", param"arg: ${varTypes.head}")
                if (tparams.nonEmpty)
                  q"""implicit final class ${Type.Name(tname.value + "_F")}[..$tparams]($impParam) extends org.sireum._RichF[$argType, $tpe] { $result }"""
                else
                  q"""implicit final class ${Type.Name(tname.value + "_F")}($impParam) extends org.sireum._RichF[$argType, $tpe] { $result }"""
              }
              q"""object ${Term.Name(tname.value)} { ..${Vector(impClass, apply)} }"""
            case _ => q"object ${Term.Name(tname.value)} { $apply }"
          }
        }
        Term.Block(Vector(classDef, objectDef))
      case Term.Block(Seq(_: Defn.Trait, _: Defn.Object)) =>
        abort(s"Cannot use Slang @rich on a trait with an existing companion object.")
      case Term.Block(Seq(_: Defn.Class, _: Defn.Object)) =>
        abort(s"Cannot use Slang @rich on a class with an existing companion object.")
      case _ => abort(s"Invalid Slang @record on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}
