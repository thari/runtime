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

final class memoize extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case q"..$mods def $name[..$tparams](...$paramss): ${tpeopt: Option[Type]} = $expr" =>
        if (paramss.size > 1)
          abort(tree.pos, s"Slang @memoize methods should only have a list of parameters (instead of several lists of parameters).")
        if (paramss.size < 1)
          abort(tree.pos, s"Slang @memoize methods should only have a list of parameters (instead of none).")
        if (paramss.head.isEmpty)
          abort(tree.pos, s"Slang @memoize methods should only have at least a parameter.")
        if (tpeopt.isEmpty)
          abort(tree.pos, s"Slang @memoize methods should be explicitly typed.")
        val returnType = tpeopt.get
        var allParamTypes = Vector[Type]()
        var paramTypes = Vector[Type]()
        var params = Vector[Term.Name]()
        if (paramss.nonEmpty) {
          paramss.head.toList.foreach {
            case param"..$mods $paramname: ${atpeopt: Option[Type.Arg]} = $_" => atpeopt match {
              case Some(targ"${tpe: Type}") =>
                var isHidden = false
                mods.foreach {
                  case mod"@hidden" => isHidden = true
                }
                allParamTypes :+= tpe
                if (!isHidden) {
                  paramTypes :+= tpe
                  params :+= arg"${Term.Name(paramname.value)}"
                }
              case _ => abort(paramname.pos, "Unsupported Slang @memoize method parameter form.")
            }
          }
        }
        val argType = if (paramTypes.length == 1) paramTypes.head else t"(..$paramTypes)"
        val arg = if (params.length == 1) params.head else q"(..$params)"
        val body =
          q"""{
                def _internal: $returnType = $expr
                val arg = $arg
                cache.get(arg) match {
                  case scala.Some(r) => return r
                  case _ =>
                }
                val r = _internal
                cache = cache + (arg -> r)
                r
              }
           """
        val cacheVar = q"var cache = scala.collection.immutable.Map[$argType, $returnType]()"
        val newName = Term.Name("_" + name.value)
        val newStat =
          if (tparams.isEmpty) q"..$mods def $newName(...$paramss): $tpeopt = $body"
          else q"..$mods def $newName[..$tparams](...$paramss): $tpeopt = $body"
        val fnType =
          if (allParamTypes.size == 1) t"${allParamTypes.head} => $returnType"
          else t"(..$allParamTypes) => $returnType"
        Defn.Val(List(Mod.Lazy()), List[Pat](Pat.Var.Term(name)), Some(fnType),
          q"{ ..${List(cacheVar, newStat, q"$newName _")} }")
      case _ =>
        abort(tree.pos, s"Invalid Slang @memoize on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}
