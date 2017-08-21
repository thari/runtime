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

// TODO: clean up quasiquotes due to IntelliJ's macro annotation inference workaround
object sig {

  def transformTrait(tree: Defn.Trait): Defn.Trait = {
    val q"..$mods trait $tname[..$tparams] extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" = tree
    if (estats.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
      abort("Slang @sig traits have to be of the form '@sig trait <id> ... { ... }'.")
    q"..$mods trait $tname[..$tparams] extends { ..$estats } with Immutable with ..$ctorcalls { $param => ..$stats }"
  }
}

class sig extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case tree: Defn.Trait => sig.transformTrait(tree)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(List[Stat](sig.transformTrait(t), o))
      case _ =>
        abort(tree.pos, s"Invalid Slang @sig on: ${tree.syntax}.")
    }
    //println(result.syntax)
    result
  }
}
