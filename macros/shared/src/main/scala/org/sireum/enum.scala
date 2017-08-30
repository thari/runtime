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
class enum extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val result: Stat = tree match {
      case q"..$mods object $name extends { ..$estats } with ..$ctorcalls { $param => ..$stats }" =>
        if (mods.nonEmpty || estats.nonEmpty || ctorcalls.nonEmpty || !param.name.isInstanceOf[Name.Anonymous])
          abort(s"Invalid @enum form on an object; it has to be of the form '@enum object ${name.value} { ... }'.")
        var decls = Vector[Stat](
          q"""sealed trait Value extends scala.Ordered[Value] {
                def ordinal: org.sireum.Z

                def name: org.sireum.String

                final def hash: org.sireum.Z = hashCode

                final def isEqual(other: Value): org.sireum.B = this == other

                final def compare(that: Value): scala.Int = this.ordinal.compareTo(that.ordinal)
              }
           """
          ,
          q"""final def byName(name: org.sireum.String): org.sireum.Option[Value] =
                elements.elements.find(_.name == name) match {
                  case scala.Some(v) => org.sireum.Some(v)
                  case _ => org.sireum.None()
              }
           """,
          q"""final def byOrdinal(n: org.sireum.Z): org.sireum.Option[Value] =
                if (0 <= n && n < elements.size) org.sireum.Some(elements(n)) else org.sireum.None()
           """
        )
        var elements = Vector[Term.Name]()
        var i = 0
        for (stat <- stats) {
          val sym = stat match {
            case Lit.Symbol(sym) => sym.name
            case Term.Apply(Term.Select(Term.Name("scala"), Term.Name("Symbol")), Seq(Lit.String(sym))) => sym
            case _ => abort(stat.pos, s"Slang @enum can only have symbols for enum element names: ${stat.structure}")
          }
          val tname = Term.Name(sym)
          val ostats = Vector(
            q"def ordinal: org.sireum.Z = ${Lit.Int(i)}",
            q"def name: org.sireum.String = ${Lit.String(sym)}"
          )
          decls :+= q"final case object $tname extends Value { ..$ostats }"
          elements :+= tname
          i += 1
        }
        decls ++= Vector(
          q"val numOfElements: org.sireum.Z = ${Lit.Int(i)}",
          q"val elements: org.sireum.ISZ[Value] = org.sireum.ISZ[Value](..$elements)"
        )
        q"object $name extends {} with org.sireum.EnumSig { type Type = Value; ..$decls }"
      case _ => abort(tree.pos, "Slang @enum can only be used on an object.")
    }
    //println(result.syntax)
    result
  }
}
