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

package org.sireum_prototype

import scala.meta._

object bits {
  def q(signed: Boolean, width: Int, index: Boolean, name: String): Term.Block = {
    val (min, max) =
      if (signed) (Lit.Long((BigInt(-2).pow(width - 1) + 1).toLong), Lit.Long((BigInt(2).pow(width - 1) - 1).toLong))
      else (Lit.Long(0l), Lit.Long((BigInt(2).pow(width) - 1).toLong))
    val typeName = Type.Name(name)
    val termName = Term.Name(name)
    val ctorName = Ctor.Name(name)
    val nameStr = Lit.String(name)
    Term.Block(List(
      q"""final class $typeName(val value: scala.Long) extends AnyVal with Z.BV.Long[$typeName] {
            @inline def name: Predef.String = $termName.name
            @inline def BitWidth: scala.Int = $termName.BitWidth
            @inline def Min: $typeName = $termName.Min
            @inline def Max: $typeName = $termName.Max
            @inline def isIndex: scala.Boolean = $termName.isIndex
            @inline def isSigned: scala.Boolean = $termName.isSigned
            def make(v: scala.Long): $typeName = $termName(v)
          }""",
      q"""object $termName {
            val name: Predef.String = $nameStr
            val BitWidth: scala.Int = ${Lit.Int(width)}
            val Min: $typeName = new $ctorName($min)
            val Max: $typeName = new $ctorName($max)
            val isIndex: scala.Boolean = ${Lit.Boolean(index)}
            val isSigned: scala.Boolean = ${Lit.Boolean(signed)}
            def apply(value: scala.Int): $typeName =
              if (!isSigned && BitWidth == 64) new $ctorName(value & 0xffffffffL)
              else new $ctorName(value)
            def apply(value: scala.Long): $typeName =
              new $ctorName(value)
            def apply(value: String): $typeName = new $ctorName(value.value.toLong)
            def unapply(n: $typeName): scala.Option[Z] = scala.Some(Z.MP(n.value))
            object Int {
              def unapply(n: $typeName): scala.Option[scala.Int] =
                if (scala.Int.MinValue <= n.value && n.value <= scala.Int.MaxValue) scala.Some(n.value.toInt)
                else scala.None
            }
            object Long {
              def unapply(n: $typeName): scala.Option[scala.Long] = scala.Some(n.value)
            }
            object String {
              def unapply(n: $typeName): scala.Option[Predef.String]= scala.Some(n.value.toString)
            }
          }"""
    ))
  }
}

class bits(signed: Boolean, width: Int) extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case q"class $tname" =>
        val q"new bits(..$args)" = this
        var width: Int = 0
        var signed: Boolean = false
        for (arg <- args) {
          arg match {
            case arg"signed = ${Term.Name("F")}" => signed = false
            case arg"signed = ${Term.Name("T")}" => signed = true
            case arg"signed = ${Lit.Boolean(b)}" => signed = b
            case arg"width = ${Lit.Int(n)}" =>
              n match {
                case 8 | 16 | 32 | 64 =>
                case _ => abort(arg.pos, s"Invalid Slang @bits width argument: ${arg.syntax} (only 8, 16, 32, or 64 are currently supported)")
              }
              width = n
            case _ => abort(arg.pos, s"Invalid Slang @bits argument: ${arg.syntax}")
          }
        }
        val result = bits.q(signed, width, index = false, tname.value)
        //println(result)
        result
      case _ => abort(tree.pos, s"Invalid Slang @bits on: ${tree.structure}")
    }
  }
}
