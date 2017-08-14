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

class index(min: Option[BigInt],
            max: Option[BigInt],
            bits: Boolean = false) extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case q"class $tname" =>
        val q"new index(..$args)" = this
        var min: BigInt = 0
        var max: BigInt = 0
        var isBitVector = false
        for (arg <- args) {
          arg match {
            case arg"min = $exp" =>
              helper.extractInt(exp) match {
                case Some(n) => min = n
                case _ => abort(exp.pos, s"Invalid Slang @index min argument: ${arg.syntax}")
              }
            case arg"max = $exp" =>
              helper.extractInt(exp) match {
                case Some(n) => max = n
                case _ => abort(exp.pos, s"Invalid Slang @index max argument: ${arg.syntax}")
              }
            case arg"bits = ${Lit.Boolean(b)}" => isBitVector = b
            case arg"bits = ${Term.Name("T")}" => isBitVector = true
            case arg"bits = ${Term.Name("F")}" => isBitVector = false
            case _ => abort(tree.pos, s"Invalid Slang @index argument: ${arg.syntax}")
          }
        }
        if (max < min) abort(tree.pos, s"Invalid Slang @index range (max < min): ${tree.syntax}")
        val result =
          if (isBitVector) {
            val (signed, width) = helper.bits(min, max).getOrElse(
              abort(tree.pos, s"Invalid Slang @index bitvector range: ${tree.syntax}"))
            bits.q(signed, width, index = true, tname.value)
          }
          else range.q(index = true, Some(min), Some(max), tname.value)
        //println(result)
        result
      case _ => abort(tree.pos, s"Invalid Slang @index on: ${tree.syntax}")
    }
  }
}
