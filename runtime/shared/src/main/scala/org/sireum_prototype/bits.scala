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

class bits(signed: Boolean, width: Int) extends scala.annotation.StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case tree: Defn.Type if helper.isZ(tree) =>
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
        val result = tree
        //println(result)
        result
      case _ => abort(tree.pos, s"Invalid Slang @bits on: ${tree.syntax}")
    }
  }
}
