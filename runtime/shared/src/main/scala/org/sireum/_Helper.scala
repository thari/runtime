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

object _Helper {

  object Up {
    import scala.language.experimental.macros
    def update[T](lhs: T, rhs: T): Unit = macro _macro.up
  }

  object Pat {
    import scala.language.experimental.macros
    def update(args: Any*): Unit = macro _macro.pat
  }

  object R {

    final def apply(r: Predef.String): R = math.Numbers.toR(r.replaceAllLiterally(" ", ""))

    final def random: R = apply(math._Z.random.toString + "." + math._N.random.toString)
  }

  def quote(s: Predef.String): Predef.String = {
    def escape(s: Predef.String): Predef.String = s.flatMap(escapedChar)

    def escapedChar(ch: Char): Predef.String = ch match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"' => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _ => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else String.valueOf(ch)
    }

    "\"" + escape(s) + "\""
  }

  def append(sb: StringBuilder, x: Any): Unit = {
    import _Type._
    def iType(c: Class[_]): String = c.toString match {
      case `zType` => "Z"
      case `z8Type` => "Z8"
      case `z16Type` => "Z16"
      case `z32Type` => "Z32"
      case `z64Type` => "Z64"
      case `nType` => "N"
      case `n8Type` => "N8"
      case `n16Type` => "N16"
      case `n32Type` => "N32"
      case `n64Type` => "N64"
      case `s8Type` => "S8"
      case `s16Type` => "S16"
      case `s32Type` => "S32"
      case `s64Type` => "S64"
      case `u8Type` => "U8"
      case `u16Type` => "U16"
      case `u32Type` => "U32"
      case `u64Type` => "U64"
    }
    x match {
      case x: Predef.String => sb.append(quote(x))
      case x: String => sb.append(quote(x.value))
      case x: IS[_, _] =>
        sb.append(s"IS${iType(x.iTag.runtimeClass)}(")
        if (x.nonEmpty) {
          sb.append(x.elements.head)
          for (e <- x.elements.tail) {
            sb.append(", ")
            sb.append(e)
          }
        }
        sb.append(")")
      case x: MS[_, _] =>
        sb.append(s"MS${iType(x.iTag.runtimeClass)}(")
        if (x.nonEmpty) {
          sb.append(x.elements.head)
          for (e <- x.elements.tail) {
            sb.append(", ")
            sb.append(e)
          }
        }
        sb.append(")")
      case _ => sb.append(x)
    }
  }
}
