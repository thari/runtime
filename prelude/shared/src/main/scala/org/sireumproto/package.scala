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

package org

package object sireumproto extends $internal.PackageTrait {

  import language.experimental.macros

  import $internal.Macro

  implicit class $Slang(val sc: StringContext) {

    def l[T](args: Any*): T = macro Macro.l[T]

    def lUnit(args: Any*): Unit = macro Macro.lUnit

    def lDef[T](args: Any*): T = macro Macro.lDef[T]

    object z {
      def apply(args: Any*): Z = macro Macro.zApply
      def unapply(n: Z): scala.Boolean = {
        assume(n.isInstanceOf[Z.MP] && sc.parts.size == 1)
        n == Z(sc.parts.head)
      }
    }

    object r {
      def apply(args: Any*): R = macro Macro.rApply
      def unapply(n: R): scala.Boolean = {
        assume(sc.parts.size == 1)
        n == R(sc.parts.head)
      }
    }

    def st(args: Any*): ST = macro Macro.st
  }

}