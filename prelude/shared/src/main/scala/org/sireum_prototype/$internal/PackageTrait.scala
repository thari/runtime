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

package org.sireum_prototype.$internal

trait PackageTrait {

  type Nothing = scala.Nothing

  def $clone[T](o: T): T = o match {
    case o: Clonable => o.$clone.asInstanceOf[T]
    case _ => halt("Unexpected Slang unclonable value.")
  }

  def halt(msg: Any): Nothing = {
    assume(assumption = false, msg.toString)
    throw new Error
  }

  import language.experimental.macros

  def $[T]: T = macro Macro.$Impl[T]

  def $assign[T](arg: T): T = macro Macro.$assignImpl

  def $$assign[T](arg: T): T = {
    arg match {
      case x: MutableMarker => (if (x.owned) x.$clone.owned = true else x.owned = true).asInstanceOf[T]
      case _ => arg
    }
  }

  object up {
    def update[T](lhs: T, rhs: T): Unit = macro Macro.up
  }

  object pat {
    def update(args: Any*): Unit = macro Macro.pat
  }

}
