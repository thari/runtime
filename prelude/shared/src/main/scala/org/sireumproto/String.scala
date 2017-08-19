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

package org.sireumproto

object String {

  object Boxer extends $internal.Boxer {
    def box[T](o: scala.Any): T = (o: @unchecked) match {
      case o: Predef.String => String(o).asInstanceOf[T]
    }

    def unbox(o: scala.Any): scala.Any = (o: @unchecked) match {
      case o: String => o.value
    }
  }

  def unapply(s: String): scala.Option[Predef.String] = scala.Some(s.value)

  def escape(o: scala.Any): Predef.String = {
    o match {
      case o: String => helper.escape(o.value)
      case o: C => val s = helper.escape(o.toString); '\'' + s.substring(1, s.length - 1) + '\''
      case o: F32 => o.toString + "f"
      case o: R => "r\"" + o + '"'
      case o: Z =>  o.Name.toLowerCase + '"' + o + '"'
      case _ => o.toString
    }
  }

  import scala.language.implicitConversions

  implicit def apply(s: Predef.String): String = new String(s)

}

final class String(val value: Predef.String) extends AnyVal with Immutable with $internal.HasBoxer {

  @inline def hash: Z = value.hashCode

  @inline def isEqual(other: Immutable): B = this == other

  @inline def string: String = this

  @inline override def toString: Predef.String = value

  def stripPrefix(prefix: Predef.String): Predef.String = value.stripPrefix(prefix)

  def boxer: $internal.Boxer = String.Boxer

}
