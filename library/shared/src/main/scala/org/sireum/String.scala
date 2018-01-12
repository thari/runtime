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

object String {

  object Boxer extends $internal.Boxer {
    def box[T](o: scala.Any): T = o match {
      case o: Predef.String => String(o).asInstanceOf[T]
    }

    def unbox(o: scala.Any): Predef.String = o match {
      case o: String => o.value
    }

    override def copyMut(src: AnyRef, srcPos: Z.MP, dest: AnyRef, destPos: Z.MP, length: Z.MP): Unit =
      copy(src, srcPos, dest, destPos, length)
  }

  def escape(o: scala.Any): Predef.String = {
    def z2s(n: scala.Long): Predef.String =
      if (scala.Int.MinValue <= n && n <= scala.Int.MaxValue) n.toString
      else n.toString + 'l'

    o match {
      case o: String => helper.escape(o.value)
      case o: C => val s = helper.escape(o.toString); '\'' + s.substring(1, s.length - 1) + '\''
      case o: F32 => o + "f"
      case o: R => "r\"" + o + '"'
      case o: Z.MP.Long => z2s(o.value)
      case o: Z.MP.BigInt => o.pack match {
        case o2: Z.MP.Long => z2s(o2.value)
        case _ => "z\"" + o + '"'
      }
      case o: Z => o.Name.toLowerCase + '"' + o + '"'
      case _ => o.toString
    }
  }

  def apply[I](cs: IS[I, C]): String = {
    if (cs.isEmpty) return ""
    String(new Predef.String(cs.data.asInstanceOf[scala.Array[scala.Char]], 0, cs.length.toInt))
  }

  def apply[I](cs: MS[I, C]): String = {
    if (cs.isEmpty) return ""
    String(new Predef.String(cs.data.asInstanceOf[scala.Array[scala.Char]], 0, cs.length.toInt))
  }

  def unapply(s: String): scala.Option[Predef.String] = scala.Some(s.value)

  import scala.language.implicitConversions

  implicit def apply(s: Predef.String): String = new String(s)
}

final class String(val value: Predef.String) extends AnyVal with Immutable with $internal.HasBoxer {

  @inline def native: Predef.String = value

  @inline def string: String = this

  @inline override def toString: Predef.String = value

  @inline def at(i: Z): C = {
    val index = i.toIndex
    assert(0 <= index && index <= value.length, s"String indexing out of bound: $i")
    value.charAt(index.toInt)
  }

  def stripPrefix(prefix: Predef.String): Predef.String = value.stripPrefix(prefix)

  @inline def boxer: $internal.Boxer = String.Boxer

  @inline def size: Z = value.length

  def toCis: IS[Z, C] =
    IS[Z, C](value.map(C.apply): _*)

  def toCms: MS[Z, C] =
    MS[Z, C](value.map(C.apply): _*)

}
