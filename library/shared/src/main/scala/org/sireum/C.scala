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


object C {

  object Boxer extends $internal.Boxer {
    def box[T](o: scala.Any): T = o match {
      case o: scala.Char => C(o).asInstanceOf[T]
    }

    def unbox(o: scala.Any): scala.Char = o match {
      case o: C => o.value
    }

    override def copyMut(src: AnyRef, srcPos: Z.MP, dest: AnyRef, destPos: Z.MP, length: Z.MP): Unit =
      copy(src, srcPos, dest, destPos, length)

    override def create(length: Z.MP): scala.AnyRef = new Array[scala.Char](length)

    override def lookup[T](a: scala.AnyRef, i: Z.MP): T = a match {
      case a: Array[scala.Char] => box(a(i))
    }

    override def store(a: scala.AnyRef, i: Z.MP, v: scala.Any): Unit = a match {
      case a: Array[scala.Char] => a(i) = unbox(v)
    }
  }

  def unapply(c: C): scala.Option[scala.Char] = scala.Some(c.value)

  import scala.language.implicitConversions

  @inline implicit def apply(c: scala.Char): C = new C(c)

}

final class C(val value: scala.Char) extends AnyVal with $internal.HasBoxer {

  @inline def native: scala.Char = value

  @inline def <(other: C): B = value < other.value

  @inline def <=(other: C): B = value <= other.value

  @inline def >(other: C): B = value > other.value

  @inline def >=(other: C): B = value >= other.value

  @inline def >>>(other: C): C = (value >>> other.value).toChar

  @inline def <<(other: C): C = (value << other.value).toChar

  @inline def &(other: C): C = (value & other.value).toChar

  @inline def |(other: C): C = (value | other.value).toChar

  @inline def |^(other: C): C = (value ^ other.value).toChar

  @inline def string: String = toString

  @inline override def toString: Predef.String = value.toString

  def boxer: $internal.Boxer = C.Boxer

}
