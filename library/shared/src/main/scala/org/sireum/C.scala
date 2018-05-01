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
      case o: scala.Int => C(o).asInstanceOf[T]
    }

    def unbox(o: scala.Any): scala.Int = o match {
      case o: C => o.value
    }

    override def copyMut(src: AnyRef, srcPos: Z, dest: AnyRef, destPos: Z, length: Z): Unit =
      copy(src, srcPos, dest, destPos, length)

    override def create(length: Z): scala.AnyRef = new Array[scala.Int](length)

    override def lookup[T](a: scala.AnyRef, i: Z): T = a match {
      case a: Array[scala.Int] => box(a(i))
    }

    override def store(a: scala.AnyRef, i: Z, v: scala.Any): Unit = a match {
      case a: Array[scala.Int] => a(i) = unbox(v)
    }
  }

  def random: C = {
    val r = new _root_.java.util.Random
    var c = r.nextInt.toChar
    while (0xD800 <= c && c <= 0xDFFF) {
      c = r.nextInt.toChar
    }
    C(c.toInt)
  }

  def unapply(c: C): scala.Option[scala.Int] = scala.Some(c.value)

  import scala.language.implicitConversions

  @inline def apply(c: scala.Int): C = new C(c)

  @inline implicit def apply(c: scala.Char): C = {
    require(!(0xD800 <= c && c <= 0xDFFF))
    new C(c.toInt)
  }

}

final class C(val value: scala.Int) extends AnyVal with Immutable with $internal.HasBoxer {

  @inline def native: scala.Char = {
    require(!(0xD800 <= value && value <= 0xDFFF))
    value.toChar
  }

  @inline def unary_~(other: C): C = C(~value)

  @inline def +(other: C): C = C(value + other.value)

  @inline def -(other: C): C = C(value - other.value)

  @inline def <(other: C): B = value < other.value

  @inline def <=(other: C): B = value <= other.value

  @inline def >(other: C): B = value > other.value

  @inline def >=(other: C): B = value >= other.value

  @inline def >>>(other: C): C = C(value >>> other.value)

  @inline def >>(other: C): C = C(value >>> other.value)

  @inline def <<(other: C): C = C(value << other.value)

  @inline def &(other: C): C = C(value & other.value)

  @inline def |(other: C): C = C(value | other.value)

  @inline def |^(other: C): C = C(value ^ other.value)

  @inline override def string: String = toString

  @inline override def toString: Predef.String = new Predef.String(Array(value), 0, 1)

  def boxer: $internal.Boxer = C.Boxer

}
