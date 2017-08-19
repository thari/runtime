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

import scala.collection.mutable.{BitSet => BS}
import scala.collection.immutable.{Map => M}

object B {

  object Boxer extends $internal.Boxer {
    val sz = Z.MP($internal.Boxer.MaxArraySize)
    val bigEndianHexMap: M[scala.Long, scala.Char] = M(
      /* 1000 */ 0x8l -> '1',
      /* 0100 */ 0x4l -> '2',
      /* 1100 */ 0xCl -> '3',
      /* 0010 */ 0x2l -> '4',
      /* 1010 */ 0xAl -> '5',
      /* 0110 */ 0x6l -> '6',
      /* 1110 */ 0xEl -> '7',
      /* 0001 */ 0x1l -> '8',
      /* 1001 */ 0x9l -> '9',
      /* 0101 */ 0x5l -> 'A',
      /* 1101 */ 0xDl -> 'B',
      /* 0011 */ 0x3l -> 'C',
      /* 1011 */ 0xBl -> 'D',
      /* 0111 */ 0x7l -> 'E',
      /* 1111 */ 0xFl -> 'F')

    def box[T](o: scala.Any): T = (o match {
      case true => T
      case false => F
    }).asInstanceOf[T]

    def unbox(o: scala.Any): scala.Boolean = o match {
      case T => true
      case F => false
    }

    override def create(length: Z.MP): scala.AnyRef = new BS(length)

    override def copy(src: scala.AnyRef, srcPos: Z.MP, dest: scala.AnyRef, destPos: Z.MP, length: Z.MP): Unit =
      ((src, dest): @unchecked) match {
        case (src: BS, dest: BS) =>
          val sp: scala.Int = srcPos
          val dp: scala.Int = destPos
          for (i <- 0 until length) {
            dest.update(dp + i, src(sp + i))
          }
      }

    override def lookup[T](a: scala.AnyRef, i: Z.MP): T = (a: @unchecked) match {
      case a: BS => box(a(i))
    }

    override def store(a: scala.AnyRef, i: Z.MP, v: scala.Any): Unit = (a: @unchecked) match {
      case a: BS => a.update(i, unbox(v))
    }

    override def size(a: scala.AnyRef): Z.MP = (a: @unchecked) match {
      case a: BS => sz
    }

    override def clone(a: scala.AnyRef, length: Z.MP, newLength: Z.MP, offset: Z.MP): scala.AnyRef = (a: @unchecked) match {
      case a: BS =>
        val r = BS()
        for (i <- 0 until length) {
          r.update(i, a(i))
        }
        r
    }

    override def toString(a: scala.AnyRef, length: Z.MP): Predef.String = (a: @unchecked) match {
      case a: BS =>
        val sb = new java.lang.StringBuilder
        val bs = a.toBitMask
        sb.append('[')
        if (length > 0) {
          var i = length
          for (b <- bs if i > 0) {
            var m = b
            for (j <- 0 until 16 if i > 0) {
              sb.append(bigEndianHexMap(m & 0xf))
              m = m >>> 4
              i = i - 4
            }
          }
        }
        sb.append(']')
        sb.toString
    }
  }

  val T = new B(true)
  val F = new B(false)

  def unapply(b: B): scala.Option[scala.Boolean] = scala.Some(b.value)

  import scala.language.implicitConversions

  @inline implicit def apply(b: Boolean): B = if (b) T else F

  @inline implicit def $4B(b: B): Boolean = b.value

}

final class B(val value: Boolean) extends AnyVal with Immutable with $internal.HasBoxer {

  @inline def &(other: B): B = value & other.value

  @inline def |(other: B): B = value | other.value

  @inline def |^(other: B): B = value ^ other.value

  @inline def &&(other: => B): B = value && other.value

  @inline def ||(other: => B): B = value && other.value

  @inline def unary_! : B = !value

  @inline def unary_~ : B = !value

  @inline def hash: Z = hashCode

  @inline def isEqual(other: Immutable): B = this == other

  @inline def string: String = toString

  @inline override def toString: Predef.String = value.toString

  @inline def boxer: $internal.Boxer = B.Boxer

}
