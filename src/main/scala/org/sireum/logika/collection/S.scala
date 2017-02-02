/*
 * Copyright (c) 2017, Robby, Kansas State University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.logika.collection

import org.sireum.logika.Clonable
import org.sireum.logika.math._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object S {
  val zType: Type = typeOf[Z]
  val z8Type: Type = typeOf[Z8.Value]
  val z16Type: Type = typeOf[Z16.Value]
  val z32Type: Type = typeOf[Z32.Value]
  val z64Type: Type = typeOf[Z64.Value]
  val nType: Type = typeOf[N]
  val n8Type: Type = typeOf[N8.Value]
  val n16Type: Type = typeOf[N16.Value]
  val n32Type: Type = typeOf[N32.Value]
  val n64Type: Type = typeOf[N64.Value]
  val s8Type: Type = typeOf[S8.Value]
  val s16Type: Type = typeOf[S16.Value]
  val s32Type: Type = typeOf[S32.Value]
  val s64Type: Type = typeOf[S64.Value]
  val u8Type: Type = typeOf[U8.Value]
  val u16Type: Type = typeOf[U16.Value]
  val u32Type: Type = typeOf[U32.Value]
  val u64Type: Type = typeOf[U64.Value]

  def apply[I <: LogikaIntegralNumber, V: ClassTag](values: V*)(
    implicit tag: TypeTag[I]): S[I, V] = {
    val sz = Z(values.length)
    val size: I = (tag.tpe match {
      case t if t <:< zType => sz
      case t if t <:< z8Type => sz.toZ8
      case t if t <:< z16Type => sz.toZ16
      case t if t <:< z32Type => sz.toZ32
      case t if t <:< z64Type => sz.toZ64
      case t if t <:< nType => sz.toN
      case t if t <:< n8Type => sz.toN8
      case t if t <:< n16Type => sz.toN16
      case t if t <:< n32Type => sz.toN32
      case t if t <:< n64Type => sz.toN64
      case t if t <:< s8Type => sz.toS8
      case t if t <:< s16Type => sz.toS16
      case t if t <:< s32Type => sz.toS32
      case t if t <:< s64Type => sz.toS64
      case t if t <:< u8Type => sz.toU8
      case t if t <:< u16Type => sz.toU16
      case t if t <:< u32Type => sz.toU32
      case t if t <:< u64Type => sz.toU64
    }).asInstanceOf[I]

    new SImpl[I, V](size, Array[V](values: _*))
  }

  def create[I <: LogikaIntegralNumber, V: ClassTag](size: I, default: V)(
    implicit tag: TypeTag[I]): S[I, V] = {
    val sz = size.toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new SImpl[I, V](size, Array.fill[V](sz.toInt)(default))
  }
}

sealed trait S[I <: LogikaIntegralNumber, V] extends Clonable {
  def elements: scala.collection.Seq[V]

  def apply(index: I): V

  def update(index: I, value: V): Unit

  def size: I

  def :+(value: V): S[I, V]

  def +:(value: V): S[I, V]

  def ++(values: S[I, V]): S[I, V]

  final override def toString: String = {
    def toBit(i: Int): Char = if (elements(i).asInstanceOf[Boolean]) '1' else '0'

    val sb = new StringBuilder
    sb.append('[')
    if (elements.nonEmpty) {
      val isBoolean = elements.head.isInstanceOf[Boolean]
      if (isBoolean) {
        sb.append(toBit(0))
        for (i <- 1 until elements.length) {
          sb.append(toBit(i))
        }
      } else {
        sb.append(elements.head.toString)
        for (i <- 1 until elements.length) {
          sb.append(", ")
          sb.append(elements(i).toString)
        }
      }
    }
    sb.append(']')
    sb.toString
  }
}

private class SImpl[I <: LogikaIntegralNumber, V: ClassTag](val size: I, val data: Array[V])(
  implicit tag: TypeTag[I]) extends S[I, V] {
  def elements: scala.collection.Seq[V] = data

  def apply(index: I): V = {
    val i = index.toZ
    require(0 <= i && i < Z(elements.length))
    data(i.toInt)
  }

  def update(index: I, value: V): Unit = {
    val i = index.toZ
    require(0 <= i && i < Z(elements.length))
    data(i.toInt) = value
  }

  override def hashCode: Int = {
    data.toSeq.hashCode
  }

  override def :+(value: V): S[I, V] = S[I, V](elements :+ value: _*)

  override def +:(value: V): S[I, V] = S[I, V](value +: elements: _*)

  override def ++(values: S[I, V]): S[I, V] = S[I, V](elements ++ values.elements: _*)

  override def equals(other: Any): Boolean = other match {
    case other: SImpl[_, _] =>
      if (other.size != size) return false
      if (other.data eq data) return true
      for (i <- data.indices) {
        if (other.data(i) != data(i)) return false
      }
      true
    case _ => false
  }

  override def clone: S[I, V] = {
    S[I, V](elements.map({
      case o: Clonable => o.clone.asInstanceOf[V]
      case o => o
    }): _*)
  }
}
