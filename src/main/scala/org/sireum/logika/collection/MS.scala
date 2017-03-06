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

object MS {

  def apply[I <: LogikaIntegralNumber, V: ClassTag](values: V*)(
    implicit tag: TypeTag[I]): MS[I, V] = {
    val sz = Z(values.length)
    val size: I = (tag.tpe match {
      case t if t <:< IS.zType => sz
      case t if t <:< IS.z8Type => sz.toZ8
      case t if t <:< IS.z16Type => sz.toZ16
      case t if t <:< IS.z32Type => sz.toZ32
      case t if t <:< IS.z64Type => sz.toZ64
      case t if t <:< IS.nType => sz.toN
      case t if t <:< IS.n8Type => sz.toN8
      case t if t <:< IS.n16Type => sz.toN16
      case t if t <:< IS.n32Type => sz.toN32
      case t if t <:< IS.n64Type => sz.toN64
      case t if t <:< IS.s8Type => sz.toS8
      case t if t <:< IS.s16Type => sz.toS16
      case t if t <:< IS.s32Type => sz.toS32
      case t if t <:< IS.s64Type => sz.toS64
      case t if t <:< IS.u8Type => sz.toU8
      case t if t <:< IS.u16Type => sz.toU16
      case t if t <:< IS.u32Type => sz.toU32
      case t if t <:< IS.u64Type => sz.toU64
    }).asInstanceOf[I]

    new MSImpl[I, V](size, Array[V](values: _*))
  }

  def create[I <: LogikaIntegralNumber, V: ClassTag](size: I, default: V)(
    implicit tag: TypeTag[I]): MS[I, V] = {
    val sz = size.toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new MSImpl[I, V](size, Array.fill[V](sz.toInt)(default))
  }
}

sealed trait MS[I <: LogikaIntegralNumber, V] extends Clonable {
  def elements: scala.collection.Seq[V]

  def apply(index: I): V

  def update(index: I, value: V): Unit

  def size: I

  def :+(value: V): MS[I, V]

  def +:(value: V): MS[I, V]

  def ++(values: MS[I, V]): MS[I, V]

  def clone(entries: (I, V)*): MS[I, V]

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

private class MSImpl[I <: LogikaIntegralNumber, V: ClassTag](val size: I, val data: Array[V])(
  implicit tag: TypeTag[I]) extends MS[I, V] {
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

  override def :+(value: V): MS[I, V] = MS[I, V](elements :+ value: _*)

  override def +:(value: V): MS[I, V] = MS[I, V](value +: elements: _*)

  override def ++(values: MS[I, V]): MS[I, V] = MS[I, V](elements ++ values.elements: _*)

  override def equals(other: Any): Boolean = other match {
    case other: MSImpl[_, _] =>
      if (other.size != size) return false
      if (other.data eq data) return true
      for (i <- data.indices) {
        if (other.data(i) != data(i)) return false
      }
      true
    case _ => false
  }

  override def clone: MS[I, V] = {
    val newData = data.clone
    for (i <- newData.indices) {
      newData(i) = newData(i) match {
        case o: Clonable => o.clone.asInstanceOf[V]
        case o => o
      }
    }
    new MSImpl[I, V](size, newData)
  }

  override def clone(entries: (I, V)*): MS[I, V] = {
    var entryMap: Map[Int, V] = Map()
    for ((i, v) <- entries) {
      entryMap += i.toZ.toInt -> v
    }
    val newData: Array[V] = data.clone
    for (i <- elements.indices) {
      entryMap.get(i) match {
        case Some(v) => newData(i) = v
        case None => newData(i) = newData(i) match {
          case o: Clonable => o.clone.asInstanceOf[V]
          case o => o
        }
      }
    }
    new MSImpl[I, V](size, newData)
  }
}
