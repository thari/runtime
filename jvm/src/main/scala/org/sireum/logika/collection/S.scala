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
import org.sireum.logika.{Z, Z8, Z16, Z32, Z64, N, N8, N16, N32, N64, S8, S16, S32, S64, U8, U16, U32, U64}
import org.sireum.logika.math.{_Z, LogikaIntegralNumber}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

sealed trait S[I <: LogikaIntegralNumber, V] extends Clonable {
  private[logika] val properties = scala.collection.mutable.HashMap[Any, Any]()

  def property[T](key: Any): T = properties(key).asInstanceOf[T]

  def elements: scala.collection.Seq[V]

  def apply(index: I): V

  def size: I

  def :+(value: V): S[I, V]

  def +:(value: V): S[I, V]

  def ++(values: S[I, V]): S[I, V]

  def apply(entries: (I, V)*): S[I, V]
}

object _IS {
  val zType: Type = typeOf[Z]
  val z8Type: Type = typeOf[Z8]
  val z16Type: Type = typeOf[Z16]
  val z32Type: Type = typeOf[Z32]
  val z64Type: Type = typeOf[Z64]
  val nType: Type = typeOf[N]
  val n8Type: Type = typeOf[N8]
  val n16Type: Type = typeOf[N16]
  val n32Type: Type = typeOf[N32]
  val n64Type: Type = typeOf[N64]
  val s8Type: Type = typeOf[S8]
  val s16Type: Type = typeOf[S16]
  val s32Type: Type = typeOf[S32]
  val s64Type: Type = typeOf[S64]
  val u8Type: Type = typeOf[U8]
  val u16Type: Type = typeOf[U16]
  val u32Type: Type = typeOf[U32]
  val u64Type: Type = typeOf[U64]

  def apply[I <: LogikaIntegralNumber, V: ClassTag](values: V*)(
    implicit tag: TypeTag[I]): _IS[I, V] = {
    val sz = _Z(values.length)
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

    new ISImpl[I, V](size, Array[V](values: _*))
  }

  def create[I <: LogikaIntegralNumber, V: ClassTag](size: I, default: V)(
    implicit tag: TypeTag[I]): _IS[I, V] = {
    val sz = size.toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new ISImpl[I, V](size, Array.fill[V](sz.toInt)(default))
  }
}

sealed trait _IS[I <: LogikaIntegralNumber, V] extends S[I, V] {
  override def :+(value: V): _IS[I, V]

  override def +:(value: V): _IS[I, V]

  override def ++(values: S[I, V]): _IS[I, V]

  override def apply(entries: (I, V)*): _IS[I, V]
}

private final class ISImpl[I <: LogikaIntegralNumber, V: ClassTag](val size: I, val data: Array[V])(
  implicit tag: TypeTag[I]) extends _IS[I, V] {
  def elements: scala.collection.Seq[V] = data

  def apply(index: I): V = {
    val i = index.toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt)
  }

  override def hashCode: Int = {
    data.toSeq.hashCode
  }

  override def toString: String = {
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

  override def :+(value: V): _IS[I, V] = _IS[I, V](elements :+ value: _*)

  override def +:(value: V): _IS[I, V] = _IS[I, V](value +: elements: _*)

  override def ++(values: S[I, V]): _IS[I, V] = _IS[I, V](elements ++ values.elements: _*)

  override def equals(other: Any): Boolean = other match {
    case other: ISImpl[_, _] =>
      if (other.size != size) return false
      if (other.data eq data) return true
      for (i <- data.indices) {
        if (other.data(i) != data(i)) return false
      }
      true
    case _ => false
  }

  override def clone: _IS[I, V] = this

  override def apply(entries: (I, V)*): _IS[I, V] = {
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
    new ISImpl[I, V](size, newData)
  }
}

object _MS {

  def apply[I <: LogikaIntegralNumber, V: ClassTag](values: V*)(
    implicit tag: TypeTag[I]): _MS[I, V] = {
    val sz = _Z(values.length)
    val size: I = (tag.tpe match {
      case t if t <:< _IS.zType => sz
      case t if t <:< _IS.z8Type => sz.toZ8
      case t if t <:< _IS.z16Type => sz.toZ16
      case t if t <:< _IS.z32Type => sz.toZ32
      case t if t <:< _IS.z64Type => sz.toZ64
      case t if t <:< _IS.nType => sz.toN
      case t if t <:< _IS.n8Type => sz.toN8
      case t if t <:< _IS.n16Type => sz.toN16
      case t if t <:< _IS.n32Type => sz.toN32
      case t if t <:< _IS.n64Type => sz.toN64
      case t if t <:< _IS.s8Type => sz.toS8
      case t if t <:< _IS.s16Type => sz.toS16
      case t if t <:< _IS.s32Type => sz.toS32
      case t if t <:< _IS.s64Type => sz.toS64
      case t if t <:< _IS.u8Type => sz.toU8
      case t if t <:< _IS.u16Type => sz.toU16
      case t if t <:< _IS.u32Type => sz.toU32
      case t if t <:< _IS.u64Type => sz.toU64
    }).asInstanceOf[I]

    new MSImpl[I, V](size, Array[V](values: _*))
  }

  def create[I <: LogikaIntegralNumber, V: ClassTag](size: I, default: V)(
    implicit tag: TypeTag[I]): _MS[I, V] = {
    val sz = size.toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new MSImpl[I, V](size, Array.fill[V](sz.toInt)(default))
  }
}

sealed trait _MS[I <: LogikaIntegralNumber, V] extends S[I, V] {
  override def :+(value: V): _MS[I, V]

  override def +:(value: V): _MS[I, V]

  override def ++(values: S[I, V]): _MS[I, V]

  override def apply(entries: (I, V)*): _MS[I, V]

  def update(index: I, value: V): Unit

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
  implicit tag: TypeTag[I]) extends _MS[I, V] {
  def elements: scala.collection.Seq[V] = data

  def apply(index: I): V = {
    val i = index.toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt)
  }

  def update(index: I, value: V): Unit = {
    val i = index.toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt) = value
  }

  override def hashCode: Int = {
    data.toSeq.hashCode
  }

  override def :+(value: V): _MS[I, V] = _MS[I, V](elements :+ value: _*)

  override def +:(value: V): _MS[I, V] = _MS[I, V](value +: elements: _*)

  override def ++(values: S[I, V]): _MS[I, V] = _MS[I, V](elements ++ values.elements: _*)

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

  override def clone: _MS[I, V] = {
    val newData = data.clone
    for (i <- newData.indices) {
      newData(i) = newData(i) match {
        case o: Clonable => o.clone.asInstanceOf[V]
        case o => o
      }
    }
    new MSImpl[I, V](size, newData)
  }

  override def apply(entries: (I, V)*): _MS[I, V] = {
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
