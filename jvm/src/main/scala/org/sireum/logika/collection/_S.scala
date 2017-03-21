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
import org.sireum.logika.{Z, Z8, Z16, Z32, Z64, N, N8, N16, N32, N64, S8, S16, S32, S64, U8, U16, U32, U64, IS, MS}
import org.sireum.logika.math._
import scala.collection.mutable.ArrayBuffer

import scala.reflect.runtime.universe._

object _S {
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

  def clona[T](o: T): T = o match {
    case o: Clonable => o.clone.asInstanceOf[T]
    case x => x
  }

  def int2i[I : TypeTag]: Int => I = typeOf[I] match {
    case t if t <:< zType => (x: Int) => _Z(x).asInstanceOf[I]
    case t if t <:< z8Type => (x: Int) => _Z(x).toZ8.asInstanceOf[I]
    case t if t <:< z16Type => (x: Int) => _Z(x).toZ16.asInstanceOf[I]
    case t if t <:< z32Type => (x: Int) => _Z(x).toZ32.asInstanceOf[I]
    case t if t <:< z64Type => (x: Int) => _Z(x).toZ64.asInstanceOf[I]
    case t if t <:< nType => (x: Int) => _Z(x).toN.asInstanceOf[I]
    case t if t <:< n8Type => (x: Int) => _Z(x).toN8.asInstanceOf[I]
    case t if t <:< n16Type => (x: Int) => _Z(x).toN16.asInstanceOf[I]
    case t if t <:< n32Type => (x: Int) => _Z(x).toN32.asInstanceOf[I]
    case t if t <:< n64Type => (x: Int) => _Z(x).toN64.asInstanceOf[I]
    case t if t <:< s8Type => (x: Int) => _Z(x).toS8.asInstanceOf[I]
    case t if t <:< s16Type => (x: Int) => _Z(x).toS16.asInstanceOf[I]
    case t if t <:< s32Type => (x: Int) => _Z(x).toS32.asInstanceOf[I]
    case t if t <:< s64Type => (x: Int) => _Z(x).toS64.asInstanceOf[I]
    case t if t <:< u8Type => (x: Int) => _Z(x).toU8.asInstanceOf[I]
    case t if t <:< u16Type => (x: Int) => _Z(x).toU16.asInstanceOf[I]
    case t if t <:< u32Type => (x: Int) => _Z(x).toU32.asInstanceOf[I]
    case t if t <:< u64Type => (x: Int) => _Z(x).toU64.asInstanceOf[I]
  }
}

import _S._

sealed trait _S[I, V] extends Clonable {
  private[logika] val properties = scala.collection.mutable.HashMap[Any, Any]()

  def property[T](key: Any): T = properties(key).asInstanceOf[T]

  def elements: scala.collection.Seq[V]

  def apply(index: I): V

  def size: I

  def :+(value: V): _S[I, V]

  def +:(value: V): _S[I, V]

  def ++(values: _S[I, V]): _S[I, V]

  def apply(entries: (I, V)*): _S[I, V]
}

object _IS {
  def apply[I : TypeTag, V](values: V*): _IS[I, V] =
    new ISImpl[I, V](int2i[I], values.length, Vector[V](values: _*))

  def create[I : TypeTag, V](size: I, default: V): _IS[I, V] = {
    val sz = size.asInstanceOf[LogikaIntegralNumber].toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new ISImpl[I, V](int2i[I], sz.toInt, Vector[V]((0 until sz.toInt).map(_ => clona(default)): _*))
  }
}

sealed trait _IS[I, V] extends _S[I, V] {
  override def :+(value: V): IS[I, V]

  override def +:(value: V): IS[I, V]

  override def ++(values: _S[I, V]): IS[I, V]

  override def apply(entries: (I, V)*): IS[I, V]
}

private[logika] final class ISImpl[I, V](val f: Int => I, val sz: Int, val data: Vector[V]) extends _IS[I, V] {
  override val hashCode: Int = data.hashCode

  override val size: I = f(sz)

  def elements: scala.collection.Seq[V] = data

  def apply(index: I): V = {
    val i = index.asInstanceOf[LogikaIntegralNumber].toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt)
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

  override def :+(value: V): _IS[I, V] = new ISImpl[I, V](f, sz + 1, data :+ clona(value))

  override def +:(value: V): _IS[I, V] = new ISImpl[I, V](f, sz + 1, clona(value) +: data)

  override def ++(values: _S[I, V]): _IS[I, V] = values match {
    case (values: ISImpl[I, V] @unchecked) => new ISImpl[I, V](f, sz + values.sz, (data ++ values.data).map(clona))
    case (values: MSImpl[I, V] @unchecked) => new ISImpl[I, V](f, sz + values.sz, (data ++ values.data).map(clona))
  }

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

  override def clone: IS[I, V] = new ISImpl[I, V](f, sz, data.map(clona))

  override def apply(entries: (I, V)*): _IS[I, V] = {
    var entryMap: Map[Int, V] = Map()
    for ((i, v) <- entries) {
      entryMap += i.asInstanceOf[LogikaIntegralNumber].toZ.toInt -> v
    }
    val newData = ArrayBuffer[V](data: _*)
    for (i <- elements.indices) {
      entryMap.get(i) match {
        case Some(v) => newData(i) = clona(v)
        case None => newData(i) = clona(newData(i))
      }
    }
    new ISImpl[I, V](f, sz, newData.toVector)
  }
}

object _MS {
  def apply[I : TypeTag, V](values: V*): _MS[I, V] = {
    new MSImpl[I, V](int2i[I], values.length, ArrayBuffer[V](values: _*))
  }

  def create[I : TypeTag, V](size: I, default: V): _MS[I, V] = {
    val sz = size.asInstanceOf[LogikaIntegralNumber].toZ
    require(sz >= 0 && sz <= Int.MaxValue)
    new MSImpl[I, V](int2i[I], sz.toInt, ArrayBuffer((0 until sz.toInt).map(_ => clona(default)): _*))
  }
}

sealed trait _MS[I, V] extends _S[I, V] {
  override def :+(value: V): MS[I, V]

  override def +:(value: V): MS[I, V]

  override def ++(values: _S[I, V]): MS[I, V]

  override def apply(entries: (I, V)*): MS[I, V]

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

private[logika] class MSImpl[I, V](val f: Int => I, val sz: Int, val data: ArrayBuffer[V]) extends _MS[I, V] {
  def size: I = f(sz)

  def elements: scala.collection.Seq[V] = data

  def apply(index: I): V = {
    val i = index.asInstanceOf[LogikaIntegralNumber].toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt)
  }

  def update(index: I, value: V): Unit = {
    val i = index.asInstanceOf[LogikaIntegralNumber].toZ
    require(0 <= i && i < _Z(elements.length))
    data(i.toInt) = clona(value)
  }

  override def hashCode: Int = data.hashCode

  override def :+(value: V): _MS[I, V] = new MSImpl[I, V](f, sz + 1, data :+ clona(value))

  override def +:(value: V): _MS[I, V] = new MSImpl[I, V](f, sz + 1, clona(value) +: data)

  override def ++(values: _S[I, V]): _MS[I, V] = values match {
    case (values: MSImpl[I, V] @unchecked) => new MSImpl[I, V](f, sz + values.sz, (data ++ values.data).map(clona))
    case (values: ISImpl[I, V] @unchecked) => new MSImpl[I, V](f, sz + values.sz, (data ++ values.data).map(clona))
  }

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

  override def clone: _MS[I, V] = new MSImpl[I, V](f, sz, data.map(clona))

  override def apply(entries: (I, V)*): _MS[I, V] = {
    var entryMap: Map[Int, V] = Map()
    for ((i, v) <- entries) {
      entryMap += i.asInstanceOf[LogikaIntegralNumber].toZ.toInt -> v
    }
    val newData: ArrayBuffer[V] = data.clone
    for (i <- elements.indices) {
      entryMap.get(i) match {
        case Some(v) => newData(i) = clona(v)
        case None => newData(i) = clona(newData(i))
      }
    }
    new MSImpl[I, V](f, sz, newData)
  }
}
