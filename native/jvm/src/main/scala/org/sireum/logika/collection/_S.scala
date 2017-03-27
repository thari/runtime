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

import org.sireum.logika.{_Clonable, _Immutable, Z, Z8, Z16, Z32, Z64, N, N8, N16, N32, N64, S8, S16, S32, S64, U8, U16, U32, U64, IS, MS, _clone}
import org.sireum.logika.math._
import scala.collection.mutable.ArrayBuffer

sealed trait _S[I, V] extends _Clonable with _Immutable {
  private[logika] val properties = scala.collection.mutable.HashMap[Any, Any]()

  def property[T](key: Any): T = properties(key).asInstanceOf[T]

  def elements: scala.collection.Seq[V]

  def apply(index: Z): V

  final def apply(index: Z8): V = apply(index.toZ)

  final def apply(index: Z16): V = apply(index.toZ)

  final def apply(index: Z32): V = apply(index.toZ)

  final def apply(index: Z64): V = apply(index.toZ)

  final def apply(index: N): V = apply(index.toZ)

  final def apply(index: N8): V = apply(index.toZ)

  final def apply(index: N16): V = apply(index.toZ)

  final def apply(index: N32): V = apply(index.toZ)

  final def apply(index: N64): V = apply(index.toZ)

  final def apply(index: S8): V = apply(index.toZ)

  final def apply(index: S16): V = apply(index.toZ)

  final def apply(index: S32): V = apply(index.toZ)

  final def apply(index: S64): V = apply(index.toZ)

  final def apply(index: U8): V = apply(index.toZ)

  final def apply(index: U16): V = apply(index.toZ)

  final def apply(index: U32): V = apply(index.toZ)

  final def apply(index: U64): V = apply(index.toZ)

  def size: Z

  def :+(value: V): _S[I, V]

  def +:(value: V): _S[I, V]

  def ++(values: _S[I, V]): _S[I, V]

  def apply[T <: _LogikaIntegralNumber](entries: (T, V)*): _S[I, V]
}

object _IS {
  def apply[I <: _LogikaIntegralNumber, V](values: V*): _IS[I, V] =
    new ISImpl[I, V](values.length, Vector[V](values: _*))

  def create[I <: _LogikaIntegralNumber, V](size: I, default: V): _IS[I, V] = {
    val sz = size.asInstanceOf[_LogikaIntegralNumber].toZ
    new ISImpl[I, V](sz.toZ32.value, Vector[V]((0 until sz.toZ32.value).map(_ => default): _*))
  }
}

sealed trait _IS[I, V] extends _S[I, V] {
  override def :+(value: V): IS[I, V]

  override def +:(value: V): IS[I, V]

  override def ++(values: _S[I, V]): IS[I, V]

  override def apply[T <: _LogikaIntegralNumber](entries: (T, V)*): IS[I, V]
}

private[logika] final class ISImpl[I, V](val sz: Int, val data: Vector[V]) extends _IS[I, V] {
  override val hashCode: Int = data.hashCode

  override val size: Z = _Z(sz)

  def elements: scala.collection.Seq[V] = data

  def apply(index: Z): V = {
    val i = index
    require(0 <= i && i < _Z(elements.length))
    data(i.toZ32.value)
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

  override def :+(value: V): _IS[I, V] = new ISImpl[I, V](sz + 1, data :+ value)

  override def +:(value: V): _IS[I, V] = new ISImpl[I, V](sz + 1, value +: data)

  override def ++(values: _S[I, V]): _IS[I, V] = values match {
    case (values: ISImpl[I, V]@unchecked) => new ISImpl[I, V](sz + values.sz, data ++ values.data)
    case (values: MSImpl[I, V]@unchecked) => new ISImpl[I, V](sz + values.sz, data ++ values.data)
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

  override def clone: IS[I, V] = this

  override def apply[T <: _LogikaIntegralNumber](entries: (T, V)*): IS[I, V] = {
    var entryMap: Map[Int, V] = Map()
    for ((i, v) <- entries) {
      entryMap += i.toZ32.value -> v
    }
    val newData = ArrayBuffer[V](data: _*)
    for (i <- elements.indices) {
      entryMap.get(i) match {
        case Some(v) => newData(i) = v
        case None => newData(i) = newData(i)
      }
    }
    new ISImpl[I, V](sz, newData.toVector)
  }
}

object _MS {
  def apply[I <: _LogikaIntegralNumber, V](values: V*): _MS[I, V] = {
    new MSImpl[I, V](values.length, ArrayBuffer[V](values: _*))
  }

  def create[I <: _LogikaIntegralNumber, V](size: I, default: V): _MS[I, V] = {
    val sz = size.asInstanceOf[_LogikaIntegralNumber].toZ
    new MSImpl[I, V](sz.toZ32.value, ArrayBuffer((0 until sz.toZ32.value).map(_ => default): _*))
  }
}

sealed trait _MS[I, V] extends _S[I, V] {
  override def :+(value: V): MS[I, V]

  override def +:(value: V): MS[I, V]

  override def ++(values: _S[I, V]): MS[I, V]

  override def apply[T <: _LogikaIntegralNumber](entries: (T, V)*): MS[I, V]

  def update(index: Z, value: V): Unit

  final def update(index: Z8, value: V): Unit = update(index.toZ, value)

  final def update(index: Z16, value: V): Unit = update(index.toZ, value)

  final def update(index: Z32, value: V): Unit = update(index.toZ, value)

  final def update(index: Z64, value: V): Unit = update(index.toZ, value)

  final def update(index: N, value: V): Unit = update(index.toZ, value)

  final def update(index: N8, value: V): Unit = update(index.toZ, value)

  final def update(index: N16, value: V): Unit = update(index.toZ, value)

  final def update(index: N32, value: V): Unit = update(index.toZ, value)

  final def update(index: N64, value: V): Unit = update(index.toZ, value)

  final def update(index: S8, value: V): Unit = update(index.toZ, value)

  final def update(index: S16, value: V): Unit = update(index.toZ, value)

  final def update(index: S32, value: V): Unit = update(index.toZ, value)

  final def update(index: S64, value: V): Unit = update(index.toZ, value)

  final def update(index: U8, value: V): Unit = update(index.toZ, value)

  final def update(index: U16, value: V): Unit = update(index.toZ, value)

  final def update(index: U32, value: V): Unit = update(index.toZ, value)

  final def update(index: U64, value: V): Unit = update(index.toZ, value)

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

private[logika] class MSImpl[I, V](val sz: Int, val data: ArrayBuffer[V]) extends _MS[I, V] {
  def size: Z = _Z(sz)

  def elements: scala.collection.Seq[V] = data

  def apply(index: Z): V = {
    val i = index
    require(0 <= i && i < _Z(elements.length))
    data(i.toZ32.value)
  }

  def update(index: Z, value: V): Unit = {
    val i = index
    require(0 <= i && i < _Z(elements.length))
    data(i.toZ32.value) = _clone(value)
  }

  override def hashCode: Int = data.hashCode

  override def :+(value: V): _MS[I, V] = new MSImpl[I, V](sz + 1, data :+ _clone(value))

  override def +:(value: V): _MS[I, V] = new MSImpl[I, V](sz + 1, _clone(value) +: data)

  override def ++(values: _S[I, V]): _MS[I, V] = values match {
    case (values: MSImpl[I, V]@unchecked) => new MSImpl[I, V](sz + values.sz, (data ++ values.data).map(_clone))
    case (values: ISImpl[I, V]@unchecked) => new MSImpl[I, V](sz + values.sz, (data ++ values.data).map(_clone))
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

  override def clone: _MS[I, V] = new MSImpl[I, V](sz, data.map(_clone))

  override def apply[T <: _LogikaIntegralNumber](entries: (T, V)*): MS[I, V] = {
    var entryMap: Map[Int, V] = Map()
    for ((i, v) <- entries) {
      entryMap += i.toZ32.value -> v
    }
    val newData: ArrayBuffer[V] = data.clone
    for (i <- elements.indices) {
      entryMap.get(i) match {
        case Some(v) => newData(i) = _clone(v)
        case None => newData(i) = _clone(newData(i))
      }
    }
    new MSImpl[I, V](sz, newData)
  }
}
