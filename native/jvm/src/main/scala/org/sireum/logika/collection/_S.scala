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

import org.sireum.logika.{_Clonable, _Immutable, TT, Z, Z8, Z16, Z32, Z64, N, N8, N16, N32, N64, S8, S16, S32, S64, U8, U16, U32, U64, IS, MS, _clone}
import org.sireum.logika.math._
import scala.collection.mutable.ArrayBuffer

object _S {

  import scala.reflect.runtime.universe._

  private[sireum] val zType = typeOf[Z].dealias.toString
  private[sireum] val z8Type = typeOf[Z8].dealias.toString
  private[sireum] val z16Type = typeOf[Z16].dealias.toString
  private[sireum] val z32Type = typeOf[Z32].dealias.toString
  private[sireum] val z64Type = typeOf[Z64].dealias.toString
  private[sireum] val nType = typeOf[N].dealias.toString
  private[sireum] val n8Type = typeOf[N8].dealias.toString
  private[sireum] val n16Type = typeOf[N16].dealias.toString
  private[sireum] val n32Type = typeOf[N32].dealias.toString
  private[sireum] val n64Type = typeOf[N64].dealias.toString
  private[sireum] val s8Type = typeOf[S8].dealias.toString
  private[sireum] val s16Type = typeOf[S16].dealias.toString
  private[sireum] val s32Type = typeOf[S32].dealias.toString
  private[sireum] val s64Type = typeOf[S64].dealias.toString
  private[sireum] val u8Type = typeOf[U8].dealias.toString
  private[sireum] val u16Type = typeOf[U16].dealias.toString
  private[sireum] val u32Type = typeOf[U32].dealias.toString
  private[sireum] val u64Type = typeOf[U64].dealias.toString

  private[sireum] def isLogikaNumber[T: TT]: Boolean = {
    scala.reflect.runtime.universe.typeOf[T].dealias.toString match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case x =>
        false
    }
  }

  private[sireum] def ln2int(value: Z): Int = value.toZ32.toInt

  private[sireum] def ln2int[T: TT](value: T): Int = {
    scala.reflect.runtime.universe.typeOf[T].dealias.toString match {
      case `zType` => value.asInstanceOf[Z].toZ32.toInt
      case `z8Type` => value.asInstanceOf[Z8].toZ32.toInt
      case `z16Type` => value.asInstanceOf[Z16].toZ32.toInt
      case `z32Type` => value.asInstanceOf[Z32].toInt
      case `z64Type` => value.asInstanceOf[Z64].toZ32.toInt
      case `nType` => value.asInstanceOf[N].toZ32.toInt
      case `n8Type` => value.asInstanceOf[N8].toZ32.toInt
      case `n16Type` => value.asInstanceOf[N16].toZ32.toInt
      case `n32Type` => value.asInstanceOf[N32].toZ32.toInt
      case `n64Type` => value.asInstanceOf[N64].toZ32.toInt
      case `s8Type` => value.asInstanceOf[S8].toZ32.toInt
      case `s16Type` => value.asInstanceOf[S16].toZ32.toInt
      case `s32Type` => value.asInstanceOf[S32].toZ32.toInt
      case `s64Type` => value.asInstanceOf[S64].toZ32.toInt
      case `u8Type` => value.asInstanceOf[U8].toZ32.toInt
      case `u16Type` => value.asInstanceOf[U16].toZ32.toInt
      case `u32Type` => value.asInstanceOf[U32].toZ32.toInt
      case `u64Type` => value.asInstanceOf[U64].toZ32.toInt
    }
  }
}

object _IS {

  import _S._

  def apply[I: TT, V](elements: V*): IS[I, V] = {
    require(isLogikaNumber[I])
    new _IS[I, V](Vector(implicitly[TT[I]], null) ++ elements)
  }

  def create[I: TT, V](size: I, default: V): IS[I, V] = {
    require(isLogikaNumber[I])
    val sz = ln2int(size)
    new _IS[I, V](Vector(implicitly[TT[I]], null) ++ (0 until sz).map(_ => default))
  }
}

final class _IS[I, V](val value: Vector[Any]) extends AnyVal {
  private[sireum] def data_=(o: Any): IS[I, V] = {
    new _IS(value.updated(1, o))
    this
  }

  private[sireum] def data[T]: T = value(1).asInstanceOf[T]

  def size: Z = value.size - 2

  def elements: scala.collection.Seq[V] = value.drop(2).asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && index < size)
    value(index + 2).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(0 <= index && index < size)
    value(index.toInt + 2).asInstanceOf[V]
  }

  def apply(index: Z8): V = apply(index.toZ)

  def apply(index: Z16): V = apply(index.toZ)

  def apply(index: Z32): V = apply(index.toZ)

  def apply(index: Z64): V = apply(index.toZ)

  def apply(index: N): V = apply(index.toZ)

  def apply(index: N8): V = apply(index.toZ)

  def apply(index: N16): V = apply(index.toZ)

  def apply(index: N32): V = apply(index.toZ)

  def apply(index: N64): V = apply(index.toZ)

  def apply(index: S8): V = apply(index.toZ)

  def apply(index: S16): V = apply(index.toZ)

  def apply(index: S32): V = apply(index.toZ)

  def apply(index: S64): V = apply(index.toZ)

  def apply(index: U8): V = apply(index.toZ)

  def apply(index: U16): V = apply(index.toZ)

  def apply(index: U32): V = apply(index.toZ)

  def apply(index: U64): V = apply(index.toZ)

  def :+(value: V): IS[I, V] = new _IS[I, V](this.value :+ value)

  def +:(value: V): IS[I, V] = new _IS[I, V]((this.value.take(2) :+ value) ++ this.value.drop(2))

  def ++(other: IS[I, V]): IS[I, V] = new _IS[I, V](value ++ other.elements)

  def ++(other: MS[I, V]): IS[I, V] = new _IS[I, V](value ++ other.elements)

  def apply[T](entries: (T, V)*): IS[I, V] = {
    var newValue = value
    val sz = value.size - 1
    for ((i, v) <- entries) {
      val index = i.asInstanceOf[_LogikaIntegralNumber].toZ32.value
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 2, v)
    }
    new _IS[I, V](newValue)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def indices: Traversable[I] = {
    import scala.reflect.runtime.universe._
    import _S._
    val sz = value.size
    implicit val iTag: TT[I] = value(0).asInstanceOf[TT[I]]
    (typeOf[I].dealias.toString match {
      case `zType` => (0 until sz).map(i => _Z(i))
      case `z8Type` => (0 until sz).map(i => _Z(i).toZ8)
      case `z16Type` => (0 until sz).map(i => _Z(i).toZ16)
      case `z32Type` => (0 until sz).map(i => _Z(i).toZ32)
      case `z64Type` => (0 until sz).map(i => _Z(i).toZ64)
      case `nType` => (0 until sz).map(i => _N(i))
      case `n8Type` => (0 until sz).map(i => _Z(i).toN8)
      case `n16Type` => (0 until sz).map(i => _Z(i).toN16)
      case `n32Type` => (0 until sz).map(i => _Z(i).toN32)
      case `n64Type` => (0 until sz).map(i => _Z(i).toN64)
      case `s8Type` => (0 until sz).map(i => _Z(i).toS8)
      case `s16Type` => (0 until sz).map(i => _Z(i).toS16)
      case `s32Type` => (0 until sz).map(i => _Z(i).toS32)
      case `s64Type` => (0 until sz).map(i => _Z(i).toS64)
      case `u8Type` => (0 until sz).map(i => _Z(i).toU8)
      case `u16Type` => (0 until sz).map(i => _Z(i).toU16)
      case `u32Type` => (0 until sz).map(i => _Z(i).toU32)
      case `u64Type` => (0 until sz).map(i => _Z(i).toU64)
    }).asInstanceOf[Traversable[I]]
  }

  def clone: IS[I, V] = this

  override def toString: String = {
    val elements = this.elements

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


object _MS {

  import _S._

  def apply[I: TT, V](elements: V*): MS[I, V] = {
    require(isLogikaNumber[I])
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]], null) ++ elements.map(_clone))
  }

  def create[I: TT, V](size: I, default: V): MS[I, V] = {
    require(isLogikaNumber[I])
    val sz = ln2int(size)
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]], null) ++ (0 until sz).map(_ => _clone(default)))
  }
}

final class _MS[I, V](val value: ArrayBuffer[Any]) extends AnyVal {
  private[sireum] def data_=(o: Any): MS[I, V] = {
    value(1) = o
    this
  }

  private[sireum] def data[T]: T = value(1).asInstanceOf[T]

  def size: Z = value.size - 2

  def elements: scala.collection.Seq[V] = value.drop(2).asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && index < size)
    value(index + 2).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(0 <= index && index < size)
    value(index.toInt + 2).asInstanceOf[V]
  }

  def update(index: Int, value: V): Unit = {
    require(0 <= index && index < size)
    this.value(index + 2) = value
  }

  def update(index: Z, value: V): Unit = {
    require(0 <= index && index < size)
    this.value(index.toInt + 2) = value
  }

  def update(index: Z8, value: V): Unit = update(index.toZ, value)

  def update(index: Z16, value: V): Unit = update(index.toZ, value)

  def update(index: Z32, value: V): Unit = update(index.toZ, value)

  def update(index: Z64, value: V): Unit = update(index.toZ, value)

  def update(index: N, value: V): Unit = update(index.toZ, value)

  def update(index: N8, value: V): Unit = update(index.toZ, value)

  def update(index: N16, value: V): Unit = update(index.toZ, value)

  def update(index: N32, value: V): Unit = update(index.toZ, value)

  def update(index: N64, value: V): Unit = update(index.toZ, value)

  def update(index: S8, value: V): Unit = update(index.toZ, value)

  def update(index: S16, value: V): Unit = update(index.toZ, value)

  def update(index: S32, value: V): Unit = update(index.toZ, value)

  def update(index: S64, value: V): Unit = update(index.toZ, value)

  def update(index: U8, value: V): Unit = update(index.toZ, value)

  def update(index: U16, value: V): Unit = update(index.toZ, value)

  def update(index: U32, value: V): Unit = update(index.toZ, value)

  def update(index: U64, value: V): Unit = update(index.toZ, value)

  def apply(index: Z8): V = apply(index.toZ)

  def apply(index: Z16): V = apply(index.toZ)

  def apply(index: Z32): V = apply(index.toZ)

  def apply(index: Z64): V = apply(index.toZ)

  def apply(index: N): V = apply(index.toZ)

  def apply(index: N8): V = apply(index.toZ)

  def apply(index: N16): V = apply(index.toZ)

  def apply(index: N32): V = apply(index.toZ)

  def apply(index: N64): V = apply(index.toZ)

  def apply(index: S8): V = apply(index.toZ)

  def apply(index: S16): V = apply(index.toZ)

  def apply(index: S32): V = apply(index.toZ)

  def apply(index: S64): V = apply(index.toZ)

  def apply(index: U8): V = apply(index.toZ)

  def apply(index: U16): V = apply(index.toZ)

  def apply(index: U32): V = apply(index.toZ)

  def apply(index: U64): V = apply(index.toZ)

  def :+(value: V): MS[I, V] = new _MS[I, V](this.value :+ value)

  def +:(value: V): MS[I, V] = new _MS[I, V]((this.value.take(2) :+ value) ++ this.value.drop(2))

  def ++(other: MS[I, V]): MS[I, V] = new _MS[I, V](value ++ other.elements)

  def ++(other: IS[I, V]): MS[I, V] = new _MS[I, V](value ++ other.elements)

  def apply[T](entries: (T, V)*): MS[I, V] = {
    var newValue = value
    val sz = value.size - 1
    for ((i, v) <- entries) {
      val index = i.asInstanceOf[_LogikaIntegralNumber].toZ32.value
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 2, v)
    }
    new _MS[I, V](newValue)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def indices: Traversable[I] = {
    import scala.reflect.runtime.universe._
    import _S._
    val sz = value.size
    implicit val iTag: TT[I] = value(0).asInstanceOf[TT[I]]
    (typeOf[I].dealias.toString match {
      case `zType` => (0 until sz).map(i => _Z(i))
      case `z8Type` => (0 until sz).map(i => _Z(i).toZ8)
      case `z16Type` => (0 until sz).map(i => _Z(i).toZ16)
      case `z32Type` => (0 until sz).map(i => _Z(i).toZ32)
      case `z64Type` => (0 until sz).map(i => _Z(i).toZ64)
      case `nType` => (0 until sz).map(i => _N(i))
      case `n8Type` => (0 until sz).map(i => _Z(i).toN8)
      case `n16Type` => (0 until sz).map(i => _Z(i).toN16)
      case `n32Type` => (0 until sz).map(i => _Z(i).toN32)
      case `n64Type` => (0 until sz).map(i => _Z(i).toN64)
      case `s8Type` => (0 until sz).map(i => _Z(i).toS8)
      case `s16Type` => (0 until sz).map(i => _Z(i).toS16)
      case `s32Type` => (0 until sz).map(i => _Z(i).toS32)
      case `s64Type` => (0 until sz).map(i => _Z(i).toS64)
      case `u8Type` => (0 until sz).map(i => _Z(i).toU8)
      case `u16Type` => (0 until sz).map(i => _Z(i).toU16)
      case `u32Type` => (0 until sz).map(i => _Z(i).toU32)
      case `u64Type` => (0 until sz).map(i => _Z(i).toU64)
    }).asInstanceOf[Traversable[I]]
  }

  def clone: MS[I, V] = new _MS[I, V](ArrayBuffer(value.map(_clone)))

  override def toString: String = {
    val elements = this.elements

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
