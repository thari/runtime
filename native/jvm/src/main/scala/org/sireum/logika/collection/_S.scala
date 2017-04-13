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

import org.sireum.logika._
import scala.collection.mutable.ArrayBuffer

object _S {

  import scala.reflect.runtime.universe._

  private[sireum] val bType = typeOf[Z].dealias.toString
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
  private[sireum] val f32Type = typeOf[F32].dealias.toString
  private[sireum] val f64Type = typeOf[F64].dealias.toString
  private[sireum] val rType = typeOf[R].dealias.toString

  private[sireum] def isLogikaNumber[T: TT]: Boolean = {
    scala.reflect.runtime.universe.typeOf[T].dealias.toString match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    }
  }

  private[sireum] def ln2int(value: Z): Int = Z_Ext.toZ32(value)

  private[sireum] def ln2int[T: TT](value: T): Int = {
    scala.reflect.runtime.universe.typeOf[T].dealias.toString match {
      case `zType` => Z_Ext.toZ32(value.asInstanceOf[Z])
      case `z8Type` => Z8_Ext.toZ32(value.asInstanceOf[Z8])
      case `z16Type` => Z16_Ext.toZ32(value.asInstanceOf[Z16])
      case `z32Type` => Z32_Ext.toZ32(value.asInstanceOf[Z32])
      case `z64Type` => Z64_Ext.toZ32(value.asInstanceOf[Z64])
      case `nType` => N_Ext.toZ32(value.asInstanceOf[N])
      case `n8Type` => N8_Ext.toZ32(value.asInstanceOf[N8])
      case `n16Type` => N16_Ext.toZ32(value.asInstanceOf[N16])
      case `n32Type` => N32_Ext.toZ32(value.asInstanceOf[N32])
      case `n64Type` => N64_Ext.toZ32(value.asInstanceOf[N64])
      case `s8Type` => S8_Ext.toZ32(value.asInstanceOf[S8])
      case `s16Type` => S16_Ext.toZ32(value.asInstanceOf[S16])
      case `s32Type` => S32_Ext.toZ32(value.asInstanceOf[S32])
      case `s64Type` => S64_Ext.toZ32(value.asInstanceOf[S64])
      case `u8Type` => U8_Ext.toZ32(value.asInstanceOf[U8])
      case `u16Type` => U16_Ext.toZ32(value.asInstanceOf[U16])
      case `u32Type` => U32_Ext.toZ32(value.asInstanceOf[U32])
      case `u64Type` => U64_Ext.toZ32(value.asInstanceOf[U64])
      case _ => value match {
        case value: Int => value
        case value: Long => Z64_Ext.toZ32(value.asInstanceOf[Z64])
      }
    }
  }
}

object _IS {

  import _S._

  import scala.language.experimental.macros

  def apply[I: TT, V](elements: V*): IS[I, V] = {
    require(isLogikaNumber[I])
    new _IS[I, V](Vector(implicitly[TT[I]]) ++ elements)
  }

  def create[I: TT, V](size: I, default: V): IS[I, V] = {
    require(isLogikaNumber[I])
    val sz = ln2int(size)
    new _IS[I, V](Vector(implicitly[TT[I]]) ++ (0 until sz).map(_ => default))
  }
}

final class _IS[I, V](val value: Vector[Any]) extends AnyVal {
  def size: Z = value.size - 1

  def elements: scala.collection.Seq[V] = value.tail.asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && index < size)
    value(index + 1).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(0 <= index && index < size)
    value(_S.ln2int(index) + 1).asInstanceOf[V]
  }

  def apply(index: Z8): V = apply(Z8_Ext.toZ(index))

  def apply(index: Z16): V = apply(Z16_Ext.toZ(index))

  def apply(index: Z64): V = apply(Z64_Ext.toZ(index))

  def :+(value: V): IS[I, V] = new _IS[I, V](this.value :+ value)

  def +:(value: V): IS[I, V] = new _IS[I, V](this.value.head +: value +: this.value.tail)

  def ++(other: IS[I, V]): IS[I, V] = new _IS[I, V](value ++ other.elements)

  def ++(other: MS[I, V]): IS[I, V] = new _IS[I, V](value ++ other.elements)

  def apply[T: TT](entries: (T, V)*): IS[I, V] = {
    var newValue = value
    val sz = value.size - 1
    for ((i, v) <- entries) {
      val index = _S.ln2int(i)
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 1, v)
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
      case `zType` => (0 until sz).map(Z32_Ext.toZ)
      case `z8Type` => (0 until sz).map(Z32_Ext.toZ8)
      case `z16Type` => (0 until sz).map(Z32_Ext.toZ16)
      case `z32Type` => 0 until sz
      case `z64Type` => (0 until sz).map(Z32_Ext.toZ64)
      case `nType` => (0 until sz).map(Z32_Ext.toN)
      case `n8Type` => (0 until sz).map(Z32_Ext.toN8)
      case `n16Type` => (0 until sz).map(Z32_Ext.toN16)
      case `n32Type` => (0 until sz).map(Z32_Ext.toN32)
      case `n64Type` => (0 until sz).map(Z32_Ext.toN64)
      case `s8Type` => (0 until sz).map(Z32_Ext.toS8)
      case `s16Type` => (0 until sz).map(Z32_Ext.toS16)
      case `s32Type` => (0 until sz).map(Z32_Ext.toS32)
      case `s64Type` => (0 until sz).map(Z32_Ext.toS64)
      case `u8Type` => (0 until sz).map(Z32_Ext.toU8)
      case `u16Type` => (0 until sz).map(Z32_Ext.toU16)
      case `u32Type` => (0 until sz).map(Z32_Ext.toU32)
      case `u64Type` => (0 until sz).map(Z32_Ext.toU64)
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
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]]) ++ elements.map(_clone))
  }

  def create[I: TT, V](size: I, default: V): MS[I, V] = {
    require(isLogikaNumber[I])
    val sz = ln2int(size)
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]]) ++ (0 until sz).map(_ => _clone(default)))
  }
}

final class _MS[I, V](val value: ArrayBuffer[Any]) extends AnyVal {
  def size: Z = value.size - 1

  def elements: scala.collection.Seq[V] = value.tail.asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && index < size)
    value(index + 1).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(0 <= index && index < size)
    value(_S.ln2int(index) + 1).asInstanceOf[V]
  }

  def update(index: Int, value: V): Unit = {
    require(0 <= index && index < size)
    this.value(index + 1) = value
  }

  def update(index: Z, value: V): Unit = {
    require(0 <= index && index < size)
    this.value(_S.ln2int(index) + 1) = value
  }

  def update(index: Z8, value: V): Unit = update(Z8_Ext.toZ(index), value)

  def update(index: Z16, value: V): Unit = update(Z16_Ext.toZ(index), value)

  def update(index: Z64, value: V): Unit = update(Z64_Ext.toZ(index), value)

  def apply(index: Z8): V = apply(Z8_Ext.toZ(index))

  def apply(index: Z16): V = apply(Z16_Ext.toZ(index))

  def apply(index: Z64): V = apply(Z64_Ext.toZ(index))

  def :+(value: V): MS[I, V] = new _MS[I, V](this.value :+ value)

  def +:(value: V): MS[I, V] = new _MS[I, V](this.value.head +: value +: this.value.tail)

  def ++(other: MS[I, V]): MS[I, V] = new _MS[I, V](value ++ other.elements)

  def ++(other: IS[I, V]): MS[I, V] = new _MS[I, V](value ++ other.elements)

  def apply[T: TT](entries: (T, V)*): MS[I, V] = {
    var newValue = value
    val sz = value.size - 1
    for ((i, v) <- entries) {
      val index = _S.ln2int(i)
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 1, v)
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
      case `zType` => (0 until sz).map(Z32_Ext.toZ)
      case `z8Type` => (0 until sz).map(Z32_Ext.toZ8)
      case `z16Type` => (0 until sz).map(Z32_Ext.toZ16)
      case `z32Type` => 0 until sz
      case `z64Type` => (0 until sz).map(Z32_Ext.toZ64)
      case `nType` => (0 until sz).map(Z32_Ext.toN)
      case `n8Type` => (0 until sz).map(Z32_Ext.toN8)
      case `n16Type` => (0 until sz).map(Z32_Ext.toN16)
      case `n32Type` => (0 until sz).map(Z32_Ext.toN32)
      case `n64Type` => (0 until sz).map(Z32_Ext.toN64)
      case `s8Type` => (0 until sz).map(Z32_Ext.toS8)
      case `s16Type` => (0 until sz).map(Z32_Ext.toS16)
      case `s32Type` => (0 until sz).map(Z32_Ext.toS32)
      case `s64Type` => (0 until sz).map(Z32_Ext.toS64)
      case `u8Type` => (0 until sz).map(Z32_Ext.toU8)
      case `u16Type` => (0 until sz).map(Z32_Ext.toU16)
      case `u32Type` => (0 until sz).map(Z32_Ext.toU32)
      case `u64Type` => (0 until sz).map(Z32_Ext.toU64)
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
