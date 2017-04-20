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

package org.sireum.collection

import scala.collection.mutable.ArrayBuffer

import org.sireum._
import org.sireum._Type._
import org.sireum.math._Z

object _IS {

  import scala.language.experimental.macros

  def apply[I: TT, V](elements: V*): IS[I, V] = {
    require(isSlangNumber[I])
    new _IS[I, V](Vector(implicitly[TT[I]]) ++ elements)
  }

  def create[I: TT, V](size: I, default: V): IS[I, V] = {
    require(isSlangNumber[I])
    val sz = ln2int(size)
    new _IS[I, V](Vector(implicitly[TT[I]]) ++ (0 until sz).map(_ => default))
  }
}

final class _IS[I, V](val value: Vector[Any]) extends AnyVal {

  def ===(other: IS[I, V]): B = this == other

  def =!=(other: IS[I, V]): B = this != other

  def size: Z = _Z(value.size - 1)

  def elements: scala.collection.Seq[V] = value.tail.asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && _Z(index) < size)
    value(index + 1).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(_Z.zero <= index && index < size)
    value(ln2int(index) + 1).asInstanceOf[V]
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
      val index = ln2int(i)
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 1, v)
    }
    new _IS[I, V](newValue)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def indices: Traversable[I] = {
    import scala.reflect.runtime.universe._
    import _Type._
    val sz = value.size
    implicit val iTag: TT[I] = value(0).asInstanceOf[TT[I]]
    (typeOf[I].dealias.toString match {
      case `zType` => (0 until sz).map(n => Z32_Ext.toZ(math.Numbers.toZ32(n)))
      case `z8Type` => (0 until sz).map(n => Z32_Ext.toZ8(math.Numbers.toZ32(n)))
      case `z16Type` => (0 until sz).map(n => Z32_Ext.toZ16(math.Numbers.toZ32(n)))
      case `z32Type` => (0 until sz).map(x => math.Numbers.toZ32(x))
      case `z64Type` => (0 until sz).map(n => Z32_Ext.toZ64(math.Numbers.toZ32(n)))
      case `nType` => (0 until sz).map(n => Z32_Ext.toN(math.Numbers.toZ32(n)))
      case `n8Type` => (0 until sz).map(n => Z32_Ext.toN8(math.Numbers.toZ32(n)))
      case `n16Type` => (0 until sz).map(n => Z32_Ext.toN16(math.Numbers.toZ32(n)))
      case `n32Type` => (0 until sz).map(n => Z32_Ext.toN32(math.Numbers.toZ32(n)))
      case `n64Type` => (0 until sz).map(n => Z32_Ext.toN64(math.Numbers.toZ32(n)))
      case `s8Type` => (0 until sz).map(n => Z32_Ext.toS8(math.Numbers.toZ32(n)))
      case `s16Type` => (0 until sz).map(n => Z32_Ext.toS16(math.Numbers.toZ32(n)))
      case `s32Type` => (0 until sz).map(n => Z32_Ext.toS32(math.Numbers.toZ32(n)))
      case `s64Type` => (0 until sz).map(n => Z32_Ext.toS64(math.Numbers.toZ32(n)))
      case `u8Type` => (0 until sz).map(n => Z32_Ext.toU8(math.Numbers.toZ32(n)))
      case `u16Type` => (0 until sz).map(n => Z32_Ext.toU16(math.Numbers.toZ32(n)))
      case `u32Type` => (0 until sz).map(n => Z32_Ext.toU32(math.Numbers.toZ32(n)))
      case `u64Type` => (0 until sz).map(n => Z32_Ext.toU64(math.Numbers.toZ32(n)))
    }).asInstanceOf[Traversable[I]]
  }

  def clone: IS[I, V] = this

  override def toString: String = {
    val elements = this.elements

    def toBit(i: Int): Char = if (elements(i).asInstanceOf[B]) '1' else '0'

    val sb = new StringBuilder
    sb.append('[')
    if (elements.nonEmpty) {
      val isBoolean = elements.head.isInstanceOf[B]
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

  def apply[I: TT, V](elements: V*): MS[I, V] = {
    require(isSlangNumber[I])
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]]) ++ elements.map(_Clonable.clone))
  }

  def create[I: TT, V](size: I, default: V): MS[I, V] = {
    require(isSlangNumber[I])
    val sz = ln2int(size)
    new _MS[I, V](ArrayBuffer(implicitly[TT[I]]) ++ (0 until sz).map(_ => _Clonable.clone(default)))
  }
}

final class _MS[I, V](val value: ArrayBuffer[Any]) extends AnyVal {

  def ===(other: MS[I, V]): B = this == other

  def =!=(other: MS[I, V]): B = this != other

  def size: Z = _Z(value.size - 1)

  def elements: scala.collection.Seq[V] = value.tail.asInstanceOf[scala.collection.Seq[V]]

  def apply(index: Int): V = {
    require(0 <= index && _Z(index) < size)
    value(index + 1).asInstanceOf[V]
  }

  def apply(index: Z): V = {
    require(_Z.zero <= index && index < size)
    value(ln2int(index) + 1).asInstanceOf[V]
  }

  def update(index: Int, value: V): Unit = {
    require(0 <= index && _Z(index) < size)
    this.value(index + 1) = value
  }

  def update(index: Z, value: V): Unit = {
    require(_Z.zero <= index && index < size)
    this.value(ln2int(index) + 1) = value
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
      val index = ln2int(i)
      require(0 <= index && index < sz)
      newValue = newValue.updated(index + 1, v)
    }
    new _MS[I, V](newValue)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def indices: Traversable[I] = {
    import scala.reflect.runtime.universe._
    import _Type._
    val sz = value.size
    implicit val iTag: TT[I] = value(0).asInstanceOf[TT[I]]
    (typeOf[I].dealias.toString match {
      case `zType` => (0 until sz).map(n => Z32_Ext.toZ(math.Numbers.toZ32(n)))
      case `z8Type` => (0 until sz).map(n => Z32_Ext.toZ8(math.Numbers.toZ32(n)))
      case `z16Type` => (0 until sz).map(n => Z32_Ext.toZ16(math.Numbers.toZ32(n)))
      case `z32Type` => (0 until sz).map(x => math.Numbers.toZ32(x))
      case `z64Type` => (0 until sz).map(n => Z32_Ext.toZ64(math.Numbers.toZ32(n)))
      case `nType` => (0 until sz).map(n => Z32_Ext.toN(math.Numbers.toZ32(n)))
      case `n8Type` => (0 until sz).map(n => Z32_Ext.toN8(math.Numbers.toZ32(n)))
      case `n16Type` => (0 until sz).map(n => Z32_Ext.toN16(math.Numbers.toZ32(n)))
      case `n32Type` => (0 until sz).map(n => Z32_Ext.toN32(math.Numbers.toZ32(n)))
      case `n64Type` => (0 until sz).map(n => Z32_Ext.toN64(math.Numbers.toZ32(n)))
      case `s8Type` => (0 until sz).map(n => Z32_Ext.toS8(math.Numbers.toZ32(n)))
      case `s16Type` => (0 until sz).map(n => Z32_Ext.toS16(math.Numbers.toZ32(n)))
      case `s32Type` => (0 until sz).map(n => Z32_Ext.toS32(math.Numbers.toZ32(n)))
      case `s64Type` => (0 until sz).map(n => Z32_Ext.toS64(math.Numbers.toZ32(n)))
      case `u8Type` => (0 until sz).map(n => Z32_Ext.toU8(math.Numbers.toZ32(n)))
      case `u16Type` => (0 until sz).map(n => Z32_Ext.toU16(math.Numbers.toZ32(n)))
      case `u32Type` => (0 until sz).map(n => Z32_Ext.toU32(math.Numbers.toZ32(n)))
      case `u64Type` => (0 until sz).map(n => Z32_Ext.toU64(math.Numbers.toZ32(n)))
    }).asInstanceOf[Traversable[I]]
  }

  def clone: MS[I, V] = new _MS[I, V](ArrayBuffer(value.map(_Clonable.clone)))

  override def toString: String = {
    val elements = this.elements

    def toBit(i: Int): Char = if (elements(i).asInstanceOf[B]) '1' else '0'

    val sb = new StringBuilder
    sb.append('[')
    if (elements.nonEmpty) {
      val isBoolean = elements.head.isInstanceOf[B]
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