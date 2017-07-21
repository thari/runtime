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

import org.sireum._
import org.sireum._Type._
import org.sireum._Type.Alias.TT
import org.sireum.math._Z

object _S {
  private[collection] def computeCapacity(array: Array[Any], len: Int): Int =
    if (array.length < len) (len * 1.5).toInt else array.length

  private[collection] def cloneValue(array: Array[Any], length: Int, newLength: Int, offset: Int): Array[Any] = {
    val capacity = computeCapacity(array, newLength)
    val r = new Array[Any](capacity)
    System.arraycopy(array, 0, r, offset, length)
    r
  }
}

object _IS {

  import scala.language.experimental.macros

  def apply[I: TT, V](elements: V*): IS[I, V] = {
    require(isSlangNumber[I])
    new _IS[I, V](implicitly[TT[I]], elements.length, elements.toArray)
  }

  def create[I: TT, V](size: I, default: V): IS[I, V] = {
    require(isSlangNumber[I])
    val sz = ln2int(size)
    new _IS[I, V](implicitly[TT[I]], sz, (0 until sz).map(_ => _Clonable.clone(default)).toArray)
  }
}

final class _IS[I, V](val iTag: TT[I],
                      private[collection] val length: Int,
                      private[collection] val array: Array[Any]) extends _Immutable {

  override lazy val hashCode: Int = elements.hashCode

  override def equals(o: Any): Boolean =
    if (this eq o.asInstanceOf[AnyRef]) true
    else o match {
      case o: _IS[_, _] => elements == o.elements
      case _ => false
    }

  def hash: Z = hashCode

  def isEqual(other: IS[I, V]): B = this == other

  def size: Z = _Z(length)

  def nonEmpty: B = length > 0

  def isEmpty: B = length == 0

  def elements: scala.collection.Seq[V] = array.asInstanceOf[Array[V]].toSeq.slice(0, length)

  def reverse: scala.collection.Seq[V] = elements.reverse

  def apply[T: TT](index: T): V = {
    val i = ln2int(index)
    require(0 <= i && i < length)
    array(i).asInstanceOf[V]
  }

  def :+(value: V): IS[I, V] = {
    implicit val it = iTag
    val len = length + 1
    val newArray = _S.cloneValue(array, length, len, 0)
    newArray(length) = value
    new _IS[I, V](iTag, len, newArray)
  }

  def +:(value: V): IS[I, V] = {
    val len = length + 1
    val newArray = _S.cloneValue(array, length, len, 1)
    newArray(0) = value
    new _IS[I, V](iTag, len, newArray)
  }

  def ++(other: IS[I, V]): IS[I, V] = {
    val len = length + other.length
    val newArray = _S.cloneValue(array, length, len, 0)
    System.arraycopy(other.array, 0, newArray, length, other.length)
    new _IS[I, V](iTag, len, newArray)
  }

  def ++(other: MS[I, V]): IS[I, V] = {
    val len = length + other.length
    val newArray = _S.cloneValue(array, length, len, 0)
    for (i <- 0 until other.length) {
      newArray(length + i) = _Clonable.clone(other.array(i))
    }
    new _IS[I, V](iTag, len, newArray)
  }

  def -(value: V): IS[I, V] = {
    val newArray = array.filterNot(v => v == null || v == value)
    new _IS[I, V](iTag, newArray.length, newArray)
  }

  def --(other: IS[I, V]): IS[I, V] = {
    val newArray = array.filterNot(v => v == null || other.elements.contains(v))
    new _IS[I, V](iTag, newArray.length, newArray)
  }

  def --(other: MS[I, V]): IS[I, V] = {
    val newArray = array.filterNot(v => v == null || other.elements.contains(v))
    new _IS[I, V](iTag, newArray.length, newArray)
  }

  def apply[T: TT](entries: (T, V)*): IS[I, V] = {
    val sz = length
    val newArray = _S.cloneValue(array, sz, sz, 0)
    for ((i, v) <- entries) {
      val index = ln2int(i)
      require(0 <= index && index < sz)
      newArray(index) = v
    }
    new _IS[I, V](iTag, length, newArray)
  }

  def withFilter(p: V => Boolean): IS[I, V] = {
    val newArray = elements.filter(p).toArray[Any]
    new _IS(iTag, newArray.length, newArray)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def map[V2](f: V => V2): IS[I, V2] = new _IS[I, V2](iTag, length, (for (e <- elements) yield f(e)).toArray)

  def flatMap[V2](f: V => IS[I, V2]): IS[I, V2] = {
    val newArray = elements.flatMap(e => f(e).elements).toArray[Any]
    new _IS[I, V2](iTag, newArray.length, newArray)
  }

  def indices: scala.collection.Seq[I] = {
    import _Type._
    val sz = length
    (iTag.runtimeClass.toString match {
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
    }).asInstanceOf[scala.collection.Seq[I]]
  }

  override def clone: IS[I, V] = this

  override def toString: Predef.String = {
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
    new _MS[I, V](implicitly[TT[I]], elements.length, elements.toArray)
  }

  def create[I: TT, V](size: I, default: V): MS[I, V] = {
    require(isSlangNumber[I])
    val sz = ln2int(size)
    new _MS[I, V](implicitly[TT[I]], sz, (0 until sz).map(_ => _Clonable.clone(default)).toArray)
  }
}

final class _MS[I, V](val iTag: TT[I],
                      private[collection] val length: Int,
                      private[collection] val array: Array[Any]) extends _Mutable {

  override def hashCode: Int = elements.hashCode

  override def equals(o: Any): Boolean =
    if (this eq o.asInstanceOf[AnyRef]) true
    else o match {
      case o: _MS[_, _] => elements == o.elements
      case _ => false
    }

  def hash: Z = hashCode

  def isEqual(other: MS[I, V]): B = this == other

  def size: Z = _Z(length)

  def nonEmpty: B = length > 0

  def isEmpty: B = length == 0

  def elements: scala.collection.Seq[V] = array.asInstanceOf[Array[V]].toSeq.slice(0, length)

  def reverse: scala.collection.Seq[V] = elements.reverse

  def apply[T: TT](index: T): V = {
    val i = ln2int(index)
    require(0 <= i && i < length)
    array(i).asInstanceOf[V]
  }

  def update[T: TT](index: T, value: V): Unit = {
    val i = ln2int(index)
    require(0 <= i && i < length)
    array(i) = _Clonable.clone(value)
  }

  def :+(value: V): MS[I, V] = {
    val len = length + 1
    val newArray = _S.cloneValue(array, length, len, 0)
    newArray(length) = _Clonable.clone(value)
    new _MS[I, V](iTag, len, newArray)
  }

  def +:(value: V): MS[I, V] = {
    val len = length + 1
    val newArray = _S.cloneValue(array, length, len, 1)
    newArray(0) = _Clonable.clone(value)
    new _MS[I, V](iTag, len, newArray)
  }

  def ++(other: MS[I, V]): MS[I, V] = {
    val len = length + other.length
    val newArray = _S.cloneValue(array, length, len, 0)
    for (i <- 0 until other.length) {
      newArray(length + i) = _Clonable.clone(other.array(i))
    }
    new _MS[I, V](iTag, len, newArray)
  }

  def ++(other: IS[I, V]): MS[I, V] = {
    val len = length + other.length
    val newArray = _S.cloneValue(array, length, len, 0)
    System.arraycopy(other.array, 0, newArray, length, other.length)
    new _MS[I, V](iTag, len, newArray)
  }

  def -(value: V): MS[I, V] = {
    val newArray = array.filterNot(v => v == null || v == value)
    new _MS[I, V](iTag, newArray.length, newArray)
  }

  def --(other: MS[I, V]): MS[I, V] = {
    val newArray = array.filterNot(v => v == null || other.elements.contains(v))
    new _MS[I, V](iTag, newArray.length, newArray)
  }

  def --(other: IS[I, V]): MS[I, V] = {
    val newArray = array.filterNot(v => v == null || other.elements.contains(v))
    new _MS[I, V](iTag, newArray.length, newArray)
  }

  def apply[T: TT](entries: (T, V)*): MS[I, V] = {
    val sz = length
    val newArray = _S.cloneValue(array, sz, sz, 0)
    for ((i, v) <- entries) {
      val index = ln2int(i)
      require(0 <= index && index < sz)
      newArray(index) = _Clonable.clone(v)
    }
    new _MS[I, V](iTag, sz, newArray)
  }

  def withFilter(p: V => Boolean): MS[I, V] = {
    val newArray = elements.filter(p).toArray[Any]
    new _MS(iTag, newArray.length, newArray)
  }

  def foreach(f: V => Unit): Unit = for (e <- elements) f(e)

  def map[V2](f: V => V2): MS[I, V2] = new _MS[I, V2](iTag, length, (for (e <- elements) yield f(e)).toArray)

  def flatMap[V2](f: V => MS[I, V2]): MS[I, V2] = {
    val newArray = elements.flatMap(e => f(e).elements).toArray[Any]
    new _MS[I, V2](iTag, newArray.length, newArray)
  }

  def indices: scala.collection.Seq[I] = {
    import _Type._
    val sz = length
    (iTag.runtimeClass.toString match {
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
    }).asInstanceOf[scala.collection.Seq[I]]
  }

  override def clone: MS[I, V] = {
    implicit val it = iTag
    new _MS[I, V](iTag, length, array.map(_Clonable.clone))
  }

  override def toString: Predef.String = {
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
