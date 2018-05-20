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

package org.sireum

import org.sireum.$internal.{Boxer, MSMarker}

object MS {

  def checkSize[I](size: Z)(implicit companion: $ZCompanion[I]): Unit = {
    assert(Z.MP.zero <= size, s"Slang MS requires a non-negative size.")
    assert(
      !companion.hasMax || companion.Index.asInstanceOf[ZLike[_]].toMP + size <= companion.Max
        .asInstanceOf[ZLike[_]]
        .toMP,
      s"Slang MS requires its index plus its size less than or equal to it max."
    )
  }

  def apply[I, V](args: V*)(implicit companion: $ZCompanion[I]): MS[I, V] = {
    checkSize(Z.MP(args.length))(companion)
    val boxer = Boxer.boxerSeq(args)
    val length = Z.MP(args.length)
    val a = boxer.create(length)
    var i = Z.MP.zero
    for (arg <- args) {
      boxer.store(a, i, helper.assign(arg))
      i = i.increase
    }
    MS[I, V](companion, a, length, boxer)
  }

  def create[I, V](size: I, default: V)(implicit companion: $ZCompanion[I]): MS[I, V] = {
    val length = size.asInstanceOf[ZLike[_]].toMP
    checkSize(length)(companion)
    val boxer = Boxer.boxer(default)
    val a = boxer.create(length)
    var i = Z.MP.zero
    while (i < length) {
      boxer.store(a, i, helper.assign(default))
      i = i.increase
    }
    MS[I, V](companion, a, length, boxer)
  }

  def zreate[I, V](size: Z, default: V)(implicit companion: $ZCompanion[I]): MS[I, V] = {
    val length = size
    checkSize(length)(companion)
    val boxer = Boxer.boxer(default)
    val a = boxer.create(length)
    var i = Z.MP.zero
    while (i < length) {
      boxer.store(a, i, helper.assign(default))
      i = i.increase
    }
    MS[I, V](companion, a, length, boxer)
  }

  def apply[I, V](companion: $ZCompanion[I], data: scala.AnyRef, length: Z, boxer: Boxer): MS[I, V] =
    new MS[I, V](companion, data, length, boxer)

  def unapplySeq[I, V](o: MS[I, V]): scala.Option[scala.Seq[V]] = scala.Some(o.elements.map(helper.cloneAssign))
}

final class MS[I, V](val companion: $ZCompanion[I], val data: scala.AnyRef, val length: Z, val boxer: Boxer)
    extends Mutable with MSMarker with _root_.java.lang.Iterable[V] {

  private var isOwned: scala.Boolean = false

  override def $owned: scala.Boolean = isOwned

  override def $owned_=(b: scala.Boolean): this.type = {
    isOwned = b
    this
  }

  def hash: Z = hashCode

  def $clone: MS[I, V] = {
    val a = boxer.cloneMut(data, length, length, Z.MP.zero)
    MS[I, V](companion, a, length, boxer)
  }

  def isEmpty: B = length == Z.MP.zero

  def nonEmpty: B = length != Z.MP.zero

  def :+(e: V): MS[I, V] =
    if (isEmpty) MS[I, V](e)(companion)
    else {
      val newLength = length.increase
      MS.checkSize(newLength)
      val a = boxer.cloneMut(data, length, newLength, Z.MP.zero)
      boxer.store(a, length, helper.assign(e))
      MS[I, V](companion, a, newLength, boxer)
    }

  def +:(e: V): MS[I, V] =
    if (isEmpty) MS[I, V](e)(companion)
    else {
      val newLength = length.increase
      MS.checkSize(newLength)
      val a = boxer.cloneMut(data, length, newLength, Z.MP.one)
      boxer.store(a, Z.MP.zero, helper.assign(e))
      MS[I, V](companion, a, newLength, boxer)
    }

  def ++[I2](other: MS[I2, V]): MS[I, V] = {
    val bxr = if (isEmpty) other.boxer else boxer
    if (other.length == Z.MP.zero) return this
    val newLength = length + other.length
    MS.checkSize(newLength)
    val a = bxr.cloneMut(data, length, newLength, Z.MP.zero)
    var i = length
    var j = Z.MP.zero
    while (i < newLength) {
      bxr.store(a, i, helper.assign(other.boxer.lookup(other.data, j)))
      i = i.increase
      j = j.increase
    }
    MS[I, V](companion, a, newLength, bxr)
  }

  def --[I2](other: MS[I2, V]): MS[I, V] =
    if (isEmpty || other.length == Z.MP.zero) this
    else {
      val otherElements = other.elements
      var sm = elements.withFilter(_ != otherElements.head)
      for (e <- other.elements.tail) {
        sm = sm.withFilter(_ != e)
      }
      val s = sm.map(identity)
      val newLength = Z.MP(s.size)
      val a = boxer.create(newLength)
      var i = Z.MP.zero
      for (e <- s) {
        boxer.store(a, i, helper.assign(e))
        i = i.increase
      }
      MS[I, V](companion, a, newLength, boxer)
    }

  def -(e: V): MS[I, V] = if (isEmpty) this else withFilter(_ != e)

  def indices: ZRange[I] = {
    var j = companion.Index.asInstanceOf[ZLike[_]]
    var i = Z.MP.zero
    while (i < length) {
      i = i.increase
      j = j.increase.asInstanceOf[ZLike[_]]
    }
    ZRange[I](
      companion.Index,
      j.decrease.asInstanceOf[I],
      1,
      _ => T,
      _.asInstanceOf[ZLike[_]].increase.asInstanceOf[I],
      _.asInstanceOf[ZLike[_]].decrease.asInstanceOf[I]
    )
  }

  def map[V2](f: V => V2): MS[I, V2] =
    if (isEmpty) MS[I, V2]()(companion)
    else {
      var a: AnyRef = null
      var boxer2: Boxer = null
      var i = Z.MP.zero
      while (i < length) {
        val v2 = f(boxer.lookup(data, i))
        if (boxer2 == null) {
          boxer2 = Boxer.boxer(v2)
          a = boxer2.create(length)
        }
        boxer2.store(a, i, helper.assign(v2))
        i = i + 1
      }
      MS[I, V2](companion, a, length, if (boxer2 == null) $internal.IdentityBoxer else boxer2)
    }

  def flatMap[V2](f: V => MS[I, V2]): MS[I, V2] =
    if (isEmpty) MS[I, V2]()(companion)
    else {
      val es = elements
      var r = f(es.head)
      for (e <- es.tail) {
        r = r ++ f(e)
      }
      r
    }

  def withFilter(p: V => B): MS[I, V] = {
    val s = elements.withFilter(e => p(e).value).map(identity)
    val newLength = Z.MP(s.length)
    val a = boxer.create(newLength)
    var i = Z.MP.zero
    for (e <- s) {
      boxer.store(a, i, helper.cloneAssign(e))
      i = i.increase
    }
    MS[I, V](companion, a, newLength, boxer)
  }

  def foreach(f: V => Unit): Unit = {
    var i = Z.MP.zero
    while (i < length) {
      f(helper.cloneAssign(boxer.lookup(data, i)))
      i = i.increase
    }
  }

  def iterator(): _root_.java.util.Iterator[V] = new _root_.java.util.Iterator[V] {
    var i: scala.Int = 0
    val es: Seq[V] = elements

    override def next(): V = {
      assert(hasNext)
      val r = helper.cloneAssign(es(i))
      i = i + 1
      r
    }

    override def hasNext: scala.Boolean = i <= es.length
  }

  def size: I =
    if (companion.isZeroIndex) companion(length)
    else halt(s"Operation 'size' can only be used on zero-indexed MS.")

  def zize: Z = length

  def toIS: IS[I, V] = {
    new IS[I, V](companion, boxer.clone(data, length, length, Z.MP.zero), length, boxer)
  }

  def apply(index: I): V = {
    val i = index.asInstanceOf[ZLike[_]].toIndex
    assert(Z.MP.zero <= i && i <= length, s"Array indexing out of bounds: $index")
    boxer.lookup(data, i)
  }

  def update(index: I, value: V): Unit = {
    val i = index.asInstanceOf[ZLike[_]].toIndex
    assert(Z.MP.zero <= i && i <= length, s"Array indexing out of bounds: $index")
    boxer.store(data, i, helper.assign(value))
  }

  def elements: scala.Seq[V] = {
    var r = scala.Vector[V]()
    var i = Z.MP.zero
    while (i < length) {
      r = r :+ boxer.lookup[V](data, i)
      i = i.increase
    }
    r
  }

  override def hashCode: scala.Int = {
    (companion, elements).hashCode
  }

  override def equals(other: scala.Any): scala.Boolean =
    if (this eq other.asInstanceOf[scala.AnyRef]) true
    else
      other match {
        case other: MS[_, _] =>
          if (companion ne other.companion) return false
          if (length != other.length) return false
          if (data eq other.data) return true
          val b1 = boxer
          val b2 = other.boxer
          val data1 = data
          val data2 = other.data
          for (i <- Z.MP.zero until length) {
            val iMP = i.toMP
            if (b1.lookup(data1, iMP) != b2.lookup(data2, iMP)) return false
          }
          true
        case _ => false
      }

  def string: String = toString

  override def toString: Predef.String = boxer.toString(data, length)
}
