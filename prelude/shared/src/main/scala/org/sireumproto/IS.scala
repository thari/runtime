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

package org.sireumproto

import org.sireumproto.$internal.{Boxer, ISMarker}

object IS {

  def checkSize[I <: Z](size: Z.MP)(implicit companion: $ZCompanion[I]): Unit = {
    assert(Z.MP.zero <= size, s"Slang IS requires a non-negative size.")
    assert(!companion.hasMax || companion.Index.toMP + size <= companion.Max.toMP, s"Slang IS requires its index plus its size less than or equal to it max.")
  }

  def apply[I <: Z, V <: Immutable](args: V*)(implicit companion: $ZCompanion[I]): IS[I, V] = {
    checkSize(Z.MP(args.length))(companion)
    val boxer = Boxer.boxerSeq(args)
    val length = Z.MP(args.length)
    val a = boxer.create(length)
    var i = Z.MP.zero
    for (arg <- args) {
      boxer.store(a, i, arg)
      i = i.increase
    }
    IS[I, V](companion, a, length, boxer)
  }

  def create[I <: Z, V <: Immutable](size: Z, default: V)(implicit companion: $ZCompanion[I]): IS[I, V] = size match {
    case size: Z.MP =>
      checkSize(size)(companion)
      val length = size
      val boxer = Boxer.boxer(default)
      val a = boxer.create(length)
      var i = Z.MP.zero
      while (i < length) {
        boxer.store(a, i, default)
        i = i.increase
      }
      IS[I, V](companion, a, length, boxer)
    case _ => halt("Slang IS operation 'create' requires size of exactly type 'Z'.")
  }

  def apply[I <: Z, V <: Immutable](companion: $ZCompanion[I],
                                    data: scala.AnyRef,
                                    length: Z.MP,
                                    boxer: Boxer): IS[I, V] = new IS[I, V](companion, data, length, boxer)

}

final class IS[I <: Z, V <: Immutable](val companion: $ZCompanion[I],
                                       val data: scala.AnyRef,
                                       val length: Z.MP,
                                       val boxer: Boxer) extends Immutable with ISMarker {

  def isEmpty: B = length == Z.MP.zero

  def nonEmpty: B = length != Z.MP.zero

  def :+(e: V): IS[I, V] = if (isEmpty) IS[I, V](e)(companion) else {
    val newLength = length.increase
    IS.checkSize(newLength)
    val a = boxer.clone(data, length, newLength, Z.MP.zero)
    boxer.store(a, length, e)
    IS[I, V](companion, a, newLength, boxer)
  }

  def +:(e: V): IS[I, V] = if (isEmpty) IS[I, V](e)(companion) else {
    val newLength = length.increase
    IS.checkSize(newLength)
    val a = boxer.clone(data, length, newLength, Z.MP.one)
    boxer.store(a, Z.MP.zero, e)
    IS[I, V](companion, a, newLength, boxer)
  }

  def ++(other: IS[I, V]): IS[I, V] = if (isEmpty) other else {
    if (other.length == Z.MP.zero) return this
    val newLength = length + other.length
    IS.checkSize(newLength)
    val a = boxer.clone(data, length, newLength, Z.MP.zero)
    var i = length
    var j = Z.MP.zero
    while (i < newLength) {
      boxer.store(a, i, boxer.lookup(other.data, j))
      i = i.increase
      j = j.increase
    }
    IS[I, V](companion, a, newLength, boxer)
  }

  def --(other: IS[I, V]): IS[I, V] =
    if (isEmpty || other.length == Z.MP.zero) this else {
      val otherElements = other.elements
      var sm = elements.withFilter(_ == otherElements.head)
      for (e <- other.elements.tail) {
        sm = sm.withFilter(_ == e)
      }
      val s = sm.map(identity)
      val newLength = Z.MP(s.size)
      val a = boxer.create(newLength)
      var i = Z.MP.zero
      for (e <- s) {
        boxer.store(a, i, e)
        i = i.increase
      }
      IS[I, V](companion, a, newLength, boxer)
    }

  def -(e: V): IS[I, V] = if (isEmpty) this else withFilter(_ == e)

  def indices: ZRange[I] = {
    var j: Z = companion.Index
    var i = Z.MP.zero
    while (i < length) {
      i = i.increase
      j = j.increase
    }
    ZRange(companion.Index, j.asInstanceOf[I], _ => T, (r, i) => if (r) i.decrease.asInstanceOf[I] else i.increase.asInstanceOf[I], F)
  }

  def map[V2 <: Immutable](f: V => V2): IS[I, V2] =
    if (isEmpty) this.asInstanceOf[IS[I, V2]] else {
      var a: AnyRef = null
      var boxer2: Boxer = null
      var i = Z.MP.zero
      while (i < length) {
        val v2 = f(boxer.lookup(a, i))
        if (boxer2 == null) {
          boxer2 = Boxer.boxer(v2)
          a = boxer2.clone(data, length, length, Z.MP.zero)
        }
        boxer2.store(a, i, v2)
        i = i.increase
      }
      IS[I, V2](companion, a, length, boxer)
    }

  def flatMap[V2 <: Immutable](f: V => IS[I, V2]): IS[I, V2] =
    if (isEmpty) this.asInstanceOf[IS[I, V2]] else {
      val es = elements
      var r = f(es.head)
      for (e <- es.tail) {
        r = r ++ f(e)
      }
      r
    }

  def withFilter(p: V => B): IS[I, V] = {
    val s = elements.withFilter(e => p(e).value).map(identity)
    val newLength = Z.MP(s.length)
    val a = boxer.create(newLength)
    var i = Z.MP.zero
    for (e <- s) {
      boxer.store(a, i, e)
      i = i.increase
    }
    IS[I, V](companion, a, newLength, boxer)
  }

  def foreach(f: V => Unit): Unit = {
    var i = Z.MP.zero
    while (i < length) {
      f(boxer.lookup(data, i))
      i = i.increase
    }
  }

  def size: I =
    if (companion.isZeroIndex) companion(length)
    else halt(s"Operation 'size' can only be used on zero-indexed IS.")

  def apply(index: I): V = {
    val i = index.toIndex
    assume(Z.MP.zero <= i && i <= length, s"Array indexing out of bounds: $index")
    boxer.lookup(data, i)
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

  lazy val hash: Z = elements.hashCode

  def isEqual(other: Immutable): B = this == other

  override def equals(other: scala.Any): scala.Boolean =
    if (this eq other.asInstanceOf[scala.AnyRef]) true
    else other match {
      case other: IS[_, _] =>
        if (companion ne other.companion) return false
        if (length != other.length) return false
        elements == other.elements
      case _ => false
    }

  def string: String = toString

  override def toString: Predef.String = boxer.toString(data, length)
}
