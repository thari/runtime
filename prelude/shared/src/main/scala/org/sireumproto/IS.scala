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

import org.sireumproto.$internal.ISMarker

object IS {

  final val MaxArraySizeInt: scala.Int = Int.MaxValue - 8

  def MaxArraySize: Z.MP = Z.MP(MaxArraySizeInt)

  final class Array[I <: Z, V <: Immutable](val companion: $ZCompanion[I],
                                            array: scala.Array[scala.Any],
                                            length: scala.Int) extends IS[I, V] {

    def isEmpty: B = length == 0

    def nonEmpty: B = length != 0

    def :+(e: V): IS[I, V] = if (isEmpty) IS[I, V](e)(companion) else {
      checkSize(sizeMP.increase)
      val newLength = length + 1
      val a = copy(array, length, newLength, 0)
      a(length) = e
      new Array[I, V](companion, a, newLength)
    }

    def +:(e: V): IS[I, V] = if (isEmpty) IS[I, V](e)(companion) else {
      checkSize(sizeMP.increase)
      val newLength = length + 1
      val a = copy(array, length, newLength, 1)
      a(0) = e
      new Array[I, V](companion, a, newLength)
    }

    def ++(other: IS[I, V]): IS[I, V] = if (isEmpty) other else {
      if (other.sizeMP == Z.MP.zero) return this
      val newSize = sizeMP + other.sizeMP
      checkSize(newSize)
      val newLength = newSize.toIntOpt.get
      val a = copy(array, length, newLength, 0)
      var j = companion.Index
      for (i <- length until newLength) {
        a(i) = other(j)
        j = j.increase.asInstanceOf[I]
      }
      new Array[I, V](companion, a, newLength)
    }

    def --(other: IS[I, V]): IS[I, V] = if (isEmpty) this else {
      if (other.sizeMP == Z.MP.zero) return this
      val otherElements = other.elements
      var sm = elements.withFilter(_ == otherElements.head)
      for (e <- other.elements.tail) sm = sm.withFilter(_ == e)
      val a = sm.map(identity).toArray[scala.Any]
      new Array[I, V](companion, a, a.length)
    }

    def -(e: V): IS[I, V] = if (isEmpty) this else withFilter(_ == e)

    def indices: IS[Z, I] = {
      val a = new scala.Array[scala.Any](length)
      var j = companion.Index
      for (i <- 0 until length) {
        a(i) = j
        j = j.increase.asInstanceOf[I]
      }
      new Array[Z, I]($ZCompanion, a, length)
    }

    def map[V2 <: Immutable](f: V => V2): IS[I, V2] = if (isEmpty) this.asInstanceOf[IS[I, V2]] else {
      val a = copy(array, length, length, 0)
      for (i <- 0 until length) {
        a(i) = f(a(i).asInstanceOf[V])
      }
      new Array[I, V2](companion, a, length)
    }

    def flatMap[V2 <: Immutable](f: V => IS[I, V2]): IS[I, V2] = if (isEmpty) this.asInstanceOf[IS[I, V2]] else {
      val es = elements
      var r = f(es.head)
      for (e <- es.tail) {
        r = r ++ f(e)
      }
      r
    }

    def withFilter(p: V => B): IS[I, V] = {
      val a = elements.withFilter(e => p(e).value).map(identity).toArray[scala.Any]
      new Array[I, V](companion, a, a.length)
    }

    def foreach(f: V => Unit): Unit = {
      for (i <- 0 until length) {
        f(array(i).asInstanceOf[V])
      }
    }

    def size: I =
      if (companion.isZeroIndex) companion.Int(length)
      else halt(s"Operation 'size' can only be used on zero-indexed IS.")

    def sizeMP: Z.MP = Z.MP(length)

    def apply(index: I): V = {
      val i = index.toIndex.toMP
      assume(Z.MP.zero <= i && i <= length, s"Array indexing out of bounds: $index")
      array(i.toIntOpt.get).asInstanceOf[V]
    }

    def elements: scala.Seq[V] = array.slice(0, length).map(_.asInstanceOf[V])

    lazy val hash: Z = elements.hashCode

    def isEqual(other: Immutable): B = this == other

    override def equals(other: scala.Any): scala.Boolean =
      if (this eq other.asInstanceOf[scala.AnyRef]) true
      else other match {
        case other: IS.Array[_, _] =>
          if (companion ne other.companion) return false
          if (size.toMP != other.size.toMP) return false
          elements == other.elements
        case _ => false
      }

    def string: String = toString

    override def toString: Predef.String = {
      val sb = new java.lang.StringBuilder
      sb.append('[')
      if (length > 0) {
        sb.append(array(0).toString)
        for (i <- 1 until length) {
          sb.append(", ")
          sb.append(array(i).toString)
        }
      }
      sb.append(']')
      sb.toString
    }

  }

  def copy(a: scala.Array[scala.Any], length: scala.Int, newLength: scala.Int, offset: scala.Int): scala.Array[scala.Any] = {
    if (a.length <= newLength) {
      val r = new scala.Array[scala.Any](a.length)
      System.arraycopy(a, 0, r, offset, length)
      r
    } else {
      val r = new scala.Array[scala.Any]((newLength * 3 / 2).min(MaxArraySizeInt))
      System.arraycopy(a, 0, r, offset, length)
      r
    }
  }

  def checkSize[I <: Z](size: Z.MP)(implicit companion: $ZCompanion[I]): Unit = {
    assert(Z.MP.zero <= size, s"Slang IS requires a non-negative size.")
    assert(size <= MaxArraySize, s"Slang IS currently only supports size up to $MaxArraySize.")
    assert(!companion.hasMax || companion.Index.toMP + size <= companion.Max.toMP, s"Slang IS requires its index plus its size less than or equal to it max.")
  }

  def apply[I <: Z, V <: Immutable](args: V*)(implicit companion: $ZCompanion[I]): IS[I, V] = {
    checkSize(Z.MP(args.length))(companion)
    val array = new scala.Array[scala.Any](args.length)
    for (i <- array.indices) array(i) = args(i)
    new Array[I, V](companion, array, args.length)
  }

  def create[I <: Z, V <: Immutable](size: Z, default: V)(implicit companion: $ZCompanion[I]): IS[I, V] = size match {
    case size: Z.MP =>
      checkSize(size)(companion)
      val length = size.toIntOpt.get
      val array = new scala.Array[scala.Any](length)
      for (i <- 0 until length) array(i) = default
      new Array[I, V](companion, array, length)
    case _ => halt("Slang IS operation 'create' requires size of exactly type 'Z'.")
  }

}

sealed trait IS[I <: Z, V <: Immutable] extends Immutable with ISMarker {

  def isEmpty: B

  def nonEmpty: B

  def :+(e: V): IS[I, V]

  def +:(e: V): IS[I, V]

  def ++(other: IS[I, V]): IS[I, V]

  def --(other: IS[I, V]): IS[I, V]

  def -(e: V): IS[I, V]

  def indices: IS[Z, I]

  def map[V2 <: Immutable](f: V => V2): IS[I, V2]

  def flatMap[V2 <: Immutable](f: V => IS[I, V2]): IS[I, V2]

  def withFilter(p: V => B): IS[I, V]

  def foreach(p: V => Unit): Unit

  def size: I

  def apply(index: I): V

  def sizeMP: Z.MP

  def elements: scala.Seq[V]

}
