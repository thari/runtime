/*
 * Copyright (c) 2016, Robby, Kansas State University
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

import org.sireum.logika.{B, Z}
import org.sireum.logika.math.{Z => ZM}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object ZS {
  type MM = MMap[Z, Z]
  type TM = java.util.TreeMap[Z, Z]

  def apply(elements: Z*): ZS = new ZSArray(ArrayBuffer(elements: _*))

  private[collection] def newZSMap: (MM, TM) = {
    val tm = new TM
    val a: scala.collection.mutable.Map[Z, Z] = {
      import scala.collection.JavaConversions._
      tm
    }
    (a, tm)
  }
}

sealed trait ZS {
  def size: Z

  def apply(index: Z): Z

  def update(index: Z, value: Z): Unit

  def :+(value: Z): ZS

  def +:(value: Z): ZS

  override def clone: ZS = sys.error("stub")

  final override def equals(other: Any): B = other match {
    case other: ZS =>
      if (this eq other) return true
      if (size != other.size) return false
      var i = ZM.zero
      while (i < size) {
        if (apply(i) != other(i)) return false
        i += ZM.one
      }
      true
    case _ => false
  }
}

private[logika] final class ZSArray(a: ArrayBuffer[Z]) extends ZS {
  var (dirty, _hashCode) = (true, 0)

  override val size: Z = ZM(a.length)

  override def apply(index: Z): Z = {
    assert(index < ZM(ZM.intMax))
    a(index.toInt)
  }

  override def update(index: Z, value: Z): Unit = {
    assert(index < ZM(ZM.intMax))
    dirty = true
    a(index.toInt) = value
  }

  override def :+(value: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
    else new ZSArray(a :+ value)

  override def +:(value: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
    else new ZSArray(value +: a)

  override def clone: ZS = new ZSArray(a.clone)

  override def hashCode: Int = {
    if (dirty) {
      _hashCode = computeHashCode
      dirty = false
    }
    _hashCode
  }

  def computeHashCode: Int = a.hashCode

  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    if (a.nonEmpty) {
      var i = 0
      sb.append(a(i))
      i += 1
      val sz = a.length
      while (i < sz) {
        sb.append(", ")
        sb.append(a(i))
        i += 1
      }
    }
    sb.append(']')
    sb.toString
  }

  private[logika] def upgrade: ZSTreeMap = {
    val (a, tm) = ZS.newZSMap
    var i = ZM.zero
    for (e <- this.a) {
      a(i) = e
      i += ZM.one
    }
    new ZSTreeMap(tm, i)
  }
}

private[logika] final class ZSTreeMap(tm: ZS.TM,
                                      override val size: Z) extends ZS {
  var (dirty, _hashCode) = (true, 0)
  val a: ZS.MM = {
    import scala.collection.JavaConversions._
    tm
  }

  def apply(index: Z): Z = a.get(index) match {
    case Some(value) => value
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  def update(index: Z, value: Z): Unit = {
    dirty = true
    if (ZM.zero <= index && index < size) a(index) = value
    else throw new IndexOutOfBoundsException(index.toString)
  }

  def :+(value: Z): ZS = {
    val (a, tm) = ZS.newZSMap
    for ((i, v) <- this.a) a(i) = v
    a(size) = value
    new ZSTreeMap(tm, ZM(this.tm.size) + ZM.one)
  }

  def +:(value: Z): ZS = {
    val (a, tm) = ZS.newZSMap
    a(ZM.zero) = value
    for ((i, v) <- this.a) a(i + ZM.one) = v
    new ZSTreeMap(tm, size + ZM.one)
  }

  override def clone: ZS = new ZSTreeMap(tm.clone.asInstanceOf[ZS.TM], size)

  override def hashCode: Int = {
    if (dirty) {
      _hashCode = computeHashCode
      dirty = false
    }
    _hashCode
  }

  def computeHashCode = a.values.toSeq.hashCode

  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    if (a.nonEmpty) {
      var i = ZM.zero
      sb.append(a(i))
      i += ZM.one
      while (i < size) {
        sb.append(", ")
        sb.append(a(i))
        i += ZM.one
      }
    }
    sb.append(']')
    sb.toString
  }
}