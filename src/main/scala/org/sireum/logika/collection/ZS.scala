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

object ZS {
  def apply(elements: Z*): ZS = new ZSArray(elements.toArray)
}

trait ZS {
  def size: Z

  def apply(index: Z): Z

  def update(index: Z, value: Z): Unit

  def :+(value: Z): ZS

  def +:(value: Z): ZS

  override def clone: ZS = sys.error("stub")

  final override def hashCode: Int = "ZS".hashCode + size.toInt

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

import scala.collection.mutable.{ListMap => LM}

private[logika] final class ZSArray(a: Array[Z]) extends ZS {
  override val size: Z = ZM(a.length)

  override def apply(index: Z): Z = {
    assert(index < ZM(ZM.intMax))
    a(index.toInt)
  }

  override def update(index: Z, value: Z): Unit = {
    assert(index < ZM(ZM.intMax))
    a(index.toInt) = value
  }

  override def :+(value: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
    else {
      val newValue = new Array[Z](a.length + 1)
      System.arraycopy(a, 0, newValue, 0, a.length)
      newValue(a.length) = value
      new ZSArray(newValue)
    }

  override def +:(value: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
    else {
      val newValue = new Array[Z](a.length + 1)
      newValue(0) = value
      System.arraycopy(a, 0, newValue, 1, a.length)
      new ZSArray(newValue)
    }

  override def clone: ZS = new ZSArray(a.clone)

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

  private def upgrade: ZSImpl = {
    val lm = LM[Z, Z]()
    var i = ZM.zero
    for (e <- a) {
      lm(i) = e
      i += ZM.one
    }
    new ZSImpl(lm, i)
  }
}

private[logika] final class ZSImpl(lm: LM[Z, Z],
                                   override val size: Z) extends ZS {
  def apply(index: Z): Z = lm.get(index) match {
    case Some(value) => value
    case _ => throw new IndexOutOfBoundsException(index.toString)
  }

  def update(index: Z, value: Z): Unit =
    if (lm.contains(index)) lm(index) = value
    else throw new IndexOutOfBoundsException(index.toString)

  def :+(value: Z): ZS = {
    val lm = LM[Z, Z]()
    for ((i, v) <- this.lm) lm(i) = v
    lm(size) = value
    new ZSImpl(lm, ZM(this.lm.size) + ZM.one)
  }

  def +:(value: Z): ZS = {
    val lm = LM[Z, Z]()
    lm(ZM.zero) = value
    for ((i, v) <- this.lm) lm(i + ZM.one) = v
    new ZSImpl(lm, size + ZM.one)
  }

  override def clone: ZS = new ZSImpl(lm.clone, size)

  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    if (lm.nonEmpty) {
      var i = ZM.zero
      sb.append(lm(i))
      i += ZM.one
      while (i < size) {
        sb.append(", ")
        sb.append(lm(i))
        i += ZM.one
      }
    }
    sb.append(']')
    sb.toString
  }
}