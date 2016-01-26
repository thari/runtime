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
  def apply(elements: Z*): ZS = ZSArray(elements.toArray)
}

trait ZS {
  def size: Z

  def apply(index: Z): Z

  def update(index: Z, v: Z): Unit

  def :+(v: Z): ZS

  def +:(v: Z): ZS

  final override def hashCode: Int = "ZS".hashCode + size.toInt

  final override def equals(other: Any): B = other match {
    case other: ZS =>
      if (this eq other) return true
      if (size == other.size) return true
      var i = ZM.zero
      while (i < size) {
        if (this (i) != other(i)) return false
        i += ZM.one
      }
      true
    case _ => false
  }
}

import scala.collection.mutable.{ListMap => LM}

private final case class ZSArray(value: Array[Z]) extends ZS {
  override def size: Z = ZM(value.length)

  override def apply(index: Z): Z = {
    assert(index < ZM(ZM.intMax))
    value(index.toInt)
  }

  override def update(index: Z, v: Z): Unit = {
    assert(index < ZM(ZM.intMax))
    value(index.toInt) = v
  }

  override def :+(v: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ v
    else {
      val newValue = new Array[Z](value.length + 1)
      System.arraycopy(value, 0, newValue, 0, value.length)
      newValue(value.length) = v
      ZSArray(newValue)
    }

  override def +:(v: Z): ZS =
    if (size + ZM.one == ZM(ZM.intMax)) v +: upgrade
    else {
      val newValue = new Array[Z](value.length + 1)
      System.arraycopy(value, 0, newValue, 1, value.length)
      newValue(0) = v
      ZSArray(newValue)
    }

  override def clone: ZS = ZSArray(value.clone)

  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    if (value.nonEmpty) {
      var i = 0
      sb.append(value(i))
      i += 1
      val sz = value.length
      while (i < sz) {
        sb.append(", ")
        sb.append(value(i))
        i += 1
      }
    }
    sb.append(']')
    sb.toString
  }

  private def upgrade: ZSImpl = {
    val lm = LM[Z, Z]()
    var i = ZM.zero
    for (e <- value) {
      lm(i) = e
      i += ZM.one
    }
    new ZSImpl(lm, i)
  }
}

private final class ZSImpl(lmArg: LM[Z, Z], lmSize: Z) extends ZS {
  private[logika] val lm: LM[Z, Z] = lmArg

  val size: Z = lmSize

  def apply(index: Z): Z = {
    lm.get(index) match {
      case Some(value) => value
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }
  }

  def update(index: Z, value: Z): Unit = {
    if (lm.contains(index)) lm(index) = value
    else throw new IndexOutOfBoundsException(index.toString)
  }

  def :+(value: Z): ZS = {
    val lm = LM[Z, Z]()
    for ((i, v) <- this.lm) {
      lm(i) = v
    }
    lm(ZM(lm.size)) = value
    new ZSImpl(lm, ZM(this.lm.size) + ZM.one)
  }

  def +:(value: Z): ZS = {
    val lm = LM[Z, Z]()
    lm(ZM.zero) = value
    for ((i, v) <- this.lm) {
      lm(i + ZM.one) = v
    }
    new ZSImpl(lm, ZM(this.lm.size) + ZM.one)
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