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

package org.sireum

import org.sireum.$internal.{ImmutableMarker, MutableMarker}
import spire.math._

import scala.annotation.StaticAnnotation

object helper {

  private val topValueError = "Unexpected a value not implementing either Slang Immutable or Mutable."

  def halt(msg: Any): Nothing = {
    assume(assumption = false, msg.toString)
    throw new Error
  }

  def clone[T](o: T): T = o match {
    case o: ImmutableMarker => o.$clone.asInstanceOf[T]
    case o: MutableMarker => o.$clone.asInstanceOf[T]
    case _ => halt(topValueError + s": $o (of ${o.getClass.getName})")
  }

  def cloneAssign[T](o: T): T = o match {
    case o: MutableMarker => (o.$clone.$owned = true).asInstanceOf[T]
    case o: ImmutableMarker => o.$clone.asInstanceOf[T]
    case _ => halt(topValueError + s": $o (of ${o.getClass.getName})")
  }

  def assign[T](x: MutableMarker): T =
    (if (x.$owned) x.$clone.$owned = true else x.$owned = true).asInstanceOf[T]

  def assign[T](arg: T): T = {
    arg match {
      case x: MutableMarker => assign[T](x)
      case _ => arg
      //case _: ImmutableMarker => arg
      //case _ => halt(topValueError + s": $arg (of ${arg.getClass.getName})")
    }
  }

  def isUByte(n: BigInt): Boolean = UByte.MinValue.toBigInt <= n && n <= UByte.MaxValue.toBigInt

  def isByte(n: BigInt): Boolean = Byte.MinValue.toInt <= n && n <= Byte.MaxValue.toInt

  def isUShort(n: BigInt): Boolean = UShort.MinValue.toBigInt <= n && n <= UShort.MaxValue.toBigInt

  def isShort(n: BigInt): Boolean = Short.MinValue.toInt <= n && n <= Short.MaxValue.toInt

  def isUInt(n: BigInt): Boolean = UInt.MinValue.toBigInt <= n && n <= UInt.MaxValue.toBigInt

  def isInt(n: BigInt): Boolean = Int.MinValue <= n && n <= Int.MaxValue

  def isULong(n: BigInt): Boolean = ULong.MinValue.toBigInt <= n && n <= ULong.MaxValue.toBigInt

  def isLong(n: BigInt): Boolean = Long.MinValue <= n && n <= Long.MaxValue

  def bits(min: BigInt, max: BigInt): Option[(Boolean, Int)] =
    if (isUByte(min) && isUByte(max)) Some((false, 8))
    else if (isByte(min) && isByte(max)) Some((true, 8))
    else if (isUShort(min) && isUShort(max)) Some((false, 16))
    else if (isShort(min) && isShort(max)) Some((true, 16))
    else if (isUInt(min) && isUInt(max)) Some((false, 32))
    else if (isInt(min) && isInt(max)) Some((true, 32))
    else if (isULong(min) && isULong(max)) Some((false, 64))
    else if (isLong(min) && isLong(max)) Some((true, 64))
    else None

  def normNum(s: Predef.String): Predef.String = {
    val sb = new _root_.java.lang.StringBuilder(s.length)
    for (c <- s) c match {
      case ',' | ' ' | '_' =>
      case _ => sb.append(c)
    }
    sb.toString
  }

  def escape(raw: Predef.String): Predef.String = {
    val sb = new _root_.java.lang.StringBuilder

    def escapeChar(ch: Char): Unit = ch match {
      case '\b' => sb.append("\\b")
      case '\t' => sb.append("\\t")
      case '\n' => sb.append("\\n")
      case '\f' => sb.append("\\f")
      case '\r' => sb.append("\\r")
      case '"' => sb.append("\\\"")
      case '\'' => sb.append("\\\'")
      case '\\' => sb.append("\\\\")
      case _ =>
        if (ch.isControl) {
          sb.append("\\0")
          sb.append(Integer.toOctalString(ch.toInt))
        }
        else sb.append(ch)
    }

    sb.append('"')
    raw.foreach(escapeChar)
    sb.append('"')
    sb.toString
  }

  import scala.language.experimental.macros

  def $assign[T](arg: T): T = macro $internal.Macro.$assign

  def $tmatch[T](arg: T): T = macro $internal.Macro.$tmatch

}

class helper extends StaticAnnotation

