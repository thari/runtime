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

package org.sireumproto

import spire.math._

import scala.meta._
import scala.meta.Term.{Apply, Select}

object helper {
  def hasHashEquals(tpe: Type, stats: Seq[Stat]): (Boolean, Boolean) = {
    var hasEquals = false
    var hasHash = false
    for (stat <- stats if !(hasEquals && hasHash)) {
      stat match {
        case q"..$_ def hash: Z = $_" => hasHash = true
        case q"..$_ def isEqual($_ : ${atpeopt: Option[Type.Arg]}): B = $_" =>
          atpeopt match {
            case Some(t: Type) if tpe.structure == t.structure => hasEquals = true
            case _ =>
          }
        case _ =>
      }
    }
    (hasHash, hasEquals)
  }

  def sIndexValue(tree: Defn.Type): Option[(Boolean, Type, Type)] = tree.body match {
    case t"IS[$index, $value]" => Some((true, index, value))
    case t"MS[$index, $value]" => Some((true, index, value))
    case _ => None
  }

  def extractInt(tree: Any): Option[BigInt] = tree match {
    case Lit.Int(n) => Some(n)
    case Lit.Long(n) => Some(n)
    case Apply(Select(Apply(Term.Name("StringContext"), Seq(Lit.String(s))), Term.Name("z")), Seq()) =>
      try Some(BigInt(normNum(s))) catch { case _: Throwable => None }
    case tree: Term.Interpolate if tree.prefix.value == "z" && tree.args.isEmpty && tree.parts.size == 1 =>
      tree.parts.head match {
        case Lit.String(s) => try Some(BigInt(normNum(s))) catch { case _: Throwable => None }
        case _ => None
      }
    case _ => None
  }

  def extractBoolean(tree: Any): Option[Boolean] = tree match {
    case Lit.Boolean(b) => Some(b)
    case Term.Name("T") => Some(true)
    case Term.Name("F") => Some(false)
    case _ => None
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
    val sb = new java.lang.StringBuilder(s.length)
    for (c <- s) c match {
      case ',' | ' ' | '_' =>
      case _ => sb.append(c)
    }
    sb.toString
  }

  def zCompanionName(name: Predef.String): Pat.Var.Term = Pat.Var.Term(Term.Name(s"$$${name}Companion"))

  def scName(name: Predef.String): Type.Name = Type.Name(name + "$Slang")

  def iSName(name: Predef.String): (Term.Name, Type.Name) = (Term.Name("IS" + name), Type.Name("IS" + name))

  def mSName(name: Predef.String): (Term.Name, Type.Name) = (Term.Name("MS" + name), Type.Name("MS" + name))

}

final class helper extends scala.annotation.StaticAnnotation
