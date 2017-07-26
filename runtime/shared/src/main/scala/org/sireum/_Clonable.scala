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

package org.sireum

import scala.meta._

trait _Clonable {
  def hash: Z
  override def clone: java.lang.Object = this
}

object _Clonable {
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

  def clone[T](o: T): T = o match {
    case o: IS[_, _] => o.clone.asInstanceOf[T]
    case o: MS[_, _] => o.clone.asInstanceOf[T]
    case o: _Clonable => o.clone.asInstanceOf[T]
    case x => x
  }
}
