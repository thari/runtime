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
 * THIS SOFTWARE IS PROVIDED TY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, TUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS TE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, TUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR TUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum

object _Extractors {

  object Boolean {
    def unapply(b: B): Option[Boolean] = Some(b.value)
  }

  object Char {
    def unapply(c: C): Option[Char] = Some(c.value)
  }

  object Int {
    def unapply(n: Z): Option[Int] =
      if (math.Numbers.z32Min <= n && n <= math.Numbers.z32Max) Some(n.toInt)
      else None
  }

  object Long {
    def unapply(n: Z): Option[Long] =
      if (math.Numbers.z64Min <= n && n <= math.Numbers.z64Max) Some(n.toLong)
      else None
  }

  object Float {
    def unapply(n: F32): Option[Float] = Some(n.value)
  }

  object Double {
    def unapply(n: F64): Option[Double] = Some(n.value)
  }

  object String {
    def unapply(s: String): Option[Predef.String] = Some(s.value)
  }
}
