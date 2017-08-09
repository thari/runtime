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

package org.sireum.ops

import org.sireum._

object BOps_Ext {
  @inline final def &(b1: B, b2: B): B = _2B(b1.value & b2.value)
  @inline final def |(b1: B, b2: B): B = _2B(b1.value | b2.value)
  @inline final def |^(b1: B, b2: B): B = _2B(b1.value ^ b2.value)
  @inline final def &&(b1: B, b2: => B): B = _2B(b1.value && b2.value)
  @inline final def ||(b1: B, b2: => B): B = _2B(b1.value || b2.value)
  @inline final def isEqual(b1: B, b2: B): B = _2B(b1.value == b2.value)
  @inline final def unary_!(b: B): B = _2B(!b.value)
  @inline final def unary_~(b: B): B = _2B(!b.value)
  @inline final def hash(b: B): Z = _Z(b.hashCode)
  @inline final def toString(b: B): Predef.String = b.value.toString
}
