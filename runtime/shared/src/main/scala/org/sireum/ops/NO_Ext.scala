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

package org.sireum.ops

import org.sireum._
import org.sireum.math.{_ZLong, _ZBigInt, _N}

object ZOps_Ext {

  @inline final def unary_-(n: Z): Z = {
    n match {
      case _ZLong(m) =>
        if (m != Long.MinValue)
          return _ZLong(-m)
      case _ =>
    }
    _ZBigInt(-n.toBigInt)
  }

  @inline final def +(n: Z, other: Z): Z = {
    (n, other) match {
      case (_ZLong(n1), _ZLong(n2)) =>
        val r = n1 + n2
        if (((n1 ^ r) & (n2 ^ r)) >= 0L)
          return _ZLong(r)
      case _ =>
    }
    _ZBigInt(n.toBigInt + other.toBigInt)
  }

  @inline final def -(n: Z, other: Z): Z = {
    (n, other) match {
      case (_ZLong(n1), _ZLong(n2)) =>
        val r = n1 - n2
        if (((n1 ^ r) & (n2 ^ r)) >= 0L)
          return _ZLong(r)
      case _ =>
    }
    _ZBigInt(n.toBigInt - other.toBigInt).pack
  }

  @inline final def *(n: Z, other: Z): Z = {
    (n, other) match {
      case (_ZLong(n1), _ZLong(n2)) =>
        val r = n1 * n2
        if (r == 0) return math._Z.zero
        var upgrade = false
        if (n2 > n1) {
          if (((n2 == -1) && (n1 == Long.MinValue)) || (r / n2 != n1))
            upgrade = true
        } else {
          if (((n1 == -1) && (n2 == Long.MinValue)) || (r / n1 != n2))
            upgrade = true
        }
        if (!upgrade) return _ZLong(r)
      case _ =>
    }
    _ZBigInt(n.toBigInt * other.toBigInt)
  }

  @inline final def /(n: Z, other: Z): Z = {
    (n, other) match {
      case (_ZLong(n1), _ZLong(n2)) =>
        val r = n1 / n2
        if (!((n1 == Long.MinValue) && (n2 == -1)))
          return _ZLong(r)
      case _ =>
    }
    _ZBigInt(n.toBigInt / other.toBigInt).pack
  }

  @inline final def %(n: Z, other: Z): Z =
    _ZBigInt(n.toBigInt % other.toBigInt).pack

  @inline final def >(n: Z, other: Z): B = (n, other) match {
    case (_ZLong(n1), _ZLong(n2)) => _2B(n1 > n2)
    case _ => _2B(n.toBigInt > other.toBigInt)
  }

  @inline final def >=(n: Z, other: Z): B = (n, other) match {
    case (_ZLong(n1), _ZLong(n2)) => _2B(n1 >= n2)
    case _ => _2B(n.toBigInt >= other.toBigInt)
  }

  @inline final def <(n: Z, other: Z): B = (n, other) match {
    case (_ZLong(n1), _ZLong(n2)) => _2B(n1 < n2)
    case _ => _2B(n.toBigInt < other.toBigInt)
  }

  @inline final def <=(n: Z, other: Z): B = (n, other) match {
    case (_ZLong(n1), _ZLong(n2)) => _2B(n1 <= n2)
    case _ => _2B(n.toBigInt <= other.toBigInt)
  }

  @inline final def hash(n: Z): Z = n

  @inline final def isEqual(n: Z, other: Z): B = (n, other) match {
    case (n: _ZLong, other: _ZLong) => n.value == other.value
    case _ => _2B(n.toBigInt == other.toBigInt)
  }

  @inline final def string(n: Z): String = _2String(n.toString)

}

object NOps_Ext {
  @inline final def +(n: N, other: N): N = _N(n.value + other.value)

  @inline final def -(n: N, other: N): N = _N(n.value - other.value)

  @inline final def *(n: N, other: N): N = _N(n.value * other.value)

  @inline final def /(n: N, other: N): N = _N(n.value / other.value)

  @inline final def %(n: N, other: N): N = _N(n.value % other.value)

  @inline final def >(n: N, other: N): B = n.value > other.value

  @inline final def >=(n: N, other: N): B = n.value >= other.value

  @inline final def <(n: N, other: N): B = n.value < other.value

  @inline final def <=(n: N, other: N): B = n.value <= other.value

  @inline final def hash(n: N): Z = n.value

  @inline final def isEqual(n: N, other: N): B = n.value.isEqual(other.value)

  @inline final def string(n: N): String = _2String(n.toString)
}