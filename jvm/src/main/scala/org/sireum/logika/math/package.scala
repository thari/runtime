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

package org.sireum.logika

import org.apfloat.Apint
import spire.math.{ULong, UInt, UShort, UByte}

package object math {
  final val defaultBitWidth: Int = {
    def err = sys.error("org.sireum.logika.math.bitWidth should be either 8, 16, 32, or 64.")
    try Option(System.getProperty("org.sireum.logika.math.bitWidth")) match {
      case Some(v) =>
        val n = v.toInt
        n match {
          case 8 | 16 | 32 | 64 => n
          case _ => err
        }
      case _ => 0
    } catch {
      case _: Throwable => err
    }
  }

  trait LogikaNumberCompanion {
    def random: _LogikaNumber
  }

  trait _LogikaNumber extends Clonable

  trait _LogikaIntegralNumber extends _LogikaNumber {
    import org.sireum.logika.{B, Z, Z8, Z16, Z32, Z64, N, N8, N16, N32, N64, S8, S16, S32, S64, U8, U16, U32, U64}

    def toBigInteger: java.math.BigInteger

    def toBigInt: BigInt

    def toApint: Apint

    def toZ: Z

    final def toZ8: Z8 = math._Z8.checkRange(toZ)

    final def toZ16: Z16 = math._Z16.checkRange(toZ)

    final def toZ32: Z32 = math._Z32.checkRange(toZ)

    final def toZ64: Z64 = math._Z64.checkRange(toZ)

    final def toS8: S8 = math._S8.ValueImpl(toZ8.value)

    final def toS16: S16 = math._S16.ValueImpl(toZ16.value)

    final def toS32: S32 = math._S32.ValueImpl(toZ32.value)

    final def toS64: S64 = math._S64.ValueImpl(toZ64.value)

    final def toN: N = math._N.checkRange(toZ)

    final def toN8: N8 = math._N8.checkRange(toZ)

    final def toN16: N16 = math._N16.checkRange(toZ)

    final def toN32: N32 = math._N32.checkRange(toZ)

    final def toN64: N64 = math._N64.checkRange(toZ)

    final def toU8: U8 = math._U8.ValueImpl(toN8.value)

    final def toU16: U16 = math._U16.ValueImpl(toN16.value)

    final def toU32: U32 = math._U32.ValueImpl(toN32.value)

    final def toU64: U64 = math._U64.ValueImpl(toN64.value)
  }
}
