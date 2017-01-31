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
    def random: LogikaNumber
  }

  trait LogikaNumber

  trait LogikaIntegralNumber extends LogikaNumber {
    def toBigInteger: java.math.BigInteger

    def toBigInt: BigInt

    def toApint: Apint

    def toZ: Z

    final def toZ8: math.Z8.Value = math.Z8.checkRange(toZ)

    final def toZ16: math.Z16.Value = math.Z16.checkRange(toZ)

    final def toZ32: math.Z32.Value = math.Z32.checkRange(toZ)

    final def toZ64: math.Z64.Value = math.Z64.checkRange(toZ)

    final def toS8: math.S8.Value = math.S8.ValueImpl(toZ.toByte)

    final def toS16: math.S16.Value = math.S16.ValueImpl(toZ.toShort)

    final def toS32: math.S32.Value = math.S32.ValueImpl(toZ.toInt)

    final def toS64: math.S64.Value = math.S64.ValueImpl(toZ.toLong)

    final def toN: N = math.N(toZ)

    final def toN8: math.N8.Value = math.N8.checkRange(toZ)

    final def toN16: math.N16.Value = math.N16.checkRange(toZ)

    final def toN32: math.N32.Value = math.N32.checkRange(toZ)

    final def toN64: math.N64.Value = math.N64.checkRange(toZ)

    final def toU8: math.U8.Value = math.U8.ValueImpl(UByte(toZ.toByte))

    final def toU16: math.U16.Value = math.U16.ValueImpl(UShort(toZ.toShort))

    final def toU32: math.U32.Value = math.U32.ValueImpl(UInt(toZ.toInt))

    final def toU64: math.U64.Value = math.U64.ValueImpl(ULong(toZ.toLong))
  }

  trait LogikaModuloIntegralNumber[V] extends LogikaIntegralNumber {
    def unary_~(): V

    def &(other: V): V

    def |(other: V): V

    def ^|(other: V): V

    def <<(distance: V): V

    def >>>(distance: V): V
  }

}
