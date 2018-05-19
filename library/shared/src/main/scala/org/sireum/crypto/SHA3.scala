// #Sireum
/*
 Copyright (c) 2018, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.crypto

import org.sireum._
import org.sireum.U8._
import org.sireum.U64._

// Adapted from https://github.com/brainhub/SHA3IUF

object SHA3 {

  @enum object Variant {
    'V256
    'V384
    'V512
  }

  val spongeWords: Z = 25
  val rounds: Z = 24

  // @formatter:off
  val rndc: ISZ[U64] = ISZ(
    u64"0x0000000000000001", u64"0x0000000000008082",
    u64"0x800000000000808a", u64"0x8000000080008000",
    u64"0x000000000000808b", u64"0x0000000080000001",
    u64"0x8000000080008081", u64"0x8000000000008009",
    u64"0x000000000000008a", u64"0x0000000000000088",
    u64"0x0000000080008009", u64"0x000000008000000a",
    u64"0x000000008000808b", u64"0x800000000000008b",
    u64"0x8000000000008089", u64"0x8000000000008003",
    u64"0x8000000000008002", u64"0x8000000000000080",
    u64"0x000000000000800a", u64"0x800000008000000a",
    u64"0x8000000080008081", u64"0x8000000000008080",
    u64"0x0000000080000001", u64"0x8000000080008008"
  )

  val rotc: ISZ[U64] = ISZ(
    u64"1", u64"3", u64"6", u64"10", u64"15", u64"21", u64"28", u64"36",
    u64"45", u64"55", u64"2", u64"14", u64"27", u64"41", u64"56", u64"8",
    u64"25", u64"43", u64"62", u64"18", u64"39", u64"61", u64"20", u64"44"
  )

  val piln: ISZ[Z] = ISZ(
    10,  7, 11, 17, 18,  3,  5, 16,
     8, 21, 24,  4, 15, 23, 19, 13,
    12,  2, 20, 14, 22,  9,  6,  1
  )
  // @formatter:on

  @pure def rotl(x: U64, y: U64): U64 = {
    return (x << y) | (x >> (u64"64" - y))
  }

  def keccakf(s: MSZ[U64]): Unit = {
    var t = u64"0"
    val bc = MSZ.create(5, u64"0")

    for (round <- z"0" until rounds) {
      /* Theta */
      for (i <- 0 until 5) {
        bc(i) = s(i) |^ s(i + 5) |^ s(i + 10) |^ s(i + 15) |^ s(i + 20)
      }

      for (i <- 0 until 5) {
        t = bc((i + 4) % 5) |^ rotl(bc((i + 1) % 5), u64"1")
        for (j <- 0 until 25 by 5) {
          s(j + i) = s(j + i) |^ t
        }
      }

      /* Rho Pi */
      t = s(1)
      for (i <- 0 until 24) {
        val j = piln(i)
        bc(0) = s(j)
        s(j) = rotl(t, rotc(i))
        t = bc(0)
      }

      /* Chi */
      for (j <- 0 until 25 by 5) {
        for (i <- 0 until 5) {
          bc(i) = s(j + i)
        }
        for (i <- 0 until 5) {
          s(j + i) = s(j + i) |^ ((~bc((i + 1) % 5)) & bc((i + 2) % 5))
        }
      }

      /* Iota */
      s(0) = s(0) |^ rndc(round)
    }
  }

  @pure def init(variant: Variant.Type): SHA3 = {
    val bits: Z = variant match {
      case Variant.V256 => 256
      case Variant.V384 => 384
      case Variant.V512 => 512
    }
    return SHA3(2 * bits / 64)
  }

  @pure def sum(variant: Variant.Type, data: ISZ[U8]): ISZ[U8] = {
    val sha3 = init(variant)
    sha3.update(data)
    val r = sha3.finalise()
    return r
  }
}

import SHA3._

@record class SHA3(capacityWords: Z) {
  var saved: U64 = u64"0"
  var byteIndex: U64 = u64"0"
  var wordIndex: Z = 0
  var s: MSZ[U64] = MSZ.create(25, u64"0")

  def update(buf: ISZ[U8]): Unit = {

    assert(byteIndex < u64"8")
    assert(wordIndex < 25)

    var oldTail = (8 - conversions.U64.toZ(byteIndex)) % 8
    var len = buf.size

    var index = 0
    if (len < oldTail) {
      while (len > 0) {
        saved = saved | (conversions.U8.toU64(buf(index)) << (byteIndex * u64"8"))
        byteIndex = byteIndex + u64"1"
        index = index + 1
        len = len - 1
      }
      assert(byteIndex < u64"8")
      return
    }

    if (oldTail > 0) {
      len = len - oldTail
      while (oldTail > 0) {
        saved = saved | (conversions.U8.toU64(buf(index)) << (byteIndex * u64"8"))
        byteIndex = byteIndex + u64"1"
        index = index + 1
        oldTail = oldTail - 1
      }
      s(wordIndex) = s(wordIndex) |^ saved
      assert(byteIndex == u64"8")
      byteIndex = u64"0"
      saved = u64"0"
      wordIndex = wordIndex + 1
      if (wordIndex == spongeWords - capacityWords) {
        keccakf(s)
        wordIndex = 0
      }
    }

    assert(byteIndex == u64"0")

    val words = len / 8
    for (_ <- z"0" until words) {
      val t = conversions.U8.toU64(buf(index)) |
        (conversions.U8.toU64(buf(index + 1)) << u64"8") |
        (conversions.U8.toU64(buf(index + 2)) << u64"16") |
        (conversions.U8.toU64(buf(index + 3)) << u64"24") |
        (conversions.U8.toU64(buf(index + 4)) << u64"32") |
        (conversions.U8.toU64(buf(index + 5)) << u64"40") |
        (conversions.U8.toU64(buf(index + 6)) << u64"48") |
        (conversions.U8.toU64(buf(index + 7)) << u64"56")
      s(wordIndex) = s(wordIndex) |^ t
      wordIndex = wordIndex + 1
      if (wordIndex == spongeWords - capacityWords) {
        keccakf(s)
        wordIndex = 0
      }
      index = index + 8
    }

    var tail = len - words * 8
    assert(byteIndex == u64"0" && tail < 8)
    while (tail > 0) {
      saved = saved | (conversions.U8.toU64(buf(index)) << (byteIndex * u64"8"))
      byteIndex = byteIndex + u64"1"
      index = index + 1
      tail = tail - 1
    }
    assert(byteIndex < u64"8")
  }

  def finalise(): ISZ[U8] = {
    s(wordIndex) = s(wordIndex) |^ (saved |^ ((u64"0x02" | (u64"1" << u64"2")) << (byteIndex * u64"8")))
    s(spongeWords - capacityWords - 1) = s(spongeWords - capacityWords - 1) |^ u64"0x8000000000000000"
    keccakf(s)

    val sb = MSZ.create(spongeWords * 8, u8"0")
    for (i <- z"0" until spongeWords) {
      val t = s(i)
      sb(i * 8) = conversions.U64.toU8(t & u64"0xFF")
      sb(i * 8 + 1) = conversions.U64.toU8((t >> u64"8") & u64"0xFF")
      sb(i * 8 + 2) = conversions.U64.toU8((t >> u64"16") & u64"0xFF")
      sb(i * 8 + 3) = conversions.U64.toU8((t >> u64"24") & u64"0xFF")
      sb(i * 8 + 4) = conversions.U64.toU8((t >> u64"32") & u64"0xFF")
      sb(i * 8 + 5) = conversions.U64.toU8((t >> u64"40") & u64"0xFF")
      sb(i * 8 + 6) = conversions.U64.toU8((t >> u64"48") & u64"0xFF")
      sb(i * 8 + 7) = conversions.U64.toU8((t >> u64"56") & u64"0xFF")
    }
    return ops.ISZOps(sb.toIS).take(capacityWords * 4)
  }
}
