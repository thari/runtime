// #Sireum
package org.sireum

import U32._
import U64._

object Hash {

  @pure def murmur3a(data: ISZ[U8], seed: U32): U32 = {

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

    @pure def process(d0: U32, h: U32): U32 = {
      var d = d0
      d = d * u32"0xCC9E2D51"
      d = (d << u32"15") | (d >>> u32"17")
      d = d * u32"0x1B873593"
      return h |^ d
    }

    val dataSize = data.size
    var h = seed

    var i: Z = 0
    if (dataSize > 3) {
      do {
        val d =
          conversions.U8.toU32(data(i)) |
            (conversions.U8.toU32(data(i + 1)) << u32"8") |
            (conversions.U8.toU32(data(i + 2)) << u32"16") |
            (conversions.U8.toU32(data(i + 3)) << u32"24")
        i = i + 4
        h = process(d, h)
        h = (h << u32"13") | (h >> u32"19")
        h = (h * u32"5") + u32"0xE6546B64"
      } while (dataSize - i >= 4)
    }

    dataSize - i match {
      case z"1" =>
        val d = conversions.U8.toU32(data(i))
        h = process(d, h)
      case z"2" =>
        val d = conversions.U8.toU32(data(i)) |
          (conversions.U8.toU32(data(i + 1)) << u32"8")
        h = process(d, h)
      case z"3" =>
        val d = conversions.U8.toU32(data(i)) |
          (conversions.U8.toU32(data(i + 1)) << u32"8") |
          (conversions.U8.toU32(data(i + 2)) << u32"16")
        h = process(d, h)
      case _ =>
    }

    h = h |^ conversions.Z.toU32(dataSize)
    h = h |^ (h >> u32"16")
    h = h * u32"0x85EBCA6B"
    h = h |^ (h >> u32"13")
    h = h * u32"0xC2B2AE35"
    h = h |^ (h >> u32"16")

    return h
  }

  @pure def t1ha0(data: ISZ[U8], seed: U64): U64 = {

    // Adapted from: https://github.com/leo-yuriev/t1ha
    /*
     *  Copyright (c) 2016-2018 Positive Technologies, https://www.ptsecurity.com,
     *  Fast Positive Hash.
     *
     *  Portions Copyright (c) 2010-2018 Leonid Yuriev <leo@yuriev.ru>,
     *  The 1Hippeus project (t1h).
     *
     *  This software is provided 'as-is', without any express or implied
     *  warranty. In no event will the authors be held liable for any damages
     *  arising from the use of this software.
     *
     *  Permission is granted to anyone to use this software for any purpose,
     *  including commercial applications, and to alter it and redistribute it
     *  freely, subject to the following restrictions:
     *
     *  1. The origin of this software must not be misrepresented; you must not
     *     claim that you wrote the original software. If you use this software
     *     in a product, an acknowledgement in the product documentation would be
     *     appreciated but is not required.
     *  2. Altered source versions must be plainly marked as such, and must not be
     *     misrepresented as being the original software.
     *  3. This notice may not be removed or altered from any source distribution.
     */

    @pure def rot(n: U32, m: U32): U32 = {
      return (n >>> m) | (n << (u32"32" - m))
    }

    @pure def fetch(i: Z): U32 = {
      data.size - i match {
        case z"1" =>
          return conversions.U8.toU32(data(i))
        case z"2" =>
          return conversions.U8.toU32(data(i)) |
            conversions.U8.toU32(data(i + 1)) << u32"8"
        case z"3" =>
          return conversions.U8.toU32(data(i)) |
            conversions.U8.toU32(data(i + 1)) << u32"8" |
            conversions.U8.toU32(data(i + 2)) << u32"16"
        case _ =>
          return conversions.U8.toU32(data(i)) |
            conversions.U8.toU32(data(i + 1)) << u32"8" |
            conversions.U8.toU32(data(i + 2)) << u32"16" |
            conversions.U8.toU32(data(i + 3)) << u32"24"
      }
    }

    val prime0: U32 = u32"0x92D78269"
    val prime1: U32 = u32"0xCA9B4735"
    val prime2: U32 = u32"0xA4ABA1C3"
    val prime3: U32 = u32"0xF6499843"
    val prime4: U32 = u32"0x86F0FD61"
    val prime5: U32 = u32"0xCA2DA6FB"
    val prime6: U32 = u32"0xC4BB3575"

    val dataSize = data.size
    val dataSize32 = conversions.Z.toU32(dataSize)

    var a: U32 = rot(dataSize32, u32"17") + conversions.U64.toU32(seed & u64"0xFFFFFFFF")
    var b: U32 = dataSize32 |^ conversions.U64.toU32(seed >>> u64"32")

    var i = 0
    if (dataSize > 16) {
      var c = ~a
      var d = rot(b, u32"5")

      do {
        val w0 = fetch(i)
        val w1 = fetch(i + 4)
        val w2 = fetch(i + 8)
        val w3 = fetch(i + 12)

        val c02 = w0 |^ rot(w2 + c, u32"11")
        val d13 = w1 + rot(w3 + d, u32"17")
        c = c |^ rot(b + w1, u32"7")
        d = d |^ rot(a + w0, u32"3")
        b = prime1 * (c02 + w3)
        a = prime0 * (d13 |^ w2)
        i = i + 16
      } while (dataSize - i >= 16)

      c = c + a
      d = d + b
      a = a |^ (prime6 * (rot(c, u32"16") + d))
      b = b |^ (prime5 * (c + rot(d, u32"16")))
    }

    val len = dataSize - i
    (len - 1) / 4 match {
      case z"3" =>
        var l = conversions.U32.toU64(b + fetch(i)) * conversions.U32.toU64(prime4)
        a = a |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        b = b + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime3)
        b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        a = a + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(b + fetch(i)) * conversions.U32.toU64(prime2)
        a = a |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        b = b + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime1)
        b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        a = a + conversions.U64.toU32(l >>> u64"32")
      case z"2" =>
        var l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime3)
        b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        a = a + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(b + fetch(i)) * conversions.U32.toU64(prime2)
        a = a |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        b = b + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime1)
        b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        a = a + conversions.U64.toU32(l >>> u64"32")
      case z"1" =>
        var l = conversions.U32.toU64(b + fetch(i)) * conversions.U32.toU64(prime2)
        a = a |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        b = b + conversions.U64.toU32(l >>> u64"32")
        i = i + 4
        l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime1)
        b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
        a = a + conversions.U64.toU32(l >>> u64"32")
      case _ =>
        if (len > 0) {
          val l = conversions.U32.toU64(a + fetch(i)) * conversions.U32.toU64(prime1)
          b = b |^ conversions.U64.toU32(l & u64"0xFFFFFFFF")
          a = a + conversions.U64.toU32(l >>> u64"32")
        }
    }

    var l = conversions.U32.toU64(b |^ rot(a, u32"13")) | (conversions.U32.toU64(a) << u64"32")
    l = l * u64"0xEC99BF0D8372CAAB"
    l = l |^ (l >> u64"41")
    l = l * u64"0x9C06FAF4D023E3AB"
    l = l |^ (l >> u64"47")
    l = l * u64"0xCB5AF53AE3AAAC31"

    return l
  }

  @pure def t1ha1(data: ISZ[U8], seed: U64): U64 = {

    @pure def mux(a: U64, b: U64): U64 = {
      // Adapted from: https://golang.org/src/runtime/softfloat64.go (mullu)
      /*
        Copyright (c) 2009 The Go Authors. All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions are
        met:

        * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following disclaimer
        in the documentation and/or other materials provided with the
        distribution.
        * Neither the name of Google Inc. nor the names of its
        contributors may be used to endorse or promote products derived from
        this software without specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
       */

      val s = u64"32"
      val mask = (u64"1" << s) - u64"1"
      val a0 = a & mask
      val a1 = a >> s
      val b0 = b & mask
      val b1 = b >> s
      val w0 = a0 * b0
      val t = a1 * b0 + (w0 >> s)
      var w1 = t & mask
      val w2 = t >> s
      w1 = w1 + a0 * b1
      return (a * b) |^ (a1 * b1 + w2 + (w1 >> s))
    }

    // Adapted from: https://github.com/leo-yuriev/t1ha
    /*
     *  Copyright (c) 2016-2018 Positive Technologies, https://www.ptsecurity.com,
     *  Fast Positive Hash.
     *
     *  Portions Copyright (c) 2010-2018 Leonid Yuriev <leo@yuriev.ru>,
     *  The 1Hippeus project (t1h).
     *
     *  This software is provided 'as-is', without any express or implied
     *  warranty. In no event will the authors be held liable for any damages
     *  arising from the use of this software.
     *
     *  Permission is granted to anyone to use this software for any purpose,
     *  including commercial applications, and to alter it and redistribute it
     *  freely, subject to the following restrictions:
     *
     *  1. The origin of this software must not be misrepresented; you must not
     *     claim that you wrote the original software. If you use this software
     *     in a product, an acknowledgement in the product documentation would be
     *     appreciated but is not required.
     *  2. Altered source versions must be plainly marked as such, and must not be
     *     misrepresented as being the original software.
     *  3. This notice may not be removed or altered from any source distribution.
     */

    @pure def rot(n: U64, m: U64): U64 = {
      return (n >>> m) | (n << (u64"64" - m))
    }

    @pure def mix(v: U64, p: U64): U64 = {
      val v2 = v * p
      return v2 |^ rot(v2, u64"41")
    }

    @pure def fetch(i: Z): U64 = {
      data.size - i match {
        case z"1" =>
          return conversions.U8.toU64(data(i))
        case z"2" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8"
        case z"3" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16"
        case z"4" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16" |
            conversions.U8.toU64(data(i + 3)) << u64"24"
        case z"5" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16" |
            conversions.U8.toU64(data(i + 3)) << u64"24" |
            conversions.U8.toU64(data(i + 4)) << u64"32"
        case z"6" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16" |
            conversions.U8.toU64(data(i + 3)) << u64"24" |
            conversions.U8.toU64(data(i + 4)) << u64"32" |
            conversions.U8.toU64(data(i + 5)) << u64"40"
        case z"7" =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16" |
            conversions.U8.toU64(data(i + 3)) << u64"24" |
            conversions.U8.toU64(data(i + 4)) << u64"32" |
            conversions.U8.toU64(data(i + 5)) << u64"40" |
            conversions.U8.toU64(data(i + 6)) << u64"48"
        case _ =>
          return conversions.U8.toU64(data(i)) |
            conversions.U8.toU64(data(i + 1)) << u64"8" |
            conversions.U8.toU64(data(i + 2)) << u64"16" |
            conversions.U8.toU64(data(i + 3)) << u64"24" |
            conversions.U8.toU64(data(i + 4)) << u64"32" |
            conversions.U8.toU64(data(i + 5)) << u64"40" |
            conversions.U8.toU64(data(i + 6)) << u64"48" |
            conversions.U8.toU64(data(i + 7)) << u64"56"
      }
    }

    val prime0: U64 = u64"0xEC99BF0D8372CAAB"
    val prime1: U64 = u64"0x82434FE90EDCEF39"
    val prime2: U64 = u64"0xD4F06DB99D67BE4B"
    val prime3: U64 = u64"0xBD9CACC22C6E9571"
    val prime4: U64 = u64"0x9C06FAF4D023E3AB"
    val prime5: U64 = u64"0xC060724A8424F345"
    val prime6: U64 = u64"0xCB5AF53AE3AAAC31"

    val dataSize = data.size
    val dataSize64 = conversions.Z.toU64(dataSize)

    var a: U64 = seed
    var b: U64 = conversions.Z.toU64(dataSize)

    var i = 0
    if (dataSize > 32) {
      var c = rot(dataSize64, u64"17") + seed
      var d = dataSize64 |^ rot(seed, u64"17")

      do {
        val w0 = fetch(i)
        val w1 = fetch(i + 8)
        val w2 = fetch(i + 16)
        val w3 = fetch(i + 24)

        val d02 = w0 |^ rot(w2 + d, u64"17")
        val c13 = w1 |^ rot(w3 + c, u64"17")
        c = c + (a |^ rot(w0, u64"41"))
        d = d - (b |^ rot(w1, u64"31"))
        a = a |^ (prime1 * (d02 + w3))
        b = b |^ (prime0 * (c13 + w2))
        i = i + 32
      } while (dataSize - i >= 32)

      a = a |^ (prime6 * (rot(c, u64"17") + d))
      b = b |^ (prime5 * (c + rot(d, u64"17")))
    }

    val len = dataSize - i
    (len - 1) / 8 match {
      case z"3" =>
        b = b + mux(fetch(i), prime4)
        i = i + 8
        a = a + mux(fetch(i), prime3)
        i = i + 8
        b = b + mux(fetch(i), prime2)
        i = i + 8
        a = a + mux(fetch(i), prime1)
      case z"2" =>
        a = a + mux(fetch(i), prime3)
        i = i + 8
        b = b + mux(fetch(i), prime2)
        i = i + 8
        a = a + mux(fetch(i), prime1)
      case z"1" =>
        b = b + mux(fetch(i), prime2)
        i = i + 8
        a = a + mux(fetch(i), prime1)
      case _ =>
        if (len > 0) {
          a = a + mux(fetch(i), prime1)
        }
    }

    return mux(rot(a + b, u64"17"), prime4) + mix(a |^ b, prime0)
  }

}
