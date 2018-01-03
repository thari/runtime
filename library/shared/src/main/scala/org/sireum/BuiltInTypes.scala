/*
 Copyright (c) 2017, Robby, Kansas State University
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

package org.sireum

import org.sireum.$internal._

import scala.language.implicitConversions


trait Immutable extends Any with ImmutableMarker {

  def string: String

}


trait Number extends Any with Immutable


trait EnumSig extends Immutable {
  def numOfElements: Z

  def string: String = halt("Unsupported Enum operation 'string'.")
}

trait DatatypeSig extends Immutable with DatatypeMarker


trait RichSig extends Immutable


trait Mutable extends Any with MutableMarker {

  def string: String

}


trait RecordSig extends Mutable {

  private var isOwned: Boolean = false

  def owned: Boolean = isOwned

  def owned_=(b: Boolean): this.type = {
    isOwned = b
    this
  }

}


trait MSig extends Mutable


@range(min = -128, max = 127) class Z8

@range(min = -32768, max = 32767) class Z16

@range(min = -2147483648, max = 2147483647) class Z32

@range(min = -9223372036854775808l, max = 9223372036854775807l) class Z64

@range(min = 0) class N

@range(min = 0, max = 255) class N8

@range(min = 0, max = 65535) class N16

@range(min = 0, max = 4294967295l) class N32

@range(min = 0, max = z"18,446,744,073,709,551,617") class N64

@bits(signed = T, width = 8) class S8

@bits(signed = F, width = 8) class U8

@bits(signed = T, width = 16) class S16

@bits(signed = F, width = 16) class U16

@bits(signed = T, width = 32) class S32

@bits(signed = F, width = 32) class U32

@bits(signed = T, width = 64) class S64

@bits(signed = F, width = 64) class U64
