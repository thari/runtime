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

package org.sireumproto

import org.sireumproto.$internal._

import scala.language.implicitConversions


trait Immutable extends Any with Clonable {

  def isEqual(other: Immutable): B

  def hash: Z

  def string: String

  def $clone: Any = this

}


trait Ordered[O <: Ordered[O]] extends Any with Immutable {

  def <(other: O): B

  def <=(other: O): B

  def >(other: O): B

  def >=(other: O): B

}


trait Number[N <: Number[N]] extends Any with Ordered[N] {

  def +(other: N): N

  def -(other: N): N

  def *(other: N): N

  def /(other: N): N

  def %(other: N): N

}


trait Datatype extends Immutable with DatatypeMarker


trait Rich extends Immutable


trait Sig extends Immutable



trait Mutable extends Any with MutableMarker {

  def string: String

  def isEqual(other: Mutable): B

  def hash: Z

}


trait MOrdered[O <: MOrdered[O]] extends Any with Mutable {

  def <(other: O): B

  def <=(other: O): B

  def >(other: O): B

  def >=(other: O): B

}


trait Record extends Mutable {

  private var isOwned: Boolean = false

  def owned: Boolean = isOwned
  def owned_=(b: Boolean): this.type = {
    isOwned = b
    this
  }

}


trait MSig extends Mutable
