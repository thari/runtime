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

import org.junit.Test
import scala.collection.mutable.{ListMap => LM}

class ZSTest {
  final val size = math.Z(1024)

  @Test
  def append(): Unit = {
    assert(ZS(1, 2, 3) == ZS(1, 2) :+ 3)
  }

  @Test
  def prepend(): Unit = {
    assert(ZS(1, 2, 3) == 1 +: ZS(2, 3))
  }

  @Test
  def zsArray(): Unit = {
    var i = math.Z.zero
    var append: ZS = new collection.ZSArray(new Array[Z](0))
    var prepend: ZS = new collection.ZSArray(new Array[Z](0))
    while (i < size) {
      append :+= i
      prepend +:= size - i - 1
      i += 1
    }
    assert(append == prepend)
  }

  @Test
  def zsImpl(): Unit = {
    var i = math.Z.zero
    var append: ZS = new collection.ZSImpl(LM(), 0)
    var prepend: ZS = new collection.ZSImpl(LM(), 0)
    while (i < size) {
      append :+= i
      prepend +:= size - i - 1
      i += 1
    }
    assert(append == prepend)
  }

  @Test
  def zsArrayImpl(): Unit = {
    var i = math.Z.zero
    var append: ZS = new collection.ZSImpl(LM(), 0)
    var prepend: ZS = new collection.ZSArray(new Array[Z](0))
    while (i < size) {
      append :+= i
      prepend +:= size - i - 1
      i += 1
    }
    assert(append == prepend)
  }

  @Test
  def zsImplArray(): Unit = {
    var i = math.Z.zero
    var append: ZS = new collection.ZSImpl(LM(), 0)
    var prepend: ZS = new collection.ZSArray(new Array[Z](0))
    while (i < size) {
      append :+= i
      prepend +:= size - i - 1
      i += 1
    }
    assert(append == prepend)
  }
}
