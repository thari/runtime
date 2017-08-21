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

package org.sireumproto.ops

import org.sireumproto._

object ISOps_Ext {
  @pure def toMS[I <: Z, V](s: IS[I, V]): MS[I, V] = {
    new MS[I, V](s.companion, s.boxer.cloneMut(s.data, s.length, s.length, Z.MP.zero), s.length, s.boxer)
  }

  @pure def sortWith[I <: Z, V](s: IS[I, V], lt: (V, V) => B): IS[I, V] = {
    val es = s.elements.sortWith((e1, e2) => lt(e1, e2).value)
    val a = s.boxer.create(s.length)
    var i = Z.MP.zero
    for (e <- es) {
      s.boxer.store(a, i, e)
      i = i.increase
    }
    new IS[I, V](s.companion, a, s.length, s.boxer)
  }
}

object ISZOps_Ext {
  @pure def toMS[V](s: IS[Z, V]): MS[Z, V] = ISOps_Ext.toMS(s)

  @pure def sortWith[V](s: IS[Z, V], lt: (V, V) => B): IS[Z, V] = ISOps_Ext.sortWith(s, lt)
}
