// #Sireum
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

@rich trait SOps[V] {

  @pure def contains(e: V): B

  @pure def forall(p: V => B): B

  @pure def exists(p: V => B): B

}

@rich trait ISOps[I <: Z, V] {

  @pure def dropRight(n: Z): IS[I, V]

}

@rich class ISZOps[V](s: IS[Z, V]) extends SOps[V] with ISOps[Z, V] {

  @pure def contains(e: V): B = {
    return exists((x: V) => x == e)
  }

  @pure def forall(p: V => B): B = {
    for (e <- s) {
      if (!p(e)) {
        return F
      }
    }
    return T
  }

  @pure def exists(p: V => B): B = {
    for (e <- s) {
      if (p(e)) {
        return T
      }
    }
    return F
  }

  @pure def dropRight(n: Z): IS[Z, V] = {
    return for (i <- 0 until s.size - n) yield s(i)
  }
}
