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

object ISOps_Ext {
  @pure def toMS[I, T](s: IS[I, T]): MS[I, T] = {
    new collection._MS[I, T](s.iTag, s.length, s.array.clone)
  }

  @pure def sortWith[I, T](s: IS[I, T], lt: (T, T) => B): IS[I, T] = {
    val newArray = new Array[Any](s.length)
    System.arraycopy(s.array, 0, newArray, 0, s.length)
    new collection._IS[I, T](s.iTag, s.length,
      newArray.sortWith((e1, e2) => lt(e1.asInstanceOf[T], e2.asInstanceOf[T])))
  }
}

object ISZOps_Ext {
  @pure def toMS[T](s: IS[Z, T]): MS[Z, T] = ISOps_Ext.toMS(s)

  @pure def sortWith[T](s: IS[Z, T], lt: (T, T) => B): IS[Z, T] = ISOps_Ext.sortWith(s, lt)
}
