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

package org.sireum.ops

import org.sireum._

@rich trait SOps[I, T] {

  @pure def :+(e: T): IS[I, T]

  @pure def +:(e: T): IS[I, T]

  @pure def ++(other: IS[I, T]): IS[I, T]

  @pure def chunk(size: I): IS[Z, IS[I, T]]

  @pure def contains(e: T): B

  @pure def drop(size: I): IS[I, T]

  @pure def dropRight(size: I): IS[I, T]

  @pure def exists(p: T => B): B

  @pure def first: T

  @pure def forall(p: T => B): B

  @pure def foldLeft[R](f: (R, T) => R, init: R): R

  @pure def foldRight[R](f: (R, T) => R, init: R): R

  @pure def indexOf(e: T): Z

  @pure def insert(i: Z, e: T): IS[Z, T]

  @pure def last: T

  @pure def laxIndexOf(e: T): Z

  @pure def laxSlice(from: Z, til: Z): IS[Z, T]

  @pure def map[U](f: T => U): IS[I, U]

  @pure def remove(i: Z): IS[Z, T]

  @pure def slice(from: Z, til: Z): IS[Z, T]

  @pure def sortWith(lt: (T, T) => B): IS[I, T]

  @pure def tail: IS[Z, T]

  @pure def take(size: I): IS[I, T]

  @pure def takeRight(size: Z): IS[Z, T]

  @pure def toMS: MS[I, T]

}

@rich trait SBOps[I] {

  @pure def toU8: U8

  @pure def toU16: U16

  @pure def toU32: U32

  @pure def toU64: U64
}