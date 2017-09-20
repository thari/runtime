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

object ISOps_Ext {
  def mParMapFoldLeft[I, V, U, R](s: IS[I, V], f: V => U, g: (R, U) => R, init: R): R = {
    val elements = s.elements
    val ies = elements.indices.zip(elements)
    val irs = $internal.Macro.par(ies).map { p => (p._1, f(p._2)) }
    val a = new Array[scala.Any](irs.length)
    irs.foreach { p => a(p._1) = p._2 }
    a.foldLeft(init)((r, u) => g(r, u.asInstanceOf[U]))
  }

  def mParMapFoldRight[I, V, U, R](s: IS[I, V], f: V => U, g: (R, U) => R, init: R): R = {
    val elements = s.elements
    val ies = elements.indices.zip(elements)
    val irs = $internal.Macro.par(ies).map { p => (p._1, f(p._2)) }
    val a = new Array[scala.Any](irs.length)
    irs.foreach { p => a(p._1) = p._2 }
    a.foldRight(init)((u, r) => g(r, u.asInstanceOf[U]))
  }

  @pure def sortWith[I, V](s: IS[I, V], lt: (V, V) => B): IS[I, V] = {
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
  def mParMapFoldLeft[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = ISOps_Ext.mParMapFoldLeft(s, f, g, init)

  def parMapFoldLeft[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = ISOps_Ext.mParMapFoldLeft(s, f, g, init)

  def mParMapFoldRight[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = ISOps_Ext.mParMapFoldRight(s, f, g, init)

  def parMapFoldRight[V, U, R](s: IS[Z, V], f: V => U, g: (R, U) => R, init: R): R = ISOps_Ext.mParMapFoldRight(s, f, g, init)

  @pure def sortWith[V](s: IS[Z, V], lt: (V, V) => B): IS[Z, V] = ISOps_Ext.sortWith(s, lt)
}
