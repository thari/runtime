/*
 * Copyright (c) 2017, Robby, Kansas State University
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

object _macro {
  def lImpl(c: scala.reflect.macros.blackbox.Context)(
    args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"{}")
  }

  def cImpl[T](c: scala.reflect.macros.blackbox.Context)(
    args: c.Expr[Any]*): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def $Impl[T](c: scala.reflect.macros.blackbox.Context): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def msApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[MS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import collection._S._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(c.enclosingPosition, "Invalid index type for Logika MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.logika.collection._MS(..$values)")
  }

  def msCreateImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[MS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import collection._S._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(c.enclosingPosition, "Invalid index type for Logika MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.logika.collection._MS.create($size, $default)")
  }

  def isApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[IS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import collection._S._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(c.enclosingPosition, "Invalid index type for Logika IS.")
    }
    import c.universe._
    c.Expr[IS[I, V]](q"org.sireum.logika.collection._IS(..$values)")
  }

  def isCreateImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[IS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import collection._S._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(c.enclosingPosition, "Invalid index type for Logika IS.")
    }
    import c.universe._
    c.Expr[IS[I, V]](q"org.sireum.logika.collection._IS.create($size, $default)")
  }
}
