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

package org.sireum

import scala.language.experimental.macros

private[sireum] object _macro {
  def l(args: Any*): Unit = macro _macro.lImpl

  def lImpl(c: scala.reflect.macros.blackbox.Context)(
    args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"{}")
  }

  def c[T](args: Any*): T = macro _macro.cImpl[T]

  def cImpl[T](c: scala.reflect.macros.blackbox.Context)(
    args: c.Expr[Any]*): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def $[T]: T = macro _macro.$Impl[T]

  def $Impl[T](c: scala.reflect.macros.blackbox.Context): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def msApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[MS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import _Type._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      if (values.nonEmpty) c.abort(values.head.tree.pos, "Invalid index type for Slang MS.")
      c.abort(c.enclosingPosition, "Invalid index type for Slang MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.collection._MS(..$values)")
  }

  def msCreateImpl[I, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[MS[I, V]] = {
    val iType = size.tree.tpe.dealias.toString
    import _Type._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(size.tree.pos, "Invalid index type for Slang MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.collection._MS.create($size, $default)")
  }

  def isApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[IS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe.dealias.toString
    import _Type._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      if (values.nonEmpty) c.abort(values.head.tree.pos, "Invalid index type for Slang IS.")
      else c.abort(c.enclosingPosition, "Invalid index type for Slang IS.")
    }
    import c.universe._
    c.Expr[IS[I, V]](q"org.sireum.collection._IS(..$values)")
  }

  def isCreateImpl[I, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[IS[I, V]] = {
    val iType = size.tree.tpe.dealias.toString
    import _Type._
    if (!(iType match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    })) {
      c.abort(size.tree.pos, "Invalid index type for Slang IS.")
    }
    import c.universe._
    c.Expr[IS[I, V]](q"org.sireum.collection._IS.create($size, $default)")
  }

  def _assign[T](arg: T): T = macro _macro._assignImpl

  def _assignImpl(c: scala.reflect.macros.blackbox.Context)(
    arg: c.Tree): c.Tree = {
    import c.universe._
    //println(showRaw(arg))
    val r = arg.tpe match {
      case t if t <:< c.typeOf[_Datatype] => arg
      case t if t <:< c.typeOf[_Record] => q"__assign($arg)"
      case t if t.erasure =:= c.typeOf[MS[_, _]].erasure => q"__assign($arg)"
      case t if t.erasure =:= c.typeOf[IS[_, _]].erasure => arg
      case _ =>
        import _Type._
        arg.tpe.dealias.toString match {
          case `bType` | `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
               `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
               `s8Type` | `s16Type` | `s32Type` | `s64Type` |
               `u8Type` | `u16Type` | `u32Type` | `u64Type` |
               `f32Type` | `f64Type` | `rType` => arg
          case _ => q"__assign($arg)"
        }
    }
    //println(showRaw(r))
    //println(showCode(r))
    r
  }

  def _cleanup[T](arg: T): Unit = macro _cleanupImpl

  def _cleanupImpl(c: scala.reflect.macros.blackbox.Context)(
    arg: c.Tree): c.Tree = {
    import c.universe._
    //println(showRaw(arg))
    val r = arg.tpe match {
      case t if t <:< c.typeOf[_Datatype] => arg
      case t if t <:< c.typeOf[_Record] => q"__cleanup($arg)"
      case t if t.erasure =:= c.typeOf[MS[_, _]].erasure => q"__cleanup($arg)"
      case t if t.erasure =:= c.typeOf[IS[_, _]].erasure => arg
      case _ =>
        import _Type._
        arg.tpe.dealias.toString match {
          case `bType` | `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
               `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
               `s8Type` | `s16Type` | `s32Type` | `s64Type` |
               `u8Type` | `u16Type` | `u32Type` | `u64Type` |
               `f32Type` | `f64Type` | `rType` => arg
          case _ => q"__cleanup($arg)"
        }
    }
    //println(showRaw(r))
    //println(showCode(r))
    r
  }

  def up(c: scala.reflect.macros.blackbox.Context)(
    lhs: c.Tree, rhs: c.Tree): c.Tree = {
    import c.universe._
    def isVar(symbol: Symbol): Boolean = {
      val t = symbol.asTerm
      if (t.isVar) true
      else if (t.isGetter & t.setter.toString != "<none>") true
      else false
    }

    def varType(t: c.Tree): Option[c.Type] = t match {
      case q"${name: Ident}.$_" =>
        if (isVar(name.symbol)) Some(name.tpe) else None
      case q"${name: Ident}.apply[..$_]($_)($_)" =>
        if (isVar(name.symbol)) Some(name.tpe) else None
      case t@Select(This(_), _) =>
        if (isVar(t.symbol)) Some(t.tpe) else None
      case Apply(Select(t@Select(This(_), _), TermName("apply")), List(_)) =>
        if (isVar(t.symbol)) Some(t.tpe) else None
      case q"${expr: c.Tree}.$_" => varType(expr)
      case q"${expr: c.Tree}.apply[..$_]($arg)($_)" => varType(expr)
      case _ =>
        c.abort(t.pos, s"Unexpected left-hand side form: ${showRaw(t)}")
    }

    def f(t: c.Tree, r: c.Tree): c.Tree = t match {
      case q"${name: c.TermName}.$tname" =>
        q"$name = $name($tname = $r)"
      case q"${name: c.TermName}.apply[..$_]($arg)($_)" =>
        q"$name = $name($arg -> $r)"
      case q"$tpname.this.$name.$tname" =>
        q"$tpname.this.$name = $name($tname = $r)"
      case q"$tpname.this.$name.apply[..$_]($arg)($_)" =>
        q"$tpname.this.$name = this.$name($arg -> $r)"
      case q"${expr: c.Tree}.$tname" => f(expr, q"$expr($tname = $r)")
      case q"${expr: c.Tree}.apply[..$_]($arg)($_)" => f(expr, q"$expr($arg -> $r)")
      case _ =>
        c.abort(t.pos, s"Unexpected left-hand side form: ${showCode(t)}")
    }

    //println(showRaw(lhs))
    //println(showRaw(rhs))
    val tpe = varType(lhs)
    //println(t)
    val r = tpe match {
      case Some(t) =>
        if (t <:< c.typeOf[_Datatype] || t.erasure.dealias =:= c.typeOf[IS[_, _]].erasure) f(lhs, rhs)
        else c.abort(lhs.pos, s"Can only use 'up(...)' for expressions rooted in a @datatype var or a var of type IS.")
      case _ => c.abort(lhs.pos, s"Can only use 'up(...)' for expressions rooted in a var.")
    }
    //println(showRaw(r))
    //println(showCode(r))
    r
  }
}
