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

object _macro {

  def $[T]: T = macro _macro.$Impl[T]

  def $Impl[T](c: scala.reflect.macros.blackbox.Context): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def msApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[MS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe

    if (!(iType =:= c.typeOf[Z] || iType =:= c.typeOf[Z8] || iType =:= c.typeOf[Z16] || iType =:= c.typeOf[Z32] || iType =:= c.typeOf[Z64] ||
      iType =:= c.typeOf[N] || iType =:= c.typeOf[N8] || iType =:= c.typeOf[N16] || iType =:= c.typeOf[N32] || iType =:= c.typeOf[N64] ||
      iType =:= c.typeOf[S8] || iType =:= c.typeOf[S16] || iType =:= c.typeOf[S32] || iType =:= c.typeOf[S64] ||
      iType =:= c.typeOf[U8] || iType =:= c.typeOf[U16] || iType =:= c.typeOf[U32] || iType =:= c.typeOf[U64])) {
      if (values.nonEmpty) c.abort(values.head.tree.pos, "Invalid index type for Slang MS.")
      c.abort(c.enclosingPosition, "Invalid index type for Slang MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.collection._MS(..$values)")
  }

  def msCreateImpl[I, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[MS[I, V]] = {
    val iType = size.tree.tpe
    if (!(iType =:= c.typeOf[Z] || iType =:= c.typeOf[Z8] || iType =:= c.typeOf[Z16] || iType =:= c.typeOf[Z32] || iType =:= c.typeOf[Z64] ||
      iType =:= c.typeOf[N] || iType =:= c.typeOf[N8] || iType =:= c.typeOf[N16] || iType =:= c.typeOf[N32] || iType =:= c.typeOf[N64] ||
      iType =:= c.typeOf[S8] || iType =:= c.typeOf[S16] || iType =:= c.typeOf[S32] || iType =:= c.typeOf[S64] ||
      iType =:= c.typeOf[U8] || iType =:= c.typeOf[U16] || iType =:= c.typeOf[U32] || iType =:= c.typeOf[U64])) {
      c.abort(size.tree.pos, "Invalid index type for Slang MS.")
    }
    import c.universe._
    c.Expr[MS[I, V]](q"org.sireum.collection._MS.create($size, $default)")
  }

  def isApplyImpl[I: c.WeakTypeTag, V](c: scala.reflect.macros.blackbox.Context)(
    values: c.Expr[V]*): c.Expr[IS[I, V]] = {
    val iType = implicitly[c.WeakTypeTag[I]].tpe
    if (!(iType =:= c.typeOf[Z] || iType =:= c.typeOf[Z8] || iType =:= c.typeOf[Z16] || iType =:= c.typeOf[Z32] || iType =:= c.typeOf[Z64] ||
      iType =:= c.typeOf[N] || iType =:= c.typeOf[N8] || iType =:= c.typeOf[N16] || iType =:= c.typeOf[N32] || iType =:= c.typeOf[N64] ||
      iType =:= c.typeOf[S8] || iType =:= c.typeOf[S16] || iType =:= c.typeOf[S32] || iType =:= c.typeOf[S64] ||
      iType =:= c.typeOf[U8] || iType =:= c.typeOf[U16] || iType =:= c.typeOf[U32] || iType =:= c.typeOf[U64])) {
      if (values.nonEmpty) c.abort(values.head.tree.pos, "Invalid index type for Slang IS.")
      else c.abort(c.enclosingPosition, "Invalid index type for Slang IS.")
    }
    import c.universe._
    c.Expr[IS[I, V]](q"org.sireum.collection._IS(..$values)")
  }

  def isCreateImpl[I, V](c: scala.reflect.macros.blackbox.Context)(
    size: c.Expr[I], default: c.Expr[V]): c.Expr[IS[I, V]] = {
    val iType = size.tree.tpe
    if (!(iType =:= c.typeOf[Z] || iType =:= c.typeOf[Z8] || iType =:= c.typeOf[Z16] || iType =:= c.typeOf[Z32] || iType =:= c.typeOf[Z64] ||
      iType =:= c.typeOf[N] || iType =:= c.typeOf[N8] || iType =:= c.typeOf[N16] || iType =:= c.typeOf[N32] || iType =:= c.typeOf[N64] ||
      iType =:= c.typeOf[S8] || iType =:= c.typeOf[S16] || iType =:= c.typeOf[S32] || iType =:= c.typeOf[S64] ||
      iType =:= c.typeOf[U8] || iType =:= c.typeOf[U16] || iType =:= c.typeOf[U32] || iType =:= c.typeOf[U64])) {
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
    val r = if (arg.tpe <:< c.typeOf[_Mutable]) q"__assign($arg)" else arg
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
      case Typed(e, t) => varType(e)
      case q"${expr: c.Tree}.$_" => varType(expr)
      case q"${expr: c.Tree}.apply[..$_]($arg)($_)" => varType(expr)
      case _ =>
        c.abort(t.pos, s"Unexpected left-hand side form: ${showCode(t)}")
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
      case Typed(e, t) => f(e, r)
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

  def pat(c: scala.reflect.macros.blackbox.Context)(
    args: c.Tree*): c.Tree = {
    import c.universe._
    if (args.last.tpe <:< c.typeOf[Product]) {
      if (args.length < 3 || !args.dropRight(1).forall({
        case Ident(TermName(_)) => true
        case _ => false
      })) {
        c.abort(c.enclosingPosition, "Invalid tuple pattern form; it should be: pat(<id>, <id>+) = <exp>.")
      }
      val lhss = args.dropRight(1)
      val rhs = args.last
      var assigns = List[c.Tree](q"val _tmp = $rhs")
      for (i <- lhss.indices) {
        assigns ::= q"${lhss(i)} = _tmp.${TermName("_" + (i + 1))}"
      }
      val r = Block(assigns.reverse, Literal(Constant(())))
      //println(showRaw(r))
      //println(showCode(r))
      r
    } else {
      if (args.length != 2) {
        c.abort(c.enclosingPosition, "Invalid extraction pattern form; it should be: pat(<pattern>) = <exp>.")
      }
      var names = Map[TermName, TermName]()

      def f(e: Any): c.Tree = {
        e match {
          case q"$expr.apply(...$exprss)" if exprss.size == 1 =>
            val exprs2 = for (exprs <- exprss) yield for (exp <- exprs) yield f(exp)
            pq"$expr(..${exprs2.head})"
          case q"(..$exprs)" if exprs.size > 1 =>
            pq"(..${for (expr <- exprs) yield f(expr)})"
          case q"${name: TermName}" =>
            val id = name.decodedName.toString
            if (id.startsWith("_placeholder")) {
              pq"_"
            } else {
              val e2 = TermName(s"_$id")
              names += e2 -> name
              pq"$e2"
            }
          case e: c.Tree => c.abort(e.pos, s"Invalid extraction pattern: ${showCode(e)}")
        }
      }

      //println(showRaw(args.head))
      val tmp = q"val ${f(args.head)} = ${args(1)}".asInstanceOf[Block].stats
      var assigns = List[c.Tree]()
      for ((rhs, lhs) <- names) {
        assigns ::= q"${Ident(lhs)} = ${Ident(rhs)}"
      }
      val r = Block(tmp ++ assigns, Literal(Constant(())))
      //println(showRaw(r))
      //println(showCode(r))
      r
    }
  }
}
