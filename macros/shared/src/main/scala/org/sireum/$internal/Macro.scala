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

package org.sireum.$internal

import scala.language.experimental.macros

object Macro {
  val templateString = "st\"...\""
}

import Macro._

class Macro(val c: scala.reflect.macros.blackbox.Context) {

  import c.universe._

  def l[T](args: c.Expr[Any]*): c.Expr[T] =
    c.Expr[T]( q"""halt("Slang l\"\"\"...\"\"\" should have been erased by the Sireum Scala plugin.")""")

  def lUnit(args: c.Expr[Any]*): c.Expr[Unit] = c.Expr[Unit](q"{}")

  def lDef[T](args: c.Expr[Any]*): c.Expr[T] =
    c.Expr[T]( q"""halt("Slang l\"\"\"...\"\"\" should have been erased by the Sireum Scala plugin.")""")

  def $[T]: c.Expr[T] = c.Expr[T]( q"""halt("Slang '$$' should have been erased by the Sireum Scala compiler plugin.")""")

  def extractParts: Seq[c.Tree] = c.prefix.tree match {
    case q"org.sireum.`package`.$$Slang(scala.StringContext.apply(..$ps)).$_" => ps
    case q"sireum.this.`package`.$$Slang(scala.StringContext.apply(..$ps)).$_" => ps
    case q"org.sireum.`package`.$$Slang(scala.StringContext.apply(..$ps))" => ps
    case q"sireum.this.`package`.$$Slang(scala.StringContext.apply(..$ps))" => ps
  }

  def zApply(args: c.Tree*): c.Tree = {
    val parts = extractParts
    if (parts.size != 1) c.abort(c.prefix.tree.pos, "Slang z\"...\" should not contain $$ arguments.")
    q"Z.String(${parts.head})"
  }

  def rApply(args: c.Tree*): c.Tree = {
    val parts = extractParts
    if (parts.size != 1) c.abort(c.prefix.tree.pos, "Slang r\"...\" should not contain $$ arguments.")
    q"R.String(${parts.head})"
  }

  def $assign(arg: c.Tree): c.Tree = {
    //    def args(n: Int): List[c.Tree] =
    //      (for (i <- 1 to n) yield
    //        Apply(Select(Ident(TermName("helper")), TermName("assign")), List(Select(arg, TermName(s"_$i"))))).toList

    //println(showRaw(arg))
    val r =
      if (arg.tpe <:< c.typeOf[MutableMarker]) q"helper.assign($arg)"
      //      else if (arg.tpe.typeSymbol.fullName.startsWith("scala.Tuple")) {
      //        val n = arg.tpe.typeSymbol.fullName.substring("scala.Tuple".length).toInt
      //        Apply(Select(Ident(TermName("scala")), TermName(s"Tuple$n")), args(n))
      //      }
      else arg
    //println(showRaw(r))
    //println(showCode(r))
    r
  }

  def up(lhs: c.Tree, rhs: c.Tree): c.Tree = {
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
      case q"${name: Ident}.apply[..$_]($_)" =>
        if (isVar(name.symbol)) Some(name.tpe) else None
      case t@Select(This(_), _) =>
        if (isVar(t.symbol)) Some(t.tpe) else None
      case Apply(Select(t@Select(This(_), _), TermName("apply")), List(_)) =>
        if (isVar(t.symbol)) Some(t.tpe) else None
      case Typed(e, t) => varType(e)
      case q"${expr: c.Tree}.$_" => varType(expr)
      case q"${expr: c.Tree}.apply[..$_]($arg)($_)" => varType(expr)
      case q"${expr: c.Tree}.apply[..$_]($arg)" => varType(expr)
      case _ =>
        c.abort(t.pos, s"Unexpected left-hand side form: ${showCode(t)}")
    }

    def f(t: c.Tree, r: c.Tree): c.Tree = t match {
      case q"${name: c.TermName}.$tname" =>
        q"$name = $name($tname = $r)"
      case q"${name: c.TermName}.apply[..$_]($arg)($_)" =>
        q"$name = $name(($arg, $r))"
      case q"${name: c.TermName}.apply[..$_]($arg)" =>
        q"$name = $name(($arg, $r))"
      case q"$tpname.this.$name.$tname" =>
        q"$tpname.this.$name = $name($tname = $r)"
      case q"$tpname.this.$name.apply[..$_]($arg)($_)" =>
        q"$tpname.this.$name = this.$name(($arg, $r))"
      case Typed(e, t) => f(e, r)
      case q"${expr: c.Tree}.$tname" => f(expr, q"$expr($tname = $r)")
      case q"${expr: c.Tree}.apply[..$_]($arg)($_)" => f(expr, q"$expr(($arg, $r))")
      case q"${expr: c.Tree}.apply[..$_]($arg)" => f(expr, q"$expr(($arg, $r))")
      case _ =>
        c.abort(t.pos, s"Unexpected left-hand side form: ${showCode(t)}")
    }

    //println(showRaw(lhs))
    //println(showRaw(rhs))
    val tpe = varType(lhs)
    //println(t)
    val r = tpe match {
      case Some(t) =>
        if (t <:< c.typeOf[DatatypeMarker] || t <:< c.typeOf[ISMarker]) f(lhs, rhs)
        else c.abort(lhs.pos, s"Can only use 'up(...)' for expressions rooted in a @datatype var or a var of type IS.")
      case _ => c.abort(lhs.pos, s"Can only use 'up(...)' for expressions rooted in a var.")
    }
    //println(showRaw(r))
    //println(showCode(r))
    r
  }

  def pat(args: c.Tree*): c.Tree = {
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

  def st(args: c.Tree*): c.Tree = {
    def processArg(e: c.Tree, sep: c.Tree): c.Tree = {
      val t = e.tpe.dealias
      val templ = c.typeOf[org.sireum.$internal.STMarker]
      val r =
        if (t <:< templ) q"ST.Templ(scala.Seq($e), $sep)"
        else if (t <:< c.typeOf[ISMarker] || t <:< c.typeOf[MSMarker]) {
          t.typeArgs.length match {
            case 1 if t.typeArgs.head <:< templ => q"ST.Templ($e.elements, $sep)"
            case 2 if t.typeArgs(1) <:< templ => q"ST.Templ($e.elements, $sep)"
            case _ => q"ST.Any($e.elements.map($$internal.Option.apply), $sep)"
          }
        } else if (t.erasure <:< c.typeOf[scala.collection.GenTraversableOnce[Any]].erasure) {
          if (t.typeArgs.head <:< templ) q"ST.Templ($e.toSeq, $sep)"
          else q"ST.Any($e.toSeq.map($$internal.Option.apply), $sep)"
        } else q"ST.Any(scala.Seq($$internal.Option($e)), $sep)"
      //println(showCode(r))
      r
    }

    //println(showRaw(c.prefix.tree))
    //println(showCode(c.prefix.tree))
    val pos = c.prefix.tree.pos
    val isSingle =
      if (pos.source.content.length >= pos.start + 5)
        pos.source.content.subSequence(pos.start, pos.start + 5).toString != "st\"\"\""
      else true
    val parts = {
      val ps = extractParts
      if (isSingle) ps.map(p => q"StringContext.treatEscapes($p)") else ps
    }
    val stArgs = for (arg <- args) yield arg match {
      case q"(..$exprs)" if exprs.size > 1 =>
        if (exprs.size != 2) c.abort(arg.pos, s"Expecting a pair instead of a ${exprs.size}-tuple.")
        val e = exprs(1)
        val t = e.tpe
        if (t <:< c.typeOf[Predef.String]) processArg(exprs.head, e)
        else if (t.typeSymbol.fullName == "org.sireum.String") processArg(exprs.head, q"$e.value")
        else c.abort(e.pos, s"Expecting a separator string instead of '${showCode(e)}'.")
      case _ =>
        processArg(arg, Literal(Constant("")))
    }
    val source = if (pos.isRange) {
      val text = pos.source.content
      val sb = new java.lang.StringBuilder
      for (_ <- 0 until pos.column - 1) sb.append(' ')
      for (i <- pos.start until pos.end) {
        sb.append(text(i))
      }
      sb.toString
    } else templateString
    q"ST(scala.Seq(..$parts), scala.Seq(..$stArgs), ${Literal(Constant(source))})"
  }
}

