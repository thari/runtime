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
    args: c.Expr[Any]*): c.Expr[Unit] =
    c.universe.reify {}

  def cImpl[T](c: scala.reflect.macros.blackbox.Context)(
    args: c.Expr[Any]*): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"???")
  }

  def recordImpl(c: scala.reflect.macros.whitebox.Context)(
    annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    def abort() =
      c.abort(c.enclosingPosition, "Invalid annotation target: not a Logika record")

    val result: c.Tree = annottees.map(_.tree).toList match {
      case ((r@q"sealed trait $tpname") :: _) => r
      case (q"case class $tpname[..$tparams](..$params) extends {} with ..$parents") :: _ =>
        var args: Vector[c.Tree] = Vector()
        var params2: Vector[c.Tree] = Vector()
        var args2: Vector[c.Tree] = Vector()
        for (param <- params) param match {
          case q"$_ val $param: $tpt = $_" =>
            args :+= q"$param.clone.asInstanceOf[$tpt]"
            params2 :+= q"val $param: $tpt = $param.clone.asInstanceOf[$tpt]"
            args2 :+= q"$param"
          case q"$_ var $param: $tpt = $_" =>
            args :+= q"$param.clone.asInstanceOf[$tpt]"
            params2 :+= q"val $param: $tpt = $param.clone.asInstanceOf[$tpt]"
            args2 :+= q"$param"
          case _ => abort()
        }
        val typeName = tpname.asInstanceOf[TypeName]
        val termName = typeName.toTermName
        val clone = q"override def clone: $typeName = $termName(..$args)"
        val apply = q"def apply(..$params2): $typeName = $termName(..$args2)"
        if (params.nonEmpty)
          q"""
              case class $tpname[..$tparams](..$params) extends {} with org.sireum.logika.Clonable with ..$parents {
                $clone
                $apply
              }
          """
        else
          q"""
              case class $tpname[..$tparams](..$params) extends {} with org.sireum.logika.Clonable with ..$parents {
                $clone
              }
          """
      case _ => abort()
    }
    c.Expr[Any](result)
  }

  def irecordImpl(c: scala.reflect.macros.whitebox.Context)(
    annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    def abort() =
      c.abort(c.enclosingPosition, "Invalid annotation target: not an immutable Logika record")

    def typeTreeOf[T: TypeTag] = reify(sys.error(""): T).tree.asInstanceOf[Typed].tpt.tpe

    def typeOf(t: Tree) = {
      val expr = c.Expr[Any](c.typecheck(q"""(sys.error(""): $t)"""))
      expr.actualType
    }

    val result: c.Tree = annottees.map(_.tree).toList match {
      case ((r@q"sealed trait $tpname") :: _) => r
      case (q"case class $tpname[..$tparams](..$params) extends {} with ..$parents") :: _ =>
        var args: Vector[c.Tree] = Vector()
        var params2: Vector[c.Tree] = Vector()
        var args2: Vector[c.Tree] = Vector()
        for (param <- params) param match {
          case q"$_ val $param: $tpt = $_" =>
            args :+= q"$param"
            params2 :+= q"val $param: $tpt = $param"
            args2 :+= q"$param"
          case _ => abort()
        }
        val typeName = tpname.asInstanceOf[TypeName]
        val termName = typeName.toTermName
        val clone = q"override def clone: $typeName = $termName(..$args)"
        val apply = q"def apply(..$params2): $typeName = $termName(..$args2)"
        if (params.nonEmpty)
          q"""
              case class $tpname[..$tparams](..$params) extends {} with org.sireum.logika.Clonable with ..$parents {
                $clone
                $apply
              }
          """
        else
          q"""
              case class $tpname[..$tparams](..$params) extends {} with org.sireum.logika.Clonable with ..$parents {
                $clone
              }
          """
      case _ => abort()
    }
    c.Expr[Any](result)
  }

  def enumImpl(c: scala.reflect.macros.whitebox.Context)(
    annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    def abort() =
      c.abort(c.enclosingPosition, "Invalid annotation target: not a Logika enum")

    val result: c.Tree = annottees.map(_.tree).toList match {
      case (q"object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }") :: _ =>
        q"""
            object $tname extends {
              type Type = Value
              ..$earlydefns
            } with Enumeration { $self => ..$body }
         """
      case _ => abort()
    }
    c.Expr[Any](result)
  }
}
