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


  def recordImpl(c: scala.reflect.macros.whitebox.Context)(
    annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    def abort() =
      c.abort(c.enclosingPosition, "Invalid annotation target: not a Logika record")

    def typeTreeOf[T: TypeTag] = reify(sys.error(""): T).tree.asInstanceOf[Typed].tpt.tpe

    def typeOf(t: Tree) = {
      val expr = c.Expr[Any](c.typecheck(q"""(sys.error(""): $t)"""))
      expr.actualType
    }

    def shouldClone(t: c.universe.Type): Boolean =
      if (t <:< typeTreeOf[BS.Value]) true
      else if (t <:< typeTreeOf[ZS.Value]) true
      else if (t <:< typeTreeOf[Z8S.Value]) true
      else if (t <:< typeTreeOf[Z16S.Value]) true
      else if (t <:< typeTreeOf[Z32S.Value]) true
      else if (t <:< typeTreeOf[Z64S.Value]) true
      else if (t <:< typeTreeOf[NS.Value]) true
      else if (t <:< typeTreeOf[N8S.Value]) true
      else if (t <:< typeTreeOf[N16S.Value]) true
      else if (t <:< typeTreeOf[N32S.Value]) true
      else if (t <:< typeTreeOf[N64S.Value]) true
      else if (t <:< typeTreeOf[S8S.Value]) true
      else if (t <:< typeTreeOf[S16S.Value]) true
      else if (t <:< typeTreeOf[S32S.Value]) true
      else if (t <:< typeTreeOf[S64S.Value]) true
      else if (t <:< typeTreeOf[U8S.Value]) true
      else if (t <:< typeTreeOf[U16S.Value]) true
      else if (t <:< typeTreeOf[U32S.Value]) true
      else if (t <:< typeTreeOf[U64S.Value]) true
      else if (t <:< typeTreeOf[F32S.Value]) true
      else if (t <:< typeTreeOf[F64S.Value]) true
      else if (t <:< typeTreeOf[RS.Value]) true
      else if (t <:< typeTreeOf[scala.Product]) true
      else false

    val result: c.Tree = annottees.map(_.tree).toList match {
      case (record@q"case class $tpname(...$paramss) extends {} with $parent") :: _ =>
        val args = paramss map { paramList =>
          paramList.map {
            case q"$_ val $param: $tpt = $_" =>
              if (shouldClone(typeOf(tpt))) q"$param.clone"
              else q"$param"
            case q"$_ var $param: $tpt = $_" =>
              if (shouldClone(typeOf(tpt))) q"$param.clone"
              else q"$param"
            case _ => abort()
          }
        }
        val typeName = tpname.asInstanceOf[TypeName]
        val termName = typeName.toTermName
        val clone = q"override def clone: $typeName = $termName(...$args)"
        q"""
             case class $tpname(...$paramss) extends {} with $parent {
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

    def shouldClone(t: c.universe.Type): Boolean =
      if (t <:< typeTreeOf[BS.Value]) true
      else if (t <:< typeTreeOf[ZS.Value]) true
      else if (t <:< typeTreeOf[Z8S.Value]) true
      else if (t <:< typeTreeOf[Z16S.Value]) true
      else if (t <:< typeTreeOf[Z32S.Value]) true
      else if (t <:< typeTreeOf[Z64S.Value]) true
      else if (t <:< typeTreeOf[NS.Value]) true
      else if (t <:< typeTreeOf[N8S.Value]) true
      else if (t <:< typeTreeOf[N16S.Value]) true
      else if (t <:< typeTreeOf[N32S.Value]) true
      else if (t <:< typeTreeOf[N64S.Value]) true
      else if (t <:< typeTreeOf[S8S.Value]) true
      else if (t <:< typeTreeOf[S16S.Value]) true
      else if (t <:< typeTreeOf[S32S.Value]) true
      else if (t <:< typeTreeOf[S64S.Value]) true
      else if (t <:< typeTreeOf[U8S.Value]) true
      else if (t <:< typeTreeOf[U16S.Value]) true
      else if (t <:< typeTreeOf[U32S.Value]) true
      else if (t <:< typeTreeOf[U64S.Value]) true
      else if (t <:< typeTreeOf[F32S.Value]) true
      else if (t <:< typeTreeOf[F64S.Value]) true
      else if (t <:< typeTreeOf[RS.Value]) true
      else if (t <:< typeTreeOf[scala.Product]) true
      else false

    val result: c.Tree = annottees.map(_.tree).toList match {
      case (record@q"case class $tpname(...$paramss)") :: _ =>
        val args = paramss map { paramList =>
          paramList.map {
            case q"$_ val $param: $_ = $_" => q"$param"
            case _ => abort()
          }
        }
        val typeName = tpname.asInstanceOf[TypeName]
        val termName = typeName.toTermName
        val clone = q"override def clone: $typeName = $termName(...$args)"
        q"""
             case class $tpname(...$paramss) {
               $clone
             }
         """
      case (record@q"case class $tpname(...$paramss) extends {} with $parent") :: _ =>
        val args = paramss map { paramList =>
          paramList.map {
            case q"$_ val $param: $_ = $_" => q"$param"
            case _ => abort()
          }
        }
        val typeName = tpname.asInstanceOf[TypeName]
        val termName = typeName.toTermName
        val clone = q"override def clone: $typeName = $termName(...$args)"
        q"""
             case class $tpname(...$paramss) extends {} with $parent {
               $clone
             }
         """
      case _ => abort()
    }
    c.Expr[Any](result)
  }

}
