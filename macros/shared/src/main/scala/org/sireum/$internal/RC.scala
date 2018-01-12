/*
 * Copyright (c) 2016, Robby, Kansas State University
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

package org.sireum.$internal

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.github.marklister.base64.Base64

import scala.language.experimental.macros
import scala.collection.mutable.{Map => MMap}

object RC {
  def toTrie(m: scala.collection.Map[Seq[String], String]): Trie.Node[String, String] = {
    var root = Trie.InNode[String, String](MMap())

    def add(path: Seq[String], content: String, node: Trie.InNode[String, String]): Unit = path match {
      case Seq(s) => node.children += ((s, Trie.Leaf[String, String](content)))
      case _ =>
        node.children.get(path.head) match {
          case Some(n: Trie.InNode[String, String] @unchecked) => add(path.tail, content, n)
          case _ =>
            val n = Trie.InNode[String, String](MMap())
            node.children += ((path.head, n))
            add(path.tail, content, n)
        }
    }

    for ((path, content) <- m) add(path, content, root)

    root
  }

  def text(p: (Seq[String], File) => Boolean): Map[Seq[String], String] = macro RC.textImpl

  def base64(p: (Seq[String], File) => Boolean): Map[Seq[String], String] = macro RC.base64Impl
}

class RC(val c: scala.reflect.macros.blackbox.Context) {

  import c.universe._

  def commonImpl(isText: Boolean, p: c.Expr[(Seq[String], File) => Boolean]): c.Expr[Map[Seq[String], String]] = {
    val anchorDir = new File(p.tree.pos.source.file.canonicalPath).getParentFile
    val anchorPath = uriOf(anchorDir)
    val pf = Macro.eval[(Seq[String], File) => Boolean](c)(p.tree)
    var args = Vector[c.Tree]()

    def rec(file: File): Unit = {
      if (file.isFile) {
        val filePath = uriOf(file)
        if (filePath.startsWith(anchorPath)) {
          val path = filePath.substring(anchorPath.length).split('/')
          if (pf(Seq(path: _*), file)) {
            val pathSegments: Seq[c.Tree] = path.map(p => Literal(Constant(p)))
            val content = if (isText) readText(file) else readBase64(file)
            val s = content.grouped(20000).toSeq.map(c => Literal(Constant(c)))
            val fs = s.indices.toVector.map(n => TermName("f" + n))
            val ms = fs.zip(s).map(p => q"def ${p._1}: Predef.String = ${p._2}")
            val b = q"..${ms :+ fs.map(f => Ident(f): c.Tree).reduce((f1, f2) => q"$f1 + $f2")}"
            args :+= q"(Seq(..$pathSegments), $b)"
          }
        }
      } else if (file.isDirectory) {
        file.listFiles.foreach(rec)
      }
    }

    rec(anchorDir)

    val r = q"scala.collection.immutable.ListMap[Seq[Predef.String], Predef.String](..$args)"
    //println(showCode(r))
    c.Expr(r)
  }

  def textImpl(p: c.Expr[(Seq[String], File) => Boolean]): c.Expr[Map[Seq[String], String]] =
    commonImpl(isText = true, p)

  def base64Impl(p: c.Expr[(Seq[String], File) => Boolean]): c.Expr[Map[Seq[String], String]] =
    commonImpl(isText = false, p)

  def uriOf(f: File): String = f.toURI.toASCIIString

  def readText(f: File): String = new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)

  def readBase64(f: File): String = Base64.Encoder(Files.readAllBytes(f.toPath)).toBase64(Base64.base64)
}
