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

package org.sireum.`macro`

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.github.marklister.base64.Base64
import org.sireum.{B, HashSMap, ISZ, String, U8, _2String}

import scala.language.experimental.macros

object RC {
  def text(p: ISZ[String] => B): HashSMap[ISZ[String], String] = macro RC.textImpl
  def base64(p: ISZ[String] => B): HashSMap[ISZ[String], String] = macro RC.base64Impl
}

class RC(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._

  def commonImpl(isText: Boolean, p: c.Expr[ISZ[String] => B]): c.Expr[HashSMap[ISZ[String], String]] = {
    val anchorDir = new File(p.tree.pos.source.file.canonicalPath).getParentFile
    val anchorPath = uriOf(anchorDir)
    val f = c.eval[ISZ[String] => B](c.Expr(c.untypecheck(p.tree)))
    var tree = q"org.sireum.HashSMap.empty[org.sireum.ISZ[org.sireum.String], org.sireum.String]"

    def rec(file: File): Unit = {
      if (file.isFile) {
        val filePath = uriOf(file)
        if (filePath.startsWith(anchorPath)) {
          val path = filePath.substring(anchorPath.length).split('/').map(_2String)
          if (f(ISZ(path: _*))) {
            val pathSegments: Seq[c.Tree] = path.map(p => q"org.sireum._2String(${Literal(Constant(p.value))})")
            val s = Literal(Constant(if (isText) readText(file) else readBase64(file)))
            tree = q"$tree.put(ISZ(..$pathSegments), org.sireum._2String($s))"
          }
        }
      } else if (file.isDirectory) {
        file.listFiles.foreach(rec)
      }
    }
    rec(anchorDir)

    c.Expr(tree)
  }

  def textImpl(p: c.Expr[ISZ[String] => B]): c.Expr[HashSMap[ISZ[String], String]] =
    commonImpl(isText = true, p)

  def base64Impl(p: c.Expr[ISZ[String] => B]): c.Expr[HashSMap[ISZ[String], String]] =
    commonImpl(isText = false, p)

  def uriOf(f: File): Predef.String = f.toURI.toASCIIString

  def readText(f: File): Predef.String = new Predef.String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)

  def readBase64(f: File): Predef.String = Base64.Encoder(Files.readAllBytes(f.toPath)).toBase64(Base64.base64)
}
