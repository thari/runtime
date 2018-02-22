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

package org.sireum.test

import org.scalatest.{FreeSpec, Tag}

trait SireumSpec extends FreeSpec {

  val ts: scala.Seq[Tag] = scala.Vector()

  private val m: scala.collection.mutable.Map[scala.Int, scala.Int] = {
    import scala.collection.JavaConverters._
    new java.util.concurrent.ConcurrentHashMap[scala.Int, scala.Int]().asScala
  }

  private def name(line: scala.Int): Predef.String = {
    val last = m.getOrElseUpdate(line, 0)
    val next = last + 1
    m(line) = next
    if (last == 0) s"L$line" else s"L$line # $next"
  }

  def *(title: Predef.String)(b: => scala.Boolean)(implicit pos: org.scalactic.source.Position): Unit = {
    registerTest(s"${name(pos.lineNumber)}: $title", ts: _*)(assert(b))(pos)
  }

  def *(b: => scala.Boolean)(implicit pos: org.scalactic.source.Position): Unit = {
    registerTest(name(pos.lineNumber), ts: _*)(assert(b))(pos)
  }

  def *(b: => scala.Boolean, msg: => Predef.String)(implicit pos: org.scalactic.source.Position): Unit = {
    registerTest(name(pos.lineNumber), ts: _*)(if (!b) assert(b, msg))(pos)
  }
}
