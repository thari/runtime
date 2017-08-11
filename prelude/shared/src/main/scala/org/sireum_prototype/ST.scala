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

package org.sireum_prototype

import org.sireum_prototype.$internal.STMarker

object ST {

  sealed trait Arg

  final case class Any(args: scala.Seq[scala.Any], sep: Predef.String = "") extends Arg

  final case class Templ(args: scala.Seq[ST], sep: Predef.String = "") extends Arg

  def isWs(c: Char): Boolean = c match {
    case ' ' | '\r' | '\n' | '\t' => true
    case _ => false
  }

  def render(t: ST, isCompact: Boolean): String = {
    val sb = new java.lang.StringBuilder
    var indent = 0
    var lastWs = false

    def appendChar(c: Char): Unit = {
      if (isCompact) {
        if (!isWs(c)) {
          sb.append(c)
          lastWs = false
        } else if (!lastWs) {
          sb.append(' ')
          lastWs = true
        }
      } else sb.append(c)
    }

    def trim(includeNewLine: Boolean = false): Unit = {
      if (!isCompact) {
        val i = sb.lastIndexOf(System.lineSeparator)
        if (i >= 0 && (i + 1 until sb.length).forall(j => isWs(sb.charAt(j))))
          sb.setLength(if (includeNewLine) i else i + System.lineSeparator.length)
      }
    }

    def appendIndent(): Unit = if (isCompact) appendChar(' ') else for (i <- 0 until indent) sb.append(' ')

    def appendLineSep(): Unit = {
      trim()
      if (isCompact) appendChar(' ') else sb.append(System.lineSeparator)
    }

    def appendPart(s: Predef.String): Unit = {
      var n = 0
      var hasLine = false
      for (tkn <- splitNlChars(s)) {
        if (tkn == "\n") {
          appendLineSep()
          appendIndent()
          hasLine = true
        } else if (tkn != "\r") {
          val stripped = if (hasLine) {
            val i = tkn.indexOf("|")
            if (i >= 0 && tkn.substring(0, i).forall(isWs)) tkn.substring(i + 1) else tkn
          } else tkn
          if (isCompact && stripped.length > 0 && lastWs && isWs(stripped.head))
            sb.append(stripped, 1, stripped.length)
          else sb.append(stripped)
          if (stripped.length > 0)
            lastWs = isWs(stripped.last)
          n = stripped.length
        }
      }
      if (hasLine) {
        indent += n
      }
    }

    def append(s: Predef.String): Unit = {
      for (tkn <- splitNlChars(s)) {
        if (tkn == "\n") {
          appendLineSep()
          appendIndent()
        } else if (tkn != "\r") {
          sb.append(tkn)
        }
      }
    }

    def splitNlChars(s: Predef.String): List[Predef.String] = {
      var l = List[Predef.String]()
      var i = 0
      val sz = s.length
      val sb = new java.lang.StringBuilder(sz)
      while (i < sz) {
        s(i) match {
          case '\r' =>
            l = if (sb.length > 0) "\r" :: sb.toString :: l else "\r" :: l
            sb.setLength(0)
          case '\n' =>
            l = if (sb.length > 0) "\n" :: sb.toString :: l else "\n" :: l
            sb.setLength(0)
          case c => sb.append(c)
        }
        i += 1
      }
      if (sb.length > 0) l = sb.toString :: l
      l.reverse
    }

    def rec(t: ST): Unit = {
      val oldIndent = indent
      val parts = if (isCompact) t.compactParts else t.parts
      appendPart(parts.head)
      var i = 1

      def trimNlPart(): Int = {
        val part = parts(i)
        val j = part.indexOf(System.lineSeparator)
        if (j >= 0 && (0 until j).forall(k => isWs(part(k)))) {
          trim(includeNewLine = true)
          j
        } else 0
      }

      def appendAny(o: scala.Any): Int = {
        o match {
          case Some(v: ST) => rec(v)
          case Some(v) => append(v.toString)
          case None => return trimNlPart()
          case v => append(v.toString)
        }
        0
      }

      for (arg <- t.args) {
        var trimPartBegin = 0
        arg match {
          case Any(vs, sep) =>
            if (vs.nonEmpty) {
              trimPartBegin = appendAny(vs.head)
              for (i <- 1 until vs.length) {
                trimPartBegin = 0
                append(sep)
                appendAny(vs(i))
              }
            } else {
              trimPartBegin = trimNlPart()
            }
          case Templ(ts, sep) =>
            if (ts.nonEmpty) {
              rec(ts.head)
              for (i <- 1 until ts.length) {
                append(sep)
                rec(ts(i))
              }
            } else {
              trimPartBegin = trimNlPart()
            }
        }
        indent = oldIndent
        val part = parts(i)
        if (trimPartBegin > 0) appendPart(part.substring(trimPartBegin))
        else appendPart(part)
        i += 1
      }
      indent = oldIndent
    }

    rec(t)
    trim()
    sb.toString
  }

}

import ST._

final case class ST(parts: scala.Seq[Predef.String],
                    args: scala.Seq[Arg],
                    source: Predef.String) extends Immutable with STMarker {
  lazy val compactParts: scala.Seq[Predef.String] = {
    for (part <- parts) yield {
      val sb = new java.lang.StringBuilder(part.length)
      var lastWs = false
      for (c <- part) {
        if (isWs(c)) {
          if (c == '\n') {
            if (lastWs) {
              sb.setCharAt(sb.length - 1, '\n')
            } else {
              sb.append('\n')
            }
            lastWs = true
          } else if (!lastWs) {
            sb.append(' ')
            lastWs = true
          }
        } else {
          sb.append(c)
          lastWs = false
        }
      }
      val r = sb.toString
      r
    }
  }

  def render: String = ST.render(this, isCompact = false)

  def renderCompact: String = ST.render(this, isCompact = true)

  def isEqual(other: Immutable): B = this == other

  def hash: Z = hashCode

  override def hashCode: Int = halt("Cannot hash Slang ST")

  override def equals(other: scala.Any): scala.Boolean = halt("Cannot test equality in Slang ST")

  override def toString: java.lang.String = source

  def string: String = source
}
