// #Sireum
/*
 Copyright (c) 2018, Robby, Kansas State University
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

package org.sireum.message

import org.sireum._

object Reporter {

  @pure def create: Reporter = {
    return Reporter(ISZ())
  }

  @pure def combine(r1: Reporter, r2: Reporter): Reporter = {
    return Reporter(r1.messages ++ r2.messages)
  }
}

@record class Reporter(var messages: ISZ[Message]) {

  def hasInternalError: B = {
    for (m <- messages) {
      m.level match {
        case Level.InternalError => return T
        case _ =>
      }
    }
    return F
  }

  def hasError: B = {
    for (m <- messages if m.isError || m.isInternalError) {
      return T
    }
    return F
  }

  def hasWarning: B = {
    for (m <- messages if m.isWarning) {
      return T
    }
    return F
  }

  def hasIssue: B = {
    for (m <- messages if m.isError || m.isWarning || m.isInternalError) {
      return T
    }
    return F
  }

  def hasInfo: B = {
    for (m <- messages if m.isInfo) {
      return T
    }
    return F
  }

  def hasMessage: B = {
    return messages.nonEmpty
  }

  def internalErrors: ISZ[Message] = {
    return for (m <- messages if m.isInternalError) yield m
  }

  def errors: ISZ[Message] = {
    return for (m <- messages if m.isError) yield m
  }

  def warnings: ISZ[Message] = {
    return for (m <- messages if m.isWarning) yield m
  }

  def issues: ISZ[Message] = {
    return for (m <- messages if m.isError || m.isWarning || m.isInternalError) yield m
  }

  def infos: ISZ[Message] = {
    return for (m <- messages if m.isInfo) yield m
  }

  def report(m: Message): Unit = {
    //assert(m.fileUriOpt.isEmpty || !ops.ISZOps(messages).contains(m))
    messages = messages :+ m
  }

  def messagesByFileUri: HashSMap[Option[String], ISZ[Message]] = {
    var r = HashSMap.empty[Option[String], ISZ[Message]]
    for (m <- messages) {
      val key: Option[String] = m.fileUriOpt
      r.get(key) match {
        case Some(ms) => r = r + key ~> (ms :+ m)
        case _ => r = r + key ~> ISZ(m)
      }
    }
    return r
  }

  def printMessages(): Unit = {
    @pure def sortMessages(ms: ISZ[Message]): ISZ[Message] = {
      return ops
        .ISZOps(ms)
        .sortWith((m1, m2) => {
          (m1.posOpt, m2.posOpt) match {
            case (Some(m1pos), Some(m2pos)) =>
              if (m1pos.beginLine < m2pos.beginLine) T
              else if (m1pos.beginLine > m2pos.beginLine) F
              else if (m1pos.beginColumn < m2pos.beginColumn) T
              else if (m1pos.beginColumn > m2pos.beginColumn) F
              else m1.text.size < m2.text.size
            case _ => m1.text.size < m2.text.size
          }
        })
    }
    val map = messagesByFileUri
    val err = hasError
    var first = T
    for (kv <- map.entries) {
      if (!first) {
        cprintln(err, "")
      }
      first = F
      val fileUriOpt = kv._1
      val ms = kv._2
      fileUriOpt match {
        case Some(fileUri) =>
          cprintln(err, s"* $fileUri")
          for (m <- sortMessages(ms)) {
            cprint(err, "  ")
            val int: String = if (m.level == Level.InternalError) "INTERNAL ERROR -- " else ""
            val mText: String = m.posOpt match {
              case Some(pos) => s"- [${pos.beginLine}, ${pos.beginColumn}] $int${m.text}"
              case _ => s"- ${m.text}"
            }
            cprintln(err, mText)
          }
        case _ =>
          for (m <- sortMessages(ms)) {
            val int: String = if (m.level == Level.InternalError) "INTERNAL ERROR -- " else ""
            val mText: String = m.posOpt match {
              case Some(pos) => s"- [${pos.beginLine}, ${pos.beginColumn}] $int${m.text}"
              case _ => s"- ${m.text}"
            }
            cprintln(err, mText)
          }
      }
    }
  }

  def internalError(posOpt: Option[Position], kind: String, message: String): Unit = {
    report(Message(Level.InternalError, posOpt, kind, message))
  }

  def error(posOpt: Option[Position], kind: String, message: String): Unit = {
    report(Message(Level.Error, posOpt, kind, message))
  }

  def warn(posOpt: Option[Position], kind: String, message: String): Unit = {
    report(Message(Level.Warning, posOpt, kind, message))
  }

  def info(posOpt: Option[Position], kind: String, message: String): Unit = {
    report(Message(Level.Info, posOpt, kind, message))
  }

  def reports(ms: ISZ[Message]): Unit = {
    for (m <- ms) {
      report(m)
    }
  }
}
