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

object _Json {

  final case class ParseException(offset: Int, message: Predef.String) extends RuntimeException(message)

  object ValueKind extends Enumeration {
    type Type = Value
    val String, Number, Object, Array, True, False, Null = Value
  }

}

trait _JsonParser {

  var offset: Int

  val input: Array[Char]

  def errorIfEof(i: Int = offset): Unit = if (i >= input.length) throw _Json.ParseException(offset, "Unexpected end-of-file.")

  def incOffset(n: Int = 1): Char = {
    offset += n
    errorIfEof()
    input(offset)
  }

  def detect(): _Json.ValueKind.Type = {
    parseWhitespace()
    errorIfEof()
    input(offset) match {
      case '"' => _Json.ValueKind.String
      case '{' => _Json.ValueKind.Object
      case '[' => _Json.ValueKind.Array
      case 't' => _Json.ValueKind.True
      case 'f' => _Json.ValueKind.False
      case 'n' => _Json.ValueKind.Null
      case '-' => _Json.ValueKind.Number
      case c if c.isDigit => _Json.ValueKind.Number
      case _ => throw _Json.ParseException(offset, "Unexpected end-of-file.")
    }
  }

  def parseObjectType(): Predef.String = {
    errorIfEof()
    input(offset) match {
      case '{' =>
        parseWhitespace()
        val i = offset + 1
        val key = parseObjectKey()
        if (key != "type") throw _Json.ParseException(i, s"Expected 'type', but '$key' found.")
        val value = parseString()
        parseValueEnd()
        value
      case c => throw _Json.ParseException(offset, s"Expected '{', but '$c' found.")
    }
  }

  def parseObjectKey(): Predef.String = {
    errorIfEof()
    val key = parseString()
    parseWhitespace()
    errorIfEof()
    input(offset) match {
      case ':' =>
        offset += 1
        parseWhitespace()
        key
      case c => throw _Json.ParseException(offset, s"Expected ':', but '$c' found.")
    }
  }

  def parseString(): Predef.String = {
    errorIfEof()

    val sb = new StringBuilder

    var c = input(offset)
    c match {
      case '"' =>
        c = incOffset()
        while (c != '"') {
          c match {
            case '\\' =>
              c = incOffset()
              c match {
                case '"' => sb.append('"')
                case '\\' => sb.append('\\')
                case '/' => sb.append('/')
                case 'b' => sb.append('\b')
                case 'f' => sb.append('\f')
                case 'n' => sb.append('\n')
                case 'r' => sb.append('\r')
                case 't' => sb.append('\t')
                case 'u' =>
                  incOffset(4)
                  c = Integer.parseInt(new Predef.String(input.slice(offset - 3, offset + 1)), 16).toChar
                  sb.append(c)
                case _ => throw _Json.ParseException(offset, s"Expected an escaped character but '$c' found.")
              }
            case _ => sb.append(c)
          }
          c = incOffset()
        }
        offset += 1
        sb.toString
      case _ => throw _Json.ParseException(offset, s"""Expected '"' but '$c' found.""")
    }
  }


  def parseNumber(): Predef.String = {
    val sb = new StringBuilder

    errorIfEof()

    var c = input(offset)
    c match {
      case '-' =>
        sb.append(c)
        c = incOffset()
      case _ if c.isDigit =>
      case _ => throw _Json.ParseException(offset, s"""Expected a '-' or a digit but '$c' found.""")
    }

    c match {
      case '0' =>
        sb.append(c)
        if (offset + 1 == input.length) {
          offset += 1
          return sb.toString
        }
        c = incOffset()
      case _ =>
        sb.append(c)
        if (offset + 1 == input.length) {
          offset += 1
          return sb.toString
        }
        c = incOffset()
        while (c.isDigit) {
          sb.append(c)
          if (offset + 1 == input.length) {
            offset += 1
            return sb.toString
          }
          c = incOffset()
        }
    }

    c match {
      case '.' =>
        sb.append(c)
        c = incOffset()
        while (c.isDigit) {
          sb.append(c)
          if (offset + 1 == input.length) {
            offset += 1
            return sb.toString
          }
          c = incOffset()
        }
      case _ =>
    }

    c match {
      case 'e' | 'E' =>
        sb.append(c)
        c = incOffset()
        c match {
          case '+' | '-' =>
            sb.append(c)
            c = incOffset()
          case _ =>
        }
        while (c.isDigit) {
          sb.append(c)
          if (offset + 1 == input.length) {
            offset += 1
            return sb.toString
          }
          c = incOffset()
        }
        sb.toString
      case _ => return sb.toString
    }
  }

  def parseValueEnd(): Boolean = {
    parseWhitespace()
    errorIfEof()
    input(offset) match {
      case ',' =>
        offset += 1
        parseWhitespace()
        true
      case '}' =>
        offset += 1
        parseWhitespace()
        false
      case c => throw _Json.ParseException(offset, s"Expected ',' or '}', but '$c' found.")
    }
  }

  def parseWhitespace(): Unit = {
    if (offset >= input.length) return
    var c = input(offset)
    while (c.isWhitespace) {
      offset += 1
      if (offset >= input.length) return
      c = input(offset)
    }
  }
}
