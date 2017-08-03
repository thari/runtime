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

import scalajson.ast.unsafe._

object _Json {

  type JsonObject = JObject
  type JsonArray = JArray
  type JsonString = JString
  type JsonNumber = JNumber
  type JsonTrue = JTrue.type
  type JsonFalse = JFalse.type

  final case class ParseException(offset: Int, message: Predef.String) extends RuntimeException(message)

  object ValueKind extends Enumeration {
    type Type = Value
    val String, Number, Object, Array, True, False, Null = Value
  }

  def parse(s: String): JsonObject = parse(s.value)

  def parse(cs: CharSequence): JsonObject = {
    val parser = new _JsonParser {
      override var offset: Int = 0
      override val input: CharSequence = cs
    }
    parser.parseTopObject()
  }

  def stringify(o: JsonObject): String = _JsonSt.stObject(o).render
}

import _Json._

trait _JsonParser {

  var offset: Int

  val input: CharSequence

  def errorIfEof(i: Int = offset): Unit =
    if (i >= input.length)
      throw ParseException(offset, "Unexpected end-of-file.")

  def incOffset(n: Int = 1): Char = {
    offset += n
    errorIfEof()
    input.charAt(offset)
  }

  def detect(): ValueKind.Type = {
    parseWhitespace()
    errorIfEof()
    input.charAt(offset) match {
      case '"' => ValueKind.String
      case '{' => ValueKind.Object
      case '[' => ValueKind.Array
      case 't' => ValueKind.True
      case 'f' => ValueKind.False
      case 'n' => ValueKind.Null
      case '-' => ValueKind.Number
      case c if c.isDigit => ValueKind.Number
      case _ => throw ParseException(offset, "Unexpected end-of-file.")
    }
  }

  def parseTopObject(): JsonObject = {
    parseWhitespace()
    val r = parseObject()
    if (offset < input.length) throw ParseException(offset, s"Expected end-of-file, but '${input.charAt(offset)}' found.")
    r
  }

  def parseValue(): JValue = detect() match {
    case ValueKind.String => JString(parseString())
    case ValueKind.Object => parseObject()
    case ValueKind.Array => parseArray()
    case ValueKind.True => parseConstant("true")
    case ValueKind.False => parseConstant("false")
    case ValueKind.Null => parseConstant("null")
    case ValueKind.Number => JNumber(parseNumber())
  }

  def parseObject(): JsonObject = {
    errorIfEof()
    input.charAt(offset) match {
      case '{' =>
        val c = incOffset()
        parseWhitespace()
        c match {
          case '}' =>
            offset += 1
            return JObject()
          case _ =>
            var fields = List[JField](JField(parseObjectKey(), parseValue()))
            while (parseObjectEnd()) fields ::= JField(parseObjectKey(), parseValue())
            JObject(fields.reverse.toArray)
        }
      case c => throw ParseException(offset, s"Expected '{', but '$c' found.")
    }
  }

  def parseArray(): JsonArray = {
    errorIfEof()
    input.charAt(offset) match {
      case '[' =>
        val c = incOffset()
        c match {
          case ']' =>
            offset += 1
            return JArray()
          case _ =>
            var values = List[JValue](parseValue())
            while (parseArrayEnd()) values ::= parseValue()
            JArray(values.reverse.toArray)
        }
      case c => throw ParseException(offset, s"Expected '[', but '$c' found.")
    }
  }

  def parseConstant(text: Predef.String): JValue = {
    val r = text match {
      case "true" => JTrue
      case "false" => JFalse
      case "null" => JNull
      case _ => throw ParseException(offset, s"Invalid constant value '$text'.")
    }
    errorIfEof(offset + text.length - 1)
    val t = slice(offset, offset + text.length)
    if (t != text) throw ParseException(offset, s"Expected '$text', but '$t' found.")
    offset += text.length
    r
  }

  def parseObjectType(): Predef.String = {
    errorIfEof()
    input.charAt(offset) match {
      case '{' =>
        parseWhitespace()
        val i = offset + 1
        val key = parseObjectKey()
        if (key != "type") throw ParseException(i, s"Expected 'type', but '$key' found.")
        val value = parseString()
        parseObjectEnd()
        value
      case c => throw ParseException(offset, s"Expected '{', but '$c' found.")
    }
  }

  def parseObjectKey(): Predef.String = {
    errorIfEof()
    val key = parseString()
    parseWhitespace()
    errorIfEof()
    input.charAt(offset) match {
      case ':' =>
        offset += 1
        parseWhitespace()
        key
      case c => throw ParseException(offset, s"Expected ':', but '$c' found.")
    }
  }

  def parseString(): Predef.String = {
    errorIfEof()

    val sb = new StringBuilder

    var c = input.charAt(offset)
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
                  c = Integer.parseInt(slice(offset - 3, offset + 1), 16).toChar
                  sb.append(c)
                case _ => throw ParseException(offset, s"Expected an escaped character but '$c' found.")
              }
            case _ => sb.append(c)
          }
          c = incOffset()
        }
        offset += 1
        sb.toString
      case _ => throw ParseException(offset, s"""Expected '"' but '$c' found.""")
    }
  }

  def parseNumber(): Predef.String = {
    val sb = new StringBuilder

    errorIfEof()

    var c = input.charAt(offset)
    c match {
      case '-' =>
        sb.append(c)
        c = incOffset()
      case _ if c.isDigit =>
      case _ => throw ParseException(offset, s"""Expected a '-' or a digit but '$c' found.""")
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

  def parseObjectEnd(): Boolean = {
    parseWhitespace()
    errorIfEof()
    input.charAt(offset) match {
      case ',' =>
        offset += 1
        parseWhitespace()
        true
      case '}' =>
        offset += 1
        parseWhitespace()
        false
      case c => throw ParseException(offset, s"Expected ',' or '}', but '$c' found.")
    }
  }

  def parseArrayEnd(): Boolean = {
    parseWhitespace()
    errorIfEof()
    input.charAt(offset) match {
      case ',' =>
        offset += 1
        parseWhitespace()
        true
      case ']' =>
        offset += 1
        parseWhitespace()
        false
      case c => throw ParseException(offset, s"Expected ',' or ']', but '$c' found.")
    }
  }

  def parseWhitespace(): Unit = {
    if (offset >= input.length) return
    var c = input.charAt(offset)
    while (c.isWhitespace) {
      offset += 1
      if (offset >= input.length) return
      c = input.charAt(offset)
    }
  }

  def slice(start: Int, til: Int): Predef.String = {
    val sb = new StringBuilder(til - start)
    for (i <- start until til) {
      sb.append(input.charAt(i))
    }
    sb.toString
  }
}

object _JsonSt {
  val trueSt: ST = st"true"
  val falseSt: ST = st"false"
  val nullSt: ST = st"null"

  def stString(s: Predef.String): ST = {
    val sb = new StringBuilder(s.length)
    for (c <- s) c match {
      case '"' => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '/' => sb.append("\\/")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case _ if 0 <= c && c < 256 => sb.append(c)
      case _ =>
        sb.append("\\u")
        sb.append(f"${c.toInt}%4X")
    }
    st""""${sb.toString}""""
  }

  def stConstant(s: Predef.String): ST = s match {
    case "true" => trueSt
    case "false" => falseSt
    case "null" => nullSt
  }

  def stNumber(s: Predef.String): ST = st"$s"

  def stObject(v: JsonObject): ST = stRawObject(v.value.map(f => (f.field, stValue(f.value))): _*)

  def stRawObject(fields: (Predef.String, ST)*): ST = {
    val fs = for ((k, v) <- fields) yield st""""$k" : $v"""
    st"""{
        |  ${(fs, ",\n")}
        |}"""
  }

  def stArray(v: JsonArray): ST =
    if (v.value.exists(e => e.isInstanceOf[JsonObject] || e.isInstanceOf[JArray]))
      stRawComplexArray(v.value.map(stValue): _*)
    else stRawSimpleArray(v.value.map(stValue): _*)

  def stRawComplexArray(elements: ST*): ST = st"[${(elements, ",\n")}]"

  def stRawSimpleArray(elements: ST*): ST = st"[${(elements, ", ")}]"

  def stValue(v: JValue): ST = v match {
    case v: JsonObject => stObject(v)
    case v: JsonArray => stArray(v)
    case v: JsonNumber => stNumber(v.value)
    case v: JsonString => stString(v.value)
    case JTrue => trueSt
    case JFalse => falseSt
    case JNull => nullSt
  }
}
