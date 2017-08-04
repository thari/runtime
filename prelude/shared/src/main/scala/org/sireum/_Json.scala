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

  final def parseB(): B = {
    errorIfEof()
    input.charAt(offset) match {
      case 't' => parseConstant("true"); T
      case 'f' => parseConstant("false"); F
      case c => throw ParseException(offset, s"Expected 'true' or 'false', but '$c...' found.")
    }
  }

  final def parseC(): C = {
    val i = offset
    val s = parsePredefString()
    if (s.length != 1) throw ParseException(i, s"Expected a C, but '$s' found.")
    _2C(s.charAt(0))
  }

  final def parseZ(): Z = {
    val i = offset
    val n = parseNumber()
    try _Z(BigInt(n)) catch {
      case _: Throwable => throw ParseException(i, s"Expected a Z, but '$n' found.")
    }
  }

  final def parseZ8(): Z8 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toZ8(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a Z8, but '$n' found.")
    }
  }

  final def parseZ16(): Z16 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toZ16(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a Z16, but '$n' found.")
    }
  }

  final def parseZ32(): Z32 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toZ32(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a Z32, but '$n' found.")
    }
  }

  final def parseZ64(): Z64 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toZ64(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a Z64, but '$n' found.")
    }
  }

  final def parseN(): N = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toN(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a N, but '$n' found.")
    }
  }

  final def parseN8(): N8 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toN8(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a N8, but '$n' found.")
    }
  }

  final def parseN16(): N16 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toN16(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a N16, but '$n' found.")
    }
  }

  final def parseN32(): N32 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toN32(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a N32, but '$n' found.")
    }
  }

  final def parseN64(): N64 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toN64(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a N64, but '$n' found.")
    }
  }

  final def parseS8(): S8 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toS8Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a S8, but '$n' found.")
    }
  }

  final def parseS16(): S16 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toS16Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a S16, but '$n' found.")
    }
  }

  final def parseS32(): S32 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toS32Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a S32, but '$n' found.")
    }
  }

  final def parseS64(): S64 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toS64Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a S64, but '$n' found.")
    }
  }

  final def parseU8(): U8 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toU8Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a U8, but '$n' found.")
    }
  }

  final def parseU16(): U16 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toU16Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a U16, but '$n' found.")
    }
  }

  final def parseU32(): U32 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toU32Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a U32, but '$n' found.")
    }
  }

  final def parseU64(): U64 = {
    val i = offset
    val n = parseNumber()
    try math.Numbers.toU64Exact(_Z(BigInt(n))) catch {
      case _: Throwable => throw ParseException(i, s"Expected a U64, but '$n' found.")
    }
  }

  final def parseF32(): F32 = {
    errorIfEof()
    val i = offset
    input.charAt(offset) match {
      case '"' =>
        parsePredefString() match {
          case "NaN" => _2F32(Float.NaN)
          case "Infinity" => _2F32(Float.PositiveInfinity)
          case "-Infinity" => _2F32(Float.NegativeInfinity)
          case s => throw ParseException(i, s"Expected a F32, but '$s' found.")
        }
      case _ =>
        val n = parseNumber()
        try n.value.toFloat catch {
          case _: Throwable => throw ParseException(i, s"Expected a F32, but '${n.value}' found.")
        }
    }
  }

  final def parseF64(): F64 = {
    errorIfEof()
    val i = offset
    input.charAt(offset) match {
      case '"' =>
        parsePredefString() match {
          case "NaN" => _2F64(Double.NaN)
          case "Infinity" => _2F64(Double.PositiveInfinity)
          case "-Infinity" => _2F64(Double.NegativeInfinity)
          case s => throw ParseException(i, s"Expected a F64, but '$s' found.")
        }
      case _ =>
        val n = parseNumber()
        try n.value.toDouble catch {
          case _: Throwable => throw ParseException(i, s"Expected a F64, but '${n.value}' found.")
        }
    }
  }

  final def parseR(): R = {
    val i = offset
    val n = parseNumber()
    try math._R(spire.math.Real(n)) catch {
      case _: Throwable => throw ParseException(i, s"Expected a U64, but '$n' found.")
    }
  }

  final def parseString(): String = _2String(parsePredefString())

  final def parseISZ[T](f: () => T): IS[Z, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS[Z, T](values.reverse: _*)
  }

  final def parseISZ8[T](f: () => T): IS[Z8, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISZ16[T](f: () => T): IS[Z16, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISZ32[T](f: () => T): IS[Z32, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISZ64[T](f: () => T): IS[Z64, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISN[T](f: () => T): IS[N, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISN8[T](f: () => T): IS[N8, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISN16[T](f: () => T): IS[N16, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISN32[T](f: () => T): IS[N32, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISN64[T](f: () => T): IS[N64, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISS8[T](f: () => T): IS[S8, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISS16[T](f: () => T): IS[S16, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISS32[T](f: () => T): IS[S32, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISS64[T](f: () => T): IS[S64, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISU8[T](f: () => T): IS[U8, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISU16[T](f: () => T): IS[U16, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISU32[T](f: () => T): IS[U32, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseISU64[T](f: () => T): IS[U64, T] = {
    if (!parseArrayBegin()) return IS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    IS(values.reverse: _*)
  }

  final def parseMSZ[T](f: () => T): MS[Z, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS[Z, T](values.reverse: _*)
  }

  final def parseMSZ8[T](f: () => T): MS[Z8, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSZ16[T](f: () => T): MS[Z16, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSZ32[T](f: () => T): MS[Z32, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSZ64[T](f: () => T): MS[Z64, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSN[T](f: () => T): MS[N, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSN8[T](f: () => T): MS[N8, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSN16[T](f: () => T): MS[N16, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSN32[T](f: () => T): MS[N32, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSN64[T](f: () => T): MS[N64, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSS8[T](f: () => T): MS[S8, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSS16[T](f: () => T): MS[S16, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSS32[T](f: () => T): MS[S32, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSS64[T](f: () => T): MS[S64, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSU8[T](f: () => T): MS[U8, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSU16[T](f: () => T): MS[U16, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSU32[T](f: () => T): MS[U32, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseMSU64[T](f: () => T): MS[U64, T] = {
    if (!parseArrayBegin()) return MS()
    var values = List[T](f())
    while (parseArrayNext()) values ::= f()
    MS(values.reverse: _*)
  }

  final def parseOption[T](f: () => T): Option[T] = {
    val tpe = parseObjectType("Some", "None")
    tpe match {
      case "Some" => parseObjectKey("value"); Some(f())
      case "None" => None()
    }
  }

  final def parseMOption[T](f: () => T): MOption[T] = {
    val tpe = parseObjectType("MSome", "MNone")
    tpe match {
      case "MSome" => parseObjectKey("value"); MSome(f())
      case "MNone" => MNone()
    }
  }

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
    case ValueKind.String => JString(parsePredefString())
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
        offset += 1
        parseWhitespace()
        errorIfEof()
        input.charAt(offset) match {
          case '}' =>
            offset += 1
            return JObject()
          case _ =>
            var fields = List[JField](JField(parseObjectKey(), parseValue()))
            while (parseObjectNext()) fields ::= JField(parseObjectKey(), parseValue())
            JObject(fields.reverse.toArray)
        }
      case c => throw ParseException(offset, s"Expected '{', but '$c' found.")
    }
  }

  def parseArray(): JsonArray = {
    errorIfEof()
    input.charAt(offset) match {
      case '[' =>
        offset += 1
        parseWhitespace()
        errorIfEof()
        input.charAt(offset) match {
          case ']' =>
            offset += 1
            return JArray()
          case _ =>
            var values = List[JValue](parseValue())
            while (parseArrayNext()) values ::= parseValue()
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

  def parseObjectType(expectedTypes: Predef.String*): Predef.String = {
    errorIfEof()
    input.charAt(offset) match {
      case '{' =>
        offset += 1
        parseWhitespace()
        errorIfEof()
        parseObjectKey("type")
        val i = offset + 1
        val value = parsePredefString()
        if (expectedTypes.nonEmpty && !expectedTypes.contains(value)) {
          expectedTypes.size match {
            case 1 => throw ParseException(i, s"Expected '${expectedTypes.head}', but '$value' found.")
            case 2 => throw ParseException(i, s"Expected '${expectedTypes.head}' or '${expectedTypes(1)}' , but '$value' found.")
            case _ => throw ParseException(i, s"Expected '${expectedTypes.dropRight(1).map(k => s"'$k'").mkString(", ")}', or '${expectedTypes.last}' , but '$value' found.")
          }
        }
        parseObjectNext()
        value
      case c => throw ParseException(offset, s"Expected '{', but '$c' found.")
    }
  }

  def parseObjectKey(expectedKeys: Predef.String*): Predef.String = {
    errorIfEof()
    val i = offset + 1
    val key = parsePredefString()
    if (expectedKeys.nonEmpty && !expectedKeys.contains(key)) {
      expectedKeys.size match {
        case 1 => throw ParseException(i, s"Expected '${expectedKeys.head}', but '$key' found.")
        case 2 => throw ParseException(i, s"Expected '${expectedKeys.head}' or '${expectedKeys(1)}' , but '$key' found.")
        case _ => throw ParseException(i, s"Expected '${expectedKeys.dropRight(1).map(k => s"'$k'").mkString(", ")}', or '${expectedKeys.last}' , but '$key' found.")
      }
    }
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

  def parsePredefString(): Predef.String = {
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

  def parseObjectNext(): Boolean = {
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

  def parseArrayBegin(): Boolean = {
    errorIfEof()
    input.charAt(offset) match {
      case '[' =>
        offset += 1
        parseWhitespace()
        errorIfEof()
        input.charAt(offset) match {
          case ']' =>
            offset += 1
            return false
          case _ =>
            return true
        }
      case c => throw ParseException(offset, s"Expected '[', but '$c' found.")
    }
  }

  def parseArrayNext(): Boolean = {
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

  def stPredefString(s: Predef.String): ST = {
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
    case v: JsonString => stPredefString(v.value)
    case JTrue => trueSt
    case JFalse => falseSt
    case JNull => nullSt
  }
}

import _JsonSt._

trait _Json_Ext {

  final def stB(o: B): ST = stConstant(o.value.toString)

  final def stC(o: C): ST = stPredefString(o.value.toString)

  final def stZ(o: Z): ST = stNumber(o.toString)

  final def stZ8(o: Z8): ST = stNumber(o.toString)

  final def stZ16(o: Z16): ST = stNumber(o.toString)

  final def stZ32(o: Z32): ST = stNumber(o.toString)

  final def stZ64(o: Z64): ST = stNumber(o.toString)

  final def stN(o: N): ST = stNumber(o.toString)

  final def stN8(o: N8): ST = stNumber(o.toString)

  final def stN16(o: N16): ST = stNumber(o.toString)

  final def stN32(o: N32): ST = stNumber(o.toString)

  final def stN64(o: N64): ST = stNumber(o.toString)

  final def stS8(o: S8): ST = stNumber(o.toString)

  final def stS16(o: S16): ST = stNumber(o.toString)

  final def stS32(o: S32): ST = stNumber(o.toString)

  final def stS64(o: S64): ST = stNumber(o.toString)

  final def stU8(o: U8): ST = stNumber(o.toString)

  final def stU16(o: U16): ST = stNumber(o.toString)

  final def stU32(o: U32): ST = stNumber(o.toString)

  final def stU64(o: U64): ST = stNumber(o.toString)

  final def stF32(o: F32): ST = {
    val v = o.value
    if (v == Float.NaN) stPredefString("NaN")
    else if (v == Float.NegativeInfinity) stPredefString("-Infinity")
    else if (v == Float.PositiveInfinity) stPredefString("Infinity")
    else stNumber(v.toString)
  }

  final def stF64(o: F64): ST = {
    val v = o.value
    if (v == Double.NaN) stPredefString("NaN")
    else if (v == Double.NegativeInfinity) stPredefString("-Infinity")
    else if (v == Double.PositiveInfinity) stPredefString("Infinity")
    else stNumber(v.toString)
  }

  final def stR(o: R): ST = stNumber(o.value.toString)

  final def stString(o: String): ST = stPredefString(o.value)

  final def stISZComplex[T](o: IS[Z, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISZSimple[T](o: IS[Z, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISZ8Complex[T](o: IS[Z8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISZ8Simple[T](o: IS[Z8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISZ16Complex[T](o: IS[Z16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISZ16Simple[T](o: IS[Z16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISZ32Complex[T](o: IS[Z32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISZ32Simple[T](o: IS[Z32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISZ64Complex[T](o: IS[Z64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISZ64Simple[T](o: IS[Z64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISNComplex[T](o: IS[N, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISNSimple[T](o: IS[N, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISN8Complex[T](o: IS[N8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISN8Simple[T](o: IS[N8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISN16Complex[T](o: IS[N16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISN16Simple[T](o: IS[N16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISN32Complex[T](o: IS[N32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISN32Simple[T](o: IS[N32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISN64Complex[T](o: IS[N64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISN64Simple[T](o: IS[N64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISS8Complex[T](o: IS[S8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISS8Simple[T](o: IS[S8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISS16Complex[T](o: IS[S16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISS16Simple[T](o: IS[S16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISS32Complex[T](o: IS[S32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISS32Simple[T](o: IS[S32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISS64Complex[T](o: IS[S64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISS64Simple[T](o: IS[S64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISU8Complex[T](o: IS[U8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISU8Simple[T](o: IS[U8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISU16Complex[T](o: IS[U16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISU16Simple[T](o: IS[U16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISU32Complex[T](o: IS[U32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISU32Simple[T](o: IS[U32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stISU64Complex[T](o: IS[U64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stISU64Simple[T](o: IS[U64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSZComplex[T](o: MS[Z, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSZSimple[T](o: MS[Z, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSZ8Complex[T](o: MS[Z8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSZ8Simple[T](o: MS[Z8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSZ16Complex[T](o: MS[Z16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSZ16Simple[T](o: MS[Z16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSZ32Complex[T](o: MS[Z32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSZ32Simple[T](o: MS[Z32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSZ64Complex[T](o: MS[Z64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSZ64Simple[T](o: MS[Z64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSNComplex[T](o: MS[N, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSNSimple[T](o: MS[N, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSN8Complex[T](o: MS[N8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSN8Simple[T](o: MS[N8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSN16Complex[T](o: MS[N16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSN16Simple[T](o: MS[N16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSN32Complex[T](o: MS[N32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSN32Simple[T](o: MS[N32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSN64Complex[T](o: MS[N64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSN64Simple[T](o: MS[N64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSS8Complex[T](o: MS[S8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSS8Simple[T](o: MS[S8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSS16Complex[T](o: MS[S16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSS16Simple[T](o: MS[S16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSS32Complex[T](o: MS[S32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSS32Simple[T](o: MS[S32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSS64Complex[T](o: MS[S64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSS64Simple[T](o: MS[S64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSU8Complex[T](o: MS[U8, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSU8Simple[T](o: MS[U8, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSU16Complex[T](o: MS[U16, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSU16Simple[T](o: MS[U16, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSU32Complex[T](o: MS[U32, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSU32Simple[T](o: MS[U32, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stMSU64Complex[T](o: MS[U64, T], f: T => ST): ST = stRawComplexArray(o.elements.map(f): _*)

  final def stMSU64Simple[T](o: MS[U64, T], f: T => ST): ST = stRawSimpleArray(o.elements.map(f): _*)

  final def stOption[T](o: Option[T], f: T => ST): ST = o match {
    case Some(t) => stRawObject(("type", stPredefString("Some")), ("value", f(t)))
    case _ => stRawObject(("type", stPredefString("None")))
  }

  final def stMOption[T](o: MOption[T], f: T => ST): ST = o match {
    case MSome(t) => stRawObject(("type", stPredefString("MSome")), ("value", f(t)))
    case _ => stRawObject(("type", stPredefString("MNone")))
  }
}