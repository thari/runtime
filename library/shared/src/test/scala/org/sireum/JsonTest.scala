/*
 Copyright (c) 2017, Robby, Kansas State University
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

package org.sireum

import org.sireum.test._

import scalajson.ast.unsafe._

import $internal.JsonAst

class JsonTest extends SireumRuntimeSpec {

  *(parseString("\"a\\rbc\"") =~= "a\rbc")

  *(parseString("\"a\\\"\\rbc\"") =~= "a\"\rbc")

  *(parseValue("\"a\\rbc\"") =~= JString("a\rbc"))

  *(parseValue("\"a\\\"\\rbc\"") =~= JString("a\"\rbc"))

  *(parseNumber("-0") =~= "-0")

  *(parseNumber("12.33") =~= "12.33")

  *(parseNumber("12e23") =~= "12e23")

  *(parseValue("-0") =~= JNumber("-0"))

  *(parseValue("12.33") =~= JNumber("12.33"))

  *(parseValue("12e23") =~= JNumber("12e23"))

  *(parseValue("{}") =~= JObject())

  *(parseValue("{ \"key\" : \"value\" }") =~= JObject(Array(JField("key", JString("value")))))

  *(parseValue("[]") =~= JArray())

  *(parseValue("[ 1, 3.0, 4e-12 ]") =~= JArray(Array[JValue](JNumber("1"), JNumber("3.0"), JNumber("4e-12"))))

  val oString = "  { \"key\" : [ { \"prop\" : true } ], \"foo\" : false, \"bar\" : [ null, [] ] }  "

  val oJson = JObject(Array(
    JField("key", JArray(JObject(Array(JField("prop", JTrue))))),
    JField("foo", JFalse),
    JField("bar", JArray(Array[JValue](JNull, JArray())))
  ))

  *(parseTopObject(oString) =~= oJson)

  *(parseTopObject(oPrettyParse(oString)) =~= oJson)

  *(oPrettyParse(oPrettyParse(oString)) =~= oPrettyParse(oString))

  *(parseTopObject(oPrettyParse(oPrettyParse(oString))) =~= oJson)

  def oPrettyParse(s: Predef.String): Predef.String = Json.printAst(JsonAst.Binding, parseTopObject(s)).render.value

  def parseTopObject(s: Predef.String): JObject =
    Json.parseAst(JsonAst.Binding, String(s)) match {
      case Either(Some(o: JObject), _) => o
      case Either(_, Some(errMsg)) =>
        assert(F, s"[${errMsg.line}, ${errMsg.column}] ${errMsg.message}"); null
      case _ => assert(F); null
    }

  def parseValue(s: Predef.String): JValue =
    Json.parseAst(JsonAst.Binding, String(s)) match {
      case Either(Some(o), _) => o
      case Either(_, Some(errMsg)) =>
        assert(F, s"[${errMsg.line}, ${errMsg.column}] ${errMsg.message}"); null
      case _ => assert(F); null
    }

  def parseString(s: Predef.String): Predef.String = parse(s, _.parseString().value)

  def parseNumber(s: Predef.String): Predef.String = parse(s, _.parseNumber().value)

  def parse[T](s: Predef.String, f: Json.Parser => T): T = {
    val parser = Json.Parser(String(s).toCis, 0, None())
    val r = f(parser)
    parser.eof()
    assert(parser.errorOpt.isEmpty)
    r
  }
}
