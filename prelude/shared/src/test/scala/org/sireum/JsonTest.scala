package org.sireum

import org.sireum.test.SireumSpec

import scalajson.ast.unsafe._

class JsonTest extends SireumSpec {

  *(parseString("\"a\\rbc\"") == "a\rbc")

  *(parseString("\"a\\\"\\rbc\"") == "a\"\rbc")

  *(parseValue("\"a\\rbc\"") == JString("a\rbc"))

  *(parseValue("\"a\\\"\\rbc\"") == JString("a\"\rbc"))

  *(parseNumber("-0") == "-0")

  *(parseNumber("12.33") == "12.33")

  *(parseNumber("12e23") == "12e23")

  *(parseValue("-0") == JNumber("-0"))

  *(parseValue("12.33") == JNumber("12.33"))

  *(parseValue("12e23") == JNumber("12e23"))

  *(parseValue("{}") == JObject())

  *(parseValue("{ \"key\" : \"value\" }") == JObject(Array(JField("key", JString("value")))))

  *(parseValue("[]") == JArray())

  *(parseValue("[ 1, 3.0, 4e-12 ]") == JArray(Array[JValue](JNumber("1"), JNumber("3.0"), JNumber("4e-12"))))

  val oString = "  { \"key\" : [ { \"prop\" : true } ], \"foo\" : false, \"bar\" : [ null, [] ] }  "

  val oJson = JObject(Array(
    JField("key", JArray(JObject(Array(JField("prop", JTrue))))),
    JField("foo", JFalse),
    JField("bar", JArray(Array[JValue](JNull, JArray())))
  ))

  *(parseTopObject(oString) == oJson)

  *(parseTopObject(oPrettyParse(oString)) == oJson)

  *(oPrettyParse(oPrettyParse(oString)) == oPrettyParse(oString))

  *(parseTopObject(oPrettyParse(oPrettyParse(oString))) == oJson)

  def oPrettyParse(s: Predef.String): Predef.String = _JsonSt.stObject(parseTopObject(s)).render.value

  def parseTopObject(s: Predef.String): JObject = parse(s, _.parseTopObject())

  def parseValue(s: Predef.String): JValue = parse(s, _.parseValue())

  def parseString(s: Predef.String): Predef.String = parse(s, _.parsePredefString())

  def parseNumber(s: Predef.String): Predef.String = parse(s, _.parseNumber())

  def parse[T](s: Predef.String, f: _JsonParser => T): T = {
    val json = new _JsonParser {
      override var offset: Int = 0
      override val input: CharSequence = s
    }
    val r = f(json)
    assert(json.input.length == json.offset)
    r
  }
}
