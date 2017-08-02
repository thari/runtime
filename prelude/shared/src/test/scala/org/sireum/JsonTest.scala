package org.sireum

import org.sireum.test.SireumSpec

class JsonTest extends SireumSpec {

  * (parseString("\"a\\rbc\"") == "a\rbc")

  * (parseString("\"a\\\"\\rbc\"") == "a\"\rbc")

  * (parseString("") == "a\"\rbc")

  * (parseNumber("-0") == "-0")

  * (parseNumber("12.33") == "12.33")

  * (parseNumber("12e23") == "12e23")

  def parseString(s: Predef.String): Predef.String = parse(s, _.parseString())

  def parseNumber(s: Predef.String): Predef.String = parse(s, _.parseNumber())

  def parse[T](s: Predef.String, f: _JsonParser => T): T = {
    val json = new _JsonParser {
      var offset: Int = 0
      val input: Array[Char] = s.toCharArray
    }
    val r = f(json)
    assert(json.input.length == json.offset)
    r
  }
}
