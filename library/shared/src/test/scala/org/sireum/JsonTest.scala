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

  val oJson = JObject(
    Array(
      JField("key", JArray(JObject(Array(JField("prop", JTrue))))),
      JField("foo", JFalse),
      JField("bar", JArray(Array[JValue](JNull, JArray())))
    )
  )

  *(parseTopObject(oString) =~= oJson)

  *(parseTopObject(oPrettyParse(oString)) =~= oJson)

  *(oPrettyParse(oPrettyParse(oString)) =~= oPrettyParse(oString))

  *(parseTopObject(oPrettyParse(oPrettyParse(oString))) =~= oJson)

  * {
    val o = Either.Left[Z, String](Z.random)
    val s = Json.Printer.printEither(o, Json.Printer.printZ _, Json.Printer.printString _).render
    val p = Json.Parser.create(s)
    val o2 = p.parseEither[Z, String](p.parseZ _, p.parseString _)
    p.errorOpt.isEmpty && o == o2
  }

  * {
    val o = MEither.Right[Z, String](String.random)
    val s = Json.Printer.printMEither(o, Json.Printer.printZ _, Json.Printer.printString _).render
    val p = Json.Parser.create(s)
    val o2 = p.parseMEither[Z, String](p.parseZ _, p.parseString _)
    p.errorOpt.isEmpty && o == o2
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => (String.random, Z.random)))) {
    * {
      val o = Map ++ is
      val s = Json.Printer.printMap(B.random, o, Json.Printer.printString _, Json.Printer.printZ _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseMap[String, Z](p.parseString _, p.parseZ _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = Set ++ is
      val s = Json.Printer.printSet(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseSet[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => (String.random, Z.random)))) {
    * {
      val o = HashMap ++ is
      val s = Json.Printer.printHashMap(B.random, o, Json.Printer.printString _, Json.Printer.printZ _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseHashMap[String, Z](p.parseString _, p.parseZ _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = HashSet ++ is
      val s = Json.Printer.printHashSet(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseHashSet[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => (String.random, Z.random)))) {
    * {
      val o = HashSMap ++ is
      val s = Json.Printer.printHashSMap(B.random, o, Json.Printer.printString _, Json.Printer.printZ _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseHashSMap[String, Z](p.parseString _, p.parseZ _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = HashSSet ++ is
      val s = Json.Printer.printHashSSet(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseHashSSet[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = Stack(is)
      val s = Json.Printer.printStack(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseStack[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = Bag ++ is
      val s = Json.Printer.printBag(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseBag[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  for (is <- (0 until 3).map(i => (z"0" until i).map(_ => String.random))) {
    * {
      val o = HashBag ++ is
      val s = Json.Printer.printHashBag(B.random, o, Json.Printer.printString _).render
      val p = Json.Parser.create(s)
      val o2 = p.parseHashBag[String](p.parseString _)
      p.errorOpt.isEmpty && o == o2
    }
  }

  * {
    val o = PosetTest.poset
    val s = Json.Printer.printPoset(B.random, o, Json.Printer.printString _).render
    val p = Json.Parser.create(s)
    val o2 = p.parsePoset[String](p.parseString _)
    p.errorOpt.isEmpty && o == o2
  }

  * {
    val o = {
      val graph = Graph.empty[Z, String]
      val n1 = Z.random
      val n2 = Z.random
      var g = graph + n1 ~> n2
      g = g + n2 ~> n1
      g = g + n1 ~> n2
      g
    }
    val s = Json.Printer.printGraph(B.random, o, Json.Printer.printZ _, Json.Printer.printString _).render
    val p = Json.Parser.create(s)
    val o2 = p.parseGraph[Z, String](p.parseZ _, p.parseString _)
    p.errorOpt.isEmpty && o == o2
  }

  * {
    val o = {
      var uf = UnionFind.create[Z](ISZ(1, 2, 3, 4, 5))
      uf = uf.merge(1, 2)
      uf = uf.merge(3, 4)
      uf = uf.merge(4, 5)
      uf
    }
    val s = Json.Printer.printUnionFind(B.random, o, Json.Printer.printZ _).render
    val p = Json.Parser.create(s)
    val o2 = p.parseUnionFind[Z](p.parseZ _)
    p.errorOpt.isEmpty && o == o2
  }

  * {
    import org.sireum.U32._
    val o = message.FlatPos(Some("Hi.txt"), u32"1", u32"1", u32"1", u32"1", u32"1", u32"1")
    val s = Json.Printer.printPosition(o).render
    val p = Json.Parser.create(s)
    val o2 = p.parsePosition()
    p.errorOpt.isEmpty && o == o2
  }

  * {
    import org.sireum.U32._
    val o = message.DocInfo(None(), ISZ(u32"0", u32"10", u32"40"))
    val s = Json.Printer.printDocInfo(o).render
    val p = Json.Parser.create(s)
    val o2 = p.parseDocInfo()
    p.errorOpt.isEmpty && o == o2
  }

  * {
    val o = message.Message(message.Level.Info, None(), "test", "test info")
    val s = Json.Printer.printMessage(o).render
    val p = Json.Parser.create(s)
    val o2 = p.parseMessage()
    p.errorOpt.isEmpty && o == o2
  }

  def oPrettyParse(s: Predef.String): Predef.String = Json.printAst(JsonAst.Binding, parseTopObject(s)).render.value

  def parseTopObject(s: Predef.String): JObject =
    Json.parseAst(JsonAst.Binding, String(s)) match {
      case Either.Left(o: JObject) => o
      case Either.Right(errMsg) =>
        assert(F, s"[${errMsg.line}, ${errMsg.column}] ${errMsg.message}"); null
      case _ => assert(F); null
    }

  def parseValue(s: Predef.String): JValue =
    Json.parseAst(JsonAst.Binding, String(s)) match {
      case Either.Left(o) => o
      case Either.Right(errMsg) =>
        assert(F, s"[${errMsg.line}, ${errMsg.column}] ${errMsg.message}"); null
      case _ => assert(F); null
    }

  def parseString(s: Predef.String): Predef.String = parse(s, _.parseString().value)

  def parseNumber(s: Predef.String): Predef.String = parse(s, _.parseNumber().value)

  def parse[T](s: Predef.String, f: Json.Parser => T): T = {
    val parser = Json.Parser(conversions.String.toCis(String(s)), 0, None())
    val r = f(parser)
    parser.eof()
    assert(parser.errorOpt.isEmpty)
    r
  }
}
