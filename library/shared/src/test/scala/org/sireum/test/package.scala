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

import org.scalactic.source.Position
import org.scalatest.FreeSpec

package object test {

  trait TestSuite extends SireumSpec {

    class Anonymous(position: Position) {
      def -(f: => Unit): Unit = {
        var fname = position.fileName
        val i = fname.lastIndexOf('.')
        if (i >= 0) {
          fname = fname.substring(0, i)
        }
        registerTest(s"$fname L${position.lineNumber}")(f)(position)
      }
    }

    def *(implicit position: Position): Anonymous = new Anonymous(position)

    def Tests(f: => Unit): Unit = {
      f
    }

    def assertMatch[T](o: T)(pf: PartialFunction[T, _]): Unit = {
      assert(pf.isDefinedAt(o))
    }

  }

  implicit class Equiv(val result: scala.Any) extends AnyVal {
    def =~=(expected: scala.Any): scala.Boolean = (result, expected) match {
      case (result: scala.Array[_], expected: scala.Array[_]) =>
        var r = true
        if (result.length != expected.length) r = false
        if (result.isInstanceOf[scala.Array[scala.Byte]] != expected.isInstanceOf[scala.Array[scala.Byte]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Char]] != expected.isInstanceOf[scala.Array[scala.Char]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Short]] != expected.isInstanceOf[scala.Array[scala.Short]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Int]] != expected.isInstanceOf[scala.Array[scala.Int]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Long]] != expected.isInstanceOf[scala.Array[scala.Long]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Float]] != expected.isInstanceOf[scala.Array[scala.Float]]) r = false
        if (result.isInstanceOf[scala.Array[scala.Double]] != expected.isInstanceOf[scala.Array[scala.Double]]) r = false
        for (i <- result.indices if r) {
          if (result(i) != expected(i)) r = false
        }
        val expectedSeq = expected.map(org.sireum.String.escape).mkString(", ")
        val resultSeq = result.map(org.sireum.String.escape).mkString(", ")
        assert(r, s"\nExpected:  [$expectedSeq] (of ${expected.getClass.getName}),\nbut found: [$resultSeq] (of ${result.getClass.getName})")
        r
      case _ =>
        val r = result == expected
        assert(r, s"\nExpected:  $expected,\nbut found: $result")
        r
    }

    def !~=(expected: scala.Any): scala.Boolean = (result, expected) match {
      case (result: scala.Array[_], expected: scala.Array[_]) =>
        var r = false
        if (result.length != expected.length) r = true
        if (result.isInstanceOf[scala.Array[scala.Byte]] != expected.isInstanceOf[scala.Array[scala.Byte]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Char]] != expected.isInstanceOf[scala.Array[scala.Char]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Short]] != expected.isInstanceOf[scala.Array[scala.Short]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Int]] != expected.isInstanceOf[scala.Array[scala.Int]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Long]] != expected.isInstanceOf[scala.Array[scala.Long]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Float]] != expected.isInstanceOf[scala.Array[scala.Float]]) r = true
        if (result.isInstanceOf[scala.Array[scala.Double]] != expected.isInstanceOf[scala.Array[scala.Double]]) r = true
        for (i <- result.indices if !r) {
          if (result(i) != expected(i)) r = true
        }
        val expectedSeq = expected.map(org.sireum.String.escape).mkString(", ")
        val resultSeq = result.map(org.sireum.String.escape).mkString(", ")
        assert(r, s"\nExpected:  [$expectedSeq] (of ${expected.getClass.getName}),\nResult: [$resultSeq] (of ${result.getClass.getName})")
        r
      case _ =>
        val r = result != expected
        assert(r, s"\nExpected:  $expected,\nResult: $result")
        r
    }
  }

}
