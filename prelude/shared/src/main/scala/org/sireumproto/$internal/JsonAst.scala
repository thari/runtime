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

package org.sireumproto.$internal

import org.sireumproto._
import org.sireumproto.Json._

import scalajson.ast.unsafe._

object JsonAst {
  object Binding extends Json.JsonAstBinding[JValue] {
    type V = JValue

    @pure def toObject(fields: ISZ[(String, V)]): V =
      JObject((for (f <- fields.elements) yield JField(f._1.value, f._2)).toArray)

    @pure def toArray(elements: ISZ[V]): V =
      JArray(elements.elements.toArray)

    @pure def toNumber(s: String): V = JNumber(s.value)

    @pure def toString(s: String): V = JString(s.value)

    @pure def toNull(): V = JNull

    @pure def toBoolean(b: B): V = if (b.value) JTrue else JFalse

    @pure def kind(o: V): ValueKind.Type = o match {
      case _: JObject => ValueKind.Object
      case _: JArray => ValueKind.Array
      case _: JString => ValueKind.String
      case _: JNumber => ValueKind.Number
      case JTrue => ValueKind.True
      case JFalse => ValueKind.False
      case JNull => ValueKind.Null
    }

    @pure def fromObject(o: V): ISZ[(String, V)] =
      ISZ(o.asInstanceOf[JObject].value.map(f => (_2String(f.field), f.value)): _*)

    @pure def fromArray(o: V): ISZ[V] =
      ISZ(o.asInstanceOf[JArray].value: _*)

    @pure def fromNumber(o: V): String = o.asInstanceOf[JNumber].value

    @pure def fromString(o: V): String = o.asInstanceOf[JString].value

    @pure def fromBoolean(o: V): B = _2B(o.asInstanceOf[JBoolean].get)

    def string: String = halt("Unsupported Binding operationg 'string'.")
  }
}
