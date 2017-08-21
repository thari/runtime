// #Sireum
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

import org.sireum.ops._
import org.sireum.ops.ISZOps._

object Json {

  @sig trait JsonAstBinding[V] {
    @pure def toObject(fields: ISZ[(String, V)]): V

    @pure def toArray(elements: ISZ[V]): V

    @pure def toNumber(s: String): V

    @pure def toString(s: String): V

    @pure def toNull(): V

    @pure def toBoolean(b: B): V

    @pure def kind(o: V): ValueKind.Type

    @pure def fromObject(o: V): ISZ[(String, V)]

    @pure def fromArray(o: V): ISZ[V]

    @pure def fromNumber(o: V): String

    @pure def fromString(o: V): String

    @pure def fromBoolean(o: V): B
  }

  @datatype class ErrorMsg(line: Z, column: Z, message: String)

  @enum object ValueKind {
    'String
    'Number
    'Object
    'Array
    'True
    'False
    'Null
  }

  object Printer {
    val trueSt: ST = st"true"
    val falseSt: ST = st"false"
    val nullSt: ST = st"null"

    @pure def printB(b: B): ST = {
      if (b) {
        return trueSt
      } else {
        return falseSt
      }
    }

    @pure def printC(c: C): ST = {
      return printString(c.string)
    }

    @pure def printZ(n: Z): ST = {
      return printNumber(n.string)
    }

    @pure def printZ8(n: Z8): ST = {
      return printNumber(n.string)
    }

    @pure def printZ16(n: Z16): ST = {
      return printNumber(n.string)
    }

    @pure def printZ32(n: Z32): ST = {
      return printNumber(n.string)
    }

    @pure def printZ64(n: Z64): ST = {
      return printNumber(n.string)
    }

    @pure def printN(n: N): ST = {
      return printNumber(n.string)
    }

    @pure def printN8(n: N8): ST = {
      return printNumber(n.string)
    }

    @pure def printN16(n: N16): ST = {
      return printNumber(n.string)
    }

    @pure def printN32(n: N32): ST = {
      return printNumber(n.string)
    }

    @pure def printN64(n: N64): ST = {
      return printNumber(n.string)
    }

    @pure def printS8(n: S8): ST = {
      return printNumber(n.string)
    }

    @pure def printS16(n: S16): ST = {
      return printNumber(n.string)
    }

    @pure def printS32(n: S32): ST = {
      return printNumber(n.string)
    }

    @pure def printS64(n: S64): ST = {
      return printNumber(n.string)
    }

    @pure def printU8(n: U8): ST = {
      return printNumber(n.string)
    }

    @pure def printU16(n: U16): ST = {
      return printNumber(n.string)
    }

    @pure def printU32(n: U32): ST = {
      return printNumber(n.string)
    }

    @pure def printU64(n: U64): ST = {
      return printNumber(n.string)
    }

    @pure def printF32(n: F32): ST = {
      return printNumber(n.string)
    }

    @pure def printF64(n: F64): ST = {
      return printNumber(n.string)
    }

    @pure def printR(n: R): ST = {
      return printNumber(n.string)
    }

    @pure def printISZ[T](isSimple: B, s: IS[Z, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISZ8[T](isSimple: B, s: IS[Z8, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISZ16[T](isSimple: B, s: IS[Z16, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISZ32[T](isSimple: B, s: IS[Z32, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISZ64[T](isSimple: B, s: IS[Z64, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISN[T](isSimple: B, s: IS[N, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISN8[T](isSimple: B, s: IS[N8, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISN16[T](isSimple: B, s: IS[N16, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISN32[T](isSimple: B, s: IS[N32, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISN64[T](isSimple: B, s: IS[N64, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISS8[T](isSimple: B, s: IS[S8, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISS16[T](isSimple: B, s: IS[S16, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISS32[T](isSimple: B, s: IS[S32, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISS64[T](isSimple: B, s: IS[S64, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISU8[T](isSimple: B, s: IS[U8, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISU16[T](isSimple: B, s: IS[U16, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISU32[T](isSimple: B, s: IS[U32, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printISU64[T](isSimple: B, s: IS[U64, T], f: T => ST): ST = {
      return printIS(isSimple, s.map(f))
    }

    @pure def printMSZ[T](isSimple: B, s: MS[Z, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSZ8[T](isSimple: B, s: MS[Z8, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSZ16[T](isSimple: B, s: MS[Z16, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSZ32[T](isSimple: B, s: MS[Z32, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSZ64[T](isSimple: B, s: MS[Z64, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSN[T](isSimple: B, s: MS[N, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSN8[T](isSimple: B, s: MS[N8, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSN16[T](isSimple: B, s: MS[N16, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSN32[T](isSimple: B, s: MS[N32, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSN64[T](isSimple: B, s: MS[N64, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSS8[T](isSimple: B, s: MS[S8, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSS16[T](isSimple: B, s: MS[S16, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSS32[T](isSimple: B, s: MS[S32, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSS64[T](isSimple: B, s: MS[S64, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSU8[T](isSimple: B, s: MS[U8, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSU16[T](isSimple: B, s: MS[U16, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSU32[T](isSimple: B, s: MS[U32, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printMSU64[T](isSimple: B, s: MS[U64, T], f: T => ST): ST = {
      return printMS(isSimple, s.map(f))
    }

    @pure def printOption[T](o: Option[T], f: T => ST): ST = {
      o match {
        case Some(t) => return printObject(ISZ(("type", printString("Some")), ("value", f(t))))
        case _ => return printObject(ISZ(("type", printString("None"))))
      }
    }

    @pure def printMOption[T](o: MOption[T], f: T => ST): ST = {
      o match {
        case MSome(t) => return printObject(ISZ(("type", printString("Some")), ("value", f(t))))
        case _ => return printObject(ISZ(("type", printString("None"))))
      }
    }

    @pure def printEither[L, R](o: Either[L, R], f0: L => ST, f1: R => ST): ST = {
      o match {
        case Either(Some(l), _) => return printObject(ISZ(("type", printString("Or")), ("i", printZ(0)), ("value", f0(l))))
        case Either(_, Some(r)) => return printObject(ISZ(("type", printString("Or")), ("i", printZ(1)), ("value", f1(r))))
        case _ => assume(F); return nullSt
      }
    }

    @pure def printMEither[L, R](o: MEither[L, R], f0: L => ST, f1: R => ST): ST = {
      o match {
        case MEither(MSome(l), _) => return printObject(ISZ(("type", printString("Or")), ("i", printZ(0)), ("value", f0(l))))
        case MEither(_, MSome(r)) => return printObject(ISZ(("type", printString("Or")), ("i", printZ(1)), ("value", f1(r))))
        case _ => assume(F); return nullSt
      }
    }

    @pure def printString(s: String): ST = {
      var r = ISZ[C]()
      for (c <- s.toCis) {
        c match {
          case '"' => r = r :+ '\\' :+ '\"'
          case '\\' => r = r :+ '\\' :+ '\\'
          case '/' => r = r :+ '\\' :+ '/'
          case '\b' => r = r :+ '\\' :+ 'b'
          case '\f' => r = r :+ '\\' :+ 'f'
          case '\n' => r = r :+ '\\' :+ 'n'
          case '\r' => r = r :+ '\\' :+ 'r'
          case '\t' => r = r :+ '\\' :+ 't'
          case _ if '\u0020' <= c && c < '\u00FF' && c != '\u007f' => r = r :+ c
          case _ =>
            val q = COps(c).toUnicodeHex
            r = r :+ '\\' :+ 'u' :+ q._1 :+ q._2 :+ q._3 :+ q._4
        }
      }
      return st""""${String(r)}""""
    }

    @pure def printConstant(s: String): ST = {
      s match {
        case "true" => return trueSt
        case "false" => return falseSt
        case "null" => return nullSt
      }
    }

    @pure def printNumber(s: String): ST = {
      return st"$s"
    }

    @pure def printObject(fields: ISZ[(String, ST)]): ST = {
      val fs: ISZ[ST] = for (p <- fields) yield st""""${p._1}" : ${p._2}"""
      return st"""{
                 |  ${(fs, ",\n")}
                 |}"""
    }

    @pure def printIS[I <: Z](isSimple: B, elements: IS[I, ST]): ST = {
      if (isSimple) {
        return st"[${(elements, ", ")}]"
      } else {
        return st"""[
                   |  ${(elements, ",\n")}
                   |]"""
      }
    }

    @pure def printMS[I <: Z](isSimple: B, elements: MS[I, ST]): ST = {
      if (isSimple) {
        return st"[${(elements, ", ")}]"
      } else {
        return st"""[
                   |  ${(elements, ",\n")}
                   |]"""
      }
    }
  }

  object Parser {
    @pure def create(input: String): Parser = {
      return Parser(input.toCis, 0, None())
    }
  }

  @record class Parser(input: ISZ[C],
                       var offset: Z,
                       var errorOpt: Option[ErrorMsg]) {

    val typesOption: ISZ[String] = ISZ("Some", "None")

    def errorMessage: String = {
      errorOpt match {
        case Some(e) => return s"[${e.line}, ${e.column}] ${e.message}"
        case _ => return ""
      }
    }

    def eof(): B = {
      if (input.size != offset) {
        if (errorOpt.nonEmpty) {
          return F
        }
        val p = computeLineColumn(offset)
        errorOpt = Some(ErrorMsg(p._1, p._2, s"Expected end-of-file, but '${input(offset)}' found."))
        return F
      } else {
        return T
      }
    }

    def parseB(): B = {
      errorIfEof(offset)
      at(offset) match {
        case 't' => parseConstant("true"); return T
        case 'f' => parseConstant("false"); return F
        case c => parseException(offset, s"Expected 'true' or 'false', but '$c...' found."); return F
      }
    }

    def parseC(): C = {
      val i = offset
      val s = parseString().toCis
      if (s.size != 1) {
        parseException(i, s"Expected a C, but '$s' found.")
        return ' '
      } else {
        return s(0)
      }
    }

    def parseZ(): Z = {
      val i = offset
      val s = parseNumber()
      Z(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a Z, but '$s' found.")
          return 0
      }
    }

    def parseZ8(): Z8 = {
      val i = offset
      val s = parseNumber()
      Z8(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a Z8, but '$s' found.")
          return Z8(0)
      }
    }

    def parseZ16(): Z16 = {
      val i = offset
      val s = parseNumber()
      Z16(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a Z16, but '$s' found.")
          return Z16(0)
      }
    }

    def parseZ32(): Z32 = {
      val i = offset
      val s = parseNumber()
      Z32(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a Z32, but '$s' found.")
          return Z32(0)
      }
    }

    def parseZ64(): Z64 = {
      val i = offset
      val s = parseNumber()
      Z64(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a Z64, but '$s' found.")
          return Z64(0)
      }
    }

    def parseN(): N = {
      val i = offset
      val s = parseNumber()
      N(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a N, but '$s' found.")
          return N(0)
      }
    }

    def parseN8(): N8 = {
      val i = offset
      val s = parseNumber()
      N8(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a N8, but '$s' found.")
          return N8(0)
      }
    }

    def parseN16(): N16 = {
      val i = offset
      val s = parseNumber()
      N16(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a N16, but '$s' found.")
          return N16(0)
      }
    }

    def parseN32(): N32 = {
      val i = offset
      val s = parseNumber()
      N32(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a N32, but '$s' found.")
          return N32(0)
      }
    }

    def parseN64(): N64 = {
      val i = offset
      val s = parseNumber()
      N64(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a N64, but '$s' found.")
          return N64(0)
      }
    }

    def parseS8(): S8 = {
      val i = offset
      val s = parseNumber()
      S8(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a S8, but '$s' found.")
          return S8(0)
      }
    }

    def parseS16(): S16 = {
      val i = offset
      val s = parseNumber()
      S16(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a S16, but '$s' found.")
          return S16(0)
      }
    }

    def parseS32(): S32 = {
      val i = offset
      val s = parseNumber()
      S32(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a S32, but '$s' found.")
          return S32(0)
      }
    }

    def parseS64(): S64 = {
      val i = offset
      val s = parseNumber()
      S64(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a S64, but '$s' found.")
          return S64(0)
      }
    }

    def parseU8(): U8 = {
      val i = offset
      val s = parseNumber()
      U8(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a U8, but '$s' found.")
          return U8(0)
      }
    }

    def parseU16(): U16 = {
      val i = offset
      val s = parseNumber()
      U16(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a U16, but '$s' found.")
          return U16(0)
      }
    }

    def parseU32(): U32 = {
      val i = offset
      val s = parseNumber()
      U32(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a U32, but '$s' found.")
          return U32(0)
      }
    }

    def parseU64(): U64 = {
      val i = offset
      val s = parseNumber()
      U64(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a U64, but '$s' found.")
          return U64(0)
      }
    }

    def parseF32(): F32 = {
      val i = offset
      val s = parseNumber()
      F32(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a F32, but '$s' found.")
          return 0f
      }
    }

    def parseF64(): F64 = {
      val i = offset
      val s = parseNumber()
      F64(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a F64, but '$s' found.")
          return 0.0
      }
    }

    def parseR(): R = {
      val i = offset
      val s = parseNumber()
      R(s) match {
        case Some(n) => return n
        case _ =>
          parseException(i, s"Expected a R, but '$s' found.")
          return R(0)
      }
    }

    def parseISZ[T](f: () => T): IS[Z, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[Z, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISZ8[T](f: () => T): IS[Z8, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[Z8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISZ16[T](f: () => T): IS[Z16, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[Z16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISZ32[T](f: () => T): IS[Z32, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[Z32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISZ64[T](f: () => T): IS[Z64, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[Z64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISN[T](f: () => T): IS[N, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[N, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISN8[T](f: () => T): IS[N8, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[N8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISN16[T](f: () => T): IS[N16, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[N16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISN32[T](f: () => T): IS[N32, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[N32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISN64[T](f: () => T): IS[N64, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[N64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISS8[T](f: () => T): IS[S8, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[S8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISS16[T](f: () => T): IS[S16, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[S16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISS32[T](f: () => T): IS[S32, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[S32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISS64[T](f: () => T): IS[S64, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[S64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISU8[T](f: () => T): IS[U8, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[U8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISU16[T](f: () => T): IS[U16, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[U16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISU32[T](f: () => T): IS[U32, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[U32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseISU64[T](f: () => T): IS[U64, T] = {
      if (!parseArrayBegin()) {
        return IS()
      }
      var e = f()
      var r = IS[U64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSZ[T](f: () => T): MS[Z, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[Z, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSZ8[T](f: () => T): MS[Z8, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[Z8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSZ16[T](f: () => T): MS[Z16, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[Z16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSZ32[T](f: () => T): MS[Z32, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[Z32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSZ64[T](f: () => T): MS[Z64, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[Z64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSN[T](f: () => T): MS[N, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[N, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSN8[T](f: () => T): MS[N8, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[N8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSN16[T](f: () => T): MS[N16, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[N16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSN32[T](f: () => T): MS[N32, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[N32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSN64[T](f: () => T): MS[N64, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[N64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSS8[T](f: () => T): MS[S8, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[S8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSS16[T](f: () => T): MS[S16, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[S16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSS32[T](f: () => T): MS[S32, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[S32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSS64[T](f: () => T): MS[S64, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[S64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSU8[T](f: () => T): MS[U8, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[U8, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSU16[T](f: () => T): MS[U16, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[U16, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSU32[T](f: () => T): MS[U32, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[U32, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseMSU64[T](f: () => T): MS[U64, T] = {
      if (!parseArrayBegin()) {
        return MS()
      }
      var e = f()
      var r = MS[U64, T](e)
      var continue = parseArrayNext()
      while (continue) {
        e = f()
        r = r :+ e
        continue = parseArrayNext()
      }
      return r
    }

    def parseOption[T](f: () => T): Option[T] = {
      val tpe = parseObjectTypes(typesOption)
      tpe match {
        case "Some" =>
          parseObjectKey("value")
          val v = f()
          parseObjectNext()
          return Some(v)
        case "None" =>
          return None()
      }
    }

    def parseMOption[T](f: () => T): MOption[T] = {
      val tpe = parseObjectTypes(typesOption)
      tpe match {
        case "Some" =>
          parseObjectKey("value")
          val v = f()
          parseObjectNext()
          return MSome(v)
        case "None" =>
          return MNone()
      }
    }

    def parseEither[L, R](f0: () => L, f1: () => R): Either[L, R] = {
      parseObjectType("Or")
      parseObjectKey("i")
      val i = parseZ()
      parseObjectKey("value")
      i match {
        case 0 =>
          val l = f0()
          parseObjectNext()
          return Either(Some(l), None())
        case 1 =>
          val r = f1()
          parseObjectNext()
          return Either(None(), Some(r))
      }
    }

    def parseMEither[L, R](f0: () => L, f1: () => R): MEither[L, R] = {
      parseObjectType("Or")
      parseObjectKey("i")
      val i = parseZ()
      parseObjectKey("value")
      i match {
        case 0 =>
          val l = f0()
          parseObjectNext()
          return MEither(MSome(l), MNone())
        case 1 =>
          val r = f1()
          parseObjectNext()
          return MEither(MNone(), MSome(r))
      }
    }

    def at(i: Z): C = {
      if (0 <= i && i < input.length && errorOpt.isEmpty) {
        return input(i)
      }
      return '\u0000'
    }

    def detect(): ValueKind.Type = {
      val default = ValueKind.Null
      parseWhitespace()
      errorIfEof(offset)
      at(offset) match {
        case '"' => return ValueKind.String
        case '{' => return ValueKind.Object
        case '[' => return ValueKind.Array
        case 't' => return ValueKind.True
        case 'f' => return ValueKind.False
        case 'n' => return ValueKind.Null
        case '-' => return ValueKind.Number
        case '0' => return ValueKind.Number
        case '1' => return ValueKind.Number
        case '2' => return ValueKind.Number
        case '3' => return ValueKind.Number
        case '4' => return ValueKind.Number
        case '5' => return ValueKind.Number
        case '6' => return ValueKind.Number
        case '7' => return ValueKind.Number
        case '8' => return ValueKind.Number
        case '9' => return ValueKind.Number
        case _ =>
          parseException(offset, "Unexpected end-of-file.")
          return ValueKind.Null
      }
    }

    def parseObjectType(expectedType: String): String = {
      parseObjectBegin()
      parseObjectKey("type")
      val i = offset + 1
      val value = parseString()
      parseObjectNext()
      if (value != expectedType) {
        parseException(i, s"Expected '${expectedType}', but '$value' found.")
      }
      return value
    }

    def parseObjectTypes(expectedTypes: ISZ[String]): String = {
      parseObjectBegin()
      parseObjectKey("type")
      val i = offset + 1
      val value = parseString()
      parseObjectNext()
      if (expectedTypes.nonEmpty && !SOps(expectedTypes).contains(value)) {
        expectedTypes.size match {
          case 1 => parseException(i, s"Expected '${expectedTypes(0)}', but '$value' found.")
          case 2 => parseException(i, s"Expected '${expectedTypes(0)}' or '${expectedTypes(1)}' , but '$value' found.")
          case _ => parseException(i, s"Expected ${st"'${(ISOps(expectedTypes).dropRight(1), "', '")}', or '${expectedTypes(expectedTypes.size - 1)}'".render} , but '$value' found.")
        }
      }
      return value
    }

    def parseObjectKey(expectedKey: String): String = {
      errorIfEof(offset)
      val i = offset + 1
      val key = parseString()
      if (key != expectedKey) {
        parseException(i, s"Expected '$expectedKey', but '$key' found.")
      }
      parseWhitespace()
      errorIfEof(offset)
      at(offset) match {
        case ':' =>
          offset = offset + 1
          parseWhitespace()
          return key
        case c =>
          parseException(offset, s"Expected ':', but '$c' found.")
          return ""
      }
    }

    def parseObjectKeys(expectedKeys: ISZ[String]): String = {
      errorIfEof(offset)
      val i = offset + 1
      val key = parseString()
      if (expectedKeys.nonEmpty && !SOps(expectedKeys).contains(key)) {
        expectedKeys.size match {
          case 1 => parseException(i, s"Expected '${expectedKeys(0)}', but '$key' found.")
          case 2 => parseException(i, s"Expected '${expectedKeys(0)}' or '${expectedKeys(1)}' , but '$key' found.")
          case _ => parseException(i, s"Expected ${st"'${(ISOps(expectedKeys).dropRight(1), "', '")}', or '${expectedKeys(expectedKeys.size - 1)}'".render} , but '$key' found.")
        }
      }
      parseWhitespace()
      errorIfEof(offset)
      at(offset) match {
        case ':' =>
          offset = offset + 1
          parseWhitespace()
          return key
        case c =>
          parseException(offset, s"Expected ':', but '$c' found.")
          return ""
      }
    }

    def parseObjectBegin(): Boolean = {
      errorIfEof(offset)
      at(offset) match {
        case '{' =>
          offset = offset + 1
          parseWhitespace()
          errorIfEof(offset)
          at(offset) match {
            case '}' =>
              offset = offset + 1
              return F
            case _ =>
              return T
          }
        case c =>
          parseException(offset, s"Expected '{', but '$c' found.")
          return F
      }
    }

    def parseObjectNext(): Boolean = {
      parseWhitespace()
      errorIfEof(offset)
      at(offset) match {
        case ',' =>
          offset = offset + 1
          parseWhitespace()
          return T
        case '}' =>
          offset = offset + 1
          parseWhitespace()
          return F
        case c =>
          parseException(offset, s"Expected ',' or '}', but '$c' found.")
          return F
      }
    }

    def parseArrayBegin(): Boolean = {
      errorIfEof(offset)
      at(offset) match {
        case '[' =>
          offset = offset + 1
          parseWhitespace()
          errorIfEof(offset)
          at(offset) match {
            case ']' =>
              offset = offset + 1
              return F
            case _ =>
              return T
          }
        case c =>
          parseException(offset, s"Expected '[', but '$c' found.")
          return F
      }
    }

    def parseArrayNext(): Boolean = {
      parseWhitespace()
      errorIfEof(offset)
      at(offset) match {
        case ',' =>
          offset = offset + 1
          parseWhitespace()
          return T
        case ']' =>
          offset = offset + 1
          parseWhitespace()
          return F
        case c => parseException(offset, s"Expected ',' or ']', but '$c' found.")
          return F
      }
    }

    def parseNumber(): String = {
      var r = ISZ[C]()

      errorIfEof(offset)

      var c = at(offset)
      c match {
        case '-' =>
          r = r :+ c
          c = incOffset(1)
        case _ =>
          if (!isDigit(c)) {
            parseException(offset, s"""Expected a '-' or a digit but '$c' found.""")
          }
      }

      c match {
        case '0' =>
          r = r :+ c
          if (offset + 1 == input.size) {
            offset = offset + 1
            return String(r)
          }
          c = incOffset(1)
        case _ =>
          r = r :+ c
          if (offset + 1 == input.size) {
            offset = offset + 1
            return String(r)
          }
          c = incOffset(1)
          while (isDigit(c)) {
            r = r :+ c
            if (offset + 1 == input.size) {
              offset = offset + 1
              return String(r)
            }
            c = incOffset(1)
          }
      }

      c match {
        case '.' =>
          r = r :+ c
          c = incOffset(1)
          while (isDigit(c)) {
            r = r :+ c
            if (offset + 1 == input.size) {
              offset = offset + 1
              return String(r)
            }
            c = incOffset(1)
          }
        case _ =>
      }

      c match {
        case 'e' =>
        case 'E' =>
        case _ => return String(r)
      }
      r = r :+ c
      c = incOffset(1)
      val hasPlusMinus: B = c match {
        case '+' => T
        case '-' => T
        case _ => F
      }
      if (hasPlusMinus) {
        r = r :+ c
        c = incOffset(1)
      }
      while (isDigit(c)) {
        r = r :+ c
        if (offset + 1 == input.size) {
          offset = offset + 1
          return String(r)
        }
        c = incOffset(1)
      }
      return String(r)
    }

    def parseString(): String = {
      errorIfEof(offset)

      var r = ISZ[C]()

      var c = at(offset)
      c match {
        case '"' =>
          c = incOffset(1)
          while (c != '"') {
            c match {
              case '\\' =>
                c = incOffset(1)
                c match {
                  case '"' => r = r :+ '"'
                  case '\\' => r = r :+ '\\'
                  case '/' => r = r :+ '/'
                  case 'b' => r = r :+ '\b'
                  case 'f' => r = r :+ '\f'
                  case 'n' => r = r :+ '\n'
                  case 'r' => r = r :+ '\r'
                  case 't' => r = r :+ '\t'
                  case 'u' =>
                    incOffset(4)
                    val hex = slice(offset - 3, offset + 1)
                    COps.fromUnicodeHex(hex) match {
                      case Some(ch) => r = r :+ ch
                      case _ => parseException(offset - 3, s"Expected a character hex but '$hex' found.")
                    }
                  case _ => parseException(offset, s"Expected an escaped character but '$c' found.")
                }
              case _ => r = r :+ c
            }
            c = incOffset(1)
          }
          offset = offset + 1
          return String(r)
        case _ =>
          parseException(offset, s"""Expected '"' but '$c' found.""")
          return ""
      }
    }

    def parseConstant(text: String): Unit = {
      errorIfEof(offset + text.size - 1)
      val t = String(slice(offset, offset + text.size))
      if (t != text) {
        parseException(offset, s"Expected '$text', but '$t' found.")
      }
      offset = offset + text.size
      text match {
        case "true" =>
        case "false" =>
        case "null" =>
        case _ => parseException(offset, s"Invalid constant value '$text'.")
      }
    }

    def computeLineColumn(i: Z): (Z, Z) = {
      var line: Z = 1
      var column: Z = 1
      var j: Z = 0
      while (j != i) {
        at(j) match {
          case '\n' =>
            line = line + 1
            column = 1
          case _ => column = column + 1
        }
        j = j + 1
      }
      return (line, column)
    }

    def parseException(i: Z, msg: String): Unit = {
      if (errorOpt.nonEmpty) {
        return
      }
      val p = computeLineColumn(i)
      errorOpt = Some(ErrorMsg(p._1, p._2, msg))
    }

    def errorIfEof(i: Z): Unit = {
      if (i >= input.length || errorOpt.nonEmpty) {
        parseException(offset, "Unexpected end-of-file.")
      }
    }

    def incOffset(n: Z): C = {
      offset = offset + n
      errorIfEof(offset)
      return at(offset)
    }

    def parseWhitespace(): Unit = {
      if (errorOpt.nonEmpty) {
        offset = input.size
        return
      }
      if (offset >= input.size) {
        return
      }
      var c = at(offset)
      while (isWhitespace(c)) {
        offset = offset + 1
        if (offset >= input.size) {
          return
        }
        c = at(offset)
      }
    }

    @pure def isDigit(c: C): B = {
      c match {
        case '0' => return T
        case '1' => return T
        case '2' => return T
        case '3' => return T
        case '4' => return T
        case '5' => return T
        case '6' => return T
        case '7' => return T
        case '8' => return T
        case '9' => return T
        case _ => return F
      }
    }

    @pure def isWhitespace(c: C): B = {
      c match {
        case ' ' => return T
        case '\n' => return T
        case '\r' => return T
        case '\t' => return T
        case _ => return F
      }
    }

    @pure def slice(start: Z, til: Z): ISZ[C] = {
      var r = ISZ[C]()
      for (i <- start until til) {
        r = r :+ at(i)
      }
      return r
    }
  }

  def parseAst[V](binding: JsonAstBinding[V], input: String): Either[V, ErrorMsg] = {
    val parser = Parser.create(input)
    val emptyKeys = ISZ[String]()

    def parseString(): V = {
      val s = parser.parseString()
      return binding.toString(s)
    }

    def parseNumber(): V = {
      val n = parser.parseNumber()
      return binding.toNumber(n)
    }

    def parseTrue(): V = {
      parser.parseConstant("true")
      return binding.toBoolean(T)
    }

    def parseFalse(): V = {
      parser.parseConstant("false")
      return binding.toBoolean(F)
    }

    def parseNull(): V = {
      parser.parseConstant("null")
      return binding.toNull()
    }

    def parseArray(): V = {
      var continue = parser.parseArrayBegin()
      if (!continue) {
        return binding.toArray(ISZ())
      }
      var v = parseValue()
      var values = ISZ[V](v)
      continue = parser.parseArrayNext()
      while (continue) {
        v = parseValue()
        values = values :+ v
        continue = parser.parseArrayNext()
      }
      return binding.toArray(values)
    }

    def parseObject(): V = {
      var continue = parser.parseObjectBegin()
      if (!continue) {
        return binding.toObject(ISZ())
      }
      var key = parser.parseObjectKeys(emptyKeys)
      var value = parseValue()
      var fields = ISZ[(String, V)]((key, value))
      continue = parser.parseObjectNext()
      while (continue) {
        key = parser.parseObjectKeys(emptyKeys)
        value = parseValue()
        fields = fields :+ ((key, value))
        continue = parser.parseObjectNext()
      }
      return binding.toObject(fields)
    }

    def parseValue(): V = {
      val k = parser.detect()
      k match {
        case ValueKind.String => val r = parseString(); return r
        case ValueKind.Object => val r = parseObject(); return r
        case ValueKind.Array => val r = parseArray(); return r
        case ValueKind.True => val r = parseTrue(); return r
        case ValueKind.False => val r = parseFalse(); return r
        case ValueKind.Null => val r = parseNull(); return r
        case ValueKind.Number => val r = parseNumber(); return r
      }
    }

    val r = parseValue()
    parser.eof()
    parser.errorOpt match {
      case Some(_) => return Either(None(), parser.errorOpt)
      case _ => return Either(Some(r), None())
    }
  }

  def printAst[V](binding: JsonAstBinding[V], v: V): ST = {
    @pure def isSimple(o: V): B = {
      binding.kind(o) match {
        case ValueKind.Object => return F
        case ValueKind.Array => return F
        case _ => return T
      }
    }

    @pure def printValue(o: V): ST = {
      binding.kind(o) match {
        case ValueKind.String => return Printer.printString(binding.fromString(o))
        case ValueKind.Number => return Printer.printNumber(binding.fromNumber(o))
        case ValueKind.Object =>
          return Printer.printObject(for (p <- binding.fromObject(o)) yield (p._1, printValue(p._2)))
        case ValueKind.Array =>
          val es = binding.fromArray(o)
          return Printer.printIS(SOps(es).forall(isSimple), es.map(printValue))
        case ValueKind.True => return Printer.trueSt
        case ValueKind.False => return Printer.falseSt
        case ValueKind.Null => return Printer.nullSt
      }
    }

    return printValue(v)
  }
}
