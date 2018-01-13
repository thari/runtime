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

@bits(signed = T, width = 16, min = -2, index = true) class S16_m2

@record trait F2

@record class Foo(x: Z, var bar: Bar) extends F2

@record class Bar(x: Z, var y: Z) extends F2

@datatype class Baz(x: Z, y: Z)

@datatype class Bazz()

@datatype class $Foo(x: Z, y: $Bar)

@datatype class $Bar(z: Z, zz: ISZ[Z])

@record class Bazzz[T](var x: T) {
  def updateX(newX: T): Unit = {
    x = newX
  }
}

@enum object Day {
  'Monday
  'Tuesday
  'Wednesday
  'Thursday
  'Friday
  'Saturday
  'Sunday
}

@ext object NFoo {

  trait NA

  val x: Z = $
  var y: NA = $

  def foo[T](x: Z): T = $
}

@range(min = 1, max = 10, index = T) class One10i

@range(min = -1, max = 16) class M1_16
