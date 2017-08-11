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

package org.sireum_prototype


object Z {

  import scala.language.implicitConversions

  @inline implicit def $2Z(n: Int): Z = ???

  @inline implicit def $2lZ(n: Long): Z = ???

  @inline implicit def $2BIZ(n: BigInt): Z = ???

  @inline implicit def $2JBIZ(n: java.math.BigInteger): Z = ???

}

sealed trait Z extends Number[Z] {

  def BitWidthOpt: Option[Z] = ???

  def MinOpt: Option[Z] = ???

  def MaxOpt: Option[Z] = ???

  def Min: Z = ???

  def Max: Z = ???

  def BitWidth: Z = ???

  def >>(other: Z): Z = ???

  def >>>(other: Z): Z = ???

  def <<(other: Z): Z = ???

  def &(other: Z): Z = ???

  def |(other: Z): Z = ???

  def |^(other: Z): Z = ???

  def unary_~(): B = ???

  def increase: Z = ???

  def decrease: Z = ???

}
