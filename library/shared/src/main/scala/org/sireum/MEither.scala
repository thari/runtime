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

@record trait MEither[L, R] {
  @pure def isLeft: B
  @pure def isRight: B
  @pure def leftOpt: Option[L]
  @pure def left: L
  @pure def rightOpt: Option[R]
  @pure def right: R
}

object MEither {

  @record class Left[L, R](value: L) extends MEither[L, R] {

    @pure override def isLeft: B = {
      l""" ensures result ≡ T """
      return T
    }

    @pure override def isRight: B = {
      l""" ensures result ≡ F """
      return F
    }

    @pure override def leftOpt: Option[L] = {
      l""" ensures result ≡ Some(value) """
      return Some(value)
    }

    @pure override def left: L = {
      l""" ensures result ≡ value """
      return value
    }

    @pure override def rightOpt: Option[R] = {
      l""" ensures result ≡ None[R]() """
      return None()
    }

    @pure override def right: R = {
      l""" requires F """
      halt("Invalid 'MEither.Left' operation 'right'.")
    }

  }

  @record class Right[L, R](value: R) extends MEither[L, R] {

    @pure override def isLeft: B = {
      l""" ensures result ≡ F """
      return F
    }

    @pure override def isRight: B = {
      l""" ensures result ≡ T """
      return T
    }

    @pure override def leftOpt: Option[L] = {
      l""" ensures result ≡ None[L]() """
      return None()
    }

    @pure override def left: L = {
      l""" requires F """
      halt("Invalid 'MEither.Right' operation 'left'.")
    }

    @pure override def rightOpt: Option[R] = {
      l""" ensures result ≡ Some(value) """
      return Some(value)
    }

    @pure override def right: R = {
      l""" ensures result ≡ value """
      return value
    }

  }

  @pure def left[L, R](value: L): MEither[L, R] = {
    return Left(value)
  }

  @pure def right[L, R](value: R): MEither[L, R] = {
    return Right(value)
  }

}
