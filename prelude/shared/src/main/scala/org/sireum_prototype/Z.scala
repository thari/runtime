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

  type Index = Int

  val zero: Z = Long(0)
  val one: Z = Long(1)
  val mone: Z = Long(-1)

  val longMin = scala.BigInt(scala.Long.MinValue)
  val longMax = scala.BigInt(scala.Long.MaxValue)

  private[sireum_prototype] sealed trait MP extends Z {

    final def isBitVector: B = F

    final def isIndex: B = F

    final def hasMin: B = F

    final def hasMax: B = F

    final def Min: Z = halt("Unsupported Z operation 'Min'.")

    final def Max: Z = halt("Unsupported Z operation 'Max'.")

    final def BitWidth: Z = halt("Unsupported Z operation 'BitWidth'.")

    final def increase: Z = this + one

    final def decrease: Z = this - one

    final def >>(other: Z): Z = halt("Unsupported Z operation '>>'.")

    final def >>>(other: Z): Z = halt("Unsupported Z operation '>>>'.")

    final def <<(other: Z): Z = halt("Unsupported Z operation '<<'.")

    final def &(other: Z): Z = halt("Unsupported Z operation '&'.")

    final def |(other: Z): Z = halt("Unsupported Z operation '|'.")

    final def |^(other: Z): Z = halt("Unsupported Z operation '|^'.")

    final def unary_~(): B = halt("Unsupported Z operation '~'.")

  }

  private[sireum_prototype] final case class Long(value: scala.Long) extends MP {

    def toIndex: Z.Index = {
      assume(Int.MinValue <= value && value <= Int.MaxValue)
      value.toInt
    }

    def toBigInt: scala.BigInt = scala.BigInt(value)

    def string: String = value.toString

    override def hashCode: Int = value.toInt

  }

  private[sireum_prototype] final case class BigInt(value: scala.BigInt) extends MP {
    def toIndex: Z.Index = {
      assume(Int.MinValue <= value && value <= Int.MaxValue)
      value.toInt
    }

    def toBigInt: scala.BigInt = value

    def string: String = value.toString

    def pack: Z =
      if ((value.compareTo(Z.longMin) >= 0) &&
        (value.compareTo(Z.longMax) <= 0)) Long(value.longValue)
      else this

    override def hashCode: Int = value.toInt
  }

  @inline def unary_-(n: Z): Z = {
    n match {
      case Long(m) =>
        if (m != scala.Long.MinValue)
          return Long(-m)
      case _ =>
    }
    BigInt(-n.toBigInt)
  }

  @inline def +(n: Z, other: Z): Z = {
    (n, other) match {
      case (Long(n1), Long(n2)) =>
        val r = n1 + n2
        if (((n1 ^ r) & (n2 ^ r)) >= 0L)
          return Long(r)
      case _ =>
    }
    BigInt(n.toBigInt + other.toBigInt)
  }

  @inline def -(n: Z, other: Z): Z = {
    (n, other) match {
      case (Long(n1), Long(n2)) =>
        val r = n1 - n2
        if (((n1 ^ r) & (n2 ^ r)) >= 0L)
          return Long(r)
      case _ =>
    }
    BigInt(n.toBigInt - other.toBigInt).pack
  }

  @inline def *(n: Z, other: Z): Z = {
    (n, other) match {
      case (Long(n1), Long(n2)) =>
        val r = n1 * n2
        if (r == 0) return Z.zero
        var upgrade = false
        if (n2 > n1) {
          if (((n2 == -1) && (n1 == scala.Long.MinValue)) || (r / n2 != n1))
            upgrade = true
        } else {
          if (((n1 == -1) && (n2 == scala.Long.MinValue)) || (r / n1 != n2))
            upgrade = true
        }
        if (!upgrade) return Long(r)
      case _ =>
    }
    BigInt(n.toBigInt * other.toBigInt)
  }

  @inline def /(n: Z, other: Z): Z = {
    (n, other) match {
      case (Long(n1), Long(n2)) =>
        val r = n1 / n2
        if (!((n1 == scala.Long.MinValue) && (n2 == -1)))
          return Long(r)
      case _ =>
    }
    BigInt(n.toBigInt / other.toBigInt).pack
  }

  @inline def %(n: Z, other: Z): Z =
    BigInt(n.toBigInt % other.toBigInt).pack

  @inline def >(n: Z, other: Z): B = (n, other) match {
    case (Long(n1), Long(n2)) => n1 > n2
    case _ => n.toBigInt > other.toBigInt
  }

  @inline def >=(n: Z, other: Z): B = (n, other) match {
    case (Long(n1), Long(n2)) => n1 >= n2
    case _ => n.toBigInt >= other.toBigInt
  }

  @inline def <(n: Z, other: Z): B = (n, other) match {
    case (Long(n1), Long(n2)) => n1 < n2
    case _ => n.toBigInt < other.toBigInt
  }

  @inline def <=(n: Z, other: Z): B = (n, other) match {
    case (Long(n1), Long(n2)) => n1 <= n2
    case _ => n.toBigInt <= other.toBigInt
  }

  @inline def isEqual(n: Z, other: Z): B = (n, other) match {
    case (n: Long, other: Long) => n.value == other.value
    case _ => n.toBigInt == other.toBigInt
  }

  @inline def string(n: Z): String = n.toString


  import scala.language.implicitConversions

  @inline implicit def $2Z(n: scala.Int): Z = Long(n)

  @inline implicit def $2lZ(n: scala.Long): Z = Long(n)

  @inline implicit def $2BIZ(n: scala.BigInt): Z = BigInt(n).pack

  @inline implicit def $2JBIZ(n: java.math.BigInteger): Z = scala.BigInt(n)

}

sealed trait Z extends Number[Z] {

  @pure def isBitVector: B

  @pure def isIndex: B

  @pure def hasMin: B

  @pure def hasMax: B

  @pure def Min: Z

  @pure def Max: Z

  @pure def BitWidth: Z

  @inline @pure final def unary_- : Z = Z.unary_-(this)

  @inline @pure final def +(other: Z): Z = Z.+(this, other)

  @inline @pure final def -(other: Z): Z = Z.-(this, other)

  @inline @pure final def *(other: Z): Z = Z.*(this, other)

  @inline @pure final def /(other: Z): Z = Z./(this, other)

  @inline @pure final def %(other: Z): Z = Z.%(this, other)

  @inline @pure final def >(other: Z): B = Z.>(this, other)

  @inline @pure final def >=(other: Z): B = Z.>=(this, other)

  @inline @pure final def <(other: Z): B = Z.<(this, other)

  @inline @pure final def <=(other: Z): B = Z.<=(this, other)

  @pure def increase: Z

  @pure def decrease: Z

  @pure def >>(other: Z): Z

  @pure def >>>(other: Z): Z

  @pure def <<(other: Z): Z

  @pure def &(other: Z): Z

  @pure def |(other: Z): Z

  @pure def |^(other: Z): Z

  @pure def unary_~(): B

  @pure def toIndex: Z.Index

  final def isEqual(other: Immutable): B = this == other

  final def hash: Z = hashCode

  private[sireum_prototype] def toBigInt: scala.BigInt

  final override def equals(other: Any): Boolean = other match {
    case other: Z => Z.isEqual(this, other)
    case _ => false
  }
}
