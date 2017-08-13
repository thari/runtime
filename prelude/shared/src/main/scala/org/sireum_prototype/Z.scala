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

import spire.math._

object Z {

  type Index = MP

  val longMin = scala.BigInt(scala.Long.MinValue)
  val longMax = scala.BigInt(scala.Long.MaxValue)

  object MP {

    val zero: MP = MP.Long(0)
    val one: MP = MP.Long(1)
    val mone: MP = MP.Long(-1)

    private[sireum_prototype] final case class Long(value: scala.Long) extends MP {

      override def toBigInt: scala.BigInt = scala.BigInt(value)

      override def toIntOpt: scala.Option[scala.Int] =
        if (scala.Int.MinValue <= value && value <= scala.Int.MaxValue) scala.Some(value.toInt)
        else scala.None

      override def toLongOpt: scala.Option[scala.Long] = scala.Some(value)

      override def toString: Predef.String = value.toString

      override def hashCode: scala.Int = value.toInt

    }

    private[sireum_prototype] final case class BigInt(value: scala.BigInt) extends MP {

      override def toBigInt: scala.BigInt = value

      override def toIntOpt: scala.Option[scala.Int] =
        if (scala.Int.MinValue <= value && value <= scala.Int.MaxValue) scala.Some(value.toInt)
        else scala.None

      override def toLongOpt: scala.Option[scala.Long] =
        if (scala.Long.MinValue <= value && value <= scala.Long.MaxValue) scala.Some(value.toLong)
        else scala.None

      override def toString: Predef.String = value.toString

      def pack: MP =
        if ((value.compareTo(Z.longMin) >= 0) &&
          (value.compareTo(Z.longMax) <= 0)) Long(value.longValue)
        else this

      override def hashCode: scala.Int = value.toInt
    }

    @inline def unsupported(op: Predef.String, other: Z): Nothing =
      halt(s"Unsupported Z operation '$op' with ${other.getClass.getSimpleName}")

    @inline def unary_-(n: Z): MP = {
      n match {
        case Long(m) =>
          if (m != scala.Long.MinValue)
            return Long(-m)
        case _: MP =>
      }
      BigInt(-n.toBigInt)
    }

    @inline def +(n: Z, other: Z): MP = {
      (n, other) match {
        case (Long(n1), Long(n2)) =>
          val r = n1 + n2
          if (((n1 ^ r) & (n2 ^ r)) >= 0L)
            return Long(r)
        case (_: MP, _: MP) =>
        case _ => unsupported("+", other)
      }
      BigInt(n.toBigInt + other.toBigInt)
    }

    @inline def -(n: Z, other: Z): MP = {
      (n, other) match {
        case (Long(n1), Long(n2)) =>
          val r = n1 - n2
          if (((n1 ^ r) & (n2 ^ r)) >= 0L)
            return Long(r)
        case (_: MP, _: MP) =>
        case _ => unsupported("-", other)
      }
      BigInt(n.toBigInt - other.toBigInt).pack
    }

    @inline def *(n: Z, other: Z): MP = {
      (n, other) match {
        case (Long(n1), Long(n2)) =>
          val r = n1 * n2
          if (r == 0) return zero
          var upgrade = false
          if (n2 > n1) {
            if (((n2 == -1) && (n1 == scala.Long.MinValue)) || (r / n2 != n1))
              upgrade = true
          } else {
            if (((n1 == -1) && (n2 == scala.Long.MinValue)) || (r / n1 != n2))
              upgrade = true
          }
          if (!upgrade) return Long(r)
        case (_: MP, _: MP) =>
        case _ => unsupported("*", other)
      }
      BigInt(n.toBigInt * other.toBigInt)
    }

    @inline def /(n: Z, other: Z): MP = {
      (n, other) match {
        case (Long(n1), Long(n2)) =>
          val r = n1 / n2
          if (!((n1 == scala.Long.MinValue) && (n2 == -1)))
            return Long(r)
        case (_: MP, _: MP) =>
        case _ => unsupported("/", other)
      }
      BigInt(n.toBigInt / other.toBigInt).pack
    }

    @inline def %(n: Z, other: Z): MP = {
      (n, other) match {
        case (_: MP, _: MP) =>
        case _ => unsupported("%", other)
      }
      BigInt(n.toBigInt % other.toBigInt).pack
    }

    @inline def >(n: Z, other: Z): B = (n, other) match {
      case (Long(n1), Long(n2)) => n1 > n2
      case (_: MP, _: MP) => n.toBigInt > other.toBigInt
      case _ => unsupported(">", other)
    }

    @inline def >=(n: Z, other: Z): B = (n, other) match {
      case (Long(n1), Long(n2)) => n1 >= n2
      case (_: MP, _: MP) => n.toBigInt >= other.toBigInt
      case _ => unsupported(">=", other)
    }

    @inline def <(n: Z, other: Z): B = (n, other) match {
      case (Long(n1), Long(n2)) => n1 < n2
      case (_: MP, _: MP) => n.toBigInt < other.toBigInt
      case _ => unsupported("<", other)
    }

    @inline def <=(n: Z, other: Z): B = (n, other) match {
      case (Long(n1), Long(n2)) => n1 <= n2
      case (_: MP, _: MP) => n.toBigInt <= other.toBigInt
      case _ => unsupported("<=", other)
    }

    @inline def isEqual(n: Z, other: Z): B = (n, other) match {
      case (n: Long, other: Long) => n.value == other.value
      case (_: MP, _: MP) => n.toBigInt == other.toBigInt
      case _ => unsupported("==", other)
    }

    @inline def apply(n: scala.Int): MP = new MP.Long(n)

    @inline def apply(n: scala.Long): MP = new MP.Long(n)

    @inline def apply(n: scala.BigInt): MP = new MP.BigInt(n).pack

    @inline def apply(n: java.math.BigInteger): MP = MP(scala.BigInt(n))

    @inline def apply(s: String): MP = MP(scala.BigInt(s.value))

  }

  sealed trait MP extends Z {

    final val isBitVector: scala.Boolean = false

    final val isSigned: scala.Boolean = false

    final val isIndex: scala.Boolean = false

    final val hasMin: scala.Boolean = false

    final val hasMax: scala.Boolean = false

    final def Min: Z = halt("Unsupported Z operation 'Min'.")

    final def Max: Z = halt("Unsupported Z operation 'Max'.")

    final def BitWidth: Int = halt("Unsupported Z operation 'BitWidth'.")

    final def toIndex: Z.Index = this

    final def unary_- : MP = MP.unary_-(this)

    final def +(other: Z): MP = MP.+(this, other)

    final def -(other: Z): MP = MP.-(this, other)

    final def *(other: Z): MP = MP.*(this, other)

    final def /(other: Z): MP = MP./(this, other)

    final def %(other: Z): MP = MP.%(this, other)

    final def >(other: Z): B = MP.>(this, other)

    final def >=(other: Z): B = MP.>=(this, other)

    final def <(other: Z): B = MP.<(this, other)

    final def <=(other: Z): B = MP.<=(this, other)

    final def increase: MP = this + MP.one

    final def decrease: MP = this - MP.one

    final def >>(other: Z): MP = halt("Unsupported Z operation '>>'.")

    final def >>>(other: Z): MP = halt("Unsupported Z operation '>>>'.")

    final def <<(other: Z): MP = halt("Unsupported Z operation '<<'.")

    final def &(other: Z): MP = halt("Unsupported Z operation '&'.")

    final def |(other: Z): MP = halt("Unsupported Z operation '|'.")

    final def |^(other: Z): MP = halt("Unsupported Z operation '|^'.")

    final def unary_~ : MP = halt("Unsupported Z operation '~'.")

    final override def equals(other: scala.Any): scala.Boolean = other match {
      case other: MP => MP.isEqual(this, other)
      case other: scala.Int => MP.isEqual(this, other)
      case other: scala.Long => MP.isEqual(this, other)
      case other: scala.BigInt => MP.isEqual(this, other)
      case other: java.lang.Integer => MP.isEqual(this, other.intValue)
      case other: java.lang.Long => MP.isEqual(this, other.longValue)
      case other: java.math.BigInteger => MP.isEqual(this, other)
      case _ => false
    }

    def toIntOpt: scala.Option[scala.Int]

    def toLongOpt: scala.Option[scala.Long]

  }

  object BV {

    trait Long[T <: Long[T]] extends Any with BV {

      def value: scala.Long

      def make(value: scala.Long): T

      def Min: T

      def Max: T

      @inline private final def toByte: scala.Byte = value.toByte

      @inline private final def toShort: scala.Short = value.toShort

      @inline private final def toInt: scala.Int = value.toInt

      @inline private final def toLong: scala.Long = value

      @inline private final def toUByte: UByte = UByte(toByte)

      @inline private final def toUShort: UShort = UShort(toShort)

      @inline private final def toUInt: UInt = UInt(toInt)

      @inline private final def toULong: ULong = ULong(value)

      @inline private final def make(value: scala.Byte): T = make(value.toLong)

      @inline private final def make(value: scala.Short): T = make(value.toLong)

      @inline private final def make(value: scala.Int): T = make(value.toLong)

      @inline private final def umake(value: UByte): T = make(value.toLong)

      @inline private final def umake(value: UShort): T = make(value.toLong)

      @inline private final def umake(value: UInt): T = make(value.toLong)

      @inline private final def umake(value: ULong): T = make(value.toLong)

      @inline private final def makeByte(value: scala.Int): T = if (isSigned) make(value.toByte) else make(UByte(value).toLong)

      @inline private final def makeShort(value: scala.Int): T = if (isSigned) make(value.toShort) else make(UShort(value).toLong)

      @inline private final def makeInt(value: scala.Int): T = if (isSigned) make(value.toLong) else make(UInt(value).toLong)

      @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
        halt(s"Unsupported ${getClass.getSimpleName} operation '$op' with ${other.getClass.getSimpleName}")

      final def unary_- : T =
        if (isSigned) BitWidth match {
          case 8 => makeByte(-toByte)
          case 16 => makeShort(-toShort)
          case 32 => makeInt(-toInt)
          case 64 => make(-value)
        } else BitWidth match {
          case 8 => umake(-toUByte)
          case 16 => umake(-toUShort)
          case 32 => umake(-toUInt)
          case 64 => umake(-toULong)
        }

      final def +(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("+", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte + other.toByte)
            case 16 => makeShort(toShort + other.toShort)
            case 32 => make(toInt + other.toInt)
            case 64 => make(toLong + other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte + other.toUByte)
            case 16 => umake(toUShort + other.toUShort)
            case 32 => umake(toUInt + other.toUInt)
            case 64 => umake(toULong + other.toULong)
          }
        case _ => unsupported("+", other)
      }

      final def -(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("-", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte - other.toByte)
            case 16 => makeShort(toShort - other.toShort)
            case 32 => make(toInt - other.toInt)
            case 64 => make(toLong - other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte - other.toUByte)
            case 16 => umake(toUShort - other.toUShort)
            case 32 => umake(toUInt + other.toUInt)
            case 64 => umake(toULong - other.toULong)
          }
        case _ => unsupported("-", other)
      }

      final def *(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("*", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte * other.toByte)
            case 16 => makeShort(toShort * other.toShort)
            case 32 => make(toInt * other.toInt)
            case 64 => make(toLong * other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte * other.toUByte)
            case 16 => umake(toUShort * other.toUShort)
            case 32 => umake(toUInt * other.toUInt)
            case 64 => umake(toULong * other.toULong)
          }
        case _ => unsupported("*", other)
      }

      final def /(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("/", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte / other.toByte)
            case 16 => makeShort(toShort / other.toShort)
            case 32 => make(toInt / other.toInt)
            case 64 => make(toLong / other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte / other.toUByte)
            case 16 => umake(toUShort / other.toUShort)
            case 32 => umake(toUInt / other.toUInt)
            case 64 => umake(toULong / other.toULong)
          }
        case _ => unsupported("/", other)
      }

      final def %(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("%", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte % other.toByte)
            case 16 => makeShort(toShort % other.toShort)
            case 32 => make(toInt % other.toInt)
            case 64 => make(toLong % other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte % other.toUByte)
            case 16 => umake(toUShort % other.toUShort)
            case 32 => umake(toUInt % other.toUInt)
            case 64 => umake(toULong % other.toULong)
          }
        case _ => unsupported("%", other)
      }

      final def >(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">", other)
          if (isSigned) BitWidth match {
            case 8 => toByte > other.toByte
            case 16 => toShort > other.toShort
            case 32 => toInt > other.toInt
            case 64 => toLong > other.toLong
          } else BitWidth match {
            case 8 => toUByte > other.toUByte
            case 16 => toUShort > other.toUShort
            case 32 => toUInt > other.toUInt
            case 64 => toULong > other.toULong
          }
        case _ => unsupported(">", other)
      }

      final def >=(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">=", other)
          if (isSigned) BitWidth match {
            case 8 => toByte >= other.toByte
            case 16 => toShort >= other.toShort
            case 32 => toInt >= other.toInt
            case 64 => toLong >= other.toLong
          } else BitWidth match {
            case 8 => toUByte >= other.toUByte
            case 16 => toUShort >= other.toUShort
            case 32 => toUInt >= other.toUInt
            case 64 => toULong >= other.toULong
          }
        case _ => unsupported(">=", other)
      }

      final def <(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<", other)
          if (isSigned) BitWidth match {
            case 8 => toByte < other.toByte
            case 16 => toShort < other.toShort
            case 32 => toInt < other.toInt
            case 64 => toLong < other.toLong
          } else BitWidth match {
            case 8 => toUByte < other.toUByte
            case 16 => toUShort < other.toUShort
            case 32 => toUInt < other.toUInt
            case 64 => toULong < other.toULong
          }
        case _ => unsupported("<", other)
      }

      final def <=(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<=", other)
          if (isSigned) BitWidth match {
            case 8 => toByte <= other.toByte
            case 16 => toShort <= other.toShort
            case 32 => toInt <= other.toInt
            case 64 => toLong <= other.toLong
          } else BitWidth match {
            case 8 => toUByte <= other.toUByte
            case 16 => toUShort <= other.toUShort
            case 32 => toUInt <= other.toUInt
            case 64 => toULong <= other.toULong
          }
        case _ => unsupported("<=", other)
      }

      final def >>(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">>", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte >> other.toByte)
            case 16 => makeShort(toShort >> other.toShort)
            case 32 => make(toInt >> other.toInt)
            case 64 => make(toLong >> other.toLong)
          } else halt(s"Unsupported '>>' operation on an unsigned value of '${getClass.getSimpleName}'.")
        case _ => unsupported(">>", other)
      }

      final def >>>(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">>>", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte >>> other.toByte)
            case 16 => makeShort(toShort >>> other.toShort)
            case 32 => make(toInt >>> other.toInt)
            case 64 => make(toLong >>> other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte >>> other.toUByte.toInt)
            case 16 => umake(toUShort >>> other.toUShort.toInt)
            case 32 => umake(toUInt >>> other.toUInt.toInt)
            case 64 => umake(toULong >>> other.toULong.toInt)
          }
        case _ => unsupported(">>>", other)
      }

      final def <<(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<<", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte << other.toByte)
            case 16 => makeShort(toShort << other.toShort)
            case 32 => make(toInt << other.toInt)
            case 64 => make(toLong << other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte << other.toUByte.toInt)
            case 16 => umake(toUShort << other.toUShort.toInt)
            case 32 => umake(toUInt << other.toUInt.toInt)
            case 64 => umake(toULong << other.toULong.toInt)
          }
        case _ => unsupported("<<", other)
      }

      final def &(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("&", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte & other.toByte)
            case 16 => makeShort(toShort & other.toShort)
            case 32 => make(toInt & other.toInt)
            case 64 => make(toLong & other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte & other.toUByte)
            case 16 => umake(toUShort & other.toUShort)
            case 32 => umake(toUInt & other.toUInt)
            case 64 => umake(toULong & other.toULong)
          }
        case _ => unsupported("&", other)
      }

      final def |(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("|", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte | other.toByte)
            case 16 => makeShort(toShort | other.toShort)
            case 32 => make(toInt | other.toInt)
            case 64 => make(toLong | other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte | other.toUByte)
            case 16 => umake(toUShort | other.toUShort)
            case 32 => umake(toUInt | other.toUInt)
            case 64 => umake(toULong | other.toULong)
          }
        case _ => unsupported("|", other)
      }

      final def |^(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("|^", other)
          if (isSigned) BitWidth match {
            case 8 => makeByte(toByte ^ other.toByte)
            case 16 => makeShort(toShort ^ other.toShort)
            case 32 => make(toInt ^ other.toInt)
            case 64 => make(toLong ^ other.toLong)
          } else BitWidth match {
            case 8 => umake(toUByte ^ other.toUByte)
            case 16 => umake(toUShort ^ other.toUShort)
            case 32 => umake(toUInt ^ other.toUInt)
            case 64 => umake(toULong ^ other.toULong)
          }
        case _ => unsupported("^", other)
      }

      final def unary_~ : T =
        if (isSigned) BitWidth match {
          case 8 => makeByte(~toByte)
          case 16 => makeShort(~toShort)
          case 32 => makeInt(~toInt)
          case 64 => make(~value)
        } else BitWidth match {
          case 8 => umake(~toUByte)
          case 16 => umake(~toUShort)
          case 32 => umake(~toUInt)
          case 64 => umake(~toULong)
        }

      final def increase: T =
        if (isSigned) BitWidth match {
          case 8 => makeByte(toByte + 1)
          case 16 => makeShort(toShort + 1)
          case 32 => makeInt(toInt + 1)
          case 64 => make(value + 1)
        } else BitWidth match {
          case 8 => umake(toUByte + UByte(1))
          case 16 => umake(toUShort + UShort(1))
          case 32 => umake(toUInt + UInt(1))
          case 64 => umake(toULong + ULong(1))
        }

      final def decrease: T =
        if (isSigned) BitWidth match {
          case 8 => makeByte(toByte - 1)
          case 16 => makeShort(toShort - 1)
          case 32 => makeInt(toInt - 1)
          case 64 => make(value - 1)
        } else BitWidth match {
          case 8 => umake(toUByte - UByte(1))
          case 16 => umake(toUShort - UShort(1))
          case 32 => umake(toUInt - UInt(1))
          case 64 => umake(toULong - ULong(1))
        }

      final override def toString: Predef.String = if (isSigned) BitWidth match {
        case 8 => toByte.toString
        case 16 => toShort.toString
        case 32 => toInt.toString
        case 64 => value.toString
      } else BitWidth match {
        case 8 => toUByte.toString
        case 16 => toUShort.toString
        case 32 => toUInt.toString
        case 64 => toULong.toString
      }

      final override def toBigInt: scala.BigInt = scala.BigInt(value)

      final override def toIndex: Z.Index =
        if (isIndex)
          if (Min > 0) MP(toBigInt - Min.toBigInt)
          else if (Min < 0) MP(toBigInt + Min.toBigInt)
          else MP(value)
        else MP(value)

    }

  }

  private[sireum_prototype] trait BV extends Any with Z {

    final def isBitVector: scala.Boolean = true

    final def hasMin: scala.Boolean = true

    final def hasMax: scala.Boolean = true

  }

  trait Range[T <: Range[T]] extends Any with Z {

    def value: MP

    def make(n: MP): T

    def Min: T

    def Max: T

    @inline final def isBitVector: scala.Boolean = false

    @inline final def BitWidth: Int = unsupported("BitWidth")

    @inline final def unary_- : T = make(-value)

    @inline final def +(other: Z): T = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("+", other)
        make(value + other.value)
      case _ => unsupported("+", other)
    }

    @inline final def -(other: Z): T = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("-", other)
        make(value - other.value)
      case _ => unsupported("-", other)
    }

    @inline final def *(other: Z): T = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("*", other)
        make(value * other.value)
      case _ => unsupported("*", other)
    }

    @inline final def /(other: Z): T = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("/", other)
        make(value / other.value)
      case _ => unsupported("/", other)
    }

    @inline final def %(other: Z): T = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("%", other)
        make(value / other.value)
      case _ => unsupported("%", other)
    }

    @inline final def <(other: Z): B = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("<", other)
        value < other.value
      case _ => unsupported("<", other)
    }

    @inline final def <=(other: Z): B = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported("<=", other)
        value <= other.value
      case _ => unsupported("<=", other)
    }

    @inline final def >(other: Z): B = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported(">", other)
        value > other.value
      case _ => unsupported(">", other)
    }

    @inline final def >=(other: Z): B = other match {
      case other: Range[_] =>
        if (!isEqType(other)) unsupported(">=", other)
        value > other.value
      case _ => unsupported(">=", other)
    }

    @inline final def decrease: T = make(value + MP.one)

    @inline final def increase: T = make(value - MP.one)

    @inline final override def toBigInt: BigInt = value.toBigInt

    @inline final def >>(other: Z): T = unsupported(">>")

    @inline final def >>>(other: Z): T = unsupported(">>>")

    @inline final def <<(other: Z): T = unsupported("<<")

    @inline final def &(other: Z): T = unsupported("&")

    @inline final def |(other: Z): T = unsupported("|")

    @inline final def |^(other: Z): T = unsupported("|^")

    @inline final def unary_~ : Z = unsupported("~")

    @inline final override def toString: Predef.String = value.toString

    @inline private final def unsupported(op: Predef.String): Nothing =
      halt(s"Unsupported ${getClass.getSimpleName} operation '$op'.")

    @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
      halt(s"Unsupported ${getClass.getSimpleName} operation '$op' with '${other.getClass.getSimpleName}'.")

  }

  object Long {
    def unapply(n: Z): scala.Option[scala.Long] = n match {
      case n: MP => n.toLongOpt
      case _ => scala.None
    }
  }

  object String {
    def unapply(n: Z): scala.Option[Predef.String] = n match {
      case n: MP => scala.Some(n.toString)
      case _ => scala.None
    }
  }

  def unapply(n: Z): scala.Option[scala.Int] = n match {
    case n: MP => n.toIntOpt
    case _ => scala.None
  }

  def apply(n: String): Z = scala.BigInt(n.value)

  import scala.language.implicitConversions

  @inline implicit def apply(n: scala.Int): Z = MP(n)

  @inline implicit def apply(n: scala.Long): Z = MP(n)

  @inline implicit def apply(n: scala.BigInt): Z = MP(n)

  @inline implicit def apply(n: java.math.BigInteger): Z = scala.BigInt(n)

}

trait Z extends Any with Number[Z] {

  def isBitVector: scala.Boolean

  def isSigned: scala.Boolean

  def isIndex: scala.Boolean

  def hasMin: scala.Boolean

  def hasMax: scala.Boolean

  def Min: Z

  def Max: Z

  def BitWidth: scala.Int

  final def isEqType(other: Z): Boolean = {
    if (isSigned != other.isSigned) return false
    if (isIndex != other.isIndex) return false
    if (isBitVector != other.isBitVector) return false
    if (hasMin != other.hasMin) return false
    if (hasMax != other.hasMax) return false
    if (isBitVector && (BitWidth != other.BitWidth)) return false
    if (hasMin && (Min != other.Min)) return false
    if (hasMax && (Max != other.Max)) return false
    true
  }

  def increase: Z

  def decrease: Z

  def unary_- : Z

  def >>(other: Z): Z

  def >>>(other: Z): Z

  def <<(other: Z): Z

  def &(other: Z): Z

  def |(other: Z): Z

  def |^(other: Z): Z

  def unary_~ : Z

  def toIndex: Z.Index

  final def isEqual(other: Immutable): B = this == other

  final def hash: Z = hashCode

  final override def string: String = toString

  def toBigInt: scala.BigInt
}
