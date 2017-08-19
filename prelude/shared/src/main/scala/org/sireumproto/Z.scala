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

package org.sireumproto

import spire.math._

object Z extends $ZCompanion[Z] {

  type Index = MP

  val longMin = scala.BigInt(scala.Long.MinValue)
  val longMax = scala.BigInt(scala.Long.MaxValue)

  object MP {

    val zero: MP = MP.Long(0)
    val one: MP = MP.Long(1)
    val mone: MP = MP.Long(-1)

    private[sireumproto] final case class Long(value: scala.Long) extends MP {

      override def toBigInt: scala.BigInt = scala.BigInt(value)

      override def toIntOpt: scala.Option[scala.Int] =
        if (scala.Int.MinValue <= value && value <= scala.Int.MaxValue) scala.Some(value.toInt)
        else scala.None

      override def toLongOpt: scala.Option[scala.Long] = scala.Some(value)

      override def toString: Predef.String = value.toString

      override def hashCode: scala.Int = value.toInt

    }

    private[sireumproto] final case class BigInt(value: scala.BigInt) extends MP {

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
      halt(s"Unsupported Z operation '$op' with ${other.Name}")

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
        case (Long(n1), Long(n2)) => return Long(n1 % n2)
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

    @inline def apply(n: scala.Int): MP = MP.Long(n)

    @inline def apply(n: scala.Long): MP = MP.Long(n)

    @inline def apply(n: scala.BigInt): MP = MP.BigInt(n).pack

    @inline def apply(n: java.math.BigInteger): MP = MP(scala.BigInt(n))

    @inline def apply(s: String): MP = MP(scala.BigInt(helper.normNum(s.value)))

  }

  sealed trait MP extends Z with $internal.HasBoxer {

    final def Name: Predef.String = Z.Name

    final def isBitVector: scala.Boolean = Z.isBitVector

    final def isSigned: scala.Boolean = Z.isSigned

    final def Index: MP = Z.Index

    final def isZeroIndex: scala.Boolean = Z.isZeroIndex

    final def hasMin: scala.Boolean = Z.hasMin

    final def hasMax: scala.Boolean = Z.hasMax

    final def Min: Z = Z.Min

    final def Max: Z = Z.Max

    final def BitWidth: Int = Z.BitWidth

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
      case other: MP => if (this eq other) true else MP.isEqual(this, other)
      case other: scala.Int => MP.isEqual(this, other)
      case other: scala.Long => MP.isEqual(this, other)
      case other: scala.BigInt => MP.isEqual(this, other)
      case other: java.lang.Integer => MP.isEqual(this, other.intValue)
      case other: java.lang.Long => MP.isEqual(this, other.longValue)
      case other: java.math.BigInteger => MP.isEqual(this, scala.BigInt(other))
      case _ => false
    }

    final def toMP: Z.MP = this

    def toIntOpt: scala.Option[scala.Int]

    def toLongOpt: scala.Option[scala.Long]

    def boxer: $internal.Boxer = Boxer.Z

  }

  object Boxer {

    trait Byte extends $internal.Boxer {
      def box[T](o: scala.Any): T = o match {
        case o: scala.Byte => make(o).asInstanceOf[T]
      }

      def unbox(o: scala.Any): scala.Byte = o match {
        case o: BV.Byte[_] => o.value
        case o: Range[_] =>
          val v: scala.Int = o.value
          v.toByte
      }

      override def create(length: MP): scala.AnyRef = new Array[scala.Byte](length)

      override def lookup[T](a: scala.AnyRef, i: MP): T = a match {
        case a: Array[scala.Byte] => box(a(i))
      }

      override def store(a: scala.AnyRef, i: MP, v: scala.Any): Unit = a match {
        case a: Array[scala.Byte] => a(i) = unbox(v)
      }

      def make(o: scala.Byte): scala.Any
    }

    trait Short extends $internal.Boxer {
      def box[T](o: scala.Any): T = o match {
        case o: scala.Short => make(o).asInstanceOf[T]
      }

      def unbox(o: scala.Any): scala.Short = o match {
        case o: BV.Short[_] => o.value
        case o: Range[_] =>
          val v: scala.Int = o.value
          v.toShort
      }

      override def create(length: MP): scala.AnyRef = new Array[scala.Short](length)

      override def lookup[T](a: scala.AnyRef, i: MP): T = a match {
        case a: Array[scala.Short] => box(a(i))
      }

      override def store(a: scala.AnyRef, i: MP, v: scala.Any): Unit = a match {
        case a: Array[scala.Short] => a(i) = unbox(v)
      }

      def make(o: scala.Short): scala.Any
    }

    trait Int extends $internal.Boxer {
      def box[T](o: scala.Any): T = o match {
        case o: scala.Int => make(o).asInstanceOf[T]
      }

      def unbox(o: scala.Any): scala.Int = o match {
        case o: BV.Int[_] => o.value
        case o: Range[_] => o.value
      }

      override def create(length: MP): scala.AnyRef = new Array[scala.Int](length)

      override def lookup[T](a: scala.AnyRef, i: MP): T = a match {
        case a: Array[scala.Int] => box(a(i))
      }

      override def store(a: scala.AnyRef, i: MP, v: scala.Any): Unit = a match {
        case a: Array[scala.Int] => a(i) = unbox(v)
      }

      def make(o: scala.Int): scala.Any
    }

    trait Long extends $internal.Boxer {
      def box[T](o: scala.Any): T = o match {
        case o: scala.Long => make(o).asInstanceOf[T]
      }

      def unbox(o: scala.Any): scala.Long = o match {
        case o: BV.Long[_] => o.value
        case o: Range[_] => o.value
      }

      override def create(length: MP): scala.AnyRef = new Array[scala.Long](length)

      override def lookup[T](a: scala.AnyRef, i: MP): T = a match {
        case a: Array[scala.Long] => box(a(i))
      }

      override def store(a: scala.AnyRef, i: MP, v: scala.Any): Unit = a match {
        case a: Array[scala.Long] => a(i) = unbox(v)
      }

      def make(o: scala.Long): scala.Any
    }

    object Z extends $internal.Boxer {
      def box[T](o: scala.Any): T = o match {
        case o: MP.Long => o.asInstanceOf[T]
        case o: scala.BigInt => MP.BigInt(o).asInstanceOf[T]
      }

      def unbox(o: scala.Any): scala.Any = o match {
        case o: MP.Long => o
        case o: MP.BigInt => o.value
      }
    }

  }

  object BV {

    trait Byte[T <: Byte[T]] extends Any with BV {

      def value: scala.Byte

      def make(value: scala.Byte): T

      def Min: T

      def Max: T

      def Index: T

      def isZeroIndex: scala.Boolean

      def isWrapped: scala.Boolean

      def BitWidth: scala.Int

      @inline private final def toByte: scala.Byte = value

      @inline private final def toUByte: UByte = UByte(toByte)

      @inline private final def make(value: MP): T = {
        assert(Min.toMP <= value, s"$value should not be less than $Name.Min ($Min)")
        assert(value <= Max.toMP, s"$value should not be greater than $Name.Max ($Max)")
        make(value match {
          case MP.Long(n) => n.toByte
          case MP.BigInt(n) => n.toByte
        })
      }

      @inline private final def umake(value: UByte): T = make(value.toByte)

      @inline private final def makeByte(value: scala.Int): T = if (isSigned) make(value.toByte) else make(UByte(value).toByte)

      @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
        halt(s"Unsupported $Name operation '$op' with ${other.Name}")

      final def unary_- : T =
        if (!isWrapped) make(-toMP)
        else if (isSigned) makeByte(-toByte)
        else umake(-toUByte)

      final def +(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("+", other)
          if (!isWrapped) make(toMP + other.toMP)
          else if (isSigned) makeByte(toByte + other.toByte)
          else umake(toUByte + other.toUByte)
        case _ => unsupported("+", other)
      }

      final def -(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("-", other)
          if (!isWrapped) make(toMP - other.toMP)
          else if (isSigned) makeByte(toByte - other.toByte)
          else umake(toUByte - other.toUByte)
        case _ => unsupported("-", other)
      }

      final def *(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("*", other)
          if (!isWrapped) make(toMP * other.toMP)
          else if (isSigned) makeByte(toByte * other.toByte)
          else umake(toUByte * other.toUByte)
        case _ => unsupported("*", other)
      }

      final def /(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("/", other)
          if (!isWrapped) make(toMP / other.toMP)
          else if (isSigned) makeByte(toByte / other.toByte)
          else umake(toUByte / other.toUByte)
        case _ => unsupported("/", other)
      }

      final def %(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("%", other)
          if (!isWrapped) make(toMP % other.toMP)
          else if (isSigned) makeByte(toByte % other.toByte)
          else umake(toUByte % other.toUByte)
        case _ => unsupported("%", other)
      }

      final def >(other: Z): B = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported(">", other)
          if (isSigned) toByte > other.toByte
          else toUByte > other.toUByte
        case _ => unsupported(">", other)
      }

      final def >=(other: Z): B = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported(">=", other)
          if (isSigned) toByte >= other.toByte
          else toUByte >= other.toUByte
        case _ => unsupported(">=", other)
      }

      final def <(other: Z): B = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("<", other)
          if (isSigned) toByte < other.toByte
          else toUByte < other.toUByte
        case _ => unsupported("<", other)
      }

      final def <=(other: Z): B = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("<=", other)
          if (isSigned) toByte <= other.toByte
          else toUByte <= other.toUByte
        case _ => unsupported("<=", other)
      }

      final def >>(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported(">>", other)
          if (isSigned) makeByte(toByte >> other.toByte)
          else halt(s"Unsupported '>>' operation on an unsigned value of '$Name'.")
        case _ => unsupported(">>", other)
      }

      final def >>>(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported(">>>", other)
          if (isSigned) makeByte(toByte >>> other.toByte)
          else umake(toUByte >>> other.toUByte.toInt)
        case _ => unsupported(">>>", other)
      }

      final def <<(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("<<", other)
          if (isSigned) makeByte(toByte << other.toByte)
          else umake(toUByte << other.toUByte.toInt)
        case _ => unsupported("<<", other)
      }

      final def &(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("&", other)
          if (isSigned) makeByte(toByte & other.toByte)
          else umake(toUByte & other.toUByte)
        case _ => unsupported("&", other)
      }

      final def |(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("|", other)
          if (isSigned) makeByte(toByte | other.toByte)
          else umake(toUByte | other.toUByte)
        case _ => unsupported("|", other)
      }

      final def |^(other: Z): T = other match {
        case other: Byte[_] =>
          if (!isEqType(other)) unsupported("|^", other)
          if (isSigned) makeByte(toByte ^ other.toByte)
          else umake(toUByte ^ other.toUByte)
        case _ => unsupported("^", other)
      }

      final def unary_~ : T =
        if (isSigned) makeByte(~toByte)
        else umake(~toUByte)

      final def increase: T =
        if (isSigned) makeByte(toByte + 1)
        else umake(toUByte + UByte(1))

      final def decrease: T =
        if (isSigned) makeByte(toByte - 1)
        else umake(toUByte - UByte(1))

      final override def toString: Predef.String =
        if (isSigned) toByte.toString
        else toUByte.toString

      final override def toBigInt: scala.BigInt =
        if (isSigned) scala.BigInt(toByte)
        else toUByte.toBigInt

      final override def toMP: MP =
        if (isSigned) MP(toByte)
        else MP(toUByte.toLong)

      final override def toIndex: Z.Index =
        if (isZeroIndex) toMP else toMP - Index.toMP
    }

    trait Short[T <: Short[T]] extends Any with BV {

      def value: scala.Short

      def make(value: scala.Short): T

      def Min: T

      def Max: T

      def Index: T

      def isZeroIndex: scala.Boolean

      def isWrapped: scala.Boolean

      @inline private final def toShort: scala.Short = value

      @inline private final def toUShort: UShort = UShort(toShort)

      @inline private final def make(value: MP): T = {
        assert(Min.toMP <= value, s"$value should not be less than $Name.Min ($Min)")
        assert(value <= Max.toMP, s"$value should not be greater than $Name.Max ($Max)")
        make(value match {
          case MP.Long(n) => n.toShort
          case MP.BigInt(n) => n.toShort
        })
      }

      @inline private final def umake(value: UShort): T = make(value.toShort)

      @inline private final def makeShort(value: scala.Int): T = if (isSigned) make(value.toShort) else make(UShort(value).toShort)

      @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
        halt(s"Unsupported $Name operation '$op' with ${other.Name}")

      final def unary_- : T =
        if (!isWrapped) make(-toMP)
        else if (isSigned) makeShort(-toShort)
        else umake(-toUShort)

      final def +(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("+", other)
          if (!isWrapped) make(toMP + other.toMP)
          else if (isSigned) makeShort(toShort + other.toShort)
          else umake(toUShort + other.toUShort)
        case _ => unsupported("+", other)
      }

      final def -(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("-", other)
          if (!isWrapped) make(toMP - other.toMP)
          else if (isSigned) makeShort(toShort - other.toShort)
          else umake(toUShort - other.toUShort)
        case _ => unsupported("-", other)
      }

      final def *(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("*", other)
          if (!isWrapped) make(toMP * other.toMP)
          else if (isSigned) makeShort(toShort * other.toShort)
          else umake(toUShort * other.toUShort)
        case _ => unsupported("*", other)
      }

      final def /(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("/", other)
          if (!isWrapped) make(toMP / other.toMP)
          else if (isSigned) makeShort(toShort / other.toShort)
          else umake(toUShort / other.toUShort)
        case _ => unsupported("/", other)
      }

      final def %(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("%", other)
          if (!isWrapped) make(toMP % other.toMP)
          else if (isSigned) makeShort(toShort % other.toShort)
          else umake(toUShort % other.toUShort)
        case _ => unsupported("%", other)
      }

      final def >(other: Z): B = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported(">", other)
          if (isSigned) toShort > other.toShort
          else toUShort > other.toUShort
        case _ => unsupported(">", other)
      }

      final def >=(other: Z): B = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported(">=", other)
          if (isSigned) toShort >= other.toShort
          else toUShort >= other.toUShort
        case _ => unsupported(">=", other)
      }

      final def <(other: Z): B = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("<", other)
          if (isSigned) toShort < other.toShort
          else toUShort < other.toUShort
        case _ => unsupported("<", other)
      }

      final def <=(other: Z): B = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("<=", other)
          if (isSigned) toShort <= other.toShort
          else toUShort <= other.toUShort
        case _ => unsupported("<=", other)
      }

      final def >>(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported(">>", other)
          if (isSigned) makeShort(toShort >> other.toShort)
          else halt(s"Unsupported '>>' operation on an unsigned value of '$Name'.")
        case _ => unsupported(">>", other)
      }

      final def >>>(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported(">>>", other)
          if (isSigned) makeShort(toShort >>> other.toShort)
          else umake(toUShort >>> other.toUShort.toInt)
        case _ => unsupported(">>>", other)
      }

      final def <<(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("<<", other)
          if (isSigned) makeShort(toShort << other.toShort)
          else umake(toUShort << other.toUShort.toInt)
        case _ => unsupported("<<", other)
      }

      final def &(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("&", other)
          if (isSigned) makeShort(toShort & other.toShort)
          else umake(toUShort & other.toUShort)
        case _ => unsupported("&", other)
      }

      final def |(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("|", other)
          if (isSigned) makeShort(toShort | other.toShort)
          else umake(toUShort | other.toUShort)
        case _ => unsupported("|", other)
      }

      final def |^(other: Z): T = other match {
        case other: Short[_] =>
          if (!isEqType(other)) unsupported("|^", other)
          if (isSigned) makeShort(toShort ^ other.toShort)
          else umake(toUShort ^ other.toUShort)
        case _ => unsupported("^", other)
      }

      final def unary_~ : T =
        if (isSigned) makeShort(~toShort)
        else umake(~toUShort)

      final def increase: T =
        if (isSigned) makeShort(toShort + 1)
        else umake(toUShort + UShort(1))

      final def decrease: T =
        if (isSigned) makeShort(toShort - 1)
        else umake(toUShort - UShort(1))

      final override def toString: Predef.String =
        if (isSigned) toShort.toString
        else toUShort.toString

      final override def toBigInt: scala.BigInt =
        if (isSigned) scala.BigInt(toShort)
        else toUShort.toBigInt

      final override def toMP: MP =
        if (isSigned) MP(toShort)
        else MP(toUShort.toLong)

      final override def toIndex: Z.Index =
        if (isZeroIndex) toMP else toMP - Index.toMP
    }

    trait Int[T <: Int[T]] extends Any with BV {

      def value: scala.Int

      def make(value: scala.Int): T

      def Min: T

      def Max: T

      def Index: T

      def isZeroIndex: scala.Boolean

      def isWrapped: scala.Boolean

      @inline private final def toUInt: UInt = UInt(value)

      @inline private final def make(value: MP): T = {
        assert(Min.toMP <= value, s"$value should not be less than $Name.Min ($Min)")
        assert(value <= Max.toMP, s"$value should not be greater than $Name.Max ($Max)")
        make(value match {
          case MP.Long(n) => n.toInt
          case MP.BigInt(n) => n.toInt
        })
      }

      @inline private final def umake(value: UInt): T = make(value.toInt)

      @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
        halt(s"Unsupported $Name operation '$op' with ${other.Name}")

      final def unary_- : T =
        if (!isWrapped) make(-toMP)
        else if (isSigned) make(-value)
        else umake(-toUInt)

      final def +(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("+", other)
          if (!isWrapped) make(toMP + other.toMP)
          else if (isSigned) make(value + other.value)
          else umake(toUInt + other.toUInt)
        case _ => unsupported("+", other)
      }

      final def -(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("-", other)
          if (!isWrapped) make(toMP - other.toMP)
          else if (isSigned) make(value - other.value)
          else umake(toUInt + other.toUInt)
        case _ => unsupported("-", other)
      }

      final def *(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("*", other)
          if (!isWrapped) make(toMP * other.toMP)
          else if (isSigned) make(value * other.value)
          else umake(toUInt * other.toUInt)
        case _ => unsupported("*", other)
      }

      final def /(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("/", other)
          if (!isWrapped) make(toMP / other.toMP)
          else if (isSigned) make(value / other.value)
          else umake(toUInt / other.toUInt)
        case _ => unsupported("/", other)
      }

      final def %(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("%", other)
          if (!isWrapped) make(toMP % other.toMP)
          else if (isSigned) make(value % other.value)
          else umake(toUInt % other.toUInt)
        case _ => unsupported("%", other)
      }

      final def >(other: Z): B = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported(">", other)
          if (isSigned) value > other.value
          else toUInt > other.toUInt
        case _ => unsupported(">", other)
      }

      final def >=(other: Z): B = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported(">=", other)
          if (isSigned) value >= other.value
          else toUInt >= other.toUInt
        case _ => unsupported(">=", other)
      }

      final def <(other: Z): B = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("<", other)
          if (isSigned) value < other.value
          else toUInt < other.toUInt
        case _ => unsupported("<", other)
      }

      final def <=(other: Z): B = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("<=", other)
          if (isSigned) value <= other.value
          else toUInt <= other.toUInt
        case _ => unsupported("<=", other)
      }

      final def >>(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported(">>", other)
          if (isSigned) make(value >> other.value)
          else halt(s"Unsupported '>>' operation on an unsigned value of '$Name'.")
        case _ => unsupported(">>", other)
      }

      final def >>>(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported(">>>", other)
          if (isSigned) make(value >>> other.value)
          else umake(toUInt >>> other.value)
        case _ => unsupported(">>>", other)
      }

      final def <<(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("<<", other)
          if (isSigned) make(value << other.value)
          else umake(toUInt << other.value)
        case _ => unsupported("<<", other)
      }

      final def &(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("&", other)
          if (isSigned) make(value & other.value)
          else umake(toUInt & other.toUInt)
        case _ => unsupported("&", other)
      }

      final def |(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("|", other)
          if (isSigned) make(value | other.value)
          else umake(toUInt | other.toUInt)
        case _ => unsupported("|", other)
      }

      final def |^(other: Z): T = other match {
        case other: Int[_] =>
          if (!isEqType(other)) unsupported("|^", other)
          if (isSigned) make(value ^ other.value)
          else umake(toUInt ^ other.toUInt)
        case _ => unsupported("^", other)
      }

      final def unary_~ : T =
        if (isSigned) make(~value)
        else umake(~toUInt)

      final def increase: T =
        if (isSigned) make(value + 1)
        else umake(toUInt + UInt(1))

      final def decrease: T =
        if (isSigned) make(value - 1)
        else umake(toUInt - UInt(1))

      final override def toString: Predef.String =
        if (isSigned) value.toString
        else toUInt.toString

      final override def toBigInt: scala.BigInt =
        if (isSigned) scala.BigInt(value)
        else toUInt.toBigInt

      final override def toMP: MP =
        if (isSigned) MP(value)
        else MP(toUInt.toLong)

      final override def toIndex: Z.Index =
        if (isZeroIndex) toMP else toMP - Index.toMP
    }

    trait Long[T <: Long[T]] extends Any with BV {

      def value: scala.Long

      def make(value: scala.Long): T

      def Min: T

      def Max: T

      def Index: T

      def isZeroIndex: scala.Boolean

      def isWrapped: scala.Boolean

      @inline private final def toULong: ULong = ULong(value)

      @inline private final def make(value: MP): T = {
        assert(Min.toMP <= value, s"$value should not be less than $Name.Min ($Min)")
        assert(value <= Max.toMP, s"$value should not be greater than $Name.Max ($Max)")
        make(value match {
          case MP.Long(n) => n
          case MP.BigInt(n) => n.toLong
        })
      }

      @inline private final def umake(value: ULong): T = make(value.toLong)

      @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
        halt(s"Unsupported $Name operation '$op' with ${other.Name}")

      final def unary_- : T =
        if (!isWrapped) make(-toMP)
        else if (isSigned) make(-value)
        else umake(-toULong)

      final def +(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("+", other)
          if (!isWrapped) make(toMP + other.toMP)
          else if (isSigned) make(value + other.value)
          else umake(toULong + other.toULong)
        case _ => unsupported("+", other)
      }

      final def -(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("-", other)
          if (!isWrapped) make(toMP - other.toMP)
          else if (isSigned) make(value - other.value)
          else umake(toULong - other.toULong)
        case _ => unsupported("-", other)
      }

      final def *(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("*", other)
          if (!isWrapped) make(toMP * other.toMP)
          else if (isSigned) make(value * other.value)
          else umake(toULong * other.toULong)
        case _ => unsupported("*", other)
      }

      final def /(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("/", other)
          if (!isWrapped) make(toMP / other.toMP)
          else if (isSigned) make(value / other.value)
          else umake(toULong / other.toULong)
        case _ => unsupported("/", other)
      }

      final def %(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("%", other)
          if (!isWrapped) make(toMP % other.toMP)
          else if (isSigned) make(value % other.value)
          else umake(toULong % other.toULong)
        case _ => unsupported("%", other)
      }

      final def >(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">", other)
          if (isSigned) value > other.value
          else toULong > other.toULong
        case _ => unsupported(">", other)
      }

      final def >=(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">=", other)
          if (isSigned) value >= other.value
          else toULong >= other.toULong
        case _ => unsupported(">=", other)
      }

      final def <(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<", other)
          if (isSigned) value < other.value
          else toULong < other.toULong
        case _ => unsupported("<", other)
      }

      final def <=(other: Z): B = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<=", other)
          if (isSigned) value <= other.value
          else toULong <= other.toULong
        case _ => unsupported("<=", other)
      }

      final def >>(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">>", other)
          if (isSigned) make(value >> other.value)
          else halt(s"Unsupported '>>' operation on an unsigned value of '$Name'.")
        case _ => unsupported(">>", other)
      }

      final def >>>(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported(">>>", other)
          if (isSigned) make(value >>> other.value)
          else umake(toULong >>> other.toULong.toInt)
        case _ => unsupported(">>>", other)
      }

      final def <<(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("<<", other)
          if (isSigned) make(value << other.value)
          else umake(toULong << other.toULong.toInt)
        case _ => unsupported("<<", other)
      }

      final def &(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("&", other)
          if (isSigned) make(value & other.value)
          else umake(toULong & other.toULong)
        case _ => unsupported("&", other)
      }

      final def |(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("|", other)
          if (isSigned) make(value | other.value)
          else umake(toULong | other.toULong)
        case _ => unsupported("|", other)
      }

      final def |^(other: Z): T = other match {
        case other: Long[_] =>
          if (!isEqType(other)) unsupported("|^", other)
          if (isSigned) make(value ^ other.value)
          else umake(toULong ^ other.toULong)
        case _ => unsupported("^", other)
      }

      final def unary_~ : T =
        if (isSigned) make(~value)
        else umake(~toULong)

      final def increase: T =
        if (isSigned) make(value + 1)
        else umake(toULong + ULong(1))

      final def decrease: T =
        if (isSigned) make(value - 1)
        else umake(toULong - ULong(1))

      final override def toString: Predef.String =
        if (isSigned) value.toString
        else toULong.toString

      final override def toBigInt: scala.BigInt =
        if (isSigned) scala.BigInt(value)
        else toULong.toBigInt

      final override def toMP: MP =
        if (isSigned) MP(value)
        else MP(toULong.toBigInt)

      final override def toIndex: Z.Index =
        if (isZeroIndex) toMP else toMP - Index.toMP
    }

  }

  private[sireumproto] trait BV extends Any with Z with $internal.HasBoxer {

    final def isBitVector: scala.Boolean = true

    final def hasMin: scala.Boolean = true

    final def hasMax: scala.Boolean = true

  }

  trait Range[T <: Range[T]] extends Any with Z with $internal.HasBoxer {

    def value: MP

    def make(n: MP): T

    def Min: T

    def Max: T

    def Index: T

    def isZeroIndex: scala.Boolean

    @inline final def isBitVector: scala.Boolean = false

    @inline final def BitWidth: Int = unsupported("BitWidth")

    @inline final def toMP: MP = value

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
        make(value % other.value)
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
        value >= other.value
      case _ => unsupported(">=", other)
    }

    @inline final def decrease: T = make(value - MP.one)

    @inline final def increase: T = make(value + MP.one)

    @inline final override def toBigInt: BigInt = value.toBigInt

    @inline final def >>(other: Z): T = unsupported(">>")

    @inline final def >>>(other: Z): T = unsupported(">>>")

    @inline final def <<(other: Z): T = unsupported("<<")

    @inline final def &(other: Z): T = unsupported("&")

    @inline final def |(other: Z): T = unsupported("|")

    @inline final def |^(other: Z): T = unsupported("|^")

    @inline final def unary_~ : Z = unsupported("~")

    @inline final def toIndex: Z.Index = if (isZeroIndex) value else value - Index.value

    @inline final override def toString: Predef.String = value.toString

    @inline private final def unsupported(op: Predef.String): Nothing =
      halt(s"Unsupported $Name operation '$op'.")

    @inline private final def unsupported(op: Predef.String, other: Z): Nothing =
      halt(s"Unsupported $Name operation '$op' with '${other.Name}'.")

  }

  def apply(n: Z): Z = n match {
    case n: Z.MP => n
    case _ => halt(s"Unsupported Z creation from ${n.Name}.")
  }

  def apply(s: String): Z = scala.BigInt(s.value)

  object Int extends $ZCompanionInt[Z] {
    @inline def apply(n: scala.Int): Z = MP(n)

    def unapply(n: Z): scala.Option[scala.Int] = n match {
      case n: MP => n.toIntOpt
      case _ => scala.None
    }
  }

  object Long extends $ZCompanionLong[Z] {
    @inline def apply(n: scala.Long): Z = MP(n)

    def unapply(n: Z): scala.Option[scala.Long] = n match {
      case n: MP => n.toLongOpt
      case _ => scala.None
    }
  }

  object String extends $ZCompanionString[Z] {
    @inline def apply(s: Predef.String): Z = MP(s)

    def unapply(n: Z): scala.Option[Predef.String] = n match {
      case n: MP => scala.Some(n.toString)
      case _ => scala.None
    }
  }

  object BigInt extends $ZCompanionBigInt[Z] {
    @inline def apply(n: scala.BigInt): Z = MP(n)

    def unapply(n: Z): scala.Option[scala.BigInt] = n match {
      case n: MP => scala.Some(n.toBigInt)
      case _ => scala.None
    }
  }

  val Name: Predef.String = "Z"

  val isBitVector: scala.Boolean = false

  val isSigned: scala.Boolean = true

  val isZeroIndex: scala.Boolean = true

  val Index: MP = MP.zero

  val hasMin: scala.Boolean = false

  val hasMax: scala.Boolean = false

  def Min: Z = halt(s"Unsupported $Name operation 'Min'")

  def Max: Z = halt(s"Unsupported $Name operation 'Max'")

  def BitWidth: scala.Int = halt(s"Unsupported $Name operation 'BitWidth'")

  def random: MP = randomSeed(System.currentTimeMillis)

  def randomSeed(seed: Z): MP = {
    val r = new scala.util.Random((seed.toMP % Z.MP(ULong(-1).toBigInt + 1)).toLongOpt.get)
    MP(scala.BigInt(numbits = r.nextInt(r.nextInt(1024) + 1), rnd = r))
  }

  import scala.language.implicitConversions

  @inline implicit def apply(n: scala.Int): Z = Int(n)

  @inline implicit def apply(n: scala.Long): Z = Long(n)

  @inline implicit def apply(n: scala.BigInt): Z = BigInt(n)

}

trait $ZCompanion[T <: Z] {

  def Name: Predef.String

  def isBitVector: scala.Boolean

  def isSigned: scala.Boolean

  def isZeroIndex: scala.Boolean

  def Index: T

  def hasMin: scala.Boolean

  def hasMax: scala.Boolean

  def Min: T

  def Max: T

  def BitWidth: scala.Int

  def random: T

  def randomSeed(seed: Z): T

  def Int: $ZCompanionInt[T]

  def Long: $ZCompanionLong[T]

  def String: $ZCompanionString[T]

  def BigInt: $ZCompanionBigInt[T]

  def apply(n: Z): T

}

trait $ZCompanionInt[T <: Z] {
  def apply(n: scala.Int): T

  def unapply(n: T): scala.Option[scala.Int]
}

trait $ZCompanionLong[T <: Z] {
  def apply(n: scala.Long): T

  def unapply(n: T): scala.Option[scala.Long]
}

trait $ZCompanionString[T <: Z] {
  def apply(s: Predef.String): T

  def unapply(n: T): scala.Option[Predef.String]
}

trait $ZCompanionBigInt[T <: Z] {
  def apply(s: scala.BigInt): T

  def unapply(n: T): scala.Option[scala.BigInt]
}

trait Z extends Any with Number[Z] {

  def Name: Predef.String

  def isBitVector: scala.Boolean

  def isSigned: scala.Boolean

  def isZeroIndex: scala.Boolean

  def Index: Z

  def hasMin: scala.Boolean

  def hasMax: scala.Boolean

  def Min: Z

  def Max: Z

  def BitWidth: scala.Int

  final def isEqType(other: Z): Boolean = Name == other.Name

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

  def toMP: Z.MP

  def to(n: Z): ZRange = ZRange(this, n, _ => T, (r, i) => if (r) i.decrease else i.increase, F)

  def until(n: Z): ZRange = ZRange(this, n, _ => T, (r, i) => if (r) i.decrease else i.increase, F)
}
