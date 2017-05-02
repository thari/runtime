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

private[sireum] object _Type {

  object Alias {
    type TT[T] = scala.reflect.ClassTag[T]
    type B = org.sireum._B
    type C = org.sireum._C
    type Z = math._Z
    type Z8 = org.sireum.math._Z8
    type Z16 = org.sireum.math._Z16
    type Z32 = org.sireum.math._Z32
    type Z64 = org.sireum.math._Z64
    type S8 = org.sireum.math._S8
    type S16 = org.sireum.math._S16
    type S32 = org.sireum.math._S32
    type S64 = org.sireum.math._S64
    type N = math._N
    type N8 = org.sireum.math._N8
    type N16 = org.sireum.math._N16
    type N32 = org.sireum.math._N32
    type N64 = org.sireum.math._N64
    type U8 = org.sireum.math._U8
    type U16 = org.sireum.math._U16
    type U32 = org.sireum.math._U32
    type U64 = org.sireum.math._U64
    type R = org.sireum.math._R
    type F32 = org.sireum.math._F32
    type F64 = org.sireum.math._F64
    type IS[I, V] = collection._IS[I, V]
    type MS[I, V] = collection._MS[I, V]
  }

  import Alias.TT

  private[sireum] val bType = classOf[B].toString
  private[sireum] val cType = classOf[C].toString
  private[sireum] val zType = classOf[Z].toString
  private[sireum] val intType = classOf[Int].toString
  private[sireum] val longType = classOf[Long].toString
  private[sireum] val z8Type = classOf[Z8].toString
  private[sireum] val z16Type = classOf[Z16].toString
  private[sireum] val z32Type = classOf[Z32].toString
  private[sireum] val z64Type = classOf[Z64].toString
  private[sireum] val nType = classOf[N].toString
  private[sireum] val n8Type = classOf[N8].toString
  private[sireum] val n16Type = classOf[N16].toString
  private[sireum] val n32Type = classOf[N32].toString
  private[sireum] val n64Type = classOf[N64].toString
  private[sireum] val s8Type = classOf[S8].toString
  private[sireum] val s16Type = classOf[S16].toString
  private[sireum] val s32Type = classOf[S32].toString
  private[sireum] val s64Type = classOf[S64].toString
  private[sireum] val u8Type = classOf[U8].toString
  private[sireum] val u16Type = classOf[U16].toString
  private[sireum] val u32Type = classOf[U32].toString
  private[sireum] val u64Type = classOf[U64].toString
  private[sireum] val f32Type = classOf[F32].toString
  private[sireum] val f64Type = classOf[F64].toString
  private[sireum] val rType = classOf[R].toString

  private[sireum] def isSlangNumber[T: TT]: Boolean = {
    implicitly[TT[T]].runtimeClass.toString match {
      case `zType` | `z8Type` | `z16Type` | `z32Type` | `z64Type` |
           `nType` | `n8Type` | `n16Type` | `n32Type` | `n64Type` |
           `s8Type` | `s16Type` | `s32Type` | `s64Type` |
           `u8Type` | `u16Type` | `u32Type` | `u64Type` => true
      case _ => false
    }
  }

  private[sireum] def ln2int(value: Z): Int = Z_Ext.toZ32(value).value

  private[sireum] def ln2int[T: TT](value: T): Int = {
    implicitly[TT[T]].runtimeClass.toString match {
      case `zType` => Z_Ext.toZ32(value.asInstanceOf[Z]).value
      case `z8Type` => Z8_Ext.toZ32(value.asInstanceOf[Z8]).value
      case `z16Type` => Z16_Ext.toZ32(value.asInstanceOf[Z16]).value
      case `z32Type` => Z32_Ext.toZ32(value.asInstanceOf[Z32]).value
      case `z64Type` => Z64_Ext.toZ32(value.asInstanceOf[Z64]).value
      case `nType` => N_Ext.toZ32(value.asInstanceOf[N]).value
      case `n8Type` => N8_Ext.toZ32(value.asInstanceOf[N8]).value
      case `n16Type` => N16_Ext.toZ32(value.asInstanceOf[N16]).value
      case `n32Type` => N32_Ext.toZ32(value.asInstanceOf[N32]).value
      case `n64Type` => N64_Ext.toZ32(value.asInstanceOf[N64]).value
      case `s8Type` => S8_Ext.toZ32(value.asInstanceOf[S8]).value
      case `s16Type` => S16_Ext.toZ32(value.asInstanceOf[S16]).value
      case `s32Type` => S32_Ext.toZ32(value.asInstanceOf[S32]).value
      case `s64Type` => S64_Ext.toZ32(value.asInstanceOf[S64]).value
      case `u8Type` => U8_Ext.toZ32(value.asInstanceOf[U8]).value
      case `u16Type` => U16_Ext.toZ32(value.asInstanceOf[U16]).value
      case `u32Type` => U32_Ext.toZ32(value.asInstanceOf[U32]).value
      case `u64Type` => U64_Ext.toZ32(value.asInstanceOf[U64]).value
      case _ => value match {
        case value: Int => value
        case value: Long => Z64_Ext.toZ32(value.asInstanceOf[Z64]).value
      }
    }
  }
}
