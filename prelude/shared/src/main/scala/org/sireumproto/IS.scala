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

import org.sireumproto.$internal.ISMarker

object IS {

  final class Array[I <: Z, V <: Immutable](val companion: $ZCompanion[I],
                                            array: scala.Array[scala.Any],
                                            length: scala.Int) extends IS[I, V] {

    def size: I =
      if (companion.isZeroIndex) companion.Int(length)
      else halt(s"Operation 'size' can only be used on zero-indexed IS.")

    def apply(index: I): V = {
      val i = index.toIndex.toMP
      assume(Z.MP.zero <= i && i <= length, s"Array indexing out of bounds: $index")
      array(i.toIntOpt.get).asInstanceOf[V]
    }

    def elements: scala.Seq[V] = array.slice(0, length).map(_.asInstanceOf[V])

    lazy val hash: Z = elements.hashCode

    def isEqual(other: Immutable): B = other match {
      case other: IS[_, _] => elements == other.elements
      case _ => F
    }

    override def equals(other: scala.Any): scala.Boolean =
      if (this eq other.asInstanceOf[scala.AnyRef]) true
      else other match {
        case other: IS.Array[_, _] =>
          if (companion ne other.companion) return false
          if (size.toMP != other.size.toMP) return false
          elements == other.elements
        case _ => false
      }

    def string: String = toString

    override def toString: Predef.String = {
      val sb = new java.lang.StringBuilder
      sb.append('[')
      if (length > 0) {
        sb.append(array(0).toString)
        for (i <- 1 until length) {
          sb.append(", ")
          sb.append(array(i).toString)
        }
      }
      sb.append(']')
      sb.toString
    }

  }

  def apply[I <: Z, V <: Immutable](args: V*)(implicit companion: $ZCompanion[I]): IS[I, V] = {
    val array = new scala.Array[scala.Any](args.length)
    for (i <- array.indices) array(i) = args(i)
    new Array[I, V](companion, array, args.length)
  }
}

sealed trait IS[I <: Z, V <: Immutable] extends Immutable with ISMarker {

  def apply(index: I): V

  def size: I

  def elements: scala.Seq[V]

}
