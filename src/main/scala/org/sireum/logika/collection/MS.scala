/*
 * Copyright (c) 2016, Robby, Kansas State University
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

package org.sireum.logika.collection

import org.sireum.logika._
import org.sireum.logika.math.{Z => ZM}
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object MS {
  private[collection] def newMS[E]: (MMap[Z, E], java.util.TreeMap[Z, E]) = {
    val tm = new java.util.TreeMap[Z, E]
    val a: MMap[Z, E] = {
      import scala.collection.JavaConversions._
      tm
    }
    (a, tm)
  }
}

trait MS[E] {

  protected trait Value {
    def elements: scala.collection.Seq[E]

    def size: Z

    def apply(index: Z): E

    def update(index: Z, value: E): Unit

    def :+(value: E): Value

    def +:(value: E): Value

    override def clone: Value = sys.error("stub")

    final override def equals(other: Any): B = other match {
      case other: Value =>
        if (this eq other) return true
        if (size != other.size) return false
        var i = ZM.zero
        while (i < size) {
          if (apply(i) != other(i)) return false
          i += ZM.one
        }
        true
      case _ => false
    }
  }

  protected abstract class
  ValueArray[T <: ValueArray[T]](a: ArrayBuffer[E]) extends Value {
    var (dirty, _hashCode) = (true, 0)

    final def elements = a.toVector

    final override val size: Z = ZM(a.length)

    final override def apply(index: Z): E = {
      assert(index < ZM(ZM.intMax))
      a(index.toInt)
    }

    final override def update(index: Z, value: E): Unit = {
      assert(index < ZM(ZM.intMax))
      dirty = true
      a(index.toInt) = value
    }

    final override def clone: T = make(a.clone)

    protected def make(a: ArrayBuffer[E]): T

    final override def hashCode: Int = {
      if (dirty) {
        _hashCode = computeHashCode
        dirty = false
      }
      _hashCode
    }

    final def computeHashCode: Int = a.hashCode

    final override def toString: String = {
      val sb = new StringBuilder
      sb.append('[')
      if (a.nonEmpty) {
        var i = 0
        sb.append(a(i))
        i += 1
        val sz = a.length
        while (i < sz) {
          sb.append(", ")
          sb.append(a(i))
          i += 1
        }
      }
      sb.append(']')
      sb.toString
    }
  }

  protected abstract class
  ValueTreeMap[T <: ValueTreeMap[T]](tm: java.util.TreeMap[Z, E],
                                     final override val size: Z) extends Value {
    var (dirty, _hashCode) = (true, 0)

    final def elements = {
      import scala.collection.JavaConversions._
      tm.values.toVector
    }

    final val a: MMap[Z, E] = {
      import scala.collection.JavaConversions._
      tm
    }

    final def apply(index: Z): E = a.get(index) match {
      case Some(value) => value
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }

    final def update(index: Z, value: E): Unit = {
      dirty = true
      if (ZM.zero <= index && index < size) a(index) = value
      else throw new IndexOutOfBoundsException(index.toString)
    }

    final def :+(value: E): T = {
      val (a, tm) = MS.newMS[E]
      for ((i, v) <- this.a) a(i) = v
      a(size) = value
      make(tm, ZM(this.tm.size) + ZM.one)
    }

    final def +:(value: E): T = {
      val (a, tm) = MS.newMS[E]
      a(ZM.zero) = value
      for ((i, v) <- this.a) a(i + ZM.one) = v
      make(tm, size + ZM.one)
    }

    final override def clone: T =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, E]], size)

    protected def make(tm: java.util.TreeMap[Z, E], size: Z): T

    final override def hashCode: Int = {
      if (dirty) {
        _hashCode = computeHashCode
        dirty = false
      }
      _hashCode
    }

    final def computeHashCode = a.values.toSeq.hashCode

    final override def toString: String = {
      val sb = new StringBuilder
      sb.append('[')
      if (a.nonEmpty) {
        var i = ZM.zero
        sb.append(a(i))
        i += ZM.one
        while (i < size) {
          sb.append(", ")
          sb.append(a(i))
          i += ZM.one
        }
      }
      sb.append(']')
      sb.toString
    }
  }

}

object BS extends MS[B] {
  type V = B

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object ZS extends MS[Z] {
  type V = Z

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z8S extends MS[Z8] {
  type V = Z8

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z16S extends MS[Z16] {
  type V = Z16

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z32S extends MS[Z32] {
  type V = Z32

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z64S extends MS[Z64] {
  type V = Z64

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S8S extends MS[S8] {
  type V = S8

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S16S extends MS[S16] {
  type V = S16

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S32S extends MS[S32] {
  type V = S32

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S64S extends MS[S64] {
  type V = S64

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object NS extends MS[N] {
  type V = N

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N8S extends MS[N8] {
  type V = N8

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N16S extends MS[N16] {
  type V = N16

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N32S extends MS[N32] {
  type V = N32

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N64S extends MS[N64] {
  type V = N64

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U8S extends MS[U8] {
  type V = U8

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U16S extends MS[U16] {
  type V = U16

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U32S extends MS[U32] {
  type V = U32

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U64S extends MS[U64] {
  type V = U64

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object RS extends MS[R] {
  type V = R

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object F32S extends MS[F32] {
  type V = F32

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object F64S extends MS[F64] {
  type V = F64

  def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newMS[V]
      var i = ZM.zero
      for (e <- this.a) {
        a(i) = e
        i += ZM.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + ZM.one == ZM(ZM.intMax)) value +: upgrade
      else make(value +: a)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}