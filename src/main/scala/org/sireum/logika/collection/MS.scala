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

import org.sireum.logika.B
import org.sireum.logika.math._
import scala.collection.mutable.{ArrayBuffer, Map => MMap}

object MS {
  private[collection] def newTreeMS[E]: (MMap[Z, E], java.util.TreeMap[Z, E]) = {
    val tm = new java.util.TreeMap[Z, E]
    val a: MMap[Z, E] = {
      import scala.collection.JavaConverters._
      tm.asScala
    }
    (a, tm)
  }
}

trait MS[E] {

  trait Value extends org.sireum.logika.Clonable {
    def elements: scala.collection.Seq[E]

    def size: Z

    def apply(index: Z): E

    def update(index: Z, value: E): Unit

    def :+(value: E): Value

    def +:(value: E): Value

    override def clone: Value = sys.error("stub")

    final override def equals(other: Any): Boolean = other match {
      case other: Value =>
        if (this eq other) return true
        if (size != other.size) return false
        var i = Z.zero
        while (i < size) {
          if (apply(i) != other(i)) return false
          i += Z.one
        }
        true
      case _ => false
    }
  }

  protected abstract class
  ValueArray[T <: ValueArray[T]](a: ArrayBuffer[E]) extends Value {
    var (dirty, _hashCode) = (true, 0)

    final def elements: Vector[E] = a.toVector

    final override val size: Z = Z(a.length)

    final override def apply(index: Z): E = {
      assert(index < Z(Z.intMax))
      a(index.toInt)
    }

    final override def update(index: Z, value: E): Unit = {
      assert(index < Z(Z.intMax))
      dirty = true
      a(index.toInt) = value
    }

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

    final def elements: Vector[E] = {
      import scala.collection.JavaConverters._
      tm.values.asScala.toVector
    }

    final val a: MMap[Z, E] = {
      import scala.collection.JavaConverters._
      tm.asScala
    }

    final def apply(index: Z): E = a.get(index) match {
      case Some(value) => value
      case _ => throw new IndexOutOfBoundsException(index.toString)
    }

    final def update(index: Z, value: E): Unit = {
      dirty = true
      if (Z.zero <= index && index < size) a(index) = value
      else throw new IndexOutOfBoundsException(index.toString)
    }

    final def :+(value: E): T = {
      val (a, tm) = MS.newTreeMS[E]
      for ((i, v) <- this.a) a(i) = v
      a(size) = value
      make(tm, Z(this.tm.size) + Z.one)
    }

    final def +:(value: E): T = {
      val (a, tm) = MS.newTreeMS[E]
      a(Z.zero) = value
      for ((i, v) <- this.a) a(i + Z.one) = v
      make(tm, size + Z.one)
    }

    protected def make(tm: java.util.TreeMap[Z, E], size: Z): T

    final override def hashCode: Int = {
      if (dirty) {
        _hashCode = computeHashCode
        dirty = false
      }
      _hashCode
    }

    final def computeHashCode: Int = a.values.toSeq.hashCode

    final override def toString: String = {
      val sb = new StringBuilder
      sb.append('[')
      if (a.nonEmpty) {
        var i = Z.zero
        sb.append(a(i))
        i += Z.one
        while (i < size) {
          sb.append(", ")
          sb.append(a(i))
          i += Z.one
        }
      }
      sb.append(']')
      sb.toString
    }
  }

}

object BS extends MS[B] {
  type V = B

  final def random: BS.Value = {
    val sz = N8.random.toInt
    val elements = new Array[B](sz)
    for (i <- 0 until sz) elements(i) = org.sireum.logika.B.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: B): BS.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (a, tm) = MS.newTreeMS[B]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[B](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object ZS extends MS[Z] {
  type V = Z

  final def random: ZS.Value = {
    val sz = N8.random.toInt
    val elements = new Array[Z](sz)
    for (i <- 0 until sz) elements(i) = Z.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: Z): ZS.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[Z]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[Z](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z8S extends MS[Z8.Value] {
  type V = Z8.Value

  final def random: Z8S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[Z8.Value](sz)
    for (i <- 0 until sz) elements(i) = Z8.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: Z8.Value): Z8S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[Z8.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[Z8.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z16S extends MS[Z16.Value] {
  type V = Z16.Value

  final def random: Z16S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[Z16.Value](sz)
    for (i <- 0 until sz) elements(i) = Z16.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: Z16.Value): Z16S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[Z16.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[Z16.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z32S extends MS[Z32.Value] {
  type V = Z32.Value

  final def random: Z32S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[Z32.Value](sz)
    for (i <- 0 until sz) elements(i) = Z32.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: Z32.Value): Z32S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[Z32.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[Z32.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object Z64S extends MS[Z64.Value] {
  type V = Z64.Value

  final def random: Z64S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[Z64.Value](sz)
    for (i <- 0 until sz) elements(i) = Z64.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: Z64.Value): Z64S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[Z64.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[Z64.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S8S extends MS[S8.Value] {
  type V = S8.Value

  final def random: S8S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[S8.Value](sz)
    for (i <- 0 until sz) elements(i) = S8.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: S8.Value): S8S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[S8.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[S8.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S16S extends MS[S16.Value] {
  type V = S16.Value

  final def random: S16S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[S16.Value](sz)
    for (i <- 0 until sz) elements(i) = S16.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: S16.Value): S16S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[S16.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[S16.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S32S extends MS[S32.Value] {
  type V = S32.Value

  final def random: S32S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[S32.Value](sz)
    for (i <- 0 until sz) elements(i) = S32.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: S32.Value): S32S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[S32.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[S32.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object S64S extends MS[S64.Value] {
  type V = S64.Value

  final def random: S64S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[S64.Value](sz)
    for (i <- 0 until sz) elements(i) = S64.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: S64.Value): S64S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[S64.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[S64.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object NS extends MS[N] {
  type V = N

  final def random: NS.Value = {
    val sz = N8.random.toInt
    val elements = new Array[N](sz)
    for (i <- 0 until sz) elements(i) = N.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: N): NS.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[N]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[N](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N8S extends MS[N8.Value] {
  type V = N8.Value

  final def random: N8S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[N8.Value](sz)
    for (i <- 0 until sz) elements(i) = N8.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: N8.Value): N8S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[N8.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[N8.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N16S extends MS[N16.Value] {
  type V = N16.Value

  final def random: N16S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[N16.Value](sz)
    for (i <- 0 until sz) elements(i) = N16.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: N16.Value): N16S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[N16.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[N16.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N32S extends MS[N32.Value] {
  type V = N32.Value

  final def random: N32S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[N32.Value](sz)
    for (i <- 0 until sz) elements(i) = N32.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: N32.Value): N32S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[N32.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[N32.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object N64S extends MS[N64.Value] {
  type V = N64.Value

  final def random: N64S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[N64.Value](sz)
    for (i <- 0 until sz) elements(i) = N64.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: N64.Value): N64S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[N64.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[N64.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U8S extends MS[U8.Value] {
  type V = U8.Value

  final def random: U8S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[U8.Value](sz)
    for (i <- 0 until sz) elements(i) = U8.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: U8.Value): U8S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[U8.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[U8.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U16S extends MS[U16.Value] {
  type V = U16.Value

  final def random: U16S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[U16.Value](sz)
    for (i <- 0 until sz) elements(i) = U16.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: U16.Value): U16S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[U16.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[U16.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U32S extends MS[U32.Value] {
  type V = U32.Value

  final def random: U32S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[U32.Value](sz)
    for (i <- 0 until sz) elements(i) = U32.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: U32.Value): U32S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[U32.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[U32.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object U64S extends MS[U64.Value] {
  type V = U64.Value

  final def random: U64S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[U64.Value](sz)
    for (i <- 0 until sz) elements(i) = U64.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: U64.Value): U64S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[U64.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[U64.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object RS extends MS[R] {
  type V = R

  final def random: RS.Value = {
    val sz = N8.random.toInt
    val elements = new Array[R](sz)
    for (i <- 0 until sz) elements(i) = R.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: R): RS.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[R]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[R](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object F32S extends MS[F32.Value] {
  type V = F32.Value

  final def random: F32S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[F32.Value](sz)
    for (i <- 0 until sz) elements(i) = F32.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: F32.Value): F32S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[F32.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[F32.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}

object F64S extends MS[F64.Value] {
  type V = F64.Value

  final def random: F64S.Value = {
    val sz = N8.random.toInt
    val elements = new Array[F64.Value](sz)
    for (i <- 0 until sz) elements(i) = F64.random
    apply(elements: _*)
  }

  final def create(size: Z, dflt: F64.Value): F64S.Value = {
    if (size < 0) throw new IllegalArgumentException
    if (size > Int.MaxValue) {
      val (_, tm) = MS.newTreeMS[F64.Value]
      var i = Z(0)
      while (i < size) {
        tm.put(i, dflt)
        i = i + 1
      }
      new ValueTreeMap(tm, size)
    } else {
      val sz = size.toInt
      val elements = new Array[F64.Value](sz)
      for (i <- 0 until sz) elements(i) = dflt
      apply(elements: _*)
    }
  }

  final def apply(elements: V*): Value = new ValueArray(ArrayBuffer(elements: _*))

  sealed trait Value extends super.Value {
    override def apply(index: Z): V

    override def update(index: Z, value: V): Unit

    override def :+(value: V): Value

    override def +:(value: V): Value

    override def size: Z

    override def clone: Value = sys.error("stub")
  }

  private[logika] final class
  ValueArray(a: ArrayBuffer[V])
    extends super.ValueArray[ValueArray](a) with Value {

    private[logika] def upgrade: ValueTreeMap = {
      val (a, tm) = MS.newTreeMS[V]
      var i = Z.zero
      for (e <- this.a) {
        a(i) = e
        i += Z.one
      }
      new ValueTreeMap(tm, i)
    }

    override def :+(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) upgrade :+ value
      else make(a :+ value)

    override def +:(value: V): Value =
      if (size + Z.one == Z(Z.intMax)) value +: upgrade
      else make(value +: a)

    override def clone: ValueArray = make(a.clone)

    protected def make(a: ArrayBuffer[V]) = new ValueArray(a)
  }

  private[logika] final class
  ValueTreeMap(tm: java.util.TreeMap[Z, V],
               size: Z)
    extends super.ValueTreeMap[ValueTreeMap](tm, size) with Value {
    override def clone: ValueTreeMap =
      make(tm.clone.asInstanceOf[java.util.TreeMap[Z, V]], size)

    protected def make(tm: java.util.TreeMap[Z, V],
                       size: Z) = new ValueTreeMap(tm, size)
  }

}