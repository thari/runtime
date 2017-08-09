package org.sireum.ops

import org.sireum._

object BO_Ext {
  def &(b1: B, b2: B): B = b1.value & b2.value
}
