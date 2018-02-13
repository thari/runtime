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

import org.sireum.test._

class PosetTest extends SireumRuntimeSpec {

  /*
        A   H     I
       / \  | \  / \
      B   C |  G   /
     / \ / \| / \ /
    D   E   F    J
         \      /   N
          \    K   /
           \  / \ /
            \/   M
             L
   */
  val poset: Poset[String] = {
    Poset
      .empty[String]
      .addChildren("A", ISZ[String]("B", "C"))
      .addChildren("B", ISZ[String]("D", "E"))
      .addChildren("C", ISZ[String]("E", "F"))
      .addNode("D")
      .addChildren("E", ISZ[String]("L"))
      .addNode("E")
      .addChildren("G", ISZ[String]("F", "J"))
      .addChildren("H", ISZ[String]("F", "G"))
      .addChildren("I", ISZ[String]("G", "J"))
      .addChildren("J", ISZ[String]("K"))
      .addChildren("K", ISZ[String]("L", "M"))
      .addNode("M")
      .addChildren("N", ISZ[String]("M"))
  }

  *(poset.isParentOf("B", "A"))

  *(poset.isChildOf("A", "C"))

  *(poset.parentsOf("F") =~= HashSet.empty[String].++(ISZ[String]("C", "G", "H")))

  *(poset.ancestorsOf("J") =~= HashSet.empty[String].++(ISZ[String]("G", "H", "I")))

  *(poset.ancestorsOf("F") =~= HashSet.empty[String].++(ISZ[String]("A", "C", "G", "H", "I")))

  *(poset.ancestorsOf("L") =~= HashSet.empty[String].++(ISZ[String]("A", "B", "C", "E", "G", "H", "I", "J", "K")))

  *(poset.lub(ISZ[String]("A", "A")) =~= Some[String]("A"))

  *(poset.lub(ISZ[String]("A", "H")) =~= None[String]())

  *(poset.lub(ISZ[String]("D", "E", "F")) =~= Some[String]("A"))

  *(poset.lub(ISZ[String]("F", "J", "K")) =~= Some[String]("G"))

  *(poset.glb(ISZ[String]("A", "A")) =~= Some[String]("A"))

  *(poset.glb(ISZ[String]("D", "E")) =~= None[String]())

  *(poset.glb(ISZ[String]("A", "I")) =~= None[String]())

  *(poset.glb(ISZ[String]("B", "C")) =~= Some[String]("E"))

  *(poset.glb(ISZ[String]("A", "B", "C")) =~= Some[String]("E"))

  *(poset.glb(ISZ[String]("B", "C", "G")) =~= Some[String]("L"))

  *(poset.glb(ISZ[String]("H", "I", "N")) =~= Some[String]("M"))
}
