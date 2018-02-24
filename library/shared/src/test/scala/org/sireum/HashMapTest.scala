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

import utest._
import org.sireum.test._

object HashMapTest extends TestSuite {

  val tests = Tests {

    * - assert(HashMap.empty[String, Z].size =~= z"0")

    * - assert(HashMap.empty[String, Z].get("a") =~= None())

    * - assert(HashMap.empty[String, Z].+("a", 1).get("a") =~= Some(1))

    * - assert(HashMap.empty[String, Z].+("a", 1).get("A") =~= None())

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("a", 2).get("a") =~= Some(2))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2).get("a") =~= Some(1))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2).get("b") =~= Some(2))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2) =~= HashMap
        .empty[String, Z]
        .+("b", 2)
        .+("a", 1))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2).-("a", 1) =~= HashMap
        .empty[String, Z]
        .+("b", 2))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2).-("b", 2) =~= HashMap
        .empty[String, Z]
        .+("a", 1))

    * - assert(
      HashMap.empty[String, Z].+("a", 1).+("b", 2).--(ISZ("a", "b")) =~= HashMap
        .empty[String, Z])
  }
}
