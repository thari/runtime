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

class HashSSetTest extends TestSuite {

  val tests = Tests {

    * - assert(HashSSet.empty[String].size =~= z"0")

    * - assert(!HashSSet.empty[String].contains("a"))

    * - assert(HashSSet.empty[String].+("a").contains("a"))

    * - assert(!HashSSet.empty[String].+("a").contains("A"))

    * - assert(HashSSet.empty[String].+("a").+("b").contains("a"))

    * - assert(HashSSet.empty[String].+("a").+("b").contains("b"))

    * - assert(HashSSet.empty[String].++(ISZ("a", "b")).-("a").-("b").isEmpty)

    * - assert(
      HashSSet
        .empty[String]
        .++(ISZ("a", "b")) =~= HashSSet.empty[String].+("b").+("a"))

    * - assert(
      HashSSet.empty[String].∪(HashSSet.empty[String].+("A")) =~= HashSSet
        .empty[String]
        .+("A"))

    * - assert(
      HashSSet.empty[String].∩(HashSSet.empty[String].+("A")) =~= HashSSet
        .empty[String])

    * - assert(
      HashSSet
        .empty[String]
        .+("a")
        .∩(HashSSet.empty[String].+("A")) =~= HashSSet.empty[String])

  }

}
