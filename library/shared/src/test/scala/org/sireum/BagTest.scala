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

class BagTest extends SireumRuntimeSpec {

  *(Bag.of[String].size =~= z"0")

  *(!Bag.of[String].contains("a"))

  *((Bag ++ ISZ("a")).contains("a"))

  *((Bag.of[String] + "a" + "a").count("a") == 2)

  *((Bag.of[String] +# (string"a" ~> z"4") + "a").count("a") == 5)

  *((Bag.of[String] +# (string"a" ~> z"4") - "a").count("a") == 3)

  *((Bag.of[String] +# (string"a" ~> z"4") -# (string"a" ~> z"10")).count("a") == 0)

  *(!(Bag.of[String] + "a").contains("A"))

  *((Bag.of[String] + "a" + "b").contains("a"))

  *((Bag.of[String] + "a" + "b").contains("b"))

  *((Bag.of[String] ++ ISZ("a", "b") - "a" - "b").isEmpty)

  *(Bag.of[String] ++ ISZ("a", "b") =~= Bag.of[String] + "b" + "a")

  *((Bag.of[String] ∪ Bag.of[String] + "A") =~= Bag.of[String] + "A")

  *((Bag.of[String] ∩ (Bag.of[String] + "A")) =~= Bag.of[String])

  *(((Bag.of[String] + "a") ∩ (Bag.of[String] + "A")) =~= Bag.of[String])

}
