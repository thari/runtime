package org.sireum

import org.sireum.test.TestSuite

import U8._
import U32._

class MurmurHash3aTest extends TestSuite {

  val tests = Tests {
    * - assert(Hash.murmur3a(ISZ(u8"65"), u32"0") == u32"1423767502")

    * - assert(Hash.murmur3a(ISZ(u8"65", u8"66"), u32"0") == u32"4094335635")

    * - assert(Hash.murmur3a(ISZ(u8"65", u8"66", u8"67"), u32"0") == u32"1136772405")

    * - assert(Hash.murmur3a(ISZ(u8"65", u8"66", u8"67", u8"68"), u32"0") == u32"1230572953")
  }
}
