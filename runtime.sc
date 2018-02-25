/*
 Copyright (c) 2018, Robby, Kansas State University
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
import $file.sireum

trait RuntimeModule extends sireum.SireumModule.CrossJvmJsPublish {
  import mill._
  import mill.scalalib._
  import sireum.SireumModule._

  final override def subUrl: String = "runtime"

  final override def developers = Seq(Developers.robby)

  final override def publishVersion = "4.0.0-SNAPSHOT"

  final override def testIvyDeps = Agg(
    ivy"com.lihaoyi::utest::$utestVersion"
  )

  final override def jvmTestIvyDeps = Agg.empty

  final override def jsTestIvyDeps = Agg.empty

  final override lazy val jvmTestFrameworks = Seq("utest.runner.Framework")

  final override def jsTestFrameworks = jvmTestFrameworks

  final override def jvmDeps = Seq()

  final override def jsDeps = Seq()

}

trait MacrosModule extends RuntimeModule {

  import mill._
  import mill.scalalib._
  import sireum.SireumModule._

  final override def description: String = "Sireum Runtime Macros"

  final override def ivyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:$scalaVersion"
  )

  final override def scalacPluginIvyDeps = Agg.empty

  final override def testScalacPluginIvyDeps = Agg.empty

  final override def deps = Seq()

}

trait LibraryModule extends RuntimeModule {
  import mill._
  import mill.scalalib._
  import sireum.SireumModule._

  final override def description: String = "Sireum Runtime Library"

  final override def ivyDeps = Agg(
    ivy"org.scala-lang.platform::scalajson:$scalaJsonVersion",
    ivy"org.spire-math::spire:$spireVersion"
  )

  final override lazy val scalacPluginIvyDeps = Agg(
    ivy"org.sireum::scalac-plugin:$scalacPluginVersion"
  )

  final override def testScalacPluginIvyDeps = scalacPluginIvyDeps

  final override def deps = Seq(macrosObject)

  def macrosObject: RuntimeModule
}
