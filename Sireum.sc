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

import mill._
import mill.scalalib._
import mill.scalajslib._
import mill.scalalib.publish._
import ammonite.ops._

trait Module extends ScalaModule {

  final override def scalaVersion = T { Module.scalaVersion }

  final override def javacOptions =
    Seq("-source", "1.8", "-target", "1.8", "-encoding", "utf8")

  final override def scalacOptions =
    Seq("-target:jvm-1.8",
        "-deprecation",
        "-Yrangepos",
        "-Ydelambdafy:method",
        "-feature",
        "-unchecked",
        "-Xfatal-warnings")

  def platformSegment: String
}

object Module {

  object Developers {

    val robby = Developer("robby-phd", "Robby", "https://github.com/robby-phd")

  }

  final val scalaVersion = "2.12.4"

  final val scalacPluginVersion = "3.2.9"

  final val scalaJsVersion = "0.6.22"

  final val utestVersion = "0.6.3"

  final val scalaJsonVersion = "1.0.0-M4"

  final val spireVersion = "0.13.0"

  final val scalaMetaVersion = "3.2.0"

  sealed trait Project

  object Project {

    trait Jvm extends ScalaModule with Module { outer =>

      def platformSegment: String

      def deps: Seq[Jvm]

      def testIvyDeps: Agg[Dep]

      def testScalacPluginIvyDeps: Agg[Dep]

      def testFrameworks: Seq[String]

      final override def sources = T.sources(
        millSourcePath / "src" / "main" / "scala"
      )

      def tests: Tests

      trait Tests extends super.Tests {

        final override def millSourcePath =
          super.millSourcePath / up / up / platformSegment / "src" / "test"

        final override def ivyDeps = T { outer.testIvyDeps }

        final override def scalacPluginIvyDeps = T {
          outer.testScalacPluginIvyDeps
        }

        final override def testFrameworks = T { outer.testFrameworks }

        final override def sources = T.sources(
          millSourcePath / "scala"
        )
      }

    }

    trait Js extends ScalaJSModule with Module { outer =>

      def deps: Seq[Js]

      def testIvyDeps: Agg[Dep]

      def testScalacPluginIvyDeps: Agg[Dep]

      def testFrameworks: Seq[String]

      final override def sources = T.sources(
        millSourcePath / "src" / "main" / "scala",
        millSourcePath / up / "shared" / "src" / "main" / "scala",
      )

      def tests: Tests

      trait Tests extends super.Tests {

        final override def millSourcePath =
          super.millSourcePath / up / up / "js" / "src" / "test"

        final override def ivyDeps = T { outer.testIvyDeps }

        final override def scalacPluginIvyDeps = T {
          outer.testScalacPluginIvyDeps
        }

        final override def testFrameworks = T { outer.testFrameworks }

        final override def sources = T.sources(
          millSourcePath / "scala",
          millSourcePath / up / up / up / "shared" / "src" / "test" / "scala"
        )
      }

    }

    trait Publish extends PublishModule {

      def description: String
      def subUrl: String
      def developers: Seq[Developer]

      final def pomSettings = PomSettings(
        description = description,
        organization = "org.sireum",
        url = s"https://github.com/sireum/$subUrl",
        licenses = Seq(
          License("BSD-2 License",
                  s"https://github.com/sireum/$subUrl/blob/master/license.txt")
        ),
        scm = SCM(
          s"git://github.com/sireum/$subUrl.git",
          s"scm:git://github.com/sireum/$subUrl.git"
        ),
        developers = developers
      )
    }

    trait JvmPublish extends Jvm with Publish {

      def deps: Seq[JvmPublish]

    }

    trait JsPublish extends Js with Publish {

      def deps: Seq[JsPublish]

    }

    trait CrossJvmJs extends mill.Module {

      def shared: Jvm

      def jvm: Jvm

      def js: Js

      def deps: Seq[CrossJvmJs]

      def jvmDeps: Seq[Jvm]

      def jsDeps: Seq[Js]

      def ivyDeps: Agg[Dep]

      def scalacPluginIvyDeps: Agg[Dep]

      def testIvyDeps: Agg[Dep]

      def jvmTestIvyDeps: Agg[Dep]

      def jsTestIvyDeps: Agg[Dep]

      def testScalacPluginIvyDeps: Agg[Dep]

      def jvmTestFrameworks: Seq[String]

      def jsTestFrameworks: Seq[String]
    }

    trait CrossJvmJsPublish extends CrossJvmJs {

      def shared: JvmPublish

      def jvm: JvmPublish

      def js: JsPublish

      def deps: Seq[CrossJvmJsPublish]

      def jvmDeps: Seq[JvmPublish]

      def jsDeps: Seq[JsPublish]
    }

  }

  trait Shared extends Project.Jvm {

    final override def platformSegment = "shared"

  }

  trait Jvm extends Project.Jvm {

    final override def platformSegment = "jvm"

  }

  trait Js extends Project.Js {

    final override def platformSegment = "js"

    final override def scalaJSVersion = T { scalaJsVersion }
  }

  trait SharedPublish extends Project.JvmPublish {

    final override def platformSegment = "shared"

  }

  trait JvmPublish extends Project.JvmPublish {

    final override def platformSegment = "jvm"

  }

  trait JsPublish extends Project.JsPublish {

    final override def platformSegment = "js"

    final override def scalaJSVersion = T { scalaJsVersion }
  }

  trait CrossJvmJs extends Project.CrossJvmJs { outer =>

    object shared extends Shared {

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jvmTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jvmTestFrameworks

      final override def deps = Seq()

      final override def moduleDeps = mDeps

      final def mDeps = for (dep <- outer.deps) yield dep.shared

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared) ++
            (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }
    }

    object jvm extends Jvm {

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jvmTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jvmTestFrameworks

      final override def deps = Seq()

      override def moduleDeps = mDeps

      final def mDeps =
        (for (dep <- outer.deps) yield dep.shared) ++
          (for (dep <- outer.deps) yield dep.jvm) ++ jvmDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared, jvm, shared.tests) ++
            (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }
    }

    object js extends Js {

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jsTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jsTestFrameworks

      final override def deps = Seq()

      final override def moduleDeps = mDeps

      final def mDeps = (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(js) ++ (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }
    }

  }

  trait CrossJvmJsPublish extends Project.CrossJvmJsPublish { outer =>

    def developers: Seq[Developer]

    def publishVersion: String

    def description: String

    def subUrl: String

    object shared extends SharedPublish {

      final override def subUrl = outer.subUrl

      final override def description: String = outer.description

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jvmTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jvmTestFrameworks

      final override def developers = outer.developers

      final override def publishVersion = outer.publishVersion

      final override def deps = Seq()

      final override def moduleDeps = mDeps

      final def mDeps = for (dep <- outer.deps) yield dep.shared

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared) ++
            (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }

    }

    object jvm extends JvmPublish {

      final override def subUrl = outer.subUrl

      final override def description: String = outer.description

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jvmTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jvmTestFrameworks

      final override def developers = outer.developers

      final override def publishVersion = outer.publishVersion

      final override def deps = Seq()

      override def moduleDeps = mDeps

      final def mDeps =
        (for (dep <- outer.deps) yield dep.shared) ++
          (for (dep <- outer.deps) yield dep.jvm) ++ jvmDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared, jvm, shared.tests) ++
            (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }

    }

    object js extends JsPublish {

      final override def subUrl = outer.subUrl

      final override def description: String =
        CrossJvmJsPublish.this.description

      final override def ivyDeps = T { CrossJvmJsPublish.this.ivyDeps }

      final override def scalacPluginIvyDeps = T {
        CrossJvmJsPublish.this.scalacPluginIvyDeps
      }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jsTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jsTestFrameworks

      final override def developers = CrossJvmJsPublish.this.developers

      final override def publishVersion = CrossJvmJsPublish.this.publishVersion

      final override def deps = Seq()

      final override def moduleDeps = mDeps

      final def mDeps = (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(js) ++ (for (dep <- mDeps) yield Seq(dep, dep.tests)).flatten

      }
    }

  }

}
