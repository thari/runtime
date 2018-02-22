object SireumModule {
  import mill._
  import mill.scalalib._
  import mill.scalajslib._
  import mill.scalalib.publish._
  import ammonite.ops.up

  object Developers {

    val robby = Developer("robby-phd", "Robby", "https://github.com/robby-phd")

  }

  final val scalaBinVersion = "2.12"

  final val scalaVersion = s"$scalaBinVersion.4"

  final val scalacPluginVersion = "3.2.9"

  final val scalaJsBinVersion = "0.6"

  final val scalaJsVersion = s"$scalaJsBinVersion.22"

  final val scalaTestVersion = "3.0.4"

  final val scalaJsonVersion = "1.0.0-M4"

  final val spireVersion = "0.13.0"

  sealed trait Project

  object Project {

    trait Jvm extends ScalaModule { outer =>

      def platformSegment: String

      def deps: Seq[Jvm]

      def testIvyDeps: Agg[Dep]

      def testScalacPluginIvyDeps: Agg[Dep]

      def testFrameworks: Seq[String]

      final override def sources = T.sources(
        millSourcePath / "src" / "main" / "scala"
      )

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

    trait Js extends ScalaJSModule { outer =>

      def deps: Seq[Js]

      def testIvyDeps: Agg[Dep]

      def testScalacPluginIvyDeps: Agg[Dep]

      def testFrameworks: Seq[String]

      final override def sources = T.sources(
        millSourcePath / "src" / "main" / "scala",
        millSourcePath / up / "shared" / "src" / "main" / "scala",
      )

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

    trait JvmPublish extends Jvm with PublishModule {

      def deps: Seq[JvmPublish]

    }

    trait JsPublish extends Js with PublishModule {

      def deps: Seq[JsPublish]

    }

    trait CrossJvmJs extends Module {

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

  trait Shared extends Project.Jvm with SireumModule {

    final override def platformSegment = "shared"

  }

  trait Jvm extends Project.Jvm with SireumModule {

    final override def platformSegment = "jvm"

  }

  trait Js extends Project.Js with SireumModule with ScalaJSModule {

    final override def platformSegment = "js"

    final override def scalaJSVersion = T { scalaJsVersion }
  }

  trait SharedPublish extends Project.JvmPublish with SireumModule {

    final override def platformSegment = "shared"

  }

  trait JvmPublish extends Project.JvmPublish with SireumModule {

    final override def platformSegment = "jvm"

  }

  trait JsPublish
      extends Project.JsPublish
      with SireumModule
      with ScalaJSModule {

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

      final override def moduleDeps =
        for (dep <- outer.deps) yield dep.shared

      object tests extends Tests {

        final override def moduleDeps = Seq(shared) ++ shared.moduleDeps

      }
    }

    object jvm extends Jvm {

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jvmTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jvmTestFrameworks

      final override def deps = Seq()

      final override def moduleDeps =
        (for (dep <- outer.deps) yield dep.shared) ++ (for (dep <- outer.deps)
          yield dep.jvm) ++ jvmDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared, jvm) ++ shared.moduleDeps ++ jvm.moduleDeps

      }
    }

    object js extends Js {

      final override def ivyDeps = T { outer.ivyDeps }

      final override def scalacPluginIvyDeps = T { outer.scalacPluginIvyDeps }

      final override def testIvyDeps = outer.testIvyDeps ++ outer.jsTestIvyDeps

      final override def testScalacPluginIvyDeps = outer.testScalacPluginIvyDeps

      final override def testFrameworks = outer.jsTestFrameworks

      final override def deps = Seq()

      final override def moduleDeps =
        (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(js) ++ (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      }
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

  trait CrossJvmJsPublish extends Project.CrossJvmJsPublish { outer =>

    def developers: Seq[Developer]

    def publishVersion: String

    def description: String

    def subUrl: String

    object shared extends SharedPublish with Publish {

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

      final override def moduleDeps =
        for (dep <- outer.deps) yield dep.shared

      object tests extends Tests {

        final override def moduleDeps = Seq(shared) ++ shared.moduleDeps

      }

    }

    object jvm extends JvmPublish with Publish {

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

      override def moduleDeps =
        (for (dep <- outer.deps) yield dep.shared) ++ (for (dep <- outer.deps)
          yield dep.jvm) ++ jvmDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(shared, jvm, shared.tests) ++ shared.moduleDeps ++ jvm.moduleDeps

      }

    }

    object js extends JsPublish with Publish {

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

      final override def moduleDeps =
        (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      object tests extends Tests {

        final override def moduleDeps =
          Seq(js) ++ (for (dep <- outer.deps) yield dep.js) ++ jsDeps

      }
    }

  }

}

trait SireumModule extends mill.scalalib.ScalaModule {

  import mill._
  import ammonite.ops.up

  final override def scalaVersion = T { SireumModule.scalaVersion }

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
