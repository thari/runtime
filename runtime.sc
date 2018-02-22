import $file.sireum

trait RuntimeModule extends sireum.SireumModule.CrossJvmJsPublish {
  import mill._

  final override def subUrl: String = "runtime"

  final override def developers = Seq(sireum.SireumModule.Developers.robby)

  final override def publishVersion = "4.0.0"

  final override def testIvyDeps = Agg.empty

}

trait MacrosModule extends RuntimeModule {

  import mill._
  import mill.scalalib._

  final override def description: String = "Sireum Runtime Macros"

  final override def ivyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:${sireum.SireumModule.scalaVersion}"
  )

  final override def jvmTestFrameworks = Seq()

  final override def jsTestFrameworks = Seq()

  final override def jvmTestIvyDeps = Agg.empty

  final override def jsTestIvyDeps = Agg.empty

  final override def scalacPluginIvyDeps = Agg.empty

  final override def testScalacPluginIvyDeps = Agg.empty

  final override def deps = Seq()

  final override def jvmDeps = Seq()

  final override def jsDeps = Seq()

}

trait LibraryModule extends RuntimeModule {
  import mill._
  import mill.scalalib._

  final override def description: String = "Sireum Runtime Library"

  final override def ivyDeps = Agg(
    ivy"org.scala-lang.platform::scalajson:${sireum.SireumModule.scalaJsonVersion}",
    ivy"org.spire-math::spire:${sireum.SireumModule.spireVersion}"
  )

  final override def jvmTestIvyDeps = Agg(
    ivy"org.scalatest::scalatest:${sireum.SireumModule.scalaTestVersion}"
  )

  final override def jsTestIvyDeps = Agg(
    ivy"org.scalatest:scalatest_sjs${sireum.SireumModule.scalaJsBinVersion}_${sireum.SireumModule.scalaBinVersion}:${sireum.SireumModule.scalaTestVersion}"
  )

  final override lazy val scalacPluginIvyDeps = Agg(
    ivy"org.sireum::scalac-plugin:${sireum.SireumModule.scalacPluginVersion}"
  )

  final override def testScalacPluginIvyDeps = scalacPluginIvyDeps

  final override lazy val jvmTestFrameworks = Seq("org.scalatest.tools.Framework")

  final override def jsTestFrameworks = jvmTestFrameworks

  final override def deps = Seq(macrosObject)

  final override def jvmDeps = Seq()

  final override def jsDeps = Seq()

  def macrosObject: RuntimeModule
}
