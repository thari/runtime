val scalaVer = "2.12.3"

val metaVersion = "1.8.0"

val paradiseVersion = "3.0.0-M10"

val runtimeVersion = "3.1.3"

val sireumScalacVersion = "3.1.2"

scalaVersion in ThisBuild := scalaVer

scalacOptions in ThisBuild := Seq("-target:jvm-1.8", "-deprecation", "-Yrangepos",
  "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings")

val commonSettings = Seq(
  organization := "org.sireum",
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  retrieveManaged := true,
  version := runtimeVersion,
  parallelExecution in Test := true,
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % metaVersion
  ),
  sources in(Compile, doc) := Seq.empty,
  resolvers += Resolver.sonatypeRepo("public"),
  addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),
  publishMavenStyle := true,
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <url>https://github.com/sireum/v3-runtime/</url>
      <licenses>
        <license>
          <name>Simplified BSD License</name>
          <url>https://github.com/sireum/v3-runtime/blob/master/license.md</url>
        </license>
      </licenses>
      <scm>
        <url>https://github.com/sireum/v3-runtime.git</url>
        <connection>scm:git:https://github.com/sireum/v3-runtime.git</connection>
      </scm>
      <developers>
        <developer>
          <id>robby-phd</id>
          <name>Robby</name>
          <url>http://cs.ksu.edu/~robby</url>
        </developer>
      </developers>
)

lazy val sireumRuntime = {
  val `sireum-runtime` = project.in(file(".")).
    aggregate(macrosJvm, macrosJs, libraryJvm, libraryJs).
    settings(
      publishArtifact := false
    )
  `sireum-runtime`
}

lazy val macros = crossProject.in(file("macros")).settings(commonSettings: _*).settings(
  name := "macros",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVer,
    "org.spire-math" %%% "spire" % "0.13.0"
  )
)

lazy val macrosJvm = macros.jvm
lazy val macrosJs = macros.js

lazy val library = crossProject.in(file("library")).settings(commonSettings: _*).settings(
  name := "library",
  libraryDependencies ++= Seq(
    "org.scala-lang.platform" %%% "scalajson" % "1.0.0-M4",
    "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
  ),
  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion)
).dependsOn(macros)

lazy val libraryJvm = library.jvm
lazy val libraryJs = library.js
