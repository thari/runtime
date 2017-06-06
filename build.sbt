val scalaVer = "2.12.2"

val metaVersion = "1.8.0"

val paradiseVersion = "3.0.0-M9"

val silencerVersion = "0.5"

val runtimeVersion = "3.0.1-1-SNAPSHOT"

val sireumScalacVersion = "3.0.0-7"

scalaVersion in ThisBuild := scalaVer

scalacOptions in ThisBuild := Seq("-target:jvm-1.8", "-deprecation",
  "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings")

val commonSettings = Seq(
  organization := "org.sireum",
  incOptions := incOptions.value.withNameHashing(true),
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  retrieveManaged := true,
  version := runtimeVersion,
  parallelExecution in Test := true,
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % metaVersion
  ),
  sources in(Compile, doc) := Seq.empty,
  publishArtifact in(Compile, packageDoc) := false,
  resolvers += Resolver.sonatypeRepo("public"),
  addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
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
    aggregate(runtimeJvm, runtimeJs, preludeJvm, preludeJs).
    settings(
      publish := {},
      publishLocal := {}
    )
  `sireum-runtime`
}

lazy val runtime = crossProject.in(file("runtime")).settings(commonSettings: _*).settings(
  name := "runtime",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVer,
    "org.spire-math" %%% "spire" % "0.13.0"
  )
)

lazy val runtimeJvm = runtime.jvm
lazy val runtimeJs = runtime.js

lazy val prelude = crossProject.in(file("prelude")).settings(commonSettings: _*).settings(
  name := "prelude",
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
    "com.github.ghik" %% "silencer-lib" % silencerVersion
  ),
  addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion)
).dependsOn(runtime)

lazy val preludeJvm = prelude.jvm
lazy val preludeJs = prelude.js
