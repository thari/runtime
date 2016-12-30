val scalaVer = "2.12.1"

lazy val logikaRuntime = Project(
  id = "logika-runtime",
  base = file("."),
  settings = Seq(
    organization := "org.sireum",
    name := "logika-runtime",
    incOptions := incOptions.value.withNameHashing(true),
    retrieveManaged := true,
    version := "3.0.0-10",
    scalaVersion := scalaVer,
    scalacOptions in(Compile, doc) := Seq("-groups", "-implicits"),
    parallelExecution in Test := true,
    libraryDependencies ++= Seq(
      "org.apfloat" % "apfloat" % "1.8.2",
      "org.scala-lang" % "scala-reflect" % scalaVer,
      "org.spire-math" %% "spire" % "0.13.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),
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
      <url>https://github.com/sireum/v3-logika-runtime/</url>
        <licenses>
          <license>
            <name>Simplified BSD License</name>
            <url>https://github.com/sireum/v3-logika-runtime/blob/master/license.md</url>
          </license>
        </licenses>
        <scm>
          <url>https://github.com/sireum/v3-logika-runtime.git</url>
          <connection>scm:git:https://github.com/sireum/v3-logika-runtime.git</connection>
        </scm>
        <developers>
          <developer>
            <id>robby-phd</id>
            <name>Robby</name>
            <url>http://people.cs.ksu.edu/~robby</url>
          </developer>
        </developers>
  )
)