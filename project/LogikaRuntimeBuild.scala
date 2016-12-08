/*
 * Copyright (c) 2016, Robby, Kansas State University
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

import sbt.Keys._
import sbt._

object LogikaRuntimeBuild extends Build {
  val scalaVer = "2.12.1"

  lazy val logikaRuntime = Project(
    id = "logika-runtime",
    base = file("."),
    settings = Seq(
      organization := "org.sireum",
      name := "logika-runtime",
      incOptions := incOptions.value.withNameHashing(true),
      retrieveManaged := true,
      version := "3.0.0-10-SNAPSHOT",
      scalaVersion := scalaVer,
      scalacOptions in(Compile, doc) := Seq("-groups", "-implicits"),
      parallelExecution in Test := true,
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
      libraryDependencies ++= Seq(
        "org.apfloat" % "apfloat" % "1.8.2",
        "org.scala-lang" % "scala-reflect" % scalaVer,
        "org.spire-math" %% "spire" % "0.13.0",
        "com.novocode" % "junit-interface" % "0.11" % Test
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
}
