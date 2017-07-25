name := "ticker"

version := "1.0"

scalaVersion := "2.12.2"
cancelable in Global := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"


scalacOptions += "-feature"
scalacOptions += "-deprecation"
scalacOptions += "-language:postfixOps"
scalacOptions += "-language:implicitConversions"

import sbtassembly.AssemblyPlugin._

import sbt.Package.ManifestAttributes

lazy val commonSettings = Seq(

  test in assembly := {}

)

lazy val ticker = (project in file(".")).
  settings(commonSettings: _*).
  configs(IntegrationTest).
  settings(Defaults.itSettings: _*).
  settings(
    mainClass in assembly := Some("Program"),
    test in assembly := {}
  )

//
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
excludeFilter in assembly ~= {
  exclude => exclude && FileFilter.globFilter("src/test/**/*.*")
}