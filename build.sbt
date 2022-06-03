name := "ticker"

version := "1.0"

scalaVersion := "2.12.15"

cancelable in Global := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

scalacOptions += "-feature"
scalacOptions += "-deprecation"
scalacOptions += "-language:postfixOps"
scalacOptions += "-language:implicitConversions"

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

/*
lazy val disseval = (project in file(".")).
  settings(commonSettings: _*).
  configs(IntegrationTest).
  settings(Defaults.itSettings: _*).
  settings(
    assemblyJarName in assembly := "disseval.jar",
    mainClass in assembly := Some("evaluation.diss.DissEvalMain"),
    test in assembly := {}
  )
*/

//
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
excludeFilter in assembly ~= {
  exclude => exclude && FileFilter.globFilter("src/test/**/*.*")
}
