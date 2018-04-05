name := "ticker"

version := "1.0"

scalaVersion := "2.12.4"
cancelable in Global := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
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

//
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
excludeFilter in assembly ~= {
  exclude => exclude && FileFilter.globFilter("src/test/**/*.*")
}