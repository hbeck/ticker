name := "steen"

version := "1.0"

scalaVersion := "2.11.8"
//scalaVersion := "2.12.1"
cancelable in Global := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

scalacOptions += "-feature"
scalacOptions += "-language:postfixOps"
scalacOptions += "-language:implicitConversions"

