name := "steen"

version := "1.0"

scalaVersion := "2.11.7"
cancelable in Global := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
//libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4"

scalacOptions += "-feature"
scalacOptions += "-language:postfixOps"
scalacOptions += "-language:implicitConversions"
