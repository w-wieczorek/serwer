ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "serwer",
    assembly / assemblyJarName := "serwer.jar",
    assembly / mainClass := Some("zawody.Serwer"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.19"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.4"
libraryDependencies += "org.scalatest" %% "scalatest-funspec" % "3.2.11" % "test"
