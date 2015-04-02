name := """play-securities-exchange"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.scalatest" % "scalatest_2.11" % "2.2.3" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.4" % "test"
)
