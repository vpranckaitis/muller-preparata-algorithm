import sbt._

lazy val commonSettings = Seq(
  organization := "lt.vpranckaitis",
  name := "muller-preparata",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val dependencies = Seq(
  "org.scala-lang.modules" %% "scala-swing" % "1.0.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= dependencies)