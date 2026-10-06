ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "lab"
ThisBuild / version := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "transfer-manager",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "4.1.0",
      "org.scalameta" %% "munit" % "1.0.4" % Test
    ),
    Test / testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )