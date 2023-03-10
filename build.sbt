ThisBuild / version := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "SuprNationTriangleChallenge"
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.9.0",
  "org.scalatest" %% "scalatest" % "3.2.14" % Test
)