val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "genericProgramming",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
      "org.scalameta" %% "munit" % "0.7.29" % Test, 
      "org.typelevel" %% "cats-core" % "2.10.0"
    )
  )
