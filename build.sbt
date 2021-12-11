val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Moped",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.google.guava" % "guava" % "27.0.1-jre",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
