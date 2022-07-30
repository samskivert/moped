enablePlugins(JavaAppPackaging)
// enablePlugins(JDKPackagerPlugin)

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val root = project.in(file(".")).settings(
  name := "Moped",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.1.0",
  fork := true,

  libraryDependencies += "com.google.guava" % "guava" % "27.0.1-jre",
  libraryDependencies += "org.ow2.asm" % "asm" % "6.0",
  libraryDependencies ++= Seq("base", "controls").map(
    m => "org.openjfx" % s"javafx-$m" % "18.0.2" classifier osName),
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

  Compile / mainClass := Some("moped.impl.MopedLauncher"),
  Compile / discoveredMainClasses := Seq()
)
