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
  scalaVersion := "3.2.1",
  scalacOptions += "-deprecation",
  fork := true,

  libraryDependencies += "com.google.guava" % "guava" % "27.0.1-jre",
  libraryDependencies += "com.googlecode.plist" % "dd-plist" % "1.8",
  libraryDependencies += "com.eclipsesource.minimal-json" % "minimal-json" % "0.9.4",
  libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.19.0",
  libraryDependencies += "org.reflections" % "reflections" % "0.10.2",

  libraryDependencies ++= Seq("base", "controls").map(
    m => "org.openjfx" % s"javafx-$m" % "19" classifier osName),
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

  Compile / mainClass := Some("moped.impl.MopedLauncher"),
  Compile / discoveredMainClasses := Seq()
)
