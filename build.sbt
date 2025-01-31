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
  scalaVersion := "3.6.3",
  scalacOptions += "-deprecation",
  fork := true,

  resolvers += "Local Maven Repository" at Path.userHome.asFile.toURI.toURL + ".m2/repository",

  libraryDependencies += "com.google.guava" % "guava" % "27.0.1-jre",
  libraryDependencies += "com.googlecode.plist" % "dd-plist" % "1.8",
  libraryDependencies += "com.eclipsesource.minimal-json" % "minimal-json" % "0.9.4",
  libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.23.1",
  libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j.websocket" % "0.23.1",
  libraryDependencies += "org.glassfish.tyrus.bundles" % "tyrus-standalone-client" % "1.21",
  libraryDependencies += "org.reflections" % "reflections" % "0.10.2",
  libraryDependencies += "org.apache.commons" % "commons-compress" % "1.15",
  libraryDependencies += "ch.usi.si.seart" % "java-tree-sitter" % "1.13.0-SNAPSHOT",

  libraryDependencies ++= Seq("base", "controls").map(
    m => "org.openjfx" % s"javafx-$m" % "19" classifier osName),
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

  Compile / mainClass := Some("moped.impl.MopedLauncher"),
  Compile / discoveredMainClasses := Seq()
)
