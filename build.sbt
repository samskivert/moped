enablePlugins(JavaAppPackaging)
// enablePlugins(JDKPackagerPlugin)

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// builds the native shared libraries for the tree-sitter grammars that aren't published as
// prebuilt Java artifacts (see native/tree-sitter-{swift,prisma}, added as git submodules); at
// runtime these are loaded via TSLanguage.load, see grammar.Sitter.loadNative
lazy val buildTreeSitterGrammars = taskKey[Seq[File]](
  "Builds the tree-sitter-swift and tree-sitter-prisma native grammar libraries.")

lazy val root = project.in(file(".")).settings(
  name := "Moped",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "3.8.4",
  scalacOptions += "-deprecation",
  fork := true,

  resolvers += "Local Maven Repository" at Path.userHome.asFile.toURI.toURL.toString + ".m2/repository",

  libraryDependencies += "com.google.guava" % "guava" % "33.6.0-jre",
  libraryDependencies += "com.googlecode.plist" % "dd-plist" % "1.28",
  libraryDependencies += "com.eclipsesource.minimal-json" % "minimal-json" % "0.9.5",
  libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0",
  libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j.websocket.jakarta" % "0.24.0",
  libraryDependencies += "org.glassfish.tyrus.bundles" % "tyrus-standalone-client" % "1.22",
  libraryDependencies += "org.reflections" % "reflections" % "0.10.2",
  libraryDependencies += "org.apache.commons" % "commons-compress" % "1.28.0",
  libraryDependencies += "io.github.bonede" % "tree-sitter" % "0.26.6",
  libraryDependencies += "io.github.bonede" % "tree-sitter-python" % "0.25.0",
  libraryDependencies += "io.github.bonede" % "tree-sitter-typescript" % "0.23.2",
  libraryDependencies += "io.github.bonede" % "tree-sitter-tsx" % "0.23.2",

  libraryDependencies ++= Seq("base", "controls").map(
    m => "org.openjfx" % s"javafx-$m" % "26" classifier osName),
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

  Compile / mainClass := Some("moped.impl.MopedLauncher"),
  Compile / discoveredMainClasses := Seq(),

  buildTreeSitterGrammars := Def.uncached {
    import scala.sys.process.Process
    val log = streams.value.log
    val libExt = osName match {
      case "mac" => "dylib"
      case "win" => "dll"
      case _     => "so"
    }
    // macOS wants -dynamiclib, everyone else wants -shared (mingw on Windows accepts -shared too)
    val sharedFlag = if (osName == "mac") "-dynamiclib" else "-shared"
    // a fixed, sbt-version-independent location (rather than target.value, whose exact layout
    // shifts between sbt versions) so grammar.Sitter.loadNative can find it at runtime without
    // needing to know anything about how the project was built
    val outDir = baseDirectory.value / "native" / "build"
    IO.createDirectory(outDir)

    // returns true if `out` doesn't exist or any of `srcs` is newer than it
    def stale (srcs :Seq[File], out :File) :Boolean =
      !out.exists || srcs.exists(_.lastModified > out.lastModified)

    def build (name :String, grammarDir :File, needsGenerate :Boolean) :File = {
      if (!(grammarDir / "grammar.js").exists) sys.error(
        s"Native grammar submodule for tree-sitter-$name is missing ($grammarDir).\n" +
        "Did you run `git submodule update --init`?")
      val srcDir = grammarDir / "src"
      val parserC = srcDir / "parser.c"
      if (needsGenerate && !parserC.exists) {
        log.info(s"Generating tree-sitter-$name parser (tree-sitter generate)...")
        if (Process(Seq("tree-sitter", "generate"), grammarDir).! != 0)
          sys.error(s"`tree-sitter generate` failed for tree-sitter-$name; " +
            "is tree-sitter-cli installed? (`brew install tree-sitter-cli`)")
      }
      val scannerC = srcDir / "scanner.c"
      val srcs = Seq(parserC) ++ (if (scannerC.exists) Seq(scannerC) else Seq())
      val out = outDir / s"libtree-sitter-$name.$libExt"
      if (stale(srcs, out)) {
        log.info(s"Compiling tree-sitter-$name -> $out")
        val cmd = Seq("cc", sharedFlag, "-fPIC", "-O2", "-I", srcDir.toString) ++
          srcs.map(_.toString) ++ Seq("-o", out.toString)
        if (Process(cmd).! != 0) sys.error(s"Failed to compile tree-sitter-$name")
      } else log.debug(s"tree-sitter-$name is up to date ($out)")
      out
    }

    val nativeDir = baseDirectory.value / "native"
    Seq(
      build("swift", nativeDir / "tree-sitter-swift", needsGenerate = true),
      build("prisma", nativeDir / "tree-sitter-prisma", needsGenerate = false),
    )
  },
  Compile / compile := Def.uncached((Compile / compile).dependsOn(buildTreeSitterGrammars).value),

  // include the built tree-sitter native libraries alongside the jars in the staged/packaged
  // distribution (same dir as the jars, so update.sh's `cp lib/*` and macapp/create.sh both pick
  // them up naturally; see grammar.Sitter.loadNative for how they're found at runtime)
  Universal / mappings ++= buildTreeSitterGrammars.value.map(f =>
    fileConverter.value.toVirtualFile(f.toPath) -> s"lib/${f.getName}")
)
