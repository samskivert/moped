//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import collection.JavaConverters._
import collection.mutable.ArrayBuffer

import com.google.common.collect.HashMultimap
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.util.HashMap
import java.util.regex.Pattern

import moped._
import moped.major._
import moped.minor._
// import moped.pacman._
import moped.util.BufferBuilder

/** Extends the base package manager with extra info needed by Moped. */
class PackageManager (log :Logger) extends AbstractService /* with PackageService */ {

  // /** A signal emitted when a package module is installed. */
  // val moduleAdded = Signal[ModuleMeta]()

  // /** A signal emitted when a package module is uninstalled. */
  // val moduleRemoved = Signal[ModuleMeta]()

  // private val pkgRepo = Pacman.repo

  /** The top-level metadata directory. */
  val metaDir :Path = locateMetaDir

  // /** Returns info on all known modules. */
  // def modules :Iterable[ModuleMeta] = metas.values

  /** Resolves the class for the mode named `name`. */
  def mode (major :Boolean, name :String) :Option[Class[_]] = Option(modeMap(major).get(name))

  /** Resolves the implementation class for the service with fq classname `name`. */
  def service (name :String) :Option[Class[_]] = Option(serviceMap.get(name))

  /** Returns the name of all modes provided by all packages. */
  def modes (major :Boolean) :Iterable[String] = modeMap(major).keySet.asScala

  /** Detects the major mode that should be used to edit a buffer named `name` and with `line0` as
    * its first line of text. */
  def detectMode (name :String, line0 :String) :String = {
    // checks for -*- mode: somemode -*- on the first or second line
    def fileLocal :Option[String] = None // TODO
    // if the file starts with #!, detects based on "interpreter"
    def interp :Option[String] = line0 match {
      case text if (text startsWith "#!") =>
        // break #!/usr/bin/perl -w into tokens, filtering out known meaningless tokens
        val tokens = text.substring(2).split("[ /]").filterNot(skipToks)
        tokens.map(i => (i, interps.get(i))) collectFirst {
          case (interp, ms) if (!ms.isEmpty) =>
            if (ms.size > 1) log.log("Multiple modes registered for interpreter '$interp': $ms")
            ms.iterator.next
        }
      case _ => None
    }
    // matches the file name against all registered mode regular expressions
    def pattern (name :String) :Option[String] = {
      val ms = patterns collect { case (p, m) if (p.matcher(name).matches()) => m }
      if (ms.size > 1) log.log(s"Multiple modes match buffer name '$name': $ms")
      ms.headOption
    }
    // println(s"Detecting mode for ${name}")
    fileLocal orElse interp orElse pattern(name) getOrElse "text"
  }
  private val skipToks = Set("", "usr", "local", "bin", "env", "opt")

  /** Returns the set of minor modes that should be auto-activated for `tags`. */
  def tagMinorModes (tags :Seq[String]) :Set[String] =
    tags.flatMap(tts => minorTags.get(tts).asScala).toSet

  override def didStartup () :Unit = {} // not used
  override def willShutdown () :Unit = {} // not used

  // override def installDir (source :String) = metas.get(Source.parse(source)).mod.root

  // override def classpath (source :String) =
  //   metas.get(Source.parse(source)).mod.depends(pkgRepo.resolver).classpath.toSeq

  // override def describePackages (bb :BufferBuilder) :Unit = {
  //   val modmetas = modules.filter(_.mod.name != "test").toSeq.sortBy(_.mod.toString)

  //   bb.addHeader("Packages")
  //   bb.addKeysValues("Root: " -> metaDir.toString,
  //                    "Modules: " -> modmetas.size.toString)

  //   val ignoreModuleJar = Props.ignoreModuleJar
  //   for (meta <- modmetas) {
  //     bb.addSubHeader(meta.mod.toString)
  //     val codeDir = metaDir.relativize(meta.loader.mod.classpath(ignoreModuleJar))
  //     bb.addKeysValues(Seq("Code: "        -> s"%root%/$codeDir",
  //                          "Majors: "      -> fmt(meta.majors),
  //                          "Minors: "      -> fmt(meta.minors),
  //                          "Services: "    -> fmt(meta.services),
  //                          "Auto-load: "   -> fmt(meta.autoSvcs),
  //                          "Plugins: "     -> fmt(meta.plugins.asMap.entrySet),
  //                          "Patterns: "    -> fmt(meta.patterns.asMap.entrySet),
  //                          "Interps: "     -> fmt(meta.interps.asMap.entrySet),
  //                          "Minor tags: "  -> fmt(meta.minorTags.asMap.entrySet)
  //                          ).filter(_._2 != ""))
  //   }
  // }

  // private def fmt (iter :JIterable[_]) :String = iter.asScala.mkString(", ")
  private def fmt (iter :scala.Iterable[_]) :String = (iter.map {
    case (k, v) => s"$k=$v"
    case v      => v
  }).mkString(", ")

  // private def moduleAdded (mod :Module) :Unit = {
  //   // create a package metadata ; there's some special hackery to handle the fact that services
  //   // are defined in moped-api and implemented in moped-editor, which is not normally allowed
  //   val meta = if (mod.source != MopedAPI) new ModuleMeta(log, pkgRepo, mod)
  //              else new ModuleMeta(log, pkgRepo, mod) {
  //                override def service (name :String) =
  //                  metas.get(MopedEditor).loadClass(services(name))
  //              }
  //   metas.put(mod.source, meta)

  //   // map this package's major and minor modes, services and plugins
  //   meta.majors.keySet foreach { majorMap.put(_, meta.major _) }
  //   meta.minors.keySet foreach { minorMap.put(_, meta.minor _) }
  //   meta.services.keySet foreach { serviceMap.put(_, meta.service _) }
  //   // map the file patterns and interpreters defined by this package's major modes
  //   meta.patterns.asMap.toMapV foreach { (m, ps) => ps foreach { p =>
  //     try patterns += (Pattern.compile(p) -> m)
  //     catch {
  //       case e :Exception => log.log(s"Mode $m specified invalid pattern: $p: $e")
  //     }
  //   }}
  //   meta.interps.asMap.toMapV foreach { (m, is) => is foreach { i => interps.put(i, m) }}
  //   // map the tags defined by this pattern's minor modes
  //   minorTags.putAll(meta.minorTags)
  //   // tell any interested parties about this new package module
  //   PackageManager.this.moduleAdded.emit(meta)
  // }

  // private def moduleRemoved (mod :Module) :Unit = {
  //   // TODO
  // }

  private def locateMetaDir :Path = {
    // if our metadir has been overridden, use the specified value
    val mopedHome = System.getenv("SCALED_HOME")
    if (mopedHome != null) return Paths.get(mopedHome)

    val homeDir = Paths.get(System.getProperty("user.home"))
    // if we're on a Mac, put things in ~/Library/Application Support/Moped
    val appSup = homeDir.resolve("Library").resolve("Application Support")
    if (Files.exists(appSup)) return appSup.resolve("Moped")
    // if we're on (newish) Windows, use AppData/Local
    val apploc = homeDir.resolve("AppData").resolve("Local")
    if (Files.exists(apploc)) return apploc.resolve("Moped")
    // otherwise use ~/.moped (where ~ is user.home)
    else return homeDir.resolve(".moped")
  }

  // private val metas = new HashMap[Source,ModuleMeta]()

  private val serviceMap = new HashMap[String,Class[_]]()
  private val majorMap = new HashMap[String,Class[_]]()
  private val minorMap = new HashMap[String,Class[_]]()
  private def modeMap (major :Boolean) = if (major) majorMap else minorMap

  private val patterns   = ArrayBuffer[(Pattern,String)]()
  private val interps    = HashMultimap.create[String,String]()
  private val minorTags  = HashMultimap.create[String,String]()

  private def registerService (clazz :Class[_]) :Unit = {
    var meta = clazz.getAnnotation(classOf[Service])
    if (meta == null) throw new Exception("Missing @Service annotation " + clazz)
    var impl = clazz.getClassLoader.loadClass("moped." + meta.impl)
    serviceMap.put(clazz.getName, impl)
  }

  registerService(classOf[WatchService])
  registerService(classOf[ConfigService])
  registerService(classOf[WorkspaceService])

  private def registerMajor (clazz :Class[_]) :Unit = {
    var meta = clazz.getAnnotation(classOf[Major])
    if (meta == null) throw new Exception("Missing @Major annotation " + clazz)
    majorMap.put(meta.name, clazz)
    meta.pats foreach { p =>
      try patterns += (Pattern.compile(p) -> meta.name)
      catch {
        case e :Exception => log.log(s"Mode ${meta.name} specified invalid pattern: $p: $e")
      }
    }
    meta.ints foreach { interps.put(_, meta.name) }
  }

  registerMajor(classOf[TextMode])
  registerMajor(classOf[MiniReadMode[_]])
  registerMajor(classOf[MiniReadOptMode])
  registerMajor(classOf[MiniYesNoMode])
  registerMajor(classOf[ISearchMode])
  registerMajor(classOf[HelpMode])
  registerMajor(classOf[LogMode])

  private def registerMinor (clazz :Class[_]) :Unit = {
    var meta = clazz.getAnnotation(classOf[Minor])
    if (meta == null) throw new Exception("Missing @Minor annotation " + clazz)
    minorMap.put(meta.name, clazz)
    meta.tags foreach { minorTags.put(_, meta.name) }
  }

  registerMinor(classOf[MetaMode])
  registerMinor(classOf[WhitespaceMode])
  registerMinor(classOf[SubProcessMode])
  registerMinor(classOf[WorkspaceMode])

  // private val MopedAPI = Source.parse("git:https://github.com/moped/moped.git#api")
  // private val MopedEditor = Source.parse("git:https://github.com/moped/moped.git#editor")

  // // reroute Pacman logging to *messages*
  // Log.target = new Log.Target() {
  //   val slog = PackageManager.this.log
  //   override def log (msg :String) = slog.log(msg)
  //   override def log (msg :String, exn :Throwable) = slog.log(msg, exn)
  // }
  // // wire up our observer
  // pkgRepo.observer = new PackageRepo.Observer() {
  //   def packageAdded (pkg :Package) :Unit = pkg.modules.foreach(moduleAdded)
  //   def packageRemoved (pkg :Package) :Unit = pkg.modules.foreach(moduleRemoved)
  // }
  // pkgRepo.packages foreach pkgRepo.observer.packageAdded
}
