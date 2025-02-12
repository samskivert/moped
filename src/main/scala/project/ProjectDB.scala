//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import moped._

/** Maintains metadata for all projects known to a project space. */
class ProjectDB (exec :Executor, wsroot :Path, log :Logger) {
  import Project._

  /** A file that contains info on all of our projects. */
  private val configFile = wsroot.resolve("Config").resolve("projects.conf")

  /** The directory in which metadata is stored for projects and depends. */
  val psdir = Files.createDirectories(wsroot.resolve("Projects"))

  /** All known projects mapped by id. */
  val byId = new ConcurrentHashMap[Id,Root]()

  /** Metadata for a named project. */
  case class Info (root :Root, name :String, ids :Set[Id]) {
    val rootName = (root, name)
    def map () :Unit = ids foreach { id => byId.put(id, root) }
    def unmap () :Unit = ids foreach { id => byId.remove(id) }
  }

  /** Current metadata for all known projects. */
  val toInfo = new ConcurrentHashMap[Root,Info]()

  /*ctor*/ {
    // load metadata from our config file
    if (Files.exists(configFile)) {
      val infos = ConfigFile.read(configFile).map(readInfo)
      val (good, bad) = infos.partition(info => Files.exists(info.root.path))
      for (info <- good) toInfo.put(info.root, info)
      if (!bad.isEmpty) {
        for (info <- bad) log.log(
          s"Project no longer exists in root '${info.name}': ${info.root.path}")
        writeConfig()
      }
    }
    // if we have no config file, potentially migrate from the old per-project info style
    else {
      val vtab = new StringBuilder().append(11.toChar).toString
      val codec = new CodecImpl(vtab, vtab)
      // read info.txt for all known projects and map them by root
      Files.list(psdir).collect(Collectors.toList[Path]).forEach { pdir =>
        if (Files.isDirectory(pdir)) try {
          val lines = List() ++ Files.readAllLines(pdir.resolve("info.txt")).asScala
          val root = codec.readRoot(lines.head)
          val info = Info(root, pdir.getFileName.toString, lines.tail.flatMap(codec.readId).toSet)
          toInfo.put(root, info)
        } catch {
          case e :Throwable => log.log(s"Failed to resolve info for $pdir: $e")
        }
      }
      writeConfig()
    }
    toInfo.values forEach { _.map() } // map all the infos we read
  }

  /** Returns the directory into which `proj` should store its metadata. */
  def metaDir (proj :Project) :Path = psdir.resolve(proj.root.hashName)

  /** Adds `proj` to this database.
    * @return true if added, false if project was already added. */
  def add (proj :Project) :Boolean = if (toInfo.containsKey(proj.root)) false else {
    // if this project's name is already in use by another project, tack -N onto it
    val names = toInfo.values.asScala.map(_.name).filter(_ `startsWith` proj.name).toSet
    val name = if (!names(proj.name)) proj.name else {
      var ext = 1
      while (names(proj.name + s"-$ext")) ext += 1
      proj.name + s"-$ext"
    }

    // map the project and write out our updated metadata db
    val info = Info(proj.root, name, proj.ids)
    toInfo.put(proj.root, info)
    info.map()
    writeConfig()

    true
  }

  /** Removes `proj` from this database.
    * @return false if project was not in database, true if it was removed. */
  def remove (proj :Project) :Boolean = toInfo.remove(proj.root) match {
    case null => false
    case info =>
      info.unmap()
      // write our project metadata db
      writeConfig()
      true
  }

  /** Checks that the metadata for `proj` is up to date; updating it if needed. */
  def checkInfo (proj :Project) :Unit = toInfo.get(proj.root) match {
    case null => // it's not a named project, nothing to update
    case oinfo =>
      val newids = proj.ids
      if (oinfo.ids != newids) {
        val ninfo = oinfo.copy(ids = newids)
        log.log(s"Updating ids for $proj: $ninfo")
        toInfo.put(proj.root, ninfo)
        oinfo.unmap()
        ninfo.map()
        writeConfig()
      }
      // TODO: name change?
  }

  private def readInfo (lines :Seq[String]) :Info =
    Info(Codec.readRoot(lines(0)), lines(1), lines.drop(2).flatMap(Codec.readId).toSet)
  private def showInfo (info :Info) :Seq[String] =
    Seq(Codec.showRoot(info.root), info.name) ++ info.ids.map(Codec.showId)

  private def writeConfig () :Unit = exec.runInBG {
    ConfigFile.write(configFile, toInfo.values.asScala.toSeq.sortBy(_.name).map(showInfo))
  }
}
