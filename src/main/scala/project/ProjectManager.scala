//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import moped._

/** Configuration for [[ProjectService]]. */
object ProjectServiceConfig extends Config.Defs {

  @Var("""The order of preference for project types. Used when multiple project types match
          a given project on the file system.""")
  val prefTypes = key(Seq("moped", "maven"))

  @Var("A list of (absolute) paths to ignore when scanning for projects.")
  val ignoredPaths = key(Seq[String]())
}

class ProjectManager (metaSvc :MetaService, editor :Editor)
    extends AbstractService with ProjectService {
  import ProjectServiceConfig._
  import Project._

  private val userHome = Paths.get(System.getProperty("user.home"))

  private def log = metaSvc.log
  private val config = metaSvc.service[ConfigService].
    resolveServiceConfig("project", ProjectServiceConfig :: Nil)

  private lazy val _rootPlugins = metaSvc.resolvePlugins(classOf[RootPlugin])
  // a special root plugin for our config file directory
  private val configRoot = new RootPlugin {
    val configRoot = metaSvc.metaFile("Config")
    def checkRoot (root :Path) = if (root == configRoot) 1 else -1
  }
  private def rootPlugins = _rootPlugins :+ configRoot

  override def pathsFor (store :Store) :Option[List[Path]] = store match {
    case FileStore(path)       => Some(parents(path.getParent))
    case ZipEntryStore(zip, _) => Some(List(zip))
    case _                     => None
  }

  override def findRoots (paths :List[Path]) :Seq[Project.Root] = {
    val ignorePaths = config(ignoredPaths).map(p => Paths.get(p.trim)).toSet
    val viablePaths = filterDegenerate(paths)
    rootPlugins.flatMap(_(viablePaths)).filter(r => !ignorePaths(r.path)).toSeq
  }

  override def resolveByPaths (paths :List[Path]) :Root = {
    val ignorePaths = config(ignoredPaths).map(p => Paths.get(p.trim)).toSet
    val viablePaths = filterDegenerate(paths)
    rootPlugins.flatMap(_(viablePaths)).filter(r => !ignorePaths(r.path)) match {
      case Seq() =>
        log.log(s"Unable to find project root, falling back to ${paths.head}")
        Root(paths.head)
      case Seq(root) => root
      case roots =>
        def weight (r :Project.Root) = r.path.getNameCount + (if (r.module == "") 0 else 1)
        val byDepth = roots.sortBy(-weight(_))
        log.log(s"Using deepest of multiple project roots: $byDepth")
        byDepth.head
    }
  }

  override def resolveById (id :Id) :Option[Root] = id match {
    case RootId(path, module) => Some(Root(path, module))
    case _ => {
        val iter = rootPlugins.iterator
        while (iter.hasNext) {
          val root = iter.next.apply(id)
          if (root.isDefined) return root
        }
        None
      }
  }

  override def unknownProject (ps :ProjectSpace) = new Project(ps, Root(Paths.get(""), "")) {
    override def isIncidental = true
  }

  override def didStartup () :Unit = {
    // create a project space whenever a new workspace is opened (it will register itself in
    // workspace state and clear itself out when the workspace closes)
    editor.workspaceOpened.onValue { ws => new ProjectSpace(ws, metaSvc) }
  }

  override def willShutdown () :Unit = {}

  // filters out "degenerate" project seeds resolved by paths (i.e. the user's home directory, the
  // root of the file system)
  private def filterDegenerate (paths :List[Path]) :List[Path] =
    paths.filterNot { p => userHome.startsWith(p) || p.getParent == null}

  @tailrec private def parents (file :Path, accum :List[Path] = Nil) :List[Path] =
    file.getParent match {
      // don't add the file system root to the path; surely there's no way that's a project root
      case null => accum.reverse
      case prnt => parents(prnt, file :: accum)
    }
}
