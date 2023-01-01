//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

import moped._
import moped.util.Close
import moped.pacman.Config

object Generic {

  // TODO: unify all this into a single sectioned config file which we parse and cache and then
  // conditionally enable components based on the existence of appropriate sections; maybe even use
  // JSON, meh
  val LangFile = ".langserver"
  val MetaFile = ".moped-project"

  class LangConfig (val config :Config) {
    val suffs = configSL("suff")
    val serverCmd = config.resolve("serverCmd", Config.StringP)
    val serverArgs = config.resolve("serverArg", Config.StringListP)
    config.finish()
    private def configS (name :String) = config.resolve(name, Config.StringP)
    private def configSL (name :String) = config.resolve(name, Config.StringListP)
  }

  class MetaConfig (config :Config) {
    val name = configS("name")
    val sourceDirs = configSL("sourceDir")
    val ignoreNames = configSL("ignoreName")
    val ignoreRegexes = configSL("ignoreRegex")
    config.finish()
    private def configS (name :String) = config.resolve(name, Config.StringP)
    private def configSL (name :String) = config.resolve(name, Config.StringListP)
  }

  def readConfig (path :Path) = new Config(Files.readAllLines(path))
  def readConfig (root :Path, name :String) :Config = readConfig(root.resolve(name))
  def readLangConfig (root :Path) :LangConfig = {
    val config = new LangConfig(readConfig(root, LangFile))
    if (config.suffs.isEmpty) println(s"No suffs in $LangFile at $root!")
    config
  }
}

@Plugin class GenericLangPlugin extends LangPlugin {
  import Generic._
  override def suffs (root :Project.Root) = readLangConfig(root.path).suffs.asScala.toSet
  override def canActivate (root :Project.Root) = Files.exists(root.path.resolve(LangFile))
  override def createClient (proj :Project) = {
    val config = readLangConfig(proj.root.path)
    val serverCmd = Seq(config.serverCmd) ++ config.serverArgs.asScala
    Future.success(new LangClient(proj.metaSvc, proj.root.path, serverCmd) {
      def name = "Generic"
    })
  }
}

@Plugin class GenericRootPlugin extends RootPlugin.File(Generic.MetaFile)

@Plugin class GenericResolverPlugin extends ResolverPlugin {
  import Generic._

  override def metaFiles (root :Project.Root) = Seq(root.path.resolve(MetaFile))

  def addComponents (project :Project) :Unit = {
    val rootPath = project.root.path
    val metaFile = rootPath.resolve(MetaFile)
    val config = new MetaConfig(readConfig(metaFile))

    var sb = Ignorer.stockIgnores
    config.ignoreNames.forEach { sb :+= Ignorer.ignoreName(_) }
    config.ignoreRegexes.forEach { sb :+= Ignorer.ignoreRegex(_) }
    project.addComponent(classOf[Filer], new DirectoryFiler(project, sb))

    // val sourceDirs = config.sourceDirs.map(rootPath.resolve(_)).toSeq
    // project.addComponent(classOf[Sources], new Sources(sourceDirs))

    val oldMeta = project.metaV()
    project.metaV() = oldMeta.copy(name = config.name)
  }
}
