//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import moped._
import java.nio.file.{Path, Files}

object SwiftPlugins {

  val PackageFile = "Package.swift"

  @Plugin class SwiftRootPlugin extends RootPlugin.File(PackageFile)

  // /** Extract projects metadata from `package.json` and `tsconfig.json` files. */
  // @Plugin class TSConfigResolverPlugin extends ResolverPlugin {

  //   override def metaFiles (root :Project.Root) = Seq(root.path.resolve(TSConfigFile))

  //   def addComponents (project :Project) :Unit = {
  //     val rootPath = project.root.path
  //     // we trigger on tsconfig.json (which is how we know it's a Swift project) but we
  //     // extract metadata from package.json
  //     val pkgFile = rootPath.resolve(PackageFile)
  //     val config = Json.parse(Files.newBufferedReader(pkgFile)).asObject

  //     val mod = project.root.module
  //     val modPath = if (mod == "") project.root.path else project.root.path.resolve(mod)
  //     val baseName = Option(config.get("name")).map(_.asString).
  //       getOrElse(rootPath.getFileName.toString)
  //     val projName = if (mod == "") baseName else s"${baseName}-${mod}"

  //     val ignores = Seq.newBuilder[Ignorer]
  //     ignores ++= Ignorer.stockIgnores
  //     ignores += Ignorer.ignorePath(project.root.path.resolve("node_modules"), project.root.path)
  //     Option(config.get("ignore")).map(_.asArray).foreach { igs =>
  //       // TODO: handle glob ignores properly
  //       igs.asScala.map(_.asString).foreach { ignores += Ignorer.ignoreName(_) }
  //     }
  //     project.addComponent(classOf[Filer], new DirectoryFiler(project, ignores.result))

  //     // TODO: package.json doesn't define source directories, so we hack some stuff
  //     // val sourceDirs = Seq("src", "test").map(modPath.resolve(_))
  //     // project.addComponent(classOf[Sources], new Sources(sourceDirs))

  //     val oldMeta = project.metaV()
  //     project.metaV() = oldMeta.copy(name = projName)
  //   }
  // }

  @Plugin class SwiftLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("swift")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(PackageFile))
    def createClient (proj :Project) = Future.success(
      new SwiftLangClient(proj, serverCmd(proj.root.path)))
  }

  private def serverCmd (root :Path) :Seq[String] = {
    Seq("xcrun", "--toolchain", "swift", "sourcekit-lsp")
  }
}

class SwiftLangClient (proj :Project, serverCmd :Seq[String])
    extends LangClient(proj, serverCmd, None) {

  override def name = "Swift"
}
