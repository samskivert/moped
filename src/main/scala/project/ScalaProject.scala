//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import moped._
import java.nio.file.{Path, Files}

object ScalaPlugins {

  @Plugin class SbtRootPlugin extends RootPlugin.File("build.sbt")

  @Plugin class ScalaLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("scala")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(".bloop"))
    def createClient (proj :Project) = Future.success(
      new ScalaLangClient(proj, serverCmd(proj.root.path)))
  }

  private def serverCmd (root :Path) :Seq[String] = {
    Seq("metals", "-Dmetals.http=true", "-DisHttpEnabled=true")
  }
}

class ScalaLangClient (proj :Project, serverCmd :Seq[String])
    extends LangClient(proj, serverCmd, None) {

  override def name = "Scala"
}
