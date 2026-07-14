//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.{Path, Files}
import com.google.gson.{Gson, JsonElement}
import org.eclipse.lsp4j.{ExecuteCommandParams, Location}
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

import moped._

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

  private val gson = new Gson()

  // Metals protocol extension (no LSP-standard equivalent): rather than returning a location as
  // the result of workspace/executeCommand, commands like goto-super-method (used by
  // LangMode.visitSuper) resolve asynchronously and then push the navigation back at us via this
  // notification instead (see "metals-goto-location" in Metals' ClientCommands.scala); any other
  // client command we don't implement is ignored
  @JsonNotification("metals/executeClientCommand")
  def executeClientCommand (params :ExecuteCommandParams) :Unit = params.getCommand match {
    case "metals-goto-location" =>
      val loc = gson.fromJson(params.getArguments.get(0).asInstanceOf[JsonElement], classOf[Location])
      // this notification arrives on lsp4j's JSON-RPC message-processing thread, not the FX
      // application thread, so any buffer/view mutation (which visit() does) must be redispatched
      exec.runOnUI {
        proj.pspace.wspace.windows.headOption.foreach(w => visit(proj, loc).apply(w))
      }
    case _ => // ignore other client commands we don't support
  }

  // Metals protocol extension: pings the client with status-bar-style text (build/compile
  // progress, indexing, doctor warnings, etc.); we don't have a persistent status bar slot to
  // dedicate to it, so we just route non-blank text through the same transient status channel
  // `messages` already feeds (see ProjectSpace.langClientFor, which wires `messages` to
  // `project.emitStatus`), ignoring the show/hide/tooltip/command bookkeeping that a real status
  // bar item would need
  @JsonNotification("metals/status")
  def metalsStatus (params :MetalsStatusParams) :Unit = {
    val hidden = params.hide != null && params.hide.booleanValue
    if (!hidden && params.text != null && !params.text.isBlank) exec.runOnUI {
      messages.emit(params.text)
    }
  }
}

// see MetalsLanguageClient.metalsStatus in Metals' own sources for the full parameter shape; we
// only bother modeling the fields metalsStatus (above) actually reads
class MetalsStatusParams (
  val text :String = null,
  val show :java.lang.Boolean = null,
  val hide :java.lang.Boolean = null,
)
