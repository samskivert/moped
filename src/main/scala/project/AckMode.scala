//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import moped._
import moped.util.Chars

/** Provides configuration for [[AckMode]]. */
object AckConfig extends Config.Defs {

  @Var("Options to pass to `ack`.")
  val ackOpts = key("--heading --smart-case")

  // encapsulates ack arguments
  sealed trait Scope
  case object PScope extends Scope
  case object WScope extends Scope
  case class Opts (term :String, opts :Seq[String], scope :Scope)
}

@Minor(name="ack", tags=Array("project"), stateTypes=Array(classOf[Project]),
       desc="""A minor mode that provides Ack searching of Moped projects.""")
class AckMode (env :Env) extends MinorMode(env) {
  import AckConfig._
  import Chars._

  val project = Project(buffer)

  override def configDefs = AckConfig :: super.configDefs
  override def keymap = super.keymap.
    bind("ack-in-project",   "C-c C-g").
    bind("ack-in-workspace", "S-C-c S-C-g");

  @Fn("Requests a query string and invokes `ack` on all project files therewith.")
  def ackInProject () :Unit = ackInScope("Search project for:", PScope)

  @Fn("Requests a query string and invokes `ack` on all workspace files therewith.")
  def ackInWorkspace () :Unit = ackInScope("Search workspace for:", WScope)

  private def searchHistory = wspace.historyRing("ack-search")

  private def ackInScope (prompt :String, scope :Scope) :Unit = {
    window.mini.read(prompt, wordAt(buffer, view.point()), searchHistory,
                     Completer.none) onSuccess { term => if (term.length > 0) {
      val opts = Opts(term, Seq.from(config(ackOpts).split(" ")), scope)
      // if we have project scope, set the results buffer up as a project buffer
      val state = scope match {
        case PScope => project.bufferState("ack-results", opts)
        case WScope => State.inits(Mode.Hint("ack-results", opts))
      }
      window.focus.visit(wspace.createBuffer(Store.scratch(s"*ack: $term*", buffer.store), state))
    }}
  }
}
