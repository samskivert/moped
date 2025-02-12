//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.minor

import moped._
import moped.util.BufferBuilder

@Minor(name="workspace", tags=Array("*"),
       desc="""A minor mode that provides workspace-related functionality.""")
class WorkspaceMode (env :Env) extends MinorMode(env) {

  override def keymap = super.keymap.
    bind("describe-workspace", "C-h w");

  @Fn("Creates a new workspace.")
  def createWorkspace () :Unit = {
    window.mini.read(s"Name:", "", nameHistory, Completer.none) `onSuccess`(wsvc.create)
  }

  @Fn("Opens an existing workspace.")
  def openWorkspace () :Unit = {
    val comp = Completer.from(wsvc.list)
    window.mini.read(s"Name:", "", nameHistory, comp) `onSuccess`(wsvc.open)
  }

  @Fn("Opens a new window in the current workspace.")
  def openWindow () :Unit = {
    var Geometry(width, height, x, y) = window.geometry
    var geom = Geometry(width, height, x + window.size.width + 10, y)
    wspace.openWindow(Some(geom)).focus.visit(buffer)
  }

  @Fn("Describes the state of the current workspace.")
  def describeWorkspace () :Unit = {
    val bb = new BufferBuilder(view.width()-1)
    wspace.describeSelf(bb)

    val hstore = Store.scratch(s"*workspace:${wspace.name}*", buffer.store)
    val hbuf = wspace.createBuffer(hstore, reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  @Fn("Opens the config file for the workspace's info window specifications in a buffer.")
  def editWindowConfig () :Unit = wspace.visitWindowConfig(window)

  /** The history ring for workspace names. */
  protected def nameHistory = wspace.historyRing("workspace-name")

  private val wsvc = env.msvc.service[WorkspaceService]
}
