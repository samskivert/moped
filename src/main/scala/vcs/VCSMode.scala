//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.vcs

import moped._
import moped.project._
import moped.util.Errors

@Minor(name="vcs", tags=Array("project"), stateTypes=Array(classOf[Project]),
       desc="""Provides VCS fns.""")
class VCSMode (env :Env) extends MinorMode(env) {

  val project = Project(buffer)
  val root = project.root.path
  lazy val vcs = env.msvc.resolvePlugins(classOf[VCSPlugin]).
    find(_.applies(root)) getOrElse NOOPPlugin

  override def keymap = super.keymap.
    bind("vcs-blame-commit", "C-c C-b C-c").
    bind("vcs-blame-diff", "C-c C-b C-d");

  //
  // Fns

  @Fn("Displays the commit message that last touched the line at the point.")
  def vcsBlameCommit () =
    blameRevAt(view.point()).flatMap(rev => vcs.commitMessage(root, rev)).
      onSuccess(lines => view.popup() = Popup.text(lines, Popup.UpRight(view.point())))

  @Fn("Displays the commit message that last touched the line at the point.")
  def vcsBlameDiff () =
    blameRevAt(view.point()).flatMap(rev => {
      vcs.commitDiff(root, rev).map(diffs => {
        val state = project.bufferState("diff")
        val diffbuff = wspace.createBuffer(Store.scratch(s"*${vcs.id}: $rev", buffer.store), state)
        diffbuff.append(diffs map Line.apply)
        diffbuff.split(diffbuff.end) // put a NL at the end
        window.focus.visit(diffbuff)
      })
    })

  private def blameRevAt (loc :Loc) :Future[String] = buffer.store.file match {
    case None => throw Errors.feedback(s"Buffer not visiting file? ${buffer.store}")
    case Some(path) => vcs.blame(path, loc.row+1).map(_ match {
      case None => throw Errors.feedback(
        s"Unable to get '${vcs.id}' blame info for ${path.getFileName}:${loc.row+1}")
      case Some(rev) => rev
    })
  }
}
