//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.vcs

import java.nio.file.{Path, Files}
import moped._
import moped.util.{Errors, Process}

/** A plugin that handles VCS integration. */
abstract class VCSPlugin extends AbstractPlugin {

  /** The string which identifies this VCS. For example: `git`, `hg`, `svn`. */
  def id :String

  /** Returns true if this VCS plugin applies to the project at `root`. */
  def applies (root :Path) :Boolean

  /** Returns the revision for the commit that last touched `line` of `path`, or `None`. */
  def blame (path :Path, line :Int) :Future[Option[String]]

  /** Obtains the commit message for the specified `revision`.
    * If no message can be obtained an error message can be returned, or the empty seq. */
  def commitMessage (root :Path, revision :String) :Future[Seq[String]]

  /** Obtains the commit message and diff for the specified `revision`.
    * If no diff can be obtained an error message can be returned, or the empty seq. */
  def commitDiff (root :Path, revision :String) :Future[Seq[String]]
}

object NOOPPlugin extends VCSPlugin {
  override def id = "none"
  override def applies (root :Path) = false
  override def blame (path :Path, line :Int) =
    Future.success(None)
  override def commitMessage (root :Path, revision :String) =
    Future.success(Seq("No known VCS in this project."))
  override def commitDiff (root :Path, revision :String) =
    Future.success(Seq("No known VCS in this project."))
}

@Plugin class GitPlugin (editor :Editor) extends VCSPlugin {

  override def id = "git"

  override def applies (root :Path) = {
    // TODO: this is kind of hacky, how many levels up should we check?
    Files.exists(root.resolve(".git")) || Files.exists(root.getParent.resolve(".git")) ||
    Files.exists(root.getParent.getParent.resolve(".git"))
  }

  override def blame (path :Path, line :Int) = {
    val cmd = Seq("git", "blame", "-L", s"$line,$line", path.getFileName.toString)
    Process.exec(editor.exec, cmd, path.getParent).map(res => {
      if (res.exitCode == 0) Some(res.stdout(0).split(" ")(0))
      else throw Errors.feedback(fail(res, cmd).mkString("\n"))
    })
  }

  override def commitMessage (root :Path, revision :String) = {
    val cmd = Seq("git", "show", "-s", revision)
    Process.exec(editor.exec, cmd, root).map(res => {
      if (res.exitCode == 0) res.stdout
      else fail(res, cmd)
    })
  }

  override def commitDiff (root :Path, revision :String) = {
    val cmd = Seq("git", "show", revision)
    Process.exec(editor.exec, cmd, root, 10000).map(res => {
      if (res.exitCode == 0) res.stdout
      else fail(res, cmd)
    })
  }

  private def fail (res :Process.Result, cmd :Seq[String]) = {
    val sb = Seq.newBuilder[String]
    sb += s"git failed (${res.exitCode}):"
    sb += cmd.mkString("[", " ", "]")
    sb ++= res.stdout
    sb ++= res.stderr
    sb.result
  }
}
