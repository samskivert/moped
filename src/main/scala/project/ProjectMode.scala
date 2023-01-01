//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import moped._
import moped.util.{BufferBuilder, Chars, Errors}

/** Provides configuration for [[ProjectMode]]. */
object ProjectConfig extends Config.Defs {
  import Intel._

  /** Provides the CSS style for `note`. */
  def noteStyle (note :Intel.Note) = note.sev match {
    case Severity.Hint    => "noteHintFace"
    case Severity.Info    => "noteInfoFace"
    case Severity.Warning => "noteWarningFace"
    case Severity.Error   => "noteErrorFace"
  }

  def isNoteStyle (style :String) = style startsWith "note"
}

/** A minor mode which provides fns for interacting with project files and services.
  *
  * Some stock key bindings are also redirected toward project-centric versions, for example
  * `C-x C-f` is rerouted to `find-file-in-project`. Where possible, the original fns are exposed
  * via slightly varied key bindings.
  *
  * Any major mode that includes the `project` tag will trigger the activation of this minor mode.
  */
@Minor(name="project", tags=Array("project"), stateTypes=Array(classOf[Project]),
       desc="""A minor mode that provides project-centric fns.""")
class ProjectMode (env :Env) extends MinorMode(env) {
  val project = Project(buffer)
  import project.pspace
  import ProjectConfig._

  override def configDefs = ProjectConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/project.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("describe-project",  "C-h p").

    // file fns
    bind("find-file-in-project",      "C-x C-p").
    bind("find-file-other-project",   "C-x C-o").

    // intel fns
    bind("describe-element", "C-c C-d").
    bind("visit-element",    "M-.").
    bind("visit-symbol",     "C-c C-v").
    bind("visit-type",       "C-c C-k").
    bind("visit-func",       "C-c C-j").
    bind("visit-value",      "C-c C-h").
    bind("rename-element",   "C-c C-r").

    // warning navigation fns
    // bind("visit-next-warning", "C-S-]").
    // bind("visit-prev-warning", "C-S-[").

    // test fns
    // bind("run-all-tests",     "C-c C-t C-a").
    // bind("run-file-tests",    "C-c C-t C-f").
    // bind("run-test-at-point", "C-c C-t C-p").
    // bind("repeat-last-test",  "C-c C-t C-r", "F6").
    // bind("visit-tests",       "C-x C-t").

    // execution fns
    bind("workspace-execute",       "C-c C-e").
    bind("workspace-execute-again", "C-c C-a");

  //
  // Behaviors

  /** Finds a file in `proj` and visits it. */
  private def findFileIn (proj :Project) :Unit = {
    window.mini.read(
      s"Find file in project (${proj.name}):", "", proj.fileHistory, proj.files.completer
    ) map(wspace.openBuffer) onSuccess(frame.visit(_))
  }

  private def bufferNotes = project.notes(buffer.store)

  // provides a custom Visit.List that cycles through the notes in the current buffer and only
  // advances to the next buffer if we have no notes in this buffer
  private def notesVisitList (notes :Seq[Intel.Note]) :Visit.List =
    new Visit.List("buffer note", notes) {
      override def next (win :Window) :Unit = if (isEmpty) skip(win,  1) else super.next(win)
      override def prev (win :Window) :Unit = if (isEmpty) skip(win, -1) else super.prev(win)
      private def skip (win :Window, delta :Int) = {
        val noteStores = project.noteStores
        if (noteStores.isEmpty) if (delta > 0) super.next(win) else super.prev(win)
        else {
          val storeIdx = noteStores.indexOf(buffer.store)
          val skipIdx = (storeIdx + delta + noteStores.length) % noteStores.length
          val skipList = notesVisitList(project.notes(noteStores(skipIdx))())
          if (delta > 0) skipList.next(win) else skipList.prev(win)
        }
      }
    }

  private def updateVisits (onCreate :Boolean)(list :Visit.List) :Unit = {
    val curlist = window.visits()
    // we only want to update the visit list on buffer creation if we're not currently visiting
    // something else or if we're currently visiting the same kind of thing
    if (!onCreate || curlist.isEmpty || curlist.thing == list.thing)
      window.visits() = list
  }

  // display the project status in the modeline
  note(env.mline.addDatum(project.status.map(_._1), project.status.map(s => s._2)))

  // forward project feedback to our window
  note(project.feedback.onValue(_ fold((window.emitStatus _).tupled, window.emitError)))

  // when new analysis notes are generated, stuff them into the visit list
  note(bufferNotes.onValue(notes => {
    // clear all note styles from the buffer and readd to the current set; this is not very
    // efficient but tracking the old notes through all possible buffer edits is rather a PITA
    buffer.removeTags(classOf[String], isNoteStyle, buffer.start, buffer.end)
    for (n <- notes) buffer.addStyle(noteStyle(n), buffer.clamp(n.region))
    updateVisits(false)(notesVisitList(notes));
  }))

  // when first visiting this buffer, maybe visit analysis notes
  if (!bufferNotes().isEmpty) updateVisits(true)(notesVisitList(bufferNotes()))

  //
  // General FNs

  @Fn("Reads a project file name from the minibuffer (with smart completion), and visits it.")
  def findFileInProject () :Unit = findFileIn(project)

  @Fn("""Reads a project name from the minibuffer, then reads a file from that project (with smart
         completion), and visits it.""")
  def findFileOtherProject () :Unit = {
    val pcomp = Completer.from(pspace.allProjects)(_._2)
    window.mini.read(s"Project:", "", projectHistory, pcomp) onSuccess { case pt =>
      findFileIn(pspace.projectFor(pt._1))
    }
  }

  //
  // Intel FNs

  @Fn("Describes the element at the point.")
  def describeElement () :Unit = Intel(buffer).describeElement(view)

  @Fn("Navigates to the referent of the element at the point.")
  def visitElement () :Unit = {
    val loc = view.point()
    Intel(buffer).visitElement(view, window).onSuccess { visited =>
      if (visited) window.visitStack.push(buffer, loc)
    }
  }

  @Fn("Queries for a project-wide symbol and visits it.")
  def visitSymbol () :Unit = visitSymbol(None)
  @Fn("Queries for a project-wide type symbol and visits it.")
  def visitType () :Unit = visitSymbol(Some(Intel.Kind.Type))
  @Fn("Queries for a project-wide function symbol and visits it.")
  def visitFunc () :Unit = visitSymbol(Some(Intel.Kind.Func))
  @Fn("Queries for a project-wide value (field, property, variable) symbol and visits it.")
  def visitValue () :Unit = visitSymbol(Some(Intel.Kind.Value))

  private def visitSymbol (kind :Option[Intel.Kind]) = {
    val intel = Intel(buffer)
    window.mini.read("Type:", wordAt(view.point()), symbolHistory,
                     intel.symbolCompleter(kind)).onSuccess(sym => {
      window.visitStack.push(view) // push current loc to the visit stack
      intel.visitSymbol(sym, window)
    })
  }

  @Fn("Renames all occurrences of the element at the point.")
  def renameElement () :Unit = {
    val loc = view.point()
    val intel = Intel(buffer)
    window.mini.read("New name:", wordAt(loc), renameHistory, Completer.none).
      flatMap(name => intel.renameElementAt(view, loc, name)).
      onSuccess(renamers => {
        println(s"Renames $renamers")
        if (renamers.isEmpty) abort(
          "No renames returned for refactor. Is there an element at the point?")

        def doit (save :Boolean) = try {
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.validate(buffer)
          }
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.apply(buffer)
            if (save) buffer.save()
          }
        } catch {
          case err :Throwable => window.exec.handleError(err)
        }

        // if there are occurrences outside the current buffer, confirm the rename
        if (renamers.size == 1 && renamers(0).store == view.buffer.store) doit(false)
        else window.mini.readYN(
          s"'Element occurs in ${renamers.size-1} source file(s) not including this one. " +
            "Undoing the rename will not be trivial, continue?").onSuccess { yes =>
          if (yes) doit(true)
        }
      }).
      onFailure(window.exec.handleError)
  }

  @Fn("Restarts the language server client for the active project.")
  def restartLangClient () :Unit = {
    project.emitStatus("Restaring langserver client...")
    project.lang.restartClient(buffer);
  }

  //
  // Execute FNs

  @Fn("Invokes a particular execution in this workspace.")
  def workspaceExecute () :Unit = {
    val exns = pspace.execs.executions
    if (exns.isEmpty) window.popStatus(s"${pspace.name} defines no executions.")
    else window.mini.read(s"Execute:", "", pspace.execHistory,
                          Completer.from(exns)(_.name)) onSuccess execute
  }

  @Fn("""Reinvokes the last invoked execution.""")
  def workspaceExecuteAgain () :Unit = {
    wspace.state[Execution].getOption match {
      case Some(e) => execute(e)
      case None    => window.popStatus("No execution has been invoked yet.")
    }
  }

  @Fn("Visits the workspace's execution configuration file.")
  def workspaceEditExecutions () :Unit = {
    pspace.execs.visitConfig(window)
  }

  //
  // Meta FNs

  @Fn("Describes the current project.")
  def describeProject () :Unit = {
    project.visitDescription(window)
  }

  @Fn("Shows all possible project roots for the file in the current buffer.")
  def showAllProjectRoots () :Unit = {
    val file = bufferFile
    val buf = project.createBuffer(s"*roots:${file}*", "help")
    val bb = new BufferBuilder(window.focus.geometry.width-1)

    val plugset = env.msvc.resolvePlugins(classOf[RootPlugin])
    bb.addHeader("Root Plugins")
    for (plug <- plugset) bb.add(plug.toString)

    val psvc = env.msvc.service[ProjectService]
    val paths = psvc.pathsFor(buffer.store).get // bufferFile will have aborted for non-file buffers
    bb.addHeader("Paths")
    for (path <- paths) bb.add(path.toString)
    bb.addHeader("Roots")
    for (root <- psvc.findRoots(paths)) bb.add(root.path.toString)
    window.focus.visit(bb.applyTo(buf))
  }

  @Fn("Adds the current project to the current workspace.")
  def addToWorkspace () :Unit = {
    pspace.addProject(project)
    window.popStatus(s"'${project.name}' added to '${pspace.name}' workspace.")
  }

  @Fn("Removes the current project from the current workspace.")
  def removeFromWorkspace () :Unit = {
    pspace.removeProject(project)
    window.popStatus(s"'${project.name}' removed from '${pspace.name}' workspace.")
  }

  @Fn("Removes a project from the current workspace.")
  def removeProject () :Unit = {
    val comp = Completer.from(pspace.allProjects)(_._2)
    window.mini.read(s"Project:", "", projectHistory, comp) onSuccess(info => {
      val (root, name) = info
      pspace.removeProject(pspace.projectFor(root))
      window.popStatus(s"Removed '$name' from '${pspace.name}' workspace.")
    })
  }

  //
  // Implementation details

  private def projectHistory = wspace.historyRing("project-name")
  private def symbolHistory = wspace.historyRing("project-symbol")
  private def renameHistory = wspace.historyRing("project-rename")

  private def bufferFile :Path = buffer.store.file getOrElse { abort(
      "This buffer has no associated file. A file is needed to detect tests.") }

  private def execute (exec :Execution) :Unit = {
    pspace.execs.execute(exec, project)
    // track our last execution in the workspace state
    wspace.state[Execution]() = exec
  }

  private def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
