//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.util.{List => JList}
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j._

import moped._
import moped.major.ReadingMode

object LangDiagnosticsConfig extends Config.Defs {

  /** The CSS style applied to file paths. */
  val pathStyle = LangFindUsesConfig.pathStyle

  case class Context (name :String, diags :Map[String, JList[Diagnostic]])
}

@Major(name="lang-diagnostics", tags=Array("project"),
       desc="""A major mode that displays all diagnostics known for a project, including
               diagnostics for files that aren't currently open in any buffer.""")
class LangDiagnosticsMode (env :Env, ctx :LangDiagnosticsConfig.Context) extends ReadingMode(env) {
  import LangDiagnosticsConfig._
  import Lang._

  override def stylesheets = stylesheetURL("/lang.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("visit-diagnostic", "ENTER")

  private val noDiag = Visit.Tag(new Visit() {
    protected def go (window :Window) = window.popStatus("No diagnostic on the current line.")
  })

  @Fn("Visits the diagnostic on the current line.")
  def visitDiagnostic () :Unit = buffer.line(view.point()).lineTag(noDiag)(window)

  // preserved across mode reconstruction on an already-populated buffer (see below), so that
  // visit-next/visit-prev keep working even if this mode instance gets rebuilt without our buffer
  // being regenerated
  private var visitList :Visit.List = null

  if (buffer.start == buffer.end) showDiagnostics()
  else if (visitList != null) window.visits() = visitList

  private def toSeverity (sev :DiagnosticSeverity) = sev match {
    case DiagnosticSeverity.Hint => Severity.Hint
    case DiagnosticSeverity.Information => Severity.Info
    case DiagnosticSeverity.Warning => Severity.Warning
    case DiagnosticSeverity.Error => Severity.Error
  }

  // the message often continues for many more lines (e.g. a multi-line type mismatch
  // explanation); show just the first line here and rely on visiting the diagnostic (which pops
  // up the full message, see Note.go) for the rest
  private def firstLine (msg :String) = {
    val nl = msg.indexOf('\n')
    if (nl < 0) msg else msg.substring(0, nl) + " ..."
  }

  private def showDiagnostics () :Unit = {
    val lines = Seq.newBuilder[Line]
    val visits = Seq.newBuilder[Visit]

    for ((uri, diags) <- ctx.diags.toSeq.sortBy(_._1) if !diags.isEmpty) {
      lines += Line.builder(uri).withStyle(pathStyle, 0, uri.length).build()
      val store = LSP.toStore(uri)
      for (diag <- diags.asScala) {
        val start = diag.getRange.getStart
        val sev = Option(diag.getSeverity).map(toSeverity) getOrElse Severity.Error
        val note = Note(
          store, Region(LSP.fromPos(start), LSP.fromPos(diag.getRange.getEnd)),
          diag.getMessage, sev)
        val text = s"  ${start.getLine+1}:${start.getCharacter+1}: ${firstLine(diag.getMessage)}"
        lines += Line.builder(text).
          withStyle(ProjectConfig.noteStyle(note), 0, text.length).
          withLineTag(Visit.Tag(note)).
          build()
        visits += note
      }
    }

    if (lines.result().isEmpty) lines += Line("No diagnostics.")
    buffer.append(lines.result())
    buffer.split(buffer.end)

    visitList = new Visit.List("diagnostic", visits.result())
    window.visits() = visitList
    view.point() = Loc.Zero
  }
}
