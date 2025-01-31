//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import moped._
import moped.code.CodeMode
import moped.util.Errors

/** Extends [[code.CodeMode]] with support for using TextMate grammars for code highlighting. Code
  * major modes which intend to use TextMate grammars for code highlighting and other purposes
  * should extend this class rather than [[code.CodeMode]].
  */
abstract class GrammarCodeMode (env :Env) extends CodeMode(env) {

  /** The TextMate scope of the language grammar for this mode. */
  def langScope :String

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = env.msvc.service[GrammarService].scoper(buffer, langScope, mkProcs).
    getOrElse(throw Errors.feedback(s"No grammar available for '$langScope'")).
    connect(buffer, disp.didInvoke)

  override def keymap = super.keymap.
    bind("show-scopes", "M-A-p") // TODO: also M-PI?

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showScopes () :Unit = {
    val ss = scoper.scopesAt(view.point())
    val text = if (ss.isEmpty) List("No scopes.") else ss
    view.popup() = Popup.text(text, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () :Unit = scoper.rethinkBuffer()

  @Fn("Forces reload of our language grammar and reloads this buffer (to force a restyle).")
  def reloadGrammar () :Unit = {
    env.msvc.service[GrammarService].resetGrammar(langScope)
    frame.revisitFile()
  }

  /** When a line changes and the TextMate grammars are reapplied to compute new syntax
    * highlighting, we must remove all of the old styles prior to restyling the line. However, not
    * all styles applied to the line come from syntax highlighting. This function must indicate
    * whether the supplied style was a result of syntax highlighting or not.
    *
    * The approach used by `GrammarCodeMode` is to prefix all syntax highlighting styles with
    * `code` to distinguish them from non-syntax highlighting styles. If you add additional styles,
    * you should either follow this `code` prefix (i.e. `codeFunction`) or override this method to
    * additionally return true for your styles (by perhaps keeping all your styles in a set and
    * checking the style for inclusion in that set).
    */
  protected def isModeStyle (style :String) = style `startsWith` "code"

  private def mkProcs (plugin :GrammarPlugin) = {
    val procs = List.newBuilder[Selector.Processor]
    if (!plugin.effacers.isEmpty) procs += new Selector.Processor(plugin.effacers) {
      override def onBeforeLine (buf :Buffer, row :Int) :Unit = { // clear any code styles
        val start = buf.lineStart(row) ; val end = buf.lineEnd(row)
        if (start != end) buf.removeTags(classOf[String], isModeStyle, start, end)
      }
    }
    if (!plugin.syntaxers.isEmpty) procs += new Selector.Processor(plugin.syntaxers) {
      override protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) :Unit = {
        buf.setSyntax(Syntax.Default, start, end) // reset syntax
      }
    }
    procs.result()
  }
}
