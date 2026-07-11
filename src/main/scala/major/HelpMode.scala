//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

@Major(name="help", tags=Array("help"), desc="""
  A major mode for displaying help text. Motion commands are available, editing commands are not.
""")
class HelpMode (env :Env) extends ReadOnlyTextMode(env) {

  // help buffers sometimes embed syntax-highlighted code and formatted doc comments (e.g. the
  // "describe element" doc viewer), which need code.css/lang.css in addition to the text.css
  // that ReadOnlyTextMode already provides. text.css must come *after* code.css/lang.css here:
  // DispatcherImpl.addSheets attaches stylesheets in the reverse of this list's order, and
  // codeDocFace (code.css, the doc-comment baseline) and textListFace (text.css, markdown inline
  // code spans) both set -fx-fill on the same overlapping text, so whichever stylesheet ends up
  // attached last (i.e. listed first here) wins; we want text.css's more specific per-construct
  // colors to win over the generic doc-comment baseline, not the other way around.
  override def stylesheets =
    super.stylesheets ::: List(stylesheetURL("/code.css"), stylesheetURL("/lang.css"))

  override def keymap = super.keymap.
    bind("visit", "ENTER");
  // TODO: other things?

  private val noopVisit = Visit.Tag(new Visit() {
    override protected def go (window :Window) :Unit = {}
  })

  @Fn("Visits the target of the current line, if any.")
  def visit () :Unit = {
    buffer.line(view.point()).lineTag(noopVisit)(window)
  }
}
