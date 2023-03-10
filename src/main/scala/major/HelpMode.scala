//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

@Major(name="help", tags=Array("help"), desc="""
  A major mode for displaying help text. Motion commands are available, editing commands are not.
""")
class HelpMode (env :Env) extends ReadOnlyTextMode(env) {

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
