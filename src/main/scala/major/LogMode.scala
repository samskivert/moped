//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

@Major(name="log", tags=Array("log"), desc="""
  A major mode for displaying log text. Motion commands are available but editing commands are not.
""")
class LogMode (env :Env) extends ReadingMode(env) {

  // mark our buffer as uneditable
  buffer.editable = false

  // start the point at the bottom of the buffer
  view.point() = buffer.end

  override def keymap = super.keymap.
    bind("clear-log", "M-k");

  @Fn("Clears the contents of this log buffer.")
  def clearLog () :Unit = {
    buffer.delete(buffer.start, buffer.end)
  }
}
