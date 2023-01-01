//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import moped._
import moped.major.ReadingMode

@Major(name="codex-info", tags=Array("project"),
       desc="""A major mode for displaying Codex information.""")
class CodexReadingMode (env :Env) extends ReadingMode(env) {

  // we use the code mode styles even though we're not a code mode
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
}
