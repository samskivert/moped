//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import moped._
import moped.code.{Commenter, Indenter}

@Major(name="ndf",
       tags=Array("code", "project", "ndf"),
       pats=Array(".*\\.ndf"),
       desc="A major mode for editing Nested Dictionary Format (NDF) files.")
class NDFMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.ndf"

  // HACK: leave indent as-is
  override def computeIndent (row :Int) :Int = Indenter.readIndent(buffer, Loc(row, 0))

  override val commenter = new Commenter() {
    override def linePrefix = "#"
  }
}
