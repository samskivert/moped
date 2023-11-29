//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

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

@Plugin
class NDFGrammarPlugin extends GrammarPlugin {
  import code.CodeConfig._

  override def grammars = Map("source.ndf" -> "grammar/NDF.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("punctuation.line-cont", typeStyle),
    effacer("keyword", keywordStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment)
  )
}
