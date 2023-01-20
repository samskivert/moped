//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.vcs

import moped._
import moped.code.{CodeConfig, Commenter}
import moped.grammar._

@Plugin
class DiffGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.diff" -> "grammar/Diff.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),

    // TODO: hacky, define real styles
    effacer("markup.inserted", stringStyle),
    effacer("markup.changed", constantStyle),
    effacer("markup.deleted", typeStyle),
    effacer("meta.diff.header", docStyle),
    effacer("meta.diff.index", constantStyle),
    effacer("meta.diff.commit", variableStyle),
    effacer("punctuation.definition.to-file", stringStyle),
    effacer("markup.changed", constantStyle),
    effacer("punctuation.definition.from-file", typeStyle)
  )
}

@Major(name="diff", pats=Array(".*\\.diff", ".*\\.patch"), desc="Displays diff output.")
class DiffMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.diff"

  override val commenter = new Commenter() {
    override def linePrefix  = "#"
  }
}
