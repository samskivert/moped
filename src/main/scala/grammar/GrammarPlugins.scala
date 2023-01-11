//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import moped._
import moped.code.CodeConfig._

class NDFGrammarInfo extends GrammarPlugin {

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
