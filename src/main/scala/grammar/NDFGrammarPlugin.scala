//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import moped._
import moped.code.CodeConfig

@Plugin(tag="textmate-grammar")
class NDFGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

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
