//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.sh

import moped._
import moped.code.Indenter
import moped.grammar._
import moped.code.{CodeConfig, Commenter}

@Plugin class ShGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.shell" -> "grammar/sh.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),
    effacer("variable", stringStyle),
    effacer("storage", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )
}

@Major(name="sh",
       tags=Array("code", "project", "sh"),
       pats=Array(".*\\.sh", ".*\\.csh", ".*\\.fish", ".*\\.bash",
                  "\\.profile", "\\.bash(rc|_profile|_login|_logout)", "\\.zshrc"),
       ints=Array("bash", "sh", "csh", "ksh", "tcsh", "zsh"),
       desc="A major mode for editing shell scripts.")
class ShMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.shell"

  // override def createIndenter() = new XmlIndenter(buffer, config)
  override val commenter = new Commenter() {
    override def linePrefix  = "#"
    override def blockOpen   = "#"
    override def blockPrefix = "#"
  }
}
