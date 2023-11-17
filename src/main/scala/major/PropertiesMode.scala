//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.Commenter
import moped.grammar._

@Major(name="properties",
       tags=Array("code", "project", "properties"),
       pats=Array(".*\\.properties", "package.moped", "module.moped"),
       desc="A major mode for editing Java properties files.")
class PropertiesMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.java-props"

  override val commenter = new Commenter {
    override def docOpen = "##"
    override def docPrefix = "##"
    // TODO: ! is also a comment start character, sigh...
    override def linePrefix = "#"
  }
}

@Plugin
class PropertiesGrammarPlugin extends GrammarPlugin {
  import code.CodeConfig._

  override def grammars = Map("source.java-props" -> "grammar/JavaProperties.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.doc", docStyle),
    effacer("keyword", keywordStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.doc", Syntax.DocComment)
  )
}
