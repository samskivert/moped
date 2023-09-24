//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.grammar._
import moped.code.{CodeConfig, Commenter}
import java.nio.file.Path

@Plugin class HtmlGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("text.html.basic" -> "grammar/HTML.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.tag", functionStyle),
    effacer("entity.other", variableStyle),

    effacer("variable.language.documentroot", preprocessorStyle),
    effacer("variable.language.entity", typeStyle)
  )
}

@Major(name="html",
       tags=Array("code", "project", "html"),
       pats=Array(".*\\.html", ".*\\.shtml"),
       desc="A major mode for editing HTML files.")
class HtmlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "text.html.basic"

  override val commenter = new Commenter()
  override def createIndenter () = new XmlIndenter(config)
}
