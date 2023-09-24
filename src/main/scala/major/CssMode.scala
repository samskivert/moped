//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.grammar._
import moped.code.{CodeConfig, Commenter, BlockIndenter}
import java.nio.file.Path

@Plugin class CssGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.css" -> "grammar/CSS.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name", typeStyle),
    effacer("entity.other.attribute-name", typeStyle),
    effacer("support.type.property-name", variableStyle),
    effacer("support.function", functionStyle),
    effacer("support.constant.font-name", stringStyle),
    effacer("support.constant", constantStyle)
  )
}

@Major(name="css",
       tags=Array("code", "project", "css"),
       pats=Array(".*\\.css", ".*\\.scss", ".*\\.less"),
       desc="A major mode for editing CSS files.")
class CssMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now
  override def langScope = "source.css"
  override val commenter = new Commenter()
  override protected def createIndenter () = new BlockIndenter(config, Seq(
    new BlockIndenter.BlockCommentRule()
  ))
}
