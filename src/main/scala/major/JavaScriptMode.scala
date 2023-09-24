//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code._
import moped.Matcher
import moped.grammar._
import moped.util.Paragrapher

@Plugin
class JavaScriptGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.javascript" -> "grammar/JavaScript.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    // effacer("meta.definition.method.javascript", functionStyle),
    effacer("meta.method.javascript", functionStyle),

    effacer("storage.modifier.import", moduleStyle),
    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.annotation", preprocessorStyle),
    effacer("storage.type.other", typeStyle),
    effacer("storage.type.def", keywordStyle),
    effacer("storage.type", keywordStyle),
    effacer("support.type", typeStyle),

    // effacer("variable.import", typeStyle),
    // effacer("variable.language", constantStyle),
    effacer("variable", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )
}

@Major(name="javascript",
       tags=Array("code", "project", "javascript"),
       pats=Array(".*\\.js", ".*\\.json"),
       ints=Array("javascript"),
       desc="A major editing mode for the JavaScript language.")
class JavaScriptMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._

  override def langScope = "source.javascript"

  override protected def createIndenter () = new BlockIndenter(config, Seq(
    // bump extends/implements in two indentation levels
    BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(extends|implements)\\b"), 2),
    // align changed method calls under their dot
    new BlockIndenter.AlignUnderDotRule(),
    // handle javadoc and block comments
    new BlockIndenter.BlockCommentRule(),
    // handle indenting switch statements properly
    new BlockIndenter.SwitchRule(),
    // handle continued statements, with some special sauce for : after case
    new BlockIndenter.CLikeContStmtRule()
  ));

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen = "/*"
    override def blockPrefix = "*"
    override def blockClose = "*/"
    override def docOpen   = "/**"
  }

  // TODO: more things!
}
