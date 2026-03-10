//
// Moped Groovy Mode - a Moped major mode for editing Groovy code
// http://github.com/moped/scala-mode/blob/master/LICENSE

package moped.groovy

import moped._
import moped.code.{Commenter, BlockIndenter, CodeConfig}
import moped.grammar.{GrammarCodeMode, GrammarPlugin}

@Major(name="groovy",
       tags=Array("code", "project", "groovy"),
       pats=Array(".*\\.groovy", ".*\\.gradle", "Jenkinsfile.*"),
       ints=Array("groovy"),
       desc="A major editing mode for the Groovy language.")
class GroovyMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.groovy"

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
  ))

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen = "/*"
    override def blockPrefix = "*"
    override def blockClose = "*/"
    override def docOpen   = "/**"
  }

  // TODO: more things!
}

@Plugin class GroovyGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.groovy" -> "grammar/Groovy.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.name.type.class", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    // effacer("meta.definition.method.groovy", functionStyle),
    effacer("meta.method.groovy", functionStyle),

    effacer("storage.modifier.import", moduleStyle),
    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.annotation", preprocessorStyle),
    effacer("storage.type.def", keywordStyle),
    effacer("storage.type", typeStyle),

    effacer("variable.import", typeStyle),
    effacer("variable.language", constantStyle),
    effacer("variable.parameter", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line",  Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant",      Syntax.OtherLiteral),
    syntaxer("string.quoted", Syntax.StringLiteral)
  )
}
