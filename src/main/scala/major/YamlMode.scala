//
// Moped YAML Mode - a Moped major mode for editing YAML files
// http://github.com/moped/yaml-mode/blob/master/LICENSE

package moped.yaml

import moped._
import moped.code.Indenter
import moped.grammar._
import moped.code.{CodeConfig, Commenter}

@Major(name="yaml",
       tags=Array("code", "project", "yaml"),
       pats=Array(".*\\.yaml", ".*\\.yml"),
       desc="A major mode for editing YAML files.")
class YamlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.yaml"

  // override def createIndenter() = new YamlIndenter(config)

  override val commenter = new Commenter() {
    override def linePrefix  = "#"
    override def blockOpen   = "#"
    override def blockPrefix = "#"
  }
}

@Plugin class YamlGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.yaml" -> "grammar/YAML.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.single", Syntax.StringLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}
