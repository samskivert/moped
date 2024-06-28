//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.Indenter
import moped.grammar._
import moped.code.{CodeConfig, Commenter}

@Major(name="python",
       tags=Array("code", "project", "python"),
       pats=Array(".*\\.py"),
       ints=Array("python", "python3"),
       desc="A major mode for editing python scripts.")
class PythonMode (env :Env) extends SitterCodeMode(env) {
  import CodeConfig._

  override def dispose () :Unit = {} // nada for now

  override def langId = ch.usi.si.seart.treesitter.Language.PYTHON

  override def styles = Map(
    "comment" -> always(commentStyle),

    "integer" -> always(constantStyle),
    "string_content" -> always(stringStyle),
    "string_start" -> always(keywordStyle),
    "string_end" -> always(keywordStyle),
    "true" -> always(constantStyle),
    "false" -> always(constantStyle),
    "none" -> always(keywordStyle),

    "import" -> always(keywordStyle),
    "from" -> always(keywordStyle),
    "with" -> always(keywordStyle),

    "def" -> always(keywordStyle),
    "lambda" -> always(keywordStyle),
    "class" -> always(keywordStyle),

    "not" -> always(keywordStyle),
    "in" -> always(keywordStyle),
    "not in" -> always(keywordStyle),
    "or" -> always(keywordStyle),
    "and" -> always(keywordStyle),
    "as" -> always(keywordStyle),

    "try" -> always(keywordStyle),
    "raise" -> always(keywordStyle),
    "except" -> always(keywordStyle),

    "if" -> always(keywordStyle),
    "for" -> always(keywordStyle),
    "while" -> always(keywordStyle),
    "continue" -> always(keywordStyle),
    "return" -> always(keywordStyle),
    "pass" -> always(keywordStyle),

    "==" -> always(keywordStyle),
    "//" -> always(keywordStyle),
    ":" -> always(keywordStyle),

    "identifier" -> (_ match {
      case List("dotted_name", "import_from_statement", _*) => moduleStyle
      case List("dotted_name", "import_statement", _*) => moduleStyle
      case List("class_definition", _*) => typeStyle
      case List("call", _*) => functionStyle
      case List("attribute", "call", _*) => functionStyle
      case List("function_definition", _*) => functionStyle
      case List("parameters", _*) => variableStyle
      case List("assignment", _*) => variableStyle
      case List("attribute", "assignment", _*) => variableStyle
      case List("attribute", "subscript", "assignment", _*) => variableStyle
      case scopes => null
    })
  )

  override def syntaxes = Map(
    "comment" -> (_ => Syntax.LineComment),
    // syntaxer("comment.block", Syntax.DocComment),

    "integer" -> (_ => Syntax.OtherLiteral),
    "string_content" -> (_ => Syntax.StringLiteral),
    "raw_str_end_part" -> (_ => Syntax.HereDocLiteral),
  )

  override val commenter = new Commenter() {
    override def linePrefix  = "#"
    override def blockOpen   = "#"
    override def blockPrefix = "#"
  }
}
