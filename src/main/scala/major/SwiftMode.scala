//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.{CodeConfig, Commenter, BlockIndenter}
import moped.grammar._
import moped.util.Paragrapher

@Major(name="swift",
       tags=Array("code", "project", "swift"),
       pats=Array(".*\\.swift"),
       ints=Array("swift"),
       desc="A major editing mode for Swift source files.")
class SwiftMode (env :Env) extends SitterCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._
  import Syntax.{HereDocLiteral => HD}
  import Selector._

  override def langId = ch.usi.si.seart.treesitter.Language.SWIFT

  override def styles = Map(
    "comment" -> always(commentStyle),

    "integer_literal" -> always(constantStyle),
    "line_string_literal" -> always(stringStyle),
    "raw_str_end_part" -> always(stringStyle),

    "import" -> always(keywordStyle),
    "typealias" -> always(keywordStyle),
    "struct" -> always(keywordStyle),
    "class" -> always(keywordStyle),
    "protocol" -> always(keywordStyle),
    "enum" -> always(keywordStyle),

    "mutating" -> always(keywordStyle),
    "static" -> always(keywordStyle),
    "private" -> always(keywordStyle),

    "func" -> always(keywordStyle),
    "init" -> always(keywordStyle),
    "inout" -> always(keywordStyle),
    "throws" -> always(keywordStyle),
    "in" -> always(keywordStyle),

    "for" -> always(keywordStyle),
    "while" -> always(keywordStyle),
    "if" -> always(keywordStyle),
    "else" -> always(keywordStyle),
    "guard" -> always(keywordStyle),
    "return" -> always(keywordStyle),
    "switch" -> always(keywordStyle),
    "case" -> always(keywordStyle),
    "default_keyword" -> always(keywordStyle),
    "continue" -> always(keywordStyle),
    "break" -> always(keywordStyle),

    "let" -> always(keywordStyle),
    "var" -> always(keywordStyle),
    "self" -> always(keywordStyle),
    "nil" -> always(keywordStyle),
    "try" -> always(keywordStyle),
    "throw_keyword" -> always(keywordStyle),

    // "&&" -> always(keywordStyle),
    // "||" -> always(keywordStyle),
    // "??" -> always(keywordStyle),
    // "==" -> always(keywordStyle),
    // "@" -> always(keywordStyle),

    "type_identifier" -> always(typeStyle),
    "simple_identifier" -> (scopes => scopes.head match {
      case "function_declaration" => functionStyle
      case "call_expression" => functionStyle
      case "navigation_suffix" => functionStyle
      case "array_literal" => typeStyle
      case "enum_entry" => typeStyle
      case _ => variableStyle
    })
  )

  override def syntaxes = Map(
    "comment" -> (_ => Syntax.LineComment),
    // syntaxer("comment.block", Syntax.DocComment),

    "integer_literal" -> (_ => Syntax.OtherLiteral),
    "line_string_literal" -> (_ => Syntax.StringLiteral),
    "raw_str_end_part" -> (_ => Syntax.HereDocLiteral),
  )

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter () = new BlockIndenter(config, Seq())

  override protected def canAutoFill (p :Loc) :Boolean =
    super.canAutoFill(p) || (buffer.syntaxNear(p) == HD)

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockPrefix = "*"
    override def blockOpen   = "/*"
    override def blockClose  = "*/"
    override def docOpen     = "/**"
  }

  // TODO: more things!
}
