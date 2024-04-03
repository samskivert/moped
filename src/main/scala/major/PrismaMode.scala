//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.{CodeConfig, Commenter, BlockIndenter}
import moped.grammar._
import moped.util.Paragrapher

@Major(name="prisma",
       tags=Array("code", "project", "prisma"),
       pats=Array(".*\\.prisma"),
       ints=Array("prisma"),
       desc="A major editing mode for Prisma schema definition files.")
class PrismaMode (env :Env) extends SitterCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._
  import Syntax.{HereDocLiteral => HD}
  import Selector._

  override def langId = ch.usi.si.seart.treesitter.Language.PRISMA

  override def styles = Map(
    "comment" -> always(commentStyle),
    "developer_comment" -> always(commentStyle),
    "string" -> always(stringStyle),

    "model" -> always(keywordStyle),
    "generator" -> always(keywordStyle),
    "datasource" -> always(keywordStyle),

    "enum" -> always(keywordStyle),
    "enumeral" -> always(constantStyle),

    "@" -> always(preprocessorStyle),
    "@@" -> always(preprocessorStyle),
    "false" -> always(constantStyle),
    "true" -> always(constantStyle),
    "number" -> always(constantStyle),

    "variable" -> always(variableStyle),

    "identifier" -> identifierStyles,
    "property_identifier" -> identifierStyles,
  )

  private def identifierStyles (scopes :List[String]) = scopes.head match {
    case "model_declaration" => typeStyle
    case "column_type" => typeStyle
    case "call_expression" => scopes.tail.headOption match {
      case Some("block_attribute_declaration") => preprocessorStyle
      case Some("attribute") => preprocessorStyle
      case _ => functionStyle
    }
    case "member_expression" => scopes.tail.headOption match {
      case Some("attribute") => preprocessorStyle
      case _ => variableStyle
    }
    case "attribute" => preprocessorStyle
    case _ => variableStyle
  }

  override def syntaxes = Map(
    "developer_comment" -> (_ => Syntax.LineComment),
    "comment" -> (_ => Syntax.LineComment),

    "string" -> (_ => Syntax.StringLiteral),
    "false" -> (_ => Syntax.OtherLiteral),
    "true" -> (_ => Syntax.OtherLiteral),
    "number" -> (_ => Syntax.OtherLiteral),
  )

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  import BlockIndenter._
  override protected def createIndenter () = new BlockIndenter(config, Seq(
    new LambdaBlockRule(" in"),
    new AlignUnderDotRule()
  ))

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
