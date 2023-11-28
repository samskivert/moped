//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.{CodeConfig, Commenter, BlockIndenter}
import moped.grammar._
import moped.util.Paragrapher

@Plugin
class SwiftGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.swift" -> "grammar/Swift.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("variable.other.namespace", moduleStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.other.field-id", preprocessorStyle),

    effacer("meta.type-name", typeStyle),
    effacer("storage.type", keywordStyle),
    effacer("definition.type", typeStyle),
    effacer("inherited-class", typeStyle),
    // storage.type.field: leaving white for now
    effacer("variable.parameter", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.triple", Syntax.HereDocLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}

@Major(name="swift",
       tags=Array("code", "project", "swift"),
       pats=Array(".*\\.swift"),
       ints=Array("swift"),
       desc="A major editing mode for Swift source files.")
class SwiftMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._
  import Syntax.{HereDocLiteral => HD}

  override def langScope = "source.swift"

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
