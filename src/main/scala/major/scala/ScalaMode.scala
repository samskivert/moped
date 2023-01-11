//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major.scala

// import codex.model.Kind
import moped._
import moped.code._
import moped.grammar.{GrammarCodeMode, GrammarPlugin}
import moped.project._
import moped.util.{Chars, Paragrapher}

@Plugin class ScalaGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.scala" -> "grammar/Scala.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.primitive", typeStyle),

    effacer("variable.package", moduleStyle),
    effacer("variable.import", typeStyle),
    effacer("variable.language", constantStyle),
    // effacer("variable.parameter", variableStyle), // leave params white
    effacer("variable.other.type", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.triple", Syntax.HereDocLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala", ".*\\.sbt"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._
  import Syntax.{HereDocLiteral => HD}

  override def langScope = "source.scala"

  override def keymap = super.keymap.
    bind("import-type", "C-c C-i");

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter () = ScalaIndenter.create(config)

  override protected def canAutoFill (p :Loc) :Boolean =
    super.canAutoFill(p) || (buffer.syntaxNear(p) == HD)

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen   = "/*"
    override def blockPrefix = "*"
    override def blockClose  = "*/"
    override def docOpen     = "/**"

    override def mkParagrapher (syn :Syntax, buf :Buffer) = new DocCommentParagrapher(syn, buf)

    // the scala grammar marks all whitespace leading up to the open doc in comment style, so we
    // have to hack this predicate a bit
    override def inDocComment (buffer :BufferV, p :Loc) :Boolean = {
      super.inDocComment(buffer, p) && {
        val line = buffer.line(p)
        (line.indexOf(docOpenM, p.col) == -1)
      }
    }
  }

  @Fn("Queries for a type (completed by the analyzer) and adds an import for it.")
  def importType () :Unit = {
    val client = LangClient(buffer)
    window.mini.read("Type:", wordAt(view.point()), wspace.historyRing("lang-type"),
                     Lang.symbolCompleter(client, Some(Lang.Kind.Type))).onSuccess(sym => {
      // ScalaCode.insertImport(buffer, sym.fqName)
    });
  }

  /** Returns the "word" at the specified location in the buffer. */
  private def wordAt (loc :Loc) = buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
