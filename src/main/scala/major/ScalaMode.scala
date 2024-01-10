//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code._
import moped.grammar.{GrammarCodeMode, GrammarPlugin}
import moped.project._
import moped.util.{Chars, Paragrapher}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala", ".*\\.sbt", ".*\\.flix"),
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

object ScalaIndenter {
  import Indenter._
  import BlockIndenter._

  def create (cfg :Config) :Indenter = new BlockIndenter(cfg, Seq(
    // bump extends/with in two indentation levels
    adjustIndentWhenMatchStart(Matcher.regexp("""(extends|with)\b"""), 2),
    new CaseRule(),
    new SingleLineBlockRule(),
    new BlockCommentRule(),
    new LambdaBlockRule(" =>"),
    new AlignUnderDotRule()
  ))

  class CaseRule extends Rule {
    override def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) = {
      // if we're in a faux case block...
      if (!state.isInstanceOf[CaseS]) base
      // ignore the block indent for subsequent case statements
      else if (info.startsWith(caseArrowM)) base - indentWidth
      // ignore the block indent for the final close bracket
      else if (info.firstChar == '}') base - 2*indentWidth
      // otherwise stick to business as usual...
      else base
    }

    override def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = {
      // if this line opens a match case which does not contain any code after the arrow, create a
      // faux block to indent the case body
      if (line.matches(caseArrowM, first) && line.charAt(last) == '>') {
        // if we're currently in the case block for the preceding case, pop it first
        new CaseS(start.popIf(_.isInstanceOf[CaseS]))
      }
      // otherwise leave the start as is
      else start
    }

    private val caseArrowM = Matcher.regexp("""case\s.*=>""")
  }

  class CaseS (next :State) extends State(next) {
    override def show = "CaseS"
  }

  class SingleLineBlockRule extends ContinuedStatementRule(".+-=") {
    private var opensSLB = false
    private var slbExprOpen = -1
    private var slbExprClose = -1
    private var slbExpectsPair = false
    private val singleLineBlockM = Matcher.regexp("""(if|else if|else|while)\b""")

    override def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = {
      // reset our SLB tracking state
      slbExprOpen = -1 ; slbExprClose = -1 ; slbExpectsPair = false
      // if we're looking at an SLB, push a state for it
      opensSLB = line.matches(singleLineBlockM, first)
      if (!opensSLB) super.adjustStart(line, first, last, start)
      else {
        val token = singleLineBlockM.group(1)
        val nstate = new SingleBlockS(token, first, start)
        // if this SLB has no associated expression (else or a do); set the expression open/close
        // column to the end of the token so that the "pop on later block" code works properly
        if (nstate.lacksExpr) {
          slbExprOpen = first+token.length
          slbExprClose = slbExprOpen
        }
        // if this is an 'if' or 'else if', or a 'do', we want to know whether or not to expect to
        // see a subsequent 'else' or 'while' so that we can determine if this statement should
        // terminate a continued statement chain; we check to see whether that expected pair
        // already occurs on this same line, in which case we don't expect it later; note that
        // it's possible for an 'if' or 'else if' to simply not be followed by an 'else', and in
        // that case we can potentially do the wrong thing, but there's only so much we can do
        // without a full fledged Scala parser
        slbExpectsPair = nstate.expectsPair(line)
        nstate
      }
    }

    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, cur :State) :State = {
      // if the last non-ws-non-comment char is beyond our SLB condition expression then pop the
      // SLB state because the "body" was on the same line (this is normally done when we see any
      // sort of bracket after our SLB expr, but it's possible that the SLB body contains no
      // brackets, so we catch that case here)
      if (opensSLB && last > slbExprClose) {
        opensSLB = false
        cur.popIf(_.isInstanceOf[SingleBlockS])
      } else super.adjustEnd(line, first, last, start, cur)
    }

    override def willOpenBlock (line :LineV, open :Char, close :Char, col :Int, state :State) :State = {
      if (!opensSLB) state
      else {
        var top = state
        // if we're processing an SLB and this is the first block on the line, note its info
        if (slbExprOpen == -1) slbExprOpen = col
        // if we're opening another block after our SLB token's expression block, then pop the SLB
        // state because we're either opening a multi-line block or we're seeing an expression
        // which is cuddled onto the same line as the SLB; in either case we don't want our SLB
        // state to cause the next line to be indented
        else if (slbExprClose != -1) {
          top = top.popIf(_.isInstanceOf[SingleBlockS])
          opensSLB = false
        }
        top
      }
    }

    override def willCloseBlock (line :LineV, close :Char, col :Int, state :State) = {
      // if we're closing the bracketed expr that goes along with our SLB, note the close column
      if (opensSLB) state match {
        case es :ExprS if (es.col == slbExprOpen) => slbExprClose = col
        case _ => // ignore
      }
    }

    override protected def isComplete (isContinued :Boolean, cur :State) =
      !slbExpectsPair && super.isComplete(isContinued, cur)

    override protected def adjustCompleteEnd (line :LineV, end :State) :State = {
      val ends = super.adjustCompleteEnd(line, end)
      // if we didn't just open an SLB and we're a complete statement, then pop any SLB
      // because this was the single line body of our single line block
      if (opensSLB) ends else ends.popIf(_.isInstanceOf[SingleBlockS])
    }
  }

  class SingleBlockS (token :String, col :Int, next :State) extends State(next) {
    def expectsPair (line :LineV) = token match {
      // if our if or else if is followed by an else on the same line, we're already paired
      case "if" | "else if" => line.lastIndexOf(elseM) match {
        case -1 => true // no else, we expect one
        case ii => ii == line.lastIndexOf(elseIfM) // the else we saw was actually an else if
      }
      case "do" => line.indexOf(whileM) == -1
      case _ => false
    }
    def lacksExpr = token == "else" || token == "do"
    // if the single-block state is on the top of the stack, then we're in the line immediately
    // following the single-block statement, so we want to indent
    override def indent (config :Config, top :Boolean) =
      (if (top) indentWidth(config) else 0) + next.indent(config)
    override def show = s"SingleBlockS($token, $col)"
  }

  private val elseIfM = Matcher.regexp("""\belse\s+if\b""")
  private val elseM = Matcher.regexp("""\belse\b""")
  private val whileM = Matcher.regexp("""\bwhile\b""")
}
