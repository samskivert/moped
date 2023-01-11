//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.Indenter
import moped.grammar.{GrammarCodeMode, GrammarPlugin}
import moped.code.{CodeConfig, Commenter, Indenter}

@Major(name="xml",
       tags=Array("code", "project", "xml"),
       pats=Array(".*\\.xml", ".*\\.dtml", ".*\\.opml", ".*\\.xsd", ".*\\.tld", ".*\\.jsp",
                  ".*\\.rss", ".*\\.tmLanguage", ".*\\.pom", ".*\\.csproj"),
       desc="A major mode for editing XML files.")
class XmlMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.xml"

  override def createIndenter() = new XmlIndenter(config)

  override val commenter = new Commenter() {
    override def blockOpen   = "<!--"
    override def blockClose  = "-->"
  }
}

@Plugin
class XmlGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.xml" -> "grammar/XML.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.tag", functionStyle),
    effacer("entity.other", variableStyle),

    effacer("variable.language.documentroot", preprocessorStyle),
    effacer("variable.language.entity", typeStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string.quoted.single", Syntax.StringLiteral),
    syntaxer("string.quoted.double", Syntax.StringLiteral)
  )
}

class XmlIndenter (cfg :Config) extends Indenter.ByState(cfg) {
  import Indenter._

  private val tagCloseM = Matcher.exact("</")
  private val dashDashM = Matcher.exact("--")

  override protected def computeIndent (state :State, base :Int, info :Info) :Int = {
    // if this line starts with a close tag, back it up one level to match its corresponding open
    // tag (TODO: really we should indent it based on the next line's state)
    if (info.startsWith(tagCloseM)) base - indentWidth
    // if we're in the middle of a comment, align comment based on whether it starts with --
    else if (state.isInstanceOf[CommentS] && !info.startsWith(dashDashM)) base + 3
    else super.computeIndent(state, base, info)
  }

  override protected def computeState (line :LineV, start :State) = {
    // a primitive XML parser which allows us to count up open and close tags and
    // base our indentation on the number of unclosed open tags at any point in the buffer
    cs = line
    len = line.length
    pos = 0
    name.setLength(0)
    isClose = false
    isProc = false
    var state = 0

    // if we're in the middle of a tag or comment, initialize our parser state accordingly
    start match {
      case pt :PartialTagS =>
        state = 2
        top = start.next
        name.append(pt.tag)
        isClose = pt.isClose
        isProc = pt.isProc

      case ct :CommentS =>
        state = 4
        top = start.next

      case _ =>
        top = start
    }

    // note the top state at the 'start' of the line; we'll use this later to determine whether an
    // open tag is the first open tag on the line or not
    startTop = top

    while (pos < len) {
      val c = cs.charAt(pos)
      pos += 1
      // println(s"step($c, $state)")
      state = step(c, len, state)
    }

    // if we're in the middle of a tag or comment, reflect that in the inter-line state
    state match {
      case 2 => new PartialTagS(name.toString, isClose, isProc, top)
      case 4 => new CommentS(top)
      // TODO: we *could* be in state 1, but that would be a crack-smoking place to wrap a tag...
      case _ => top
    }
  }

  protected class XmlS (tag :String, open :Boolean, dt :Int, next :State) extends State(next) {
    def isMatchingOpen (tag :String) = open && (this.tag == tag)
    override def indent (cfg :Config, top :Boolean) =
      (if (open) dt else -dt) * indentWidth(cfg) + next.indent(cfg)
    override def show = s"XmlS($tag, $dt)"
  }

  protected class PartialTagS (val tag :String, val isClose :Boolean, val isProc :Boolean,
                               next :State) extends State(next) {
    override def indent (cfg :Config, top :Boolean) = tag.length + 2 + next.indent(cfg)
    override def show = s"PartialTagS($tag, $isClose, $isProc)"
  }

  protected class CommentS (next :State) extends State(next) {
    // we indent two here and computeIndent will look at the line and indent it further if it does
    // not start with '--'
    override def indent (cfg :Config, top :Boolean) = 2 + next.indent(cfg)
    override def show = s"CommentS"
  }

  // line parser state; reset on each call to parse()
  private[this] val name = new StringBuilder()
  private[this] var cs :CharSequence = _
  private[this] var len = 0
  private[this] var top :State = _
  private[this] var startTop :State = _

  // state:
  // 0 - between tags
  // 1 - seen < or </ (isClose tracks) or <?/<! (isProc tracks)
  // 2 - parsed name, ignoring rest of tag
  // 3 - in string
  // 4 - in comment
  private[this] var pos = 0
  private[this] var isClose = false
  private[this] var isProc = false

  // this assumes pos is incremented *before* calling step()
  private def peek :Char = if (pos >= len) 0 else cs.charAt(pos)
  private def eat (c :Char) = if (peek != c) false else { pos += 1 ; true }
  private def eat (seq :String) :Boolean = {
    var p = pos ; var s = 0 ; while (s < seq.length) {
      if (p >= len || cs.charAt(p) != seq.charAt(s)) return false
      p += 1 ; s += 1
    }
    pos += seq.length
    true
  }

  private def step (c :Char, len :Int, stepState :Int) :Int = stepState match {
    case 0 =>
      if (c != '<') 0 // keep looking for open tag
      else if (eat("!--")) 4 // transition to "in comment"
      else {
        isClose = eat('/')
        isProc = eat('?') || eat('!')
        1 // transition to "parsing tag name"
      }

    case 1 =>
      if (isNameChar(c)) { name.append(c) ; 1 }
      // otherwise reprocess 'c' in state 2
      else step(c, len, 2)

    case 2 =>
      val autoClose = (c == '/' && eat('>'))
      if (c == '>' || autoClose) {
        // TODO: should we complain about </foo/>? probably not, correctness is not our problem
        if (!isProc && !autoClose) {
          val tag = name.toString
          def isMatchingOpen = top match {
            case xs :XmlS if (xs.isMatchingOpen(tag)) => true
            case _ => false
          }
          // if this is a close tag which matches the open tag on the top of the stack, pop the
          // open tag
          top = if (isClose && isMatchingOpen) top.next else {
            // if this is not the first tag on this line, then don't adjust indent; this ensures
            // that constructs like:
            // <foo><bar>
            //   <stuff/>
            // </bar></foo>
            // doesn't end up doubling indenting <stuff/>
            new XmlS(tag, !isClose, if (top == startTop) 1 else 0, top)
          }
        }
        name.clear()
        0 // back to in between tags
      }
      else if (c == '"') 3
      else 2

    case 3 =>
      if (c == '"') 2 else 3

    case 4 =>
      if (c == '-' && eat("->")) 0 // back to between tags
      else 4
  }

  private def isNameChar (c :Char) = {
    def in (l :Char, u :Char) = l <= c && c <= u
    c != ' ' && (
      in('a', 'z') || in('A', 'Z') || in('0', '9') || c == ':' || c == '_' || c == '-' ||
      c == '.' || c == '\u00B7' || in('\u00C0', '\u00D6') || in('\u00D8', '\u00F6') ||
      in('\u00F8', '\u02FF') || in('\u0370', '\u037D') || in('\u037F', '\u1FFF') ||
      in('\u200C', '\u200D') || in('\u2070', '\u218F') || in('\u2C00', '\u2FEF') ||
      in('\u3001', '\uD7FF') || in('\uF900', '\uFDCF') || in('\uFDF0', '\uFFFD') ||
      in('\u0300', '\u036F') || in('\u203F', '\u2040') /*|| in('\u10000', '\uEFFFF')*/
    )
  }
}
