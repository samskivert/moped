//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.lua

import moped._
import moped.code.Indenter
import moped.grammar.{GrammarPlugin, GrammarCodeMode}
import moped.code.{CodeConfig, Commenter, BlockIndenter}

@Plugin class LuaGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.lua" -> "grammar/Lua.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),
    effacer("variable", stringStyle),
    effacer("storage", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )
}

@Major(name="lua",
       tags=Array("code", "project", "lua"),
       pats=Array(".*\\.lua", ".*\\.p8"),
       ints=Array("lua"),
       desc="A major mode for editing Lua scripts.")
class LuaMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.lua"

  // override def createIndenter() = new XmlIndenter(buffer, config)
  override val commenter = new Commenter() {
    override def linePrefix  = "--"
    override def blockOpen   = "/*"
    override def blockPrefix = "*/"
  }

  override protected def createIndenter () = LuaIndenter.create(config)
}

object LuaIndenter {
  import Indenter._
  import BlockIndenter._

  def create (cfg :Config) :Indenter = new BlockIndenter(cfg, Seq(
    new TokenBlockRule(Matcher.regexp("function\\s"), Matcher.exact("end"), '@'),
    new TokenBlockRule(Matcher.regexp("if.*(then|and|or)"), Matcher.exact("else"),
                       Matcher.exact("end"), '?'),
    new TokenBlockRule(Matcher.regexp("while.*(do|and|or)"), Matcher.exact("end"), '%'),
    new TokenBlockRule(Matcher.regexp("for\\s"), Matcher.exact("end"), '#'),
  ))

  class TokenBlockS (next :State, var close :Char) extends State(next) {
    override def popBlock (close :Char) = if (this.close == close) next else next.popBlock(close)
    override def show = s"TokenBlockS:$close"
  }

  class TokenBlockRule (
    startM :Matcher, elseM :Matcher, endM :Matcher, fauxClose :Char
  ) extends Rule {
    def this (startToken :String, elseToken :String, endToken :String, fauxClose :Char) =
      this(Matcher.exact(startToken), Matcher.exact(elseToken), Matcher.exact(endToken), fauxClose)
    def this (startToken :String, endToken :String, fauxClose :Char) =
      this(startToken, "!!NOMATCH!!", endToken, fauxClose)
    def this (startM :Matcher, endM :Matcher, fauxClose :Char) =
      this(startM, Matcher.exact("!!NOMATCH!!"), endM, fauxClose)

    override def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = {
      // if this line opens a token block, create a token block state
      if (line.matches(startM, first)) new TokenBlockS(start, fauxClose)
      else  start
    }

    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, end :State) :State =
      // if the starting state of this line is our block state, and we match our end token, pop
      // the block off; we don't check the ending state because an earlier rule may have already
      // matched the same token (like `end`) and popped its own state off the end, and we don't
      // want to erroniously also match the same `end` token; this is an unfortunate limitation of
      // the inability to "consume" a token once a rule uses it to adjust the state...
      start match {
        case tok :TokenBlockS if tok.close == fauxClose && line.matches(endM, first) =>
          start.popBlock(fauxClose)
        case _ => end
      }

    override def adjustIndent (state :State, info :Info, indentWidth :Int, base :Int) =
      // ignore the block indent for the else token (if we have one) and the final end token
      state match {
        case tok :TokenBlockS if (tok.close == fauxClose) &&
                                 (info.startsWith(elseM) || info.startsWith(endM)) =>
          base - indentWidth
        case _ => base
      }
  }
}
