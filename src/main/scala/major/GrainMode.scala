//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code._
import moped.Matcher
import moped.grammar._
import moped.code.{CodeConfig, Commenter}

@Plugin class GrainGrammarPlugin extends GrammarPlugin {
  import EditorConfig._
  import CodeConfig._

  override def grammars = Map("source.grain" -> "grammar/Grain.ndf",
                              "source.grain.hover.type" -> "grammar/GrainHover.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block.string", stringStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),
    effacer("variable", variableStyle),
    effacer("support.function", functionStyle),
    effacer("support.constant", constantStyle),
    effacer("support.class", typeStyle),
    effacer("support.other.module", moduleStyle),
    effacer("storage", variableStyle)
  )
}

@Major(name="grain",
       tags=Array("code", "project", "grain"),
       pats=Array(".*\\.gr"),
       desc="A major mode for editing Grain code.")
class GrainMode (env :Env) extends GrammarCodeMode(env) {

  override def dispose () :Unit = {} // nada for now

  override def langScope = "source.grain"

  override def keymap = super.keymap.
    bind("self-insert-command", "'"); // don't auto-pair single quote

  override protected def createIndenter () = new BlockIndenter(config, Seq(
    // align changed method calls under their dot
    new BlockIndenter.AlignUnderDotRule(),
    // handle javadoc and block comments
    new BlockIndenter.BlockCommentRule(),
    // handle indenting switch statements properly
    new BlockIndenter.SwitchRule(),
    // handle continued statements
    new BlockIndenter.ContinuedStatementRule(".+-=?:")
  ));

  override val commenter = new Commenter() {
    import moped.code.CodeConfig._

    override def linePrefix  = "//"
    override def blockOpen = "/*"
    override def blockClose = "*/"
    override def blockPrefix = "*"
    override def docPrefix   = "/**"

    // look for longer prefix first, then shorter
    override def commentDelimLen (line :LineV, col :Int) :Int = {
      if (line.matches(blockPrefixM, col)) blockPrefixM.matchLength
      else if (line.matches(linePrefixM, col)) linePrefixM.matchLength
      else 0
    }
  }
}
