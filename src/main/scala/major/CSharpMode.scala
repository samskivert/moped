//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.matching._

import moped._
import moped.project._
import moped.code.{Commenter, Indenter, BlockIndenter, CodeConfig}
import moped.grammar.{GrammarCodeMode, GrammarPlugin}

object CSharpConfig extends Config.Defs {

  @Var("If true, cases inside switch blocks are indented one step.")
  val indentCase = key(false)
}

@Major(name="csharp",
       tags=Array("code", "project", "csharp"),
       pats=Array(".*\\.cs"),
       desc="A major editing mode for the C# language.")
class CSharpMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.cs"
  override def configDefs = CSharpConfig :: super.configDefs

  override protected def createIndenter () = new CSharpIndenter(config)

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen   = "/*"
    override def blockPrefix = "*"
    override def blockClose  = "*/"
    override def docOpen     = "///"
    override def docPrefix   = "///"

    /** Used to identify paragraphs in C# doc comments. Does some special handling to handle XML
      * `<tag>`s. */
    class XmlDocCommentParagrapher (syn :Syntax, buf :Buffer) extends CommentParagrapher(syn, buf) {
      // TODO: if xmlTagRegexp matches just <c> we should not treat that as a <tag> line
      private val xmlTagM = Matcher.regexp("</?[a-z]+>")
      /** Returns true if we're on a `<tag>` line (or its moral equivalent). */
      def isXmlTagLine (line :LineV) = line.matches(xmlTagM, commentStart(line))

      private val openSummaryM = Matcher.exact("<summary>")
      def isBareOpenSummaryLine (line :LineV) = {
        val cs = commentStart(line)
        line.matches(openSummaryM, cs) && line.length == cs + openSummaryM.matchLength
      }

      override def canPrepend (row :Int) =
        // don't extend paragraph upwards if the current top is a <tag> line
        super.canPrepend(row) && !isXmlTagLine(line(row+1)) &&
        // nor if the to-be-prepended line is just <summary> all by itself
        !isBareOpenSummaryLine(line(row))

      // don't extend paragraph downwards if the new line is a <tag> line
      override def canAppend (row :Int) = super.canAppend(row) && !isXmlTagLine(line(row))
    }

    override def mkParagrapher (syn :Syntax, buf :Buffer) = new XmlDocCommentParagrapher(syn, buf)
  }
}

@Plugin class CSharpGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.cs" -> "grammar/CSharp.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.name.tag", constantStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("meta.method.annotation", preprocessorStyle),

    effacer("storage.modifier", keywordStyle),
    effacer("storage.type", typeStyle),

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
    syntaxer("string.quoted", Syntax.StringLiteral)
  )
}

object CSharpRules {
  import BlockIndenter._
  type State = Indenter.State // cope with scale.State BlockIndenter.State name clash

  class SingleBlockS (next :State) extends State(next) {
    override def indent (config :Config, top :Boolean) = indentWidth(config) + next.indent(config)
    override def show = s"SingleBlockS"
  }

  class LambdaPropertyRule extends Rule {
    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, cur :State) :State = {
      var end = cur
      // if we ended a line with a SingleBlockS on the top of the stack, it's time to pop it
      if (end.isInstanceOf[SingleBlockS]) end = end.next
      // otherwise if the line ends with => then start a new single-line block (note: the check
      // that we're currently nested inside a block is perhaps not needed)
      else if (end.isInstanceOf[BlockS]) {
        val arrowStart = last+1-lambdaArrowM.show.length
        if (arrowStart >= 0 && line.matches(lambdaArrowM, arrowStart)) end = new SingleBlockS(end)
      }
      end
    }
    private val lambdaArrowM = Matcher.exact(" =>")
  }
}

class CSharpIndenter (config :Config) extends BlockIndenter(config, Seq(
  // bump `extends`/`implements` in two indentation levels
  BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(extends|implements)\\b"), 2),
  // bump `where` in two indentation levels
  BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(where)\\b"), 1),
  // align chained method calls under their dot
  new BlockIndenter.AlignUnderDotRule(),
  // handle indenting switch statements properly
  new BlockIndenter.SwitchRule() {
    override def indentCaseBlocks = config(CSharpConfig.indentCase)
  },
  // handle continued statements, with some special sauce for : after case
  new BlockIndenter.CLikeContStmtRule(),
  // handle properties defined with lambda arrows
  new CSharpRules.LambdaPropertyRule()
)) {
  import Indenter._
  import BlockIndenter._

  override protected def openBlock (line :LineV, open :Char, close :Char, col :Int, state :State) =
    super.openBlock(line, open, close, col, state) match {
      case bstate :BlockIndenter.BlockS if (col >= lastNonWS(line)) =>
        if (line.matches(namespaceM)) new I0BlockS(close, -1, bstate.next)
        else bstate
      case ostate => ostate
    }

  private class I0BlockS (close :Char, col :Int, next :Indenter.State)
      extends BlockS(close, col, next) {
    override protected def indentWidth (config :Config) :Int = 0
    override protected def show :String = s"I0BlockS($close, $col)"
  }

  private val namespaceM = Matcher.regexp("\\s*namespace ")
}

/** Plugins to extract project metadata from `.sln` files. */
object CSharpPlugins {

  def findSln (root :Path) :Option[Path] =
    Files.list(root).filter(p => p.getFileName.toString.endsWith(".sln")).findAny().toScala

  @Plugin class SlnRootPlugin extends RootPlugin {
    def checkRoot (root :Path) = if (findSln(root).isDefined) 1 else -1
  }

  @Plugin class SlnResolverPlugin extends ResolverPlugin {

    override def metaFiles (root :Project.Root) = findSln(root.path).toSeq

    def addComponents (project :Project) :Unit = {
      val rootPath = project.root.path
      val projName = rootPath.getFileName.toString // TOOD: read from sln?
      val ignores = Seq.newBuilder[Ignorer]
      ignores ++= Ignorer.stockIgnores
      ignores += Ignorer.ignoreName("bin")
      ignores += Ignorer.ignoreName("obj")
      // TODO: only ignore these if we detect Unity
      ignores += Ignorer.ignoreName("bin~")
      ignores += Ignorer.ignoreName("obj~")
      ignores += Ignorer.ignoreRegex(".*\\.meta")

      // TODO: we should probably do this in stockIgnores?
      ignores ++= Ignorer.gitIgnores(rootPath)

      // val sources = Seq.newBuilder[Path]
      def addProject (pdir :Path) :Unit = {
        // if this appears to be a Unity project, ignore Unity stuff as well
        if (Files.exists(pdir.resolve("Assets"))) {
          ignores += Ignorer.ignorePath(pdir.resolve("Library"), rootPath)
          ignores += Ignorer.ignorePath(pdir.resolve("Temp"), rootPath)
          ignores += Ignorer.ignorePath(pdir.resolve("Logs"), rootPath)
        }

        // sources += pdir
      }

      for (sln <- findSln(rootPath) ;
           slnLine <- Files.newBufferedReader(sln).lines.iterator.asScala) slnLine match {
        case projRe(proj, path) if (path `endsWith` ".csproj") =>
          val csproj = rootPath.resolve(path.replace('\\', '/'))
          if (Files.exists(csproj)) addProject(csproj.getParent)
          else println("Invalid .csproj file? " + csproj)
          // case projRe(proj, path) => println("NOPE " + proj + " // " + path)
        case line if (line `startsWith` "Project") => println("NOMATCH " + line)
        case _ => // ignore
      }

      // add a sources component with our source directories
      // project.addComponent(classOf[Sources], new Sources(sources.result))

      project.addComponent(classOf[Filer], new DirectoryFiler(project, ignores.result))

      // add a compiler that runs 'dotnet build' and parses the output
      // project.addComponent(classOf[Compiler], new DotNetCompiler(project, sln))

      val oldMeta = project.metaV()
      project.metaV() = oldMeta.copy(name = projName)
    }
  }

  val projRe = raw"""Project\("\{.*\}"\) = "(.*)", "(.*)", "\{.*\}"""".r
}
