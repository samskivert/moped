//
// Moped Dart Mode - a Moped major mode for editing Dart code
// http://github.com/moped/dart-mode/blob/master/LICENSE

package moped.code

import java.io.File
import java.nio.file.{Path, Paths, Files}

import moped._
import moped.Matcher
import moped.grammar._
import moped.project._
import moped.util.Paragrapher

object DartConfig extends Config.Defs {

  @Var("If true, cases inside switch blocks are indented one step.")
  val indentCase = key(false)
}

@Major(name="dart",
       tags=Array("code", "project", "dart"),
       pats=Array(".*\\.dart"),
       ints=Array("dart"),
       desc="A major editing mode for the Dart language.")
class DartMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._

  override def langScope = "source.dart"
  override def configDefs = DartConfig :: super.configDefs

  override protected def createIndenter () = new BlockIndenter(config, Seq(
    // bump extends/implements in two indentation levels
    BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(extends|implements)\\b"), 2),
    // align changed method calls under their dot
    new BlockIndenter.AlignUnderDotRule(),
    // handle javadoc and block comments
    new BlockIndenter.BlockCommentRule(),
    // handle indenting switch statements properly
    new BlockIndenter.SwitchRule() {
      override def indentCaseBlocks = config(DartConfig.indentCase)
    },
    // handle continued statements, with some special sauce for : after case
    new BlockIndenter.CLikeContStmtRule()
  ));

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen   = "/*"
    override def blockPrefix = "*"
    override def blockClose  = "*/"
    override def docOpen     = "///"
    override def docPrefix   = "///"
  }
}

@Plugin class DartGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.dart" -> "grammar/Dart.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("class.name", typeStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function.annotation", constantStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("function.name", functionStyle),
    effacer("meta.method.dart", functionStyle),

    effacer("storage.modifier.import", moduleStyle),
    effacer("storage.modifier", keywordStyle),
    effacer("storage.type", typeStyle),
    effacer("support.type", constantStyle),

    // effacer("variable.import", typeStyle),
    // effacer("variable.language", constantStyle),
    effacer("variable", variableStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )
}

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

class AnalyzerStatusParams {
  private var _isAnalyzing = false
  def isAnalyzing :Boolean = _isAnalyzing
  def setAnalyzing(isAnalyzing :Boolean) = _isAnalyzing = isAnalyzing
}

class DartLangClient (metaSvc :MetaService, root :Path, cmd :Seq[String])
    extends LangClient(metaSvc, root, cmd) {

  override def name = "Dart"

  @JsonNotification("$/analyzerStatus")
  def analyzerStatus (params :AnalyzerStatusParams) :Unit = {
    // TODO: something with the analyzer status?
  }
}

@Plugin class DartRootPlugin extends RootPlugin.File("pubspec.yaml")

@Plugin class DartLangPlugin extends LangPlugin {

  private def which (cmd :String) = System.getenv("PATH").split(File.pathSeparator).map(
    dir => Paths.get(dir).resolve(cmd)).find(Files.exists(_))
  private def dartRoot = which("dart").map(dart => dart.getParent.getParent) // TODO: validate

  override def suffs (root :Project.Root) = Set("dart") // TODO: others?
  override def canActivate (root :Project.Root) = Files.exists(root.path.resolve("pubspec.yaml"))
  override def createClient (proj :Project) = dartRoot match {
    case Some(dsdk) => Future.success(new DartLangClient(
      proj.metaSvc, proj.root.path, Seq(
        dsdk.resolve("bin").resolve("dart").toString,
        dsdk.resolve("bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot").toString,
        "--lsp")
    ))
    case None => Future.failure(new Throwable("Unable to locate Dart SDK"))
  }
}
