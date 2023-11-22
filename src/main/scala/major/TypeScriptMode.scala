//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.typescript

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import com.eclipsesource.json._

import moped._
import moped.Matcher
import moped.code.{CodeConfig, Commenter, BlockIndenter, Indenter}
import moped.grammar._
import moped.project._

@Plugin class TypeScriptGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.typescript" -> "grammar/TypeScript.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.name.type", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("support.type", typeStyle),

    // effacer("meta.definition.method.typescript", functionStyle),
    effacer("meta.method.typescript", functionStyle),

    effacer("storage.modifier.import", moduleStyle),
    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.annotation", preprocessorStyle),
    effacer("storage.type.def", keywordStyle),
    effacer("storage.type", keywordStyle),

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

object TypeScriptConfig extends Config.Defs {

  @Var("If true, cases inside switch blocks are indented one step.")
  val indentCaseBlocks = key(true)
}

@Major(name="typescript",
       tags=Array("code", "project", "typescript"),
       pats=Array(".*\\.ts", ".*\\.tsx"),
       ints=Array("typescript"),
       desc="A major editing mode for the TypeScript language.")
class TypeScriptMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._

  override def langScope = "source.typescript"
  override def configDefs = TypeScriptConfig :: super.configDefs

  override protected def createIndenter () = new BlockIndenter(config, Seq(
    // bump extends/implements in two indentation levels
    BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(extends|implements)\\b"), 2),
    // align changed method calls under their dot
    new BlockIndenter.AlignUnderDotRule(),
    // handle javadoc and block comments
    new BlockIndenter.BlockCommentRule(),
    // handle indenting switch statements properly
    new BlockIndenter.SwitchRule() {
      override def indentCaseBlocks = config(TypeScriptConfig.indentCaseBlocks)
    },
    // handle continued statements, with some special sauce for : after case
    new BlockIndenter.CLikeContStmtRule(),
    // handle indenting lambda blocks
    new BlockIndenter.LambdaBlockRule(" =>")
  ))

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen = "/*"
    override def blockPrefix = "*"
    override def blockClose = "*/"
    override def docOpen   = "/**"
  }

  // TODO: more things!
}

object TypeScriptPlugins {

  val TSConfigFile = "tsconfig.json"
  val PackageFile = "package.json"

  @Plugin class TSConfigRootPlugin extends RootPlugin.File(TSConfigFile) {
    override protected def createRoot (paths :List[Path], path :Path) = {
      if (Files.exists(path.resolve(PackageFile))) Project.Root(path)
      else {
        var module = s"${path.getFileName()}"
        var root = path.getParent()
        while (root != null && !Files.exists(root.resolve(PackageFile))) {
          module = s"${path.getFileName()}/${module}"
          root = path.getParent()
        }
        if (root == null) Project.Root(path) else Project.Root(root, module)
      }
    }
  }

  /** Extract projects metadata from `package.json` and `tsconfig.json` files. */
  @Plugin class TSConfigResolverPlugin extends ResolverPlugin {

    override def metaFiles (root :Project.Root) = Seq(root.path.resolve(TSConfigFile))

    def addComponents (project :Project) :Unit = {
      val rootPath = project.root.path
      // we trigger on tsconfig.json (which is how we know it's a TypeScript project) but we
      // extract metadata from package.json
      val pkgFile = rootPath.resolve(PackageFile)
      val config = Json.parse(Files.newBufferedReader(pkgFile)).asObject

      val mod = project.root.module
      val modPath = if (mod == "") project.root.path else project.root.path.resolve(mod)
      val baseName = Option(config.get("name")).map(_.asString).
        getOrElse(rootPath.getFileName.toString)
      val projName = if (mod == "") baseName else s"${baseName}-${mod}"

      val ignores = Seq.newBuilder[Ignorer]
      ignores ++= Ignorer.stockIgnores
      ignores += Ignorer.ignorePath(project.root.path.resolve("node_modules"), project.root.path)
      Option(config.get("ignore")).map(_.asArray).foreach { igs =>
        // TODO: handle glob ignores properly
        igs.asScala.map(_.asString).foreach { ignores += Ignorer.ignoreName(_) }
      }
      project.addComponent(classOf[Filer], new DirectoryFiler(project, ignores.result))

      // TODO: package.json doesn't define source directories, so we hack some stuff
      // val sourceDirs = Seq("src", "test").map(modPath.resolve(_))
      // project.addComponent(classOf[Sources], new Sources(sourceDirs))

      val oldMeta = project.metaV()
      project.metaV() = oldMeta.copy(name = projName)
    }
  }

  // TODO: we should try to figure out if the typescript-language-server node module is actually
  // installed (globally); of course there's going to be no pleasant way to do this...
  @Plugin class TypeScriptLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("ts", "tsx")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(TSConfigFile))
    def createClient (proj :Project) = Future.success(
      new TypeScriptLangClient(proj, serverCmd(proj.root.path)))
  }

  private def serverCmd (root :Path) :Seq[String] = {
    Seq("typescript-language-server", "--stdio")
  }
}

class TypeScriptLangClient (proj :Project, serverCmd :Seq[String])
    extends LangClient(proj, serverCmd, None) {

  override def name = "TypeScript"
}
