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
class TypeScriptMode (env :Env) extends SitterCodeMode(env) {
  import CodeConfig._
  import moped.util.Chars._

  // .tsx files use a distinct (JSX-aware) grammar from plain .ts files
  override def langId =
    if (buffer.store.name.endsWith(".tsx")) new org.treesitter.TreeSitterTsx()
    else new org.treesitter.TreeSitterTypescript()

  override def configDefs = TypeScriptConfig :: super.configDefs

  // JSDoc-style doc comments (`/** ... */`) are parsed by tree-sitter as a single opaque "comment"
  // node with no internal structure, so `@tag`/backtick-code highlighting within them is done via
  // regex rather than grammar-driven styling; see Sitter.styleDocComment
  override def docStylers = Map("comment" -> (keywordStyle, stringStyle, variableStyle))

  override def styles = {
    // bare keyword tokens (i.e. those not better described as a type, constant, etc. below); local
    // to this method (rather than class-level vals) because SitterCodeMode's constructor calls
    // `styles` as part of building its `Sitter`, which runs *before* this class's own field
    // initializers, so a class-level val here would still be null when this is first invoked
    val keywords = Set(
      "abstract", "as", "assert", "asserts", "async", "await", "break", "case", "catch", "class",
      "const", "continue", "debugger", "declare", "default", "delete", "do", "else", "enum",
      "export", "extends", "finally", "for", "from", "function", "get", "if", "implements",
      "import", "in", "infer", "instanceof", "interface", "is", "keyof", "let", "module",
      "namespace", "new", "of", "override", "private", "protected", "public", "readonly", "return",
      "satisfies", "set", "static", "super", "switch", "this", "throw", "try", "type", "typeof",
      "unique", "using", "var", "while", "with", "yield")

    // primitive type keywords; unlike "number"/"string"/"object" these don't double as some other
    // node type elsewhere in the grammar, so they can be styled unconditionally
    val predefinedTypes = Set("any", "unknown", "never", "symbol", "boolean", "void")

    keywords.map(_ -> always(keywordStyle)).toMap ++
    predefinedTypes.map(_ -> always(typeStyle)).toMap ++ Map(
    "comment" -> always(commentStyle),
    "html_comment" -> always(commentStyle),

    "true" -> always(constantStyle),
    "false" -> always(constantStyle),
    "null" -> always(constantStyle),
    "undefined" -> always(constantStyle),
    "regex" -> always(stringStyle),
    "template_string" -> always(stringStyle),
    // "number" and "string" are used both for literal values (`0`, `"foo"`) *and* (confusingly)
    // for the primitive type keywords `number`/`string` (as the sole child of a `predefined_type`
    // node), so we have to look at the parent scope to tell which is which
    "number" -> (scopes => scopes.head match {
      case "predefined_type" => typeStyle
      case _ => constantStyle
    }),
    "string" -> (scopes => scopes.head match {
      case "predefined_type" => typeStyle
      case _ => stringStyle
    }),
    // "object" is used both for the `object` primitive type keyword *and* (far more commonly) for
    // every plain object literal expression (`{ foo: 1 }`), which is not something we style at all;
    // conflating the two used to slap typeStyle across every object literal's entire (often multi-
    // line) span, which then visually leaked through/competed with its children's own styling
    "object" -> (scopes => scopes.head match {
      case "predefined_type" => typeStyle
      case _ => null
    }),

    "type_identifier" -> always(typeStyle),

    "identifier" -> (scopes => scopes.head match {
      case "extends_clause" => typeStyle // `class Foo extends Base`: Base is typed identifier
      case "enum_declaration" => typeStyle // enum names are typed identifier, not type_identifier
      // function declarations/expressions, same color as a call/invocation of that function
      case "function_declaration" | "function_expression" |
           "generator_function_declaration" | "generator_function" => functionStyle
      case "call_expression" | "new_expression" => functionStyle
      case "import_specifier" | "namespace_import" => moduleStyle
      // JSX tag names (both the opening `<Foo` and closing `</Foo>` identifier): `<div>`,
      // `<MyComponent>`, etc. (tsx only; harmless to match in plain .ts, which never has these)
      case "jsx_opening_element" | "jsx_closing_element" | "jsx_self_closing_element" =>
        markupTagStyle
      case _ => variableStyle
    }),
    "property_identifier" -> (scopes => scopes match {
      case "method_definition" :: _ => functionStyle
      case "method_signature" :: _ => functionStyle
      // `obj.method(...)`, including chained/optional access like `a?.b?.method()`: the method
      // name's *direct* parent is always member_expression (never call_expression directly), and
      // it's only a method *call* (as opposed to a plain property read like `obj.field`) when
      // that member_expression is itself the callee of a call_expression
      case "member_expression" :: "call_expression" :: _ => functionStyle
      // JSX attribute name: <div className="app">
      case "jsx_attribute" :: _ => markupAttributeStyle
      // object literal key (`{ display: "flex" }`): more akin to a symbol/keyword-literal than a
      // variable reference, since it's not bound to anything, just a fixed property name
      case "pair" :: _ => constantStyle
      case _ => variableStyle
    }),
    "shorthand_property_identifier" -> always(variableStyle),
    "shorthand_property_identifier_pattern" -> always(variableStyle),
    "private_property_identifier" -> always(variableStyle),
    "statement_identifier" -> always(variableStyle),
    )
  }

  override def syntaxes = Map(
    "comment" -> (_ => Syntax.LineComment),
    "html_comment" -> (_ => Syntax.LineComment),
    "string" -> (_ => Syntax.StringLiteral),
    "template_string" -> (_ => Syntax.StringLiteral),
    "regex" -> (_ => Syntax.StringLiteral),
    "number" -> (_ => Syntax.OtherLiteral),
  )

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
      project.addComponent(classOf[Filer], new DirectoryFiler(project, ignores.result()))

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

  // by default tsserver returns bare-name completions for functions/methods with no parens or
  // argument placeholders, and won't suggest symbols that aren't yet imported; these preferences
  // (passed straight through by typescript-language-server to tsserver's own UserPreferences) opt
  // into the richer behavior. `includeCompletionsWithSnippetText` requires (and we already declare)
  // `completionItem.snippetSupport` on the client side to actually take effect.
  override def initializationOptions :Object = {
    val prefs = new java.util.HashMap[String, Object]()
    prefs.put("includeCompletionsWithSnippetText", java.lang.Boolean.TRUE)
    prefs.put("includeCompletionsForModuleExports", java.lang.Boolean.TRUE)
    prefs.put("includeCompletionsWithInsertText", java.lang.Boolean.TRUE)
    val opts = new java.util.HashMap[String, Object]()
    opts.put("preferences", prefs)
    opts
  }

  // typescript-language-server's reference/implementation code lenses default to disabled (as
  // they do in vscode's own typescript extension) and, unlike most other settings, are only ever
  // read from a client-pushed workspace/didChangeConfiguration notification; the server never
  // pulls them via workspace/configuration, so we have to opt in proactively or textDocument/
  // codeLens will just silently return an empty list forever.
  override def initialConfiguration :Object = {
    val lensPrefs = new java.util.HashMap[String, Object]()
    lensPrefs.put("referencesCodeLens", java.util.Collections.singletonMap("enabled", java.lang.Boolean.TRUE))
    lensPrefs.put("implementationsCodeLens", java.util.Collections.singletonMap("enabled", java.lang.Boolean.TRUE))
    val cfg = new java.util.HashMap[String, Object]()
    cfg.put("typescript", lensPrefs)
    cfg.put("javascript", lensPrefs)
    cfg
  }
}
