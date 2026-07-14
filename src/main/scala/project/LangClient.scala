//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.io.{InputStream, OutputStream, PrintWriter}
import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.{CompletableFuture, ExecutorService}
import java.util.{Arrays, Collections, List => JList, HashMap, HashSet}
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.{Either, Message}
import org.eclipse.lsp4j.jsonrpc.{Launcher, MessageConsumer}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageServer}

import moped._
import moped.code.CodeCompleter
import moped.grammar.GrammarService
import moped.util.{BufferBuilder, Close, SubProcess}

object LangClient {

  /** Extracts the `LangClient` from `buffer` state. */
  def apply (buffer :Buffer) :LangClient = buffer.state.get[LangClient].getOrElse {
    throw new IllegalStateException(s"No LSP client configured in buffer: '$buffer'")
  }

  class Component (project :Project) extends Project.Component {
    private def suff (buffer :RBuffer) = {
      val name = buffer.store.name
      name.substring(name.lastIndexOf('.')+1).toLowerCase
    }

    def clientFor (suff :String) :Option[Future[LangClient]] =
      project.pspace.langClientFor(project, suff)

    def restartClient (buffer :RBuffer) :Unit = {
      project.pspace.closeLangClientFor(project, suff(buffer))
      addToBuffer(buffer)
    }

    override def addToBuffer (buffer :RBuffer) :Unit = {
      // add a lang client if one is available
      clientFor(suff(buffer)).map(_.onSuccess(_.addToBuffer(project, buffer)))
    }
  }
}

abstract class LangClient (
  project :Project, val serverCmd :Seq[String], serverPort :Option[Int]
) extends LanguageClient with AutoCloseable {

  private def root :Path = project.root.path
  private def metaSvc :MetaService = project.metaSvc
  private def grammarSvc = metaSvc.service[GrammarService]

  private val debugMode = java.lang.Boolean.getBoolean("moped.debug")
  trace(s"Starting ${serverCmd}...")
  private val serverProc = new ProcessBuilder(serverCmd.asJava).
    directory(root.toFile).
    start();
  private var closeSocket :() => Unit = () => {}

  // read and pass along stderr
  private def debugRead (in :InputStream, prefix :String) = SubProcess.reader(
    in, line => System.err.println(s"$prefix: $line"), _.printStackTrace(System.err)).start()
  debugRead(serverProc.getErrorStream, "STDERR")

  // for debugging, it can sometimes be useful to record a transcript of the raw data we got from
  // the language server (particularly when the server sends invalid JSON RPC, whee!)
  private val transOut = new java.io.ByteArrayOutputStream()
  private def record (in :InputStream) = new java.io.FilterInputStream(in) {
    override def read (target :Array[Byte], off :Int, len :Int) = {
      val got = super.read(target, off, len)
      transOut.write(target, off, got)
      got
    }
    override def read() = {
      val got = super.read()
      transOut.write(got)
      got
    }
  }

  protected def langServerClass :Class[?] = classOf[LanguageServer]

  /** A proxy for talking to the server. */
  def server = serverV()
  private val serverV = Value[LanguageServer](null)

  /** Provides the server capabilities, once known. */
  val serverCaps = Promise[ServerCapabilities]()

  /** Emitted when the server sends messages. */
  val messages = Signal[String]()

  /** Emitted when the server sends `workspace/codeLens/refresh`, meaning previously fetched code
    * lenses (in any open buffer) may now be stale and should be re-requested. Servers send this
    * when something outside a client's view could change lens results, e.g. a reference added or
    * removed in a file that isn't even open. */
  val codeLensesRefreshed = Signal[Unit]()

  /** A user friendly name for this language server (i.e. 'Dotty', 'Eclpse', etc.). */
  def name :String

  /** Sent as `initializationOptions` in the `initialize` request. Most language servers don't need
    * any (the default, `null`) and instead rely purely on standard `ClientCapabilities` plus a
    * `workspace/configuration` pull; others (like typescript-language-server) use this instead to
    * configure server-specific preferences that have no standard LSP capability equivalent (e.g.
    * whether to include parameter-placeholder snippets or auto-import suggestions in completions).
    * The value must be JSON-serializable (a `java.util.Map`/`List`/primitive, per lsp4j's Gson
    * (de)serialization), since it's sent to the server as an arbitrary JSON blob. */
  protected def initializationOptions :Object = null

  /** The `settings` payload sent in a `workspace/didChangeConfiguration` notification immediately
    * after the server reports itself initialized, or `null` (the default) to skip sending one.
    * Most servers pull whatever per-file settings they need via a `workspace/configuration`
    * request instead (a request we currently answer with an empty stub, see `configuration`
    * below); others (again, like typescript-language-server) never pull configuration themselves
    * and instead expect the client to proactively push it via this notification, e.g. to opt into
    * optional features like reference/implementation code lenses which default to off. The value
    * must be JSON-serializable, same constraints as [[initializationOptions]]. */
  protected def initialConfiguration :Object = null

  /** The execute commands supported by the server. */
  def execCommands :Set[String] = execCmds
  private var execCmds = Set[String]()
  serverCaps.onSuccess { caps =>
    val ecp = caps.getExecuteCommandProvider
    if (ecp != null) execCmds = ecp.getCommands.asScala.toSet
  }

  def exec = metaSvc.exec

  override def toString = s"$name langserver"

  private def textSvc = server.getTextDocumentService
  private def wspaceSvc = server.getWorkspaceService

  private val uriToProject = new HashMap[String, Project]()
  private val uriToDiagnostics = new HashMap[String, JList[Diagnostic]]()

  /** Returns the most recently published diagnostics for `uri`, or an empty list if none have been
    * published (or they've all been cleared). Used to populate `CodeActionContext.diagnostics`
    * when requesting code actions. */
  def diagnosticsFor (uri :String) :JList[Diagnostic] =
    uriToDiagnostics.getOrDefault(uri, Collections.emptyList())

  /** Returns a snapshot of the most recently published diagnostics for every URI the server has
    * ever sent us diagnostics for, keyed by URI, including files that aren't open in any buffer
    * (the server happily reports diagnostics for those too; we just have nowhere to show them
    * in-buffer, so they'd otherwise be silently dropped). URIs whose diagnostics have all been
    * cleared are not included. Used by `view-diagnostics` (see LangDiagnosticsMode). */
  def allDiagnostics :Map[String, JList[Diagnostic]] = uriToDiagnostics.asScala.toMap

  /** Emitted (on the UI thread) whenever a `textDocument/publishDiagnostics` notification updates
    * [[allDiagnostics]], so that anything summarizing it (e.g. LangMode's warning/error modeline
    * counts) knows to recompute. */
  val diagnosticsChanged = Signal[Unit]()

  // once we are connected, our server instance will be set and we can initialize our session
  serverV.onValue(server => {
    val initParams = new InitializeParams()
    // only ask the server to be chatty (more $/logTrace and window/logMessage traffic) when we
    // actually want debug info; otherwise this floods *messages*/stderr on every normal run
    initParams.setTrace(if (debugMode) "verbose" else "off")
    initParams.setCapabilities(createClientCaps)
    initParams.setInitializationOptions(initializationOptions)
    val name = root.toString // TODO: get project name?
    initParams.setWorkspaceFolders(List(WorkspaceFolder(root.toUri.toString, name)).asJava)
    // TEMP: metals does not support workspace folders (yet?)
    initParams.setRootUri(root.toUri.toString) : @nowarn
    // TODO: can we get our real PID via a Java API? Ensime fails if we don't send something, sigh
    initParams.setProcessId(0)
    trace(s"Initializing at root: $root")
    messages.emit(s"$name langserver initializing...")
    server.initialize(initParams).thenAccept(rsp => {
      server.initialized(new InitializedParams())
      Option(initialConfiguration).foreach(
        cfg => wspaceSvc.didChangeConfiguration(new DidChangeConfigurationParams(cfg)))
      serverCaps.succeed(rsp.getCapabilities)
      messages.emit(s"$name langserver ready.")
    }).exceptionally(err => {
      import org.eclipse.lsp4j.jsonrpc.MessageIssueException
      messages.emit(s"$name init failure: ${err.getMessage}")
      err.getCause match {
        case me :MessageIssueException =>
          println(s"Broken message: '${me.getMessage}'")
          me.getIssues.forEach { issue =>
            println(s"Issue ${issue.getIssueCode}: ${issue.getText}")
            issue.getCause.printStackTrace(System.out)
          }
        case err => err.printStackTrace(System.err)
      }
      null
    })
  })

  private def initLauncher (launcher :Launcher.Builder[LanguageServer]) = launcher.
    setLocalService(LangClient.this).
    setRemoteInterface(langServerClass.asInstanceOf[Class[LanguageServer]]).
    wrapMessages(consumer => {
      ((message :Message) => { trace(message) ; consumer.consume(message) }) :MessageConsumer
    }).
    setExecutorService(exec.bgService)

  // connect to the language server process either implicitly (via stdio) or over a websocket
  serverPort.match {
    case Some(port) =>
      import javax.websocket._
      import org.eclipse.lsp4j.websocket.jakarta._
      debugRead(serverProc.getInputStream, "STDOUT")
      val endpoint = new WebSocketEndpoint[LanguageServer] {
        override protected def configure (launcher :Launcher.Builder[LanguageServer]) =
          initLauncher(launcher)
        override protected def connect (
          localServices :java.util.Collection[Object], server :LanguageServer
        ) = serverV.update(server)
      }
      val prov = ContainerProvider.getWebSocketContainer()
      // in the socket case, we may have to wait for the language server to be ready which means
      // we'll have a (hopefully short) period during which we are not fully initialized; though we
      // will be added to the buffer during this time, we will avoid doing anything that triggers
      // calls to the language client; messy, but the alternative is a major rearchitecture of how
      // services are added to buffers and how modes are resolved, which does not seem like a fun
      // exercise
      exec.runInBG({
        val uri = new URI(s"ws://localhost:$port")
        var attempts = 0 ; var done = false ; while (!done) {
          try {
            trace(s"Connecting websocket: $uri")
            var sess = prov.connectToServer(endpoint, uri)
            closeSocket = () => sess.close()
            done = true
          } catch {
            case ce :DeploymentException =>
              println(s"Failed to connect to langserver ($ce), retrying...")
              if (attempts >= 10) done = true
              else {
                Thread.sleep(500)
                attempts += 1
              }
          }
        }
      })

    case None =>
      val launcher = initLauncher(new Launcher.Builder[LanguageServer]()).
        setInput(record(serverProc.getInputStream)).
        setOutput(serverProc.getOutputStream).
        create()
      launcher.startListening()
      serverV.update(launcher.getRemoteProxy())
  }

  private def init[T] (t :T)(f :T => Unit) = { f(t) ; t }
  private def createClientCaps = init(new ClientCapabilities()) { caps =>
    caps.setTextDocument(init(new TextDocumentClientCapabilities()) { caps =>
      caps.setPublishDiagnostics(init(new PublishDiagnosticsCapabilities()) { caps =>
        caps.setRelatedInformation(true)
      })
      caps.setCompletion(init(new CompletionCapabilities()) { caps =>
        caps.setCompletionItem(init(new CompletionItemCapabilities()) { caps =>
          caps.setSnippetSupport(true)
          // we honor InsertReplaceEdit (using its narrower "insert" range) as well as plain TextEdit
          caps.setInsertReplaceSupport(true)
          // ask that these be filled in on completionItem/resolve if the server didn't already
          // include them in the initial completion list (many servers lazily resolve additional
          // text edits, e.g. auto-imports, to keep the initial list fast)
          caps.setResolveSupport(new CompletionItemResolveSupportCapabilities(
            Arrays.asList("documentation", "detail", "additionalTextEdits")))
        })
        // completionItemKind? { valueSet? :CompletionItemKind[] }
        caps.setContextSupport(true)
        // tell servers we understand list-wide item defaults (LSP 3.17): some servers (e.g. we've
        // seen this from Java language servers) set insertTextFormat/editRange once on the list
        // instead of repeating them on every item
        caps.setCompletionList(new CompletionListCapabilities(
          Arrays.asList("commitCharacters", "editRange", "insertTextFormat", "insertTextMode", "data")))
      })
      caps.setHover(init(new HoverCapabilities()) { caps =>
        caps.setContentFormat(Arrays.asList("markdown", "plaintext"))
      })
      caps.setDocumentHighlight(new DocumentHighlightCapabilities())
      caps.setCodeLens(new CodeLensCapabilities())
      caps.setTypeHierarchy(new TypeHierarchyCapabilities())
      // we already handle LocationLink responses (see visitLocations's Right branch), not just
      // plain Location, for all four of these
      caps.setDeclaration(new DeclarationCapabilities(true))
      caps.setDefinition(new DefinitionCapabilities(true))
      caps.setImplementation(new ImplementationCapabilities(true))
      caps.setTypeDefinition(new TypeDefinitionCapabilities(true))
      // we send textDocument/prepareRename before renameElement opens its prompt, when the server
      // advertises support for it (see LangMode.prepareRename)
      caps.setRename(init(new RenameCapabilities()) { caps =>
        caps.setPrepareSupport(true)
      })
      caps.setSignatureHelp(init(new SignatureHelpCapabilities()) { caps =>
        caps.setSignatureInformation(init(new SignatureInformationCapabilities()) { caps =>
          caps.setDocumentationFormat(Arrays.asList("markdown", "plaintext"))
        })
      })
      caps.setDocumentSymbol(init(new DocumentSymbolCapabilities()) { caps =>
        // TODO:   symbolKind?: { valueSet?: SymbolKind[] }
        caps.setHierarchicalDocumentSymbolSupport(true)
      })
      caps.setSynchronization(init(new SynchronizationCapabilities()) { caps =>
        // TODO: support will save (& wait until)?
        caps.setDidSave(true)
        // TODO: commitCharactersSupport?: boolean
        // TODO: documentationFormat?: MarkupKind[];
      })
      caps.setCodeAction(init(new CodeActionCapabilities()) { caps =>
        caps.setCodeActionLiteralSupport(new CodeActionLiteralSupportCapabilities(
          new CodeActionKindCapabilities(Arrays.asList(
            CodeActionKind.QuickFix, CodeActionKind.Refactor, CodeActionKind.RefactorExtract,
            CodeActionKind.RefactorInline, CodeActionKind.RefactorRewrite, CodeActionKind.Source,
            CodeActionKind.SourceOrganizeImports))))
        caps.setIsPreferredSupport(true)
        caps.setResolveSupport(new CodeActionResolveSupportCapabilities(Arrays.asList("edit")))
      })
    })
    caps.setWorkspace(init(new WorkspaceClientCapabilities()) { caps =>
      // we implement workspace/applyEdit (see `applyEdit` below), including resource operations
      caps.setApplyEdit(true)
      caps.setWorkspaceEdit(init(new WorkspaceEditCapabilities()) { caps =>
        caps.setDocumentChanges(true)
        caps.setResourceOperations(Arrays.asList("create", "rename", "delete"))
      })
      // lets the server push workspace/codeLens/refresh when lenses we already fetched may have
      // gone stale for a reason we couldn't have seen locally (e.g. a reference added/removed in
      // a different file); see `refreshCodeLenses` below and `codeLensesRefreshed`
      caps.setCodeLens(new CodeLensWorkspaceCapabilities(true))
      // we send workspace/executeCommand (execCommand) and workspace/symbol (symbolCompleter)
      caps.setExecuteCommand(new ExecuteCommandCapabilities())
      caps.setSymbol(new SymbolCapabilities())
      // we implement workspace/configuration (see `configuration` below)
      caps.setConfiguration(true)
    })
  }

  /** Executes `cmd` (with `args`, if any) on the language server. `cmd` should come from
    * [[execCommands]] which enumerates all commands supported by the server. */
  def execCommand (cmd :String, args :JList[Object] = Collections.emptyList()) :Future[Any] = {
    val params = new ExecuteCommandParams()
    params.setCommand(cmd)
    params.setArguments(args)
    LSP.adapt(wspaceSvc.executeCommand(params), exec)
  }

  /** Executes the command described by `cmd` (as returned, e.g., by a code action) on the
    * language server. */
  def execCommand (cmd :Command) :Future[Any] = execCommand(cmd.getCommand, cmd.getArguments)

  /** Converts LSP completion information into Moped's format. `itemDefaults` comes from the
    * containing `CompletionList` (LSP 3.17); some servers set `insertTextFormat`/`editRange` there
    * once instead of repeating them on every item, so we fall back to it when an item omits them. */
  def toChoice (item :CompletionItem, itemDefaults :CompletionItemDefaults) = {
    def firstNonNull (a :String, b :String) = if (a != null) a else if (b != null) b else ???
    new CodeCompleter.Choice(firstNonNull(item.getInsertText, item.getLabel)) {
      override def label = firstNonNull(item.getLabel, item.getInsertText)
      override def sig = Option(item.getDetail).map(Format.formatSig).map(Line.apply)

      // some servers only populate documentation/textEdit/additionalTextEdits lazily via resolve;
      // memoize so that showing the docs popup and then accepting the completion don't each pay
      // for their own completionItem/resolve round trip
      private lazy val resolved :Future[CompletionItem] =
        LSP.adapt(textSvc.resolveCompletionItem(item), exec)

      override def details (viewWidth :Int) = resolved.map(ritem => Option(ritem.getDocumentation).
        map(Format.formatDocs(Buffer.scratch("*details*"), viewWidth-4, _, grammarSvc)))

      override def commit (view :RBufferView, region :Region) :Future[Unit] = resolved.map { ritem =>
        // completionItem/resolve is only obligated to *add* whatever the initial list response
        // left out (typically documentation/detail, or a lazily-computed additionalTextEdits for
        // auto-import); it's free to leave other fields like insertText/textEdit unset even when
        // the original item already had them, so we merge: prefer the resolved item's value where
        // it bothered to set one, otherwise fall back to the original, unresolved item's
        def merged[T] (resolved :T, original :T) :T = if (resolved != null) resolved else original
        val textEdit = merged(ritem.getTextEdit, item.getTextEdit)
        val insertText = merged(ritem.getInsertText, item.getInsertText)
        val insertTextFormat = {
          val fmt = merged(ritem.getInsertTextFormat, item.getInsertTextFormat)
          if (fmt != null || itemDefaults == null) fmt else itemDefaults.getInsertTextFormat
        }
        val additionalTextEdits = merged(ritem.getAdditionalTextEdits, item.getAdditionalTextEdits)
        val command = merged(ritem.getCommand, item.getCommand)

        // prefer the server's own edit (exact range + text) over the plain insertText/label; for
        // an InsertReplaceEdit (or InsertReplaceRange, from itemDefaults) we use the narrower
        // "insert" range, since our completion UI always operates right at the point with nothing
        // after it that we'd want to overwrite
        val defaultRange =
          if (textEdit != null || itemDefaults == null) null else itemDefaults.getEditRange
        val (mainRegion, rawText) = Option(textEdit).map(LSP.toScala) match {
          case Some(Left(edit)) => (LSP.fromRange(edit.getRange), edit.getNewText)
          case Some(Right(ire)) => (LSP.fromRange(ire.getInsert), ire.getNewText)
          case None => Option(defaultRange).map(LSP.toScala) match {
            // the list's default edit range has no newText of its own; it's meant to combine with
            // the item's own insertText (LSP 3.17 CompletionList.itemDefaults semantics)
            case Some(Left(range)) => (LSP.fromRange(range), firstNonNull(insertText, ritem.getLabel))
            case Some(Right(irr)) => (LSP.fromRange(irr.getInsert), firstNonNull(insertText, ritem.getLabel))
            case None => (region, firstNonNull(insertText, ritem.getLabel))
          }
        }
        val (text, cursorOffset) =
          if (insertTextFormat == InsertTextFormat.Snippet) stripSnippet(rawText)
          else (rawText, rawText.length)
        // additionalTextEdits (e.g. auto-inserting an import) apply alongside the main edit;
        // applyTextEdits sorts and applies them back-to-front so they don't invalidate one another
        val mainEdit = new TextEdit(LSP.toRange(mainRegion), text)
        val extraEdits = Option(additionalTextEdits).map(_.asScala.toSeq) getOrElse Seq()
        Lang.applyTextEdits(view.buffer, mainEdit +: extraEdits)
        view.point() = locForOffset(mainRegion.start, text, cursorOffset)
        Option(command).foreach {
          // some servers (e.g. java-language-server) attach a VS-Code-specific client UI command
          // like "editor.action.triggerParameterHints" to a completion instead of embedding
          // per-argument snippet placeholders, expecting the *client* to show live signature help
          // while the user fills in the call; these are never real server commands, so forwarding
          // them via workspace/executeCommand would just error against a server that (rightly)
          // doesn't recognize its own editor's UI action names
          case cmd if cmd.getCommand == "editor.action.triggerParameterHints" =>
            showSignatureHelp(view).onFailure(exec.handleError)
          case cmd if cmd.getCommand `startsWith` "editor.action." => // ignore other client-only UI actions
          case cmd => execCommand(cmd).onFailure(exec.handleError)
        }
      }
    }
  }

  /** Requests signature help at `view`'s point and shows it in a popup. Bound to the
    * `show-signature-help` Fn, and also used to fulfil completions whose `command` is the
    * client-only `editor.action.triggerParameterHints` pseudo-command (see [[toChoice]]). */
  def showSignatureHelp (view :RBufferView) :Future[Unit] = {
    val sparams = new SignatureHelpParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.signatureHelp(sparams), exec).map(help => {
      val sigs = Option(help).map(_.getSignatures) getOrElse Collections.emptyList()
      if (!sigs.isEmpty) {
        val popbuf = Buffer.scratch("*signature*")
        val wrapWidth = view.width()-4
        sigs.asScala.zipWithIndex.foreach { case (sig, ii) =>
          if (ii > 0) popbuf.split(popbuf.end)
          Format.format(popbuf, wrapWidth, sig.getLabel)
          Option(sig.getDocumentation).foreach(doc => Format.formatDocs(popbuf, wrapWidth, doc, grammarSvc))
        }
        view.popup() = Popup.buffer(popbuf, Popup.UpRight(view.point()))
      }
    })
  }

  // converts LSP snippet syntax (tabstops `$1`/`${1:default}`/`${1|a,b|}`, escapes `\$`/`\}`/`\\`)
  // into plain text, using each placeholder's default text if present; we don't support
  // interactive tab-stop cycling, just landing the point at a sensible spot afterward (the first
  // tabstop's position, or the end of the text if there were none)
  private def stripSnippet (text :String) :(String, Int) = {
    val out = new StringBuilder
    var firstTabstop = -1
    var ii = 0
    val n = text.length
    while (ii < n) {
      val c = text.charAt(ii)
      if (c == '\\' && ii+1 < n) { out.append(text.charAt(ii+1)) ; ii += 2 }
      else if (c == '$' && ii+1 < n) {
        ii += 1
        if (text.charAt(ii) == '{') {
          val close = text.indexOf('}', ii)
          val (body, next) =
            if (close < 0) (text.substring(ii+1), n) else (text.substring(ii+1, close), close+1)
          ii = next
          val colon = body.indexOf(':') ; val pipe = body.indexOf('|')
          val default =
            if (colon >= 0) body.substring(colon+1)
            else if (pipe >= 0) body.substring(pipe+1, body.lastIndexOf('|'))
            else ""
          if (firstTabstop < 0) firstTabstop = out.length
          out.append(default)
        } else {
          val start = ii
          while (ii < n && Character.isDigit(text.charAt(ii))) ii += 1
          if (ii == start) out.append('$') // lone '$', not followed by a digit or '{'; keep it literal
          else if (firstTabstop < 0) firstTabstop = out.length
        }
      } else { out.append(c) ; ii += 1 }
    }
    (out.toString, if (firstTabstop >= 0) firstTabstop else out.length)
  }

  // converts a character offset within `text` (as inserted starting at `start`) into a Loc,
  // accounting for any newlines in `text`
  private def locForOffset (start :Loc, text :String, offset :Int) :Loc = {
    val prefix = text.substring(0, math.min(offset, text.length))
    val nl = prefix.lastIndexOf('\n')
    if (nl < 0) Loc(start.row, start.col + prefix.length)
    else Loc(start.row + prefix.count(_ == '\n'), prefix.length - nl - 1)
  }

  /** Creates a visit for `loc` in `project`. */
  def visit (project :Project, loc :Location) :Visit =
    visit(project, new URI(loc.getUri), loc.getRange)
  /** Creates a visit for `loc` in `project`. */
  def visit (project :Project, loc :LocationLink) :Visit =
    visit(project, new URI(loc.getTargetUri), loc.getTargetRange)
  /** Creates a visit for `loc` in `project`. */
  def visit (project :Project, loc :WorkspaceSymbolLocation) :Visit =
    visit(project, new URI(loc.getUri), new Range(new Position(0, 0), new Position(0, 0)))

  /** Creates a visit for `uri`/`range` in `project`. */
  def visit (project :Project, uri :URI, range :Range) :Visit = new Visit {
    protected override def go (window :Window) = {
      val store = Store(Paths.get(uri))
      val point = LSP.fromPos(range.getStart)
      if (uri.getScheme == "file") window.focus.visitFile(store).point() = point
      else fetchContents(uri, window.exec).onSuccess(source => {
        val initState = State.init(classOf[LangClient], LangClient.this) ::
        State.init(classOf[TextDocumentIdentifier], new TextDocumentIdentifier(uri.toString)) ::
          project.bufferState(modeFor(uri))
        val textStore = Store.text(name, source, root)
        val buf = project.pspace.wspace.createBuffer(textStore, initState, true)
        window.focus.visit(buf).point() = point
      }).onFailure(err => window.popStatus(err.getMessage))
    }
  }

  /** Provides the major editing mode source code fetched from the language server via
    * [[fetchContents]]. */
  def modeFor (loc :URI) :String = "text"

  /** Fetches the contents for a "synthetic" location, one hosted by the language server. */
  def fetchContents (uri :URI, exec :Executor) :Future[String] = {
    Future.failure(new Exception("No support for fetching contents.\n" + uri))
  }

  private def debug (item :CompletionItem) :CompletionItem = {
    println("AdditionalTextEdits " + item.getAdditionalTextEdits)
    println("Command " + item.getCommand)
    println("Data " + item.getData)
    println("Detail " + item.getDetail)
    println("Docs " + item.getDocumentation)
    println("Filter " + item.getFilterText)
    println("Insert " + item.getInsertText)
    println("Insert Format " + item.getInsertTextFormat)
    println("Kind Format " + item.getKind)
    println("Label " + item.getLabel)
    println("Sort " + item.getSortText)
    println("Edit " + item.getTextEdit)
    item
  }

  /** Adds this lang client to `buffer`, stuffing various things into the buffer state that enable
    * code smarts. */
  def addToBuffer (project :Project, buffer :RBuffer) :Unit = {
    buffer.state[LangClient]() = this

    buffer.state[CodeCompleter]() = new CodeCompleter() {
      import CodeCompleter._
      def completeAt (window :Window, buffer :Buffer, pos :Loc, point :Loc) = {
        buffer.state.get[Syncer].foreach { _.flushEdits() }
        // TODO: add completion context
        val cparams = new CompletionParams(LSP.docId(buffer), LSP.toPos(pos))
        LSP.adapt(textSvc.completion(cparams), window.exec).map(result => {
          val (items, itemDefaults) = LSP.toScala(result).fold(
            items => (items, null :CompletionItemDefaults),
            list => (list.getItems, list.getItemDefaults))
          val sorted = items.asScala.toSeq.sortBy(it => Option(it.getSortText) || it.getLabel)
          Completion(pos, sorted.map(toChoice(_, itemDefaults)))
        })
      }
    }

    // wait for server to transition to non-null before creating our syncers
    serverV.onValueNotify(server => {
      // let the lang server know we've opened a file (if it corresponds to a file on disk)
      if (server != null) buffer.store.file.foreach(path => serverCaps.onSuccess(caps => {
        val uri = path.toUri.toString
        uriToProject.put(uri, project)
        buffer.state[Syncer]() = new Syncer(caps, buffer, uri)
      }))
    })
    // TODO: if a file transitions from not having a disk-backed store to having one (i.e. newly
    // created file that is then saved, or file that is save-as-ed), we should tell the lang server
    // about that too
  }

  /** Forces any pending (debounced) buffer content changes to be sent to the server right away,
    * instead of waiting for the usual sync debounce to elapse (up to a full second, for
    * full-sync servers - see `Syncer`'s `TextDocumentSyncKind.Full` case).
    *
    * Call this before any request whose answer depends on the server's view of `buffer` being up
    * to date, if that request is fired reactively (e.g. off a point-moved or buffer-edited
    * listener) rather than in synchronous response to a single keystroke. Otherwise the request
    * can race the sync debounce and be answered against stale, pre-edit content - e.g. a
    * documentHighlight request sent 250ms after an edit, while a full-sync server hasn't been
    * told about that edit for up to 1000ms, will happily return highlight positions for the *old*
    * document. `CodeCompleter.completeAt` above already does this for completion requests. */
  def flushEdits (buffer :Buffer) :Unit = buffer.state.get[Syncer].foreach(_.flushEdits())

  class Syncer (caps :ServerCapabilities, buffer :RBuffer, uri :String) {
    var vers = 1
    def incDocId = { vers += 1 ; new VersionedTextDocumentIdentifier(uri, vers) }
    val docId = new TextDocumentIdentifier(uri)

    // tell the server about the file *before* publishing docId to buffer state; anything reacting
    // to that state (e.g. LangMode's code lens cache) uses docId's availability as its signal that
    // it's safe to make requests about this document, which isn't true until didOpen has gone out
    textSvc.didOpen({
      val item = new TextDocumentItem(uri, LSP.langId(uri), vers, Line.toText(buffer.lines))
      new DidOpenTextDocumentParams(item)
    })
    buffer.state[TextDocumentIdentifier]() = docId

    def decodeSave (save :Either[JBoolean, SaveOptions]) :(Boolean, Boolean) =
      if (save == null) (false, false)
      else if (save.isLeft) (save.getLeft, false)
      else (true, save.getRight.getIncludeText)

    // let the server know when we save the buffer (if desired)
    val (wantDidSave, wantSaveText) = LSP.toScala(caps.getTextDocumentSync).fold(
      k => (true, false),
      o => decodeSave(o.getSave))
    if (wantDidSave) buffer.dirtyV.onValue { dirty =>
      if (!dirty) textSvc.didSave(new DidSaveTextDocumentParams(docId))
    }
    // TODO: handle SaveOptions.includeText?

    // send a close event when the buffer is closed
    buffer.killed.onEmit { textSvc.didClose(new DidCloseTextDocumentParams(docId)) }

    // keep the lang server in sync with buffer changes
    LSP.toScala(caps.getTextDocumentSync).fold(k => k, o => o.getChange) match {
      case TextDocumentSyncKind.Incremental =>
        buffer.edited onValue { edit =>
          val changes = Collections.singletonList(toChangeEvent(buffer, edit))
          textSvc.didChange(new DidChangeTextDocumentParams(incDocId, changes))
        }

      case TextDocumentSyncKind.Full =>
        // for full sync servers, we note that a sync is needed, but debounce rapid edits
        buffer.edited onEmit { syncNeeded = true }
        // when enough time elapses after the last edit, trigger a flush
        val debounceTime = 1000L
        buffer.edited.debounce(debounceTime, exec.ui) onEmit { flushEdits() }

      case _ => // no sync desired
    }

    var syncNeeded = false
    def flushEdits () :Unit = if (syncNeeded) {
      val events = Collections.singletonList(
        new TextDocumentContentChangeEvent(Line.toText(buffer.lines)))
      textSvc.didChange(new DidChangeTextDocumentParams(incDocId, events))
      syncNeeded = false
    }
  }

  protected def toChangeEvent (buffer :RBuffer, edit :Buffer.Edit) = {
    def mkRange (start :Loc, end :Loc) = new Range(LSP.toPos(start), LSP.toPos(end))
    def mkChange (start :Loc, end :Loc, text :String) =
      new TextDocumentContentChangeEvent(mkRange(start, end), text)
    import Buffer._
    edit match {
      case Insert(start, end) =>
        mkChange(start, start, Line.toText(buffer.region(start, end)))
      case Delete(start, end, deleted) =>
        mkChange(start, end, "")
      case Transform(start, end, orig) =>
        mkChange(start, end, Line.toText(buffer.region(start, end)))
    }
  }

  override def close () :Unit = {
    val transcript = transOut.toByteArray()
    if (transcript.length > 0) {
      trace("-- Session transcript: --")
      trace(new String(transcript))
      trace("-- End transcript --")
    }

    trace("Shutting down...")
    // have to spell things out for the type checker here...
    val onShutdown :java.util.function.BiConsumer[AnyRef, Throwable] = (res, err) => {
      trace("Shutdown complete.")
      server.exit()
      // give the langserver five seconds to shutdown, then stick a fork in it
      exec.bg.schedule(5000L, () => if (serverProc.isAlive) serverProc.destroy())
      // if we have a socket connection to the server, close that
      closeSocket()
    }
    server.shutdown().whenComplete(onShutdown)
  }

  /**
   * The workspace/applyEdit request is sent from the server to the client to modify resource on
   * the client side.
   */
  override def applyEdit (
    params :ApplyWorkspaceEditParams
  ) :CompletableFuture[ApplyWorkspaceEditResponse] = {
    val rsp = new CompletableFuture[ApplyWorkspaceEditResponse]()
    exec.ui.execute(() => {
      try {
        Lang.applyWorkspaceEdit(project.pspace.wspace, params.getEdit)
        rsp.complete(new ApplyWorkspaceEditResponse(true))
      } catch {
        case err :Throwable =>
          trace(s"applyEdit failed: $err")
          val failed = new ApplyWorkspaceEditResponse(false)
          failed.setFailureReason(err.getMessage)
          rsp.complete(failed)
      }
    })
    rsp
  }

  /**
   * The client/registerCapability request is sent from the server to the client to register for a
   * new capability on the client side. Not all clients need to support dynamic capability
   * registration. A client opts in via the ClientCapabilities.dynamicRegistration property
   */
  override def registerCapability (params :RegistrationParams) :CompletableFuture[Void] = {
    params.getRegistrations.forEach { reg => trace(s"registerCapability Unsupported [reg=$reg]") }
    CompletableFuture.completedFuture(null)
  }

  /**
   * The client/unregisterCapability request is sent from the server to the client to unregister a
   * previously register capability.
   */
  override def unregisterCapability (params :UnregistrationParams) :CompletableFuture[Void] =
    throw new UnsupportedOperationException()

  /**
   * The telemetry notification is sent from the server to the client to ask the client to log a
   * telemetry event.
   */
  def telemetryEvent (data :Object) :Unit = {
    trace(s"telemetryEvent ${data}") // TODO
  }

  /**
   * Diagnostics notifications are sent from the server to the client to signal results of
   * validation runs.
   */
  def publishDiagnostics (pdp :PublishDiagnosticsParams) :Unit = {
    import Lang._
    def sevToNote (sev :DiagnosticSeverity) = sev match {
      case DiagnosticSeverity.Hint => Severity.Hint
      case DiagnosticSeverity.Information => Severity.Info
      case DiagnosticSeverity.Warning => Severity.Warning
      case DiagnosticSeverity.Error => Severity.Error
    }
    exec.ui.execute(() => {
      if (pdp.getDiagnostics.isEmpty) uriToDiagnostics.remove(pdp.getUri)
      else uriToDiagnostics.put(pdp.getUri, pdp.getDiagnostics)
      val project = uriToProject.get(pdp.getUri)
      if (project == null) trace(s"Got diagnostics for unmapped URI: ${pdp.getUri}")
      else {
        val store = LSP.toStore(pdp.getUri)
        val diags = pdp.getDiagnostics
        project.notes(store)() = Seq() ++ diags.asScala.map(diag => Note(
          store,
          Region(LSP.fromPos(diag.getRange.getStart), LSP.fromPos(diag.getRange.getEnd)),
          diag.getMessage,
          Option(diag.getSeverity) map sevToNote getOrElse Severity.Error))
      }
      diagnosticsChanged.emit(())
    })
  }

  /**
   * The show message notification is sent from a server to a client to ask the client to display a
   * particular message in the user interface.
   */
  def showMessage (params :MessageParams) :Unit =
    messages.emit(s"${params.getType}: ${params.getMessage}")

  /**
   * The show message request is sent from a server to a client to ask the client to display a
   * particular message in the user interface. In addition to the show message notification the
   * request allows to pass actions and to wait for an answer from the client.
   */
  def showMessageRequest (params :ShowMessageRequestParams) :CompletableFuture[MessageActionItem] = {
    trace(s"showMessageRequest ${params.getMessage} (${params.getType})")
    for (action <- params.getActions.asScala) {
      trace(s"action ${action.getTitle}")
    }
    CompletableFuture.completedFuture(null)
  }

  // we just log progress messages, so we don't care about the id token
  override def createProgress (params :WorkDoneProgressCreateParams) =
    CompletableFuture.completedFuture(null)

  override def notifyProgress (params :ProgressParams) :Unit = LSP.toScala(params.getValue) match {
    case Left(note) => note match {
      case begin :WorkDoneProgressBegin => messages.emit(begin.getTitle)
      case end :WorkDoneProgressEnd => messages.emit(end.getMessage)
      case _ => println(s"notifyProgress: $note")
    }
    case Right(obj) => println(s"notifyProgress: $obj")
  }

  /**
   * The log message notification is send from the server to the client to ask the client to log a
   * particular message.
   */
  def logMessage (msg :MessageParams) :Unit = {
    exec.ui.execute(() => metaSvc.log.log(s"${msg.getType}: ${msg.getMessage}"))
  }

  override def workspaceFolders () :CompletableFuture[JList[WorkspaceFolder]] = {
    // TODO: do we have any other folders? or should we list just source folders, etc?
    CompletableFuture.completedFuture(List(WorkspaceFolder(root.toUri.toString, name)).asJava)
  }

  /** Returns the value to report for `section` (e.g. `"typescript.referencesCodeLens"` or
    * `"editor.tabSize"`) when the server pulls it via `workspace/configuration`, or `null` (the
    * default) to signal "no opinion," which per spec tells the server to fall back to its own
    * default rather than treating `null` as a real (absent) value. `scopeUri`, if present, narrows
    * the request to a specific resource (usually the file being configured). The value must be
    * JSON-serializable, same constraints as [[initializationOptions]]. */
  protected def configurationValue (section :String, scopeUri :Option[String]) :Object = null

  override def configuration (params :ConfigurationParams) :CompletableFuture[JList[Object]] = {
    // the response must have exactly one entry per requested item, in the same order (an empty
    // list, regardless of how many items were requested, is a protocol violation some servers
    // may not tolerate), so we map every item through configurationValue rather than short-circuit
    val values = params.getItems.asScala.map(
      item => configurationValue(item.getSection, Option(item.getScopeUri))).toList
    CompletableFuture.completedFuture(values.asJava)
  }

  override def logTrace (params :LogTraceParams) = trace(params.getMessage)

  // NOTE: these overrides exist solely to keep Scala 3 from synthesizing mixin forwarders for
  // LanguageClient's default methods; those forwarders copy the @JsonRequest/@JsonNotification
  // annotations from the interface onto the forwarder, and lsp4j's RPC method scanner then finds
  // the same RPC method twice (once on the interface, once on the forwarder) and throws
  // "Duplicate RPC method". Providing our own bodies avoids the synthesized, annotated forwarder.
  override def showDocument (params :ShowDocumentParams) :CompletableFuture[ShowDocumentResult] =
    CompletableFuture.completedFuture(new ShowDocumentResult(true))

  override def refreshSemanticTokens () :CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)

  override def refreshCodeLenses () :CompletableFuture[Void] = {
    exec.ui.execute(() => codeLensesRefreshed.emit(()))
    CompletableFuture.completedFuture(null)
  }

  override def refreshInlayHints () :CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)

  override def refreshInlineValues () :CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)

  override def refreshDiagnostics () :CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)

  protected def trace (msg :Any) :Unit = {
    if (debugMode) println(s"$name langserver: $msg")
  }
}
