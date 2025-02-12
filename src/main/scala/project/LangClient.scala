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
import moped.code.{CodeCompleter, CodeConfig}
import moped.grammar.GrammarService
import moped.util.{BufferBuilder, Close, Filler, SubProcess}

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

  /** A user friendly name for this language server (i.e. 'Dotty', 'Eclpse', etc.). */
  def name :String

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

  // once we are connected, our server instance will be set and we can initialize our session
  serverV.onValue(server => {
    val initParams = new InitializeParams()
    initParams.setTrace("verbose")
    initParams.setCapabilities(createClientCaps)
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
        })
        // completionItemKind? { valueSet? :CompletionItemKind[] }
        caps.setContextSupport(true)
      })
      caps.setHover(init(new HoverCapabilities()) { caps =>
        caps.setContentFormat(Arrays.asList("markdown", "plaintext"))
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
    })
    caps.setWorkspace(init(new WorkspaceClientCapabilities()) { caps =>
      // TODO: nothing to set re: workspace capabilities as of yet
    })
  }

  /** Executes `cmd` on the language server. The command should come from [[execCommands]] which
    * enumerates all commands supported by the server. */
  def execCommand (cmd :String) :Future[Any] = {
    val params = new ExecuteCommandParams()
    params.setCommand(cmd)
    // TODO: args?
    LSP.adapt(wspaceSvc.executeCommand(params), exec)
  }

  /** Formats (and styles) a `text` block, appending it to `buffer`. */
  def format (buffer :Buffer, wrapWidth :Int, text :String) :Buffer = {
    text.split(System.getProperty("line.separator")) foreach { line =>
      if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
      val filler = new Filler(wrapWidth)
      filler.append(line)
      val start = buffer.end
      val end = buffer.insert(start, filler.toLines)
      buffer.addStyle(CodeConfig.docStyle, start, end)
    }
    buffer
  }

  /** Formats a marked `code` block, appending it to `buffer`. */
  @nowarn def format (buffer :Buffer, code :MarkedString) :Buffer =
    formatCode(buffer, code.getValue, "source." + code.getLanguage)

  /** Formats `markup`, appending it to `buffer`. */
  def format (buffer :Buffer, wrapWidth :Int, markup :MarkupContent) :Buffer =
    markup.getKind match {
      case "markdown"      => formatMarkdown(buffer, wrapWidth, markup.getValue)
      case _ /*plaintext*/ => format(buffer, wrapWidth, markup.getValue)
    }

  /** Formats `either` a text or code block, appending it to `buffer`. */
  @nowarn def format (
    buffer :Buffer, wrapWidth :Int, either :Either[String, MarkedString]
  ) :Buffer = LSP.toScala(either) match {
    case Left(text) => format(buffer, wrapWidth, text)
    case Right(mark) => format(buffer, mark)
  }

  /** Formats a markdown `text` block, appending it to `buffer`. */
  def formatMarkdown (buffer :Buffer, wrapWidth :Int, text :String) :Buffer = {
    // TEMP: do something vaguely useful
    tempFormatMarkdown(buffer, wrapWidth, Seq.from(text.split("\n")), 0)
    // TODO: use Markdown parser &c
    buffer
  }

  private def tempFormatMarkdown (
    buffer :Buffer, wrapWidth :Int, lines :Seq[String], iter :Int
  ) :Unit = {
    if (lines.size > 0) {
      val open = lines.indexWhere(l => l.startsWith("```"), 0)
      if (open == -1) format(buffer, wrapWidth, lines.mkString("\n"))
      else {
        val close = lines.indexWhere(l => l.startsWith("```"), open+1)
        if (close == -1) format(buffer, wrapWidth, lines.mkString("\n"))
        else {
          if (open > 0) format(buffer, wrapWidth, lines.take(open).mkString("\n"))
          val code = lines.slice(open+1, close).mkString("\n")
          val scope = lines(open).substring(3).trim()
          if (iter > 0) { buffer.split(buffer.end) ; buffer.split(buffer.end) }
          formatCode(buffer, code, if (scope == "") "text" else s"source.$scope")
          val rest = lines.drop(close+1)
          if (rest.size > 0) {
            if (iter > 0) { buffer.split(buffer.end) ; buffer.split(buffer.end) }
            tempFormatMarkdown(buffer, wrapWidth, rest, iter+1)
          }
        }
      }
    }
  }

  /** Formats a styled `code` block using TextMate `scope`, appending it to `buffer`. */
  def formatCode (buffer :Buffer, code :String, scope :String) :Buffer = {
    if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
    val start = buffer.end
    val end = buffer.insert(start, Line.fromText(code))
    grammarSvc.scoper(buffer, scope).foreach(_.rethinkIsolatedRegion(start.row, end.row+1))
    buffer
  }

  /** Formats a `docs` string, appending to `buffer`. May contain newlines. */
  def formatDocs (buffer :Buffer, wrapWidth :Int, docs :String) = format(buffer, wrapWidth, docs)

  /** Formats `either` a text or markup block, appending it to `buffer`. */
  def formatDocs (buffer :Buffer, wrapWidth :Int, either :Either[String, MarkupContent]) :Buffer =
    LSP.toScala(either) match {
      case Left(text) => format(buffer, wrapWidth, text)
      case Right(mark) => format(buffer, wrapWidth, mark)
    }

  /** Formats a `detail` string into a signature (to be shown next to the completion text).
    * Any newlines must be removed. */
  def formatSig (detail :String) :LineV = Line.apply(Filler.flatten(detail).take(40))

  /** Converts LSP completion information into Moped's format. */
  def toChoice (item :CompletionItem) = {
    def firstNonNull (a :String, b :String) = if (a != null) a else if (b != null) b else ???
    new CodeCompleter.Choice(firstNonNull(item.getInsertText, item.getLabel)) {
      override def label = firstNonNull(item.getLabel, item.getInsertText)
      override def sig = Option(item.getDetail).map(formatSig).map(Line.apply)
      override def details (viewWidth :Int) =
        LSP.adapt(textSvc.resolveCompletionItem(item), exec).
          map(item => Option(item.getDocumentation).
          map(formatDocs(Buffer.scratch("*details*"), viewWidth-4, _)))
    }
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
          val (items, incomplete) = LSP.toScala(result).fold(
            items => (items, false),
            list => (list.getItems, list.isIncomplete))
          val sorted = items.asScala.toSeq.sortBy(it => Option(it.getSortText) || it.getLabel)
          Completion(pos, sorted.map(toChoice))
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

  class Syncer (caps :ServerCapabilities, buffer :RBuffer, uri :String) {
    var vers = 1
    def incDocId = { vers += 1 ; new VersionedTextDocumentIdentifier(uri, vers) }
    val docId = new TextDocumentIdentifier(uri)
    buffer.state[TextDocumentIdentifier]() = docId

    textSvc.didOpen({
      val item = new TextDocumentItem(uri, LSP.langId(uri), vers, Line.toText(buffer.lines))
      new DidOpenTextDocumentParams(item)
    })

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
  ) :CompletableFuture[ApplyWorkspaceEditResponse] =
    throw new UnsupportedOperationException()

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

  override def configuration (params :ConfigurationParams) :CompletableFuture[JList[Object]] = {
    trace(s"TODO: workspace/configuration ${params}")
    CompletableFuture.completedFuture(Collections.emptyList[Object])
  }

  override def logTrace (params :LogTraceParams) = trace(params.getMessage)

  protected def trace (msg :Any) :Unit = {
    if (debugMode) println(s"$name langserver: $msg")
  }
}
