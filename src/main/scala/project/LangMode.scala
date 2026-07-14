//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.net.URI
import java.nio.file.Path
import scala.annotation.nowarn
import scala.collection.mutable.{Map => MMap}
import scala.jdk.CollectionConverters._
import com.google.gson.{Gson, JsonElement}
import org.eclipse.lsp4j._

import moped._
import moped.grammar.GrammarService
import moped.major.ReadingMode
import moped.util.{BufferBuilder, Chars, Errors}

/** A minor mode which provides fns for interacting with an LSP server. */
@Minor(name="lang", tags=Array("project"), stateTypes=Array(classOf[Project], classOf[LangClient]),
       desc="""A minor mode that provides LSP fns.""")
class LangMode (env :Env, major :ReadingMode) extends MinorMode(env) {
  import Lang._

  private val project = Project(buffer)
  private val client = LangClient(buffer)
  private def textSvc = client.server.getTextDocumentService
  private def wspaceSvc = client.server.getWorkspaceService
  private def grammarSvc = project.metaSvc.service[GrammarService]

  private val commandRing = new Ring(8)
  private val codeActionRing = new Ring(8)
  private val lensRing = new Ring(8)
  private val superclassRing = new Ring(8)
  // regions currently tagged by the automatic occurrence highlighter (see refreshHighlights)
  private var highlighted :Seq[Region] = Seq()
  private def renameHistory = wspace.historyRing("lang-rename")
  private def symbolHistory = wspace.historyRing("lang-symbol")
  private def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString

  override def keymap = super.keymap.
    bind("describe-element",     "C-c C-d").
    bind("show-enclosers",       "S-C-c S-C-d").
    bind("goto-definition",      "M-.").
    bind("goto-type-definition", "M-/").
    bind("visit-symbol",         "C-c C-v").
    bind("visit-type",           "C-c C-k").
    bind("visit-func",           "C-c C-j").
    bind("visit-value",          "C-c C-h").
    bind("visit-super",          "C-c C-s").
    bind("rename-element",       "C-c C-r").
    bind("code-action",          "C-c C-a").
    bind("find-uses",            "C-c C-f").
    bind("view-diagnostics",     "C-c C-w").
    bind("show-signature-help",  "C-c C-p").
    bind("invoke-lens",          "C-c C-l").
    bind("lang-exec-command",    "C-c C-x")

  //   bind("codex-visit-module", "C-c C-v C-m").
  //   bind("codex-visit-type",   "C-c C-v C-t").
  //   bind("codex-visit-func",   "C-c C-v C-f").
  //   bind("codex-visit-value",  "C-c C-v C-v").
  //   bind("codex-visit-super",  "C-c C-v C-s").

  //
  // FNs

  @Fn("Describes the element at the point.")
  def describeElement () :Unit = {
    val hparams = new HoverParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.hover(hparams), view.window.exec).onSuccess(hover => {
      val contents = if (hover == null) null else hover.getContents
      if (contents == null || (contents.isLeft && contents.getLeft.isEmpty))
        view.window.popStatus("No info available.")
      else {
        val popbuf = Buffer.scratch("*popup*")
        val wrapWidth = view.width()-4
        // Hover.getContents() can still return the deprecated MarkedString variant per the LSP
        // spec (in case a server doesn't send MarkupContent), so we must keep handling it
        @nowarn val scalaContents = LSP.toScala(contents)
        scalaContents match {
          case Left(segs) => for (seg <- segs.asScala) Format.format(popbuf, wrapWidth, seg, grammarSvc)
          case Right(markup) => Format.format(popbuf, wrapWidth, markup, grammarSvc)
        }
        // if the formatted docs are too tall to show comfortably in a popup, open them in a real
        // buffer instead of an overlay that would spill off the bottom of the view
        if (popbuf.lines.length > view.height()/2) {
          val docbuf = wspace.createBuffer(
            Store.scratch(s"*doc: ${wordAt(view.point())}*", buffer.store),
            State.inits(Mode.Hint("help")))
          docbuf.insert(docbuf.start, popbuf.lines)
          window.focus.visit(docbuf)
        }
        else view.popup() = Popup.buffer(popbuf, Popup.UpRight(view.point()))
      }
    })
  }

  @Fn("Shows help for the signature of the call at the point.")
  def showSignatureHelp () :Unit = client.showSignatureHelp(view).onFailure(window.exec.handleError)

  // once the point rests in one spot for a bit, highlight all other occurrences of the symbol
  // under it and refresh the code lens (if any) shown in the modeline for the current line;
  // debounced so we don't hit the server on every intermediate cursor position while the point is
  // moving around, e.g. during a multi-keystroke motion or while typing
  note(view.point.asSignal.debounce(250L, window.exec.ui).onValue(_ => {
    refreshHighlights()
    updateLensText()
  }))
  // a buffer edit can shift or invalidate previously-tagged occurrence highlights without the
  // point necessarily moving in a way the listener above would catch (e.g. a rename or code
  // action applying a multi-location WorkspaceEdit reuses this buffer's edit machinery directly,
  // not a simulated keystroke), so also refresh once edits settle
  note(buffer.edited.debounce(250L, window.exec.ui).onValue(_ => refreshHighlights()))

  private def refreshHighlights () :Unit = {
    clearHighlights()
    val loc = view.point()
    client.serverCaps.onSuccess(caps => if (caps.getDocumentHighlightProvider != null) {
      // this fires reactively (off point-moved/buffer-edited listeners, not a single synchronous
      // keystroke), so it can otherwise race a full-sync server's (up to 1000ms) didChange
      // debounce and get answered against stale, pre-edit content - see LangClient.flushEdits
      client.flushEdits(view.buffer)
      val hparams = new DocumentHighlightParams(LSP.docId(view.buffer), LSP.toPos(loc))
      LSP.adapt(textSvc.documentHighlight(hparams), window.exec).onSuccess(hs => {
        // the point may have moved on to its next resting spot (kicking off another request)
        // while this one was in flight; only apply results that are still relevant
        if (view.point() == loc) {
          highlighted = Option(hs).map(_.asScala.toSeq.map(h => LSP.fromRange(h.getRange))) getOrElse Seq()
          highlighted.foreach(buffer.addStyle(EditorConfig.occurrenceStyle, _))
        }
      }).onFailure(err => env.log.log("documentHighlight request failed", err))
    })
  }

  private def clearHighlights () :Unit = {
    // rather than trust our own bookkeeping of exactly which regions we last tagged (which can go
    // stale the instant a rename/code-action edit shifts text out from under those coordinates,
    // per the confusingly-misplaced-highlight bug this comment is fixing), just sweep the whole
    // buffer for the dedicated occurrence style and remove it, however it's currently positioned
    buffer.removeTags(classOf[String], (_ :String) == EditorConfig.occurrenceStyle, buffer.start, buffer.end)
    highlighted = Seq()
  }

  // the (unresolved) code lenses last fetched for the whole buffer; refreshed wholesale (rather
  // than incrementally, since the protocol has no range-scoped codeLens request) whenever the
  // buffer is edited or the server tells us (via workspace/codeLens/refresh) that lenses anywhere
  // may have gone stale, e.g. because a reference was added/removed in a file we don't have open
  private var codeLenses :Seq[CodeLens] = Seq()
  // the resolved lens(es) (commands filled in) for whichever line the point is currently on; kept
  // around so invokeLens doesn't have to re-resolve what's already showing in the modeline
  private var lineLenses :Seq[CodeLens] = Seq()
  // the text displayed in the modeline for whichever lens(es) apply to the current line
  private val lensText = Value("")

  note(env.mline.addDatum(lensText, Value("Code lens for the current line")))
  note(buffer.edited.debounce(1000L, window.exec.ui).onValue(_ => refreshCodeLensCache()))
  note(client.codeLensesRefreshed.onEmit(refreshCodeLensCache()))
  // the buffer's TextDocumentIdentifier (and thus LSP.docId) isn't available until the server has
  // connected, initialized, *and* this buffer's didOpen/Syncer setup has run, all of which happen
  // asynchronously; firing our first fetch eagerly here (instead of waiting for this) would race
  // that setup and throw "No TextDocumentIdentifier" if we lost. onValueNotify covers both cases:
  // it fires right away if the id is already set (e.g. a second buffer opened against an
  // already-initialized server) and otherwise fires the moment it becomes set.
  note(buffer.state[TextDocumentIdentifier].onValueNotify(_.foreach(_ => refreshCodeLensCache())))

  // a "W / E" warning/error count for the whole project (not just this buffer), reusing the same
  // colors as in-buffer diagnostic styling; either half (or the separator) disappears when its
  // count is zero, and the whole datum disappears when there are none of either
  private val diagCounts = Value(Seq[ModeLine.Segment]())
  note(env.mline.addStyledDatum(
    diagCounts, Value("Project warnings / errors"), Some(() => viewDiagnostics())))
  note(client.diagnosticsChanged.onEmit(refreshDiagCounts()))
  refreshDiagCounts()

  private def refreshDiagCounts () :Unit = {
    var warns = 0 ; var errs = 0
    client.allDiagnostics.valuesIterator.flatMap(_.asScala).foreach(d =>
      Option(d.getSeverity).getOrElse(DiagnosticSeverity.Error) match {
        case DiagnosticSeverity.Warning => warns += 1
        case DiagnosticSeverity.Error => errs += 1
        case _ => // hints/infos aren't counted here
      })
    val segs = Seq.newBuilder[ModeLine.Segment]
    if (warns > 0) segs += ModeLine.Segment(s"W$warns", EditorConfig.warnStyle)
    if (warns > 0 && errs > 0) segs += ModeLine.Segment(" ")
    if (errs > 0) segs += ModeLine.Segment(s"E$errs", EditorConfig.errorStyle)
    diagCounts() = segs.result()
  }

  private def refreshCodeLensCache () :Unit = client.serverCaps.onSuccess(caps => {
    if (caps.getCodeLensProvider != null) {
      // same reasoning as refreshHighlights: this fires reactively off buffer edits, so it can
      // otherwise race a full-sync server's own (slower) didChange debounce
      client.flushEdits(view.buffer)
      val cparams = new CodeLensParams(LSP.docId(view.buffer))
      LSP.adapt(textSvc.codeLens(cparams), window.exec).onSuccess(lenses => {
        codeLenses = Option(lenses).map(_.asScala.toSeq) getOrElse Seq()
        updateLensText()
      }).onFailure(err => env.log.log("codeLens request failed", err))
    }
  })

  // resolves whichever (possibly bare, per resolveCodeLens) lenses apply to the current line,
  // caches them for invokeLens, and shows their titles in the modeline; a no-op network-wise for
  // lines with no lens
  private def updateLensText () :Unit = {
    val row = view.point().row
    val onLine = codeLenses.filter(l => LSP.fromRange(l.getRange).start.row == row)
    if (onLine.isEmpty) { lineLenses = Seq() ; lensText() = "" }
    else Future.sequence(onLine.map(resolveLens)).onSuccess(ls => {
      // the point may have moved to a new line by the time resolution completes; only apply
      // results that are still relevant
      if (view.point().row == row) {
        lineLenses = ls
        lensText() = ls.flatMap(l => Option(l.getCommand).map(_.getTitle)).mkString("  ")
      }
    }).onFailure(err => env.log.log("codeLens resolve failed", err))
  }

  // resolves a lens's command/title if the server didn't already include it (servers that declare
  // codeLensProvider.resolveProvider commonly return bare ranges up front and fill in the title
  // lazily, to keep the initial per-document request cheap)
  private def resolveLens (lens :CodeLens) :Future[CodeLens] =
    if (lens.getCommand != null) Future.success(lens)
    else LSP.adapt(textSvc.resolveCodeLens(lens), window.exec)

  private val gson = new Gson()

  @Fn("""Invokes the command associated with the code lens on the current line. If more than one
         lens applies, prompts for which to invoke.""")
  def invokeLens () :Unit = {
    val cmds = lineLenses.flatMap(l => Option(l.getCommand)).
      filter(c => !Option(c.getCommand).getOrElse("").isEmpty)
    if (cmds.isEmpty) window.popStatus("No code lens on this line.")
    // as with code actions, always show the picker (even for a single lens) so the user can see
    // what it is before committing; picking it is itself the confirmation
    else window.mini.read("Lens:", "", lensRing, Completer.from(cmds, singleCol = true)(_.getTitle)).
      onSuccess(runLensCommand)
  }

  private def runLensCommand (cmd :Command) :Unit = cmd.getCommand match {
    // the "N references"/"N implementations" lens most servers emit resolves to this VS-Code-only
    // client command (arguments: uri, position, Location[]) rather than anything a server
    // implements via workspace/executeCommand, so we have to interpret it ourselves
    case "editor.action.showReferences" =>
      val locs = gson.fromJson(
        cmd.getArguments.get(2).asInstanceOf[JsonElement], classOf[Array[Location]])
      visitAll("reference", locs.toSeq)
    // other editor.action.* commands are VS Code UI actions with no server-side meaning and no
    // client-side equivalent we support yet; ignore rather than sending them to the server, where
    // they'd just fail
    case c if c.startsWith("editor.action.") => ()
    case _ => client.execCommand(cmd).onFailure(window.exec.handleError)
  }

  override def deactivate () :Unit = {
    super.deactivate()
    clearHighlights()
  }

  @Fn("Navigates to the declaration of the element at the point.")
  def gotoDeclaration () :Unit = {
    val loc = view.point()
    val dparams = new DeclarationParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.declaration(dparams), window.exec).onSuccess(visitLocations("declaration"))
  }

  @Fn("Navigates to the definition of the element at the point.")
  def gotoDefinition () :Unit = {
    val loc = view.point()
    val dparams = new DefinitionParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.definition(dparams), window.exec).onSuccess(visitLocations("definition"))
  }

  @Fn("Visits all implementations of the element at the point.")
  def findImplementations () :Unit = {
    val loc = view.point()
    val dparams = new ImplementationParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.implementation(dparams), window.exec).
      onSuccess(visitLocations("implementation"))
  }

  @Fn("Navigates to the type definition of the element at the point.")
  def gotoTypeDefinition () :Unit = {
    val loc = view.point()
    val dparams = new TypeDefinitionParams(LSP.docId(view.buffer), LSP.toPos(view.point()))
    LSP.adapt(textSvc.typeDefinition(dparams), window.exec).
      onSuccess(visitLocations("typeDefinition"))
  }

  @Fn("""Visits the supertype of the type enclosing the point. If it has more than one supertype
         (e.g. a superclass plus implemented interfaces/traits), prompts for which to visit.
         If the point is in a method that overrides a method in a supertype, visits that
         supertype's implementation. There's no LSP-standard request for this, so it only works
         against servers that expose it as a custom command (currently: Metals).""")
  def visitSuper () :Unit = {
    enclosers(view.point()).onSuccess(encs => {
      val kinds = if (client.execCommands(gotoSuperCmd)) Set(SymbolKind.Class, SymbolKind.Method)
                  else Set(SymbolKind.Class)
      encs.find(enc => kinds(enc.kind)) match {
        case Some(enc) => visitSymbol(enc)
        case None => window.popStatus("Failed to find enclosing method or class.")
      }
    })
  }

  private val gotoSuperCmd = "goto-super-method"

  private def visitSymbol (sym :Symbol) :Unit = {
    if (sym.kind == SymbolKind.Method) {
      val pos = new TextDocumentPositionParams(LSP.docId(buffer), sym.sigRange.getStart)
      client.execCommand(gotoSuperCmd, java.util.Collections.singletonList[Object](pos)).
        onFailure(window.exec.handleError)
      client.messages.emit(s"Visiting superclass implementation of '${sym.name}'...")
      // Metals doesn't return the target location as this request's result; instead it
      // navigates by pushing a metals/executeClientCommand("metals-goto-location")
      // notification back at us (see ScalaLangClient.executeClientCommand) once it's
      // resolved the super method, or simply sends nothing at all if the point isn't on/in a
      // method that overrides anything
    }
    else client.serverCaps.onSuccess(caps => {
      if (caps.getTypeHierarchyProvider == null)
        window.popStatus(s"${client.name} language server does not support type hierarchy.")
      else {
        val pparams = new TypeHierarchyPrepareParams(LSP.docId(buffer), sym.sigRange.getStart)
        LSP.adapt(textSvc.prepareTypeHierarchy(pparams), window.exec).onSuccess(items => {
          (Option(items).map(_.asScala.toSeq) getOrElse Seq()).headOption match {
            case None => window.popStatus(s"Unable to resolve type '${sym.name}'.")
            case Some(item) =>
              val sparams = new TypeHierarchySupertypesParams(item)
              LSP.adapt(textSvc.typeHierarchySupertypes(sparams), window.exec).onSuccess(supers => {
                val superSeq = Option(supers).map(_.asScala.toSeq) getOrElse Seq()
                if (superSeq.isEmpty) window.popStatus(s"No supertype found for '${item.getName}'.")
                else visitTypeHierarchyItems(superSeq)
              }).onFailure(window.exec.handleError)
          }
        }).onFailure(window.exec.handleError)
      }
    })
  }

  private def visitTypeHierarchyItems (items :Seq[TypeHierarchyItem]) :Unit =
    if (items.size == 1) visitTypeHierarchyItem(items.head)
    else window.mini.read("Supertype:", "", superclassRing,
                          Completer.from(items, singleCol = true)(_.getName)).
      onSuccess(visitTypeHierarchyItem)

  private def visitTypeHierarchyItem (item :TypeHierarchyItem) :Unit =
    client.visit(project, new URI(item.getUri), item.getSelectionRange).apply(window)

  import java.util.{List => JList}
  import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
  private def visitLocations (what :String)(
    locs :JEither[JList[? <: Location],JList[? <: LocationLink]]
  ) = LSP.toScala(locs) match {
    case Left(locs) => visitAll(what, locs.asScala.toSeq)
    case Right(links) => {
      val visits = links.asScala.filter(_.getTargetUri != null).map(client.visit(project, _))
      if (visits.isEmpty) view.window.popStatus(s"Unable to locate $what.")
      else {
        window.visits() = Visit.List(what, visits.toSeq)
        window.visits().next(window)
      }
    }
  }

  private def visitAll (what :String, locs :Seq[Location]) :Unit = {
    val visits = locs.filter(_.getUri != null).map(client.visit(project, _))
    if (visits.isEmpty) view.window.popStatus(s"Unable to locate $what.")
    else {
      window.visits() = Visit.List(what, visits)
      window.visits().next(window)
    }
  }

  @Fn("Queries for a project-wide symbol and visits it.")
  def visitSymbol () :Unit = visitSymbol(None)
  @Fn("Queries for a project-wide type symbol and visits it.")
  def visitType () :Unit = visitSymbol(Some(Lang.Kind.Type))
  @Fn("Queries for a project-wide function symbol and visits it.")
  def visitFunc () :Unit = visitSymbol(Some(Lang.Kind.Func))
  @Fn("Queries for a project-wide value (field, property, variable) symbol and visits it.")
  def visitValue () :Unit = visitSymbol(Some(Lang.Kind.Value))

  private def visitSymbol (kind :Option[Lang.Kind]) = {
    window.mini.read("Type:", wordAt(view.point()), symbolHistory,
                     symbolCompleter(client, kind)).onSuccess(sym => {
      client.visit(project, sym.uri, sym.range).apply(window)
    })
  }

  private def renameElementAt (loc :Loc, newName :String) :Future[WorkspaceEdit] =
    client.serverCaps.flatMap(caps => {
      val canRename = Option(caps.getRenameProvider).map(LSP.toScala).map(_ match {
        case Left(bv) => bv.booleanValue
        case Right(opts) => true
      }) || false
      if (!canRename) abort("Language Server does not support rename refactoring.")

      val rparams = new RenameParams(LSP.docId(view.buffer), LSP.toPos(loc), newName)
      LSP.adapt(textSvc.rename(rparams), view.window.exec)
    })

  // resolves to the placeholder text renameElement should pre-fill its prompt with, or fails (via
  // abort) if the server tells us the point isn't a renameable position at all - in which case
  // renameElement never shows a prompt in the first place, rather than only discovering the
  // position was invalid after the user has already typed a new name and confirmed it
  private def prepareRename (loc :Loc) :Future[String] = client.serverCaps.flatMap(caps => {
    val prepareSupported = Option(caps.getRenameProvider).map(LSP.toScala).exists {
      case Left(_) => false
      case Right(opts) => Option(opts.getPrepareProvider).exists(_.booleanValue)
    }
    // servers that don't advertise prepareRename get the old behavior: no validation round trip,
    // just moped's own local word-at-point guess
    if (!prepareSupported) Future.success(wordAt(loc))
    else {
      val pparams = new PrepareRenameParams(LSP.docId(view.buffer), LSP.toPos(loc))
      LSP.adapt(textSvc.prepareRename(pparams), view.window.exec).map {
        case null => abort("Cannot rename this element.")
        case result if result.isFirst => Line.toText(buffer.region(LSP.fromRange(result.getFirst)))
        case result if result.isSecond => result.getSecond.getPlaceholder
        // PrepareRenameDefaultBehavior: server says "renameable" but leaves range detection to us
        case _ => wordAt(loc)
      }
    }
  })

  @Fn("Renames all occurrences of the element at the point.")
  def renameElement () :Unit = {
    val loc = view.point()
    prepareRename(loc).flatMap(placeholder =>
      window.mini.read("New name:", placeholder, renameHistory, Completer.none)).
      flatMap(name => renameElementAt(loc, name)).
      onSuccess(edit => {
        val stores = Lang.editedStores(edit)
        if (stores.isEmpty) abort(
          "No renames returned for refactor. Is there an element at the point?")

        def doit (save :Boolean) = try {
          val edited = Lang.applyWorkspaceEdit(project.pspace.wspace, edit)
          if (save) edited.foreach(store => project.pspace.wspace.openBuffer(store).save())
        } catch {
          case err :Throwable => window.exec.handleError(err)
        }

        // if the rename is confined to the current buffer, leave it dirty for review, same as any
        // other edit; if it touches other files, they're not visibly open for the user to notice
        // and save themselves, so save them automatically once the user confirms the rename
        if (stores.size == 1 && stores(0) == view.buffer.store) doit(false)
        else window.mini.readYN(
          s"'Element occurs in ${stores.size-1} source file(s) not including this one. " +
            "Undoing the rename will not be trivial, continue?").onSuccess { yes =>
          if (yes) doit(true)
        }
      }).
      onFailure(window.exec.handleError)
  }

  @Fn("""Queries for and applies a code action (quick fix, refactor, etc.) available at the
         point. If more than one action is available, prompts for which to apply.""")
  def codeAction () :Unit = {
    val doc = LSP.docId(view.buffer)
    val pos = LSP.toPos(view.point())
    // TODO: use the mark-to-point region here, if a mark/selection is active, instead of always
    // requesting actions for a zero-width range at the point
    val range = new Range(pos, pos)
    val ctx = new CodeActionContext(overlappingDiagnostics(doc.getUri, range))
    val aparams = new CodeActionParams(doc, range, ctx)
    LSP.adapt(textSvc.codeAction(aparams), window.exec).
      onSuccess(applyCodeActions).
      onFailure(window.exec.handleError)
  }

  private def overlappingDiagnostics (uri :String, range :Range) :JList[Diagnostic] = {
    def le (p1 :Position, p2 :Position) =
      p1.getLine < p2.getLine || (p1.getLine == p2.getLine && p1.getCharacter <= p2.getCharacter)
    def overlaps (dr :Range) = le(dr.getStart, range.getEnd) && le(range.getStart, dr.getEnd)
    client.diagnosticsFor(uri).asScala.filter(d => overlaps(d.getRange)).asJava
  }

  private def titleOf (action :JEither[Command, CodeAction]) = LSP.toScala(action) match {
    case Left(cmd) => cmd.getTitle
    case Right(action) => action.getTitle
  }

  private def applyCodeActions (result :JList[JEither[Command, CodeAction]]) :Unit = {
    val actions = if (result == null) Seq() else result.asScala.toSeq
    if (actions.isEmpty) view.window.popStatus("No code actions available.")
    // always show the picker, even for a single action, so the user can see what it is before
    // committing to it (rather than it just silently happening); picking one from the list is
    // itself the confirmation, so we run it immediately rather than asking again
    else window.mini.read(
      "Code action:", "", codeActionRing, Completer.from(actions, singleCol = true)(titleOf)).
      onSuccess(runCodeAction)
  }

  private def runCodeAction (action :JEither[Command, CodeAction]) :Unit = LSP.toScala(action) match {
    case Left(cmd) => runCommand(cmd)
    case Right(action) =>
      // if the action wasn't already resolved (no edit or command), ask the server to resolve it
      if (action.getEdit == null && action.getCommand == null)
        LSP.adapt(textSvc.resolveCodeAction(action), window.exec).
          onSuccess(finishCodeAction).
          onFailure(window.exec.handleError)
      else finishCodeAction(action)
  }

  private def runCommand (cmd :Command) :Unit =
    client.execCommand(cmd).onFailure(window.exec.handleError)

  private def finishCodeAction (action :CodeAction) :Unit = try {
    Option(action.getEdit).foreach(edit => Lang.applyWorkspaceEdit(project.pspace.wspace, edit))
    Option(action.getCommand).foreach(runCommand)
  } catch {
    case err :Throwable => window.exec.handleError(err)
  }

  @Fn("Describes the status and capabilities of the current language client.")
  def describeLangClient () :Unit = {
    val bb = new BufferBuilder(this.view.width()-1)
    bb.addHeader("Client")
    bb.addKeysValues(
      "Name: " -> client.name,
      "Server command: " -> client.serverCmd,
    )

    bb.addHeader("Exec commands")
    client.execCommands.foreach(bb.add(_))

    def addInfo (subhead :String, info :AnyRef) = {
      bb.addSubHeader(subhead)
      String.valueOf(info).split("\n").foreach(line => bb.add(line))
    }
    bb.addHeader("Server capabilities")
    client.serverCaps.onSuccess(caps => {
      addInfo("Code actions", caps.getCodeActionProvider)
      addInfo("Code lens", caps.getCodeLensProvider)
      addInfo("Color", caps.getColorProvider)
      addInfo("Completion", caps.getCompletionProvider)
      addInfo("Declaration", caps.getDeclarationProvider)
      addInfo("Definition", caps.getDefinitionProvider)
      addInfo("Document formatting", caps.getDocumentFormattingProvider)
      addInfo("Document highlight", caps.getDocumentHighlightProvider)
      addInfo("Document link", caps.getDocumentLinkProvider)
      addInfo("Document type formatting", caps.getDocumentOnTypeFormattingProvider)
      addInfo("Document range formatting", caps.getDocumentRangeFormattingProvider)
      addInfo("Document symbol", caps.getDocumentSymbolProvider)
      addInfo("Execute command", caps.getExecuteCommandProvider)
      addInfo("Experimental", caps.getExperimental)
      addInfo("Folding range", caps.getFoldingRangeProvider)
      addInfo("Hover", caps.getHoverProvider)
      addInfo("Goto Implementation", caps.getImplementationProvider)
      addInfo("Linked editing range", caps.getLinkedEditingRangeProvider)
      addInfo("Moniker", caps.getMonikerProvider)
      addInfo("References", caps.getReferencesProvider)
      addInfo("Rename", caps.getRenameProvider)
      addInfo("Selection range", caps.getSelectionRangeProvider)
      addInfo("Semantic tokens", caps.getSemanticTokensProvider)
      addInfo("Signature help", caps.getSignatureHelpProvider)
      addInfo("Text document sync", caps.getTextDocumentSync)
      addInfo("Type definition", caps.getTypeDefinitionProvider)
      addInfo("Type hierarchy", caps.getTypeHierarchyProvider)
      addInfo("Workspace", caps.getWorkspace)
      addInfo("Workspace symbol", caps.getWorkspaceSymbolProvider)
    })

    val hbuf = wspace.createBuffer(Store.scratch(s"*lang-client*", buffer.store),
                                   reuse=true, state=State.inits(Mode.Hint("help")))
    frame.visit(bb.applyTo(hbuf))
  }

  @Fn("Restarts the language server client for the active project.")
  def restartLangClient () :Unit = {
    project.emitStatus("Restaring langserver client...")
    project.lang.restartClient(buffer);
  }

  @Fn("""Queries for the name of an 'execute command' supported by this buffer's language
         server and instructs the server to execute it.""")
  def langExecCommand () :Unit = {
    if (client.execCommands.isEmpty) window.popStatus(s"Lang server exposes no commands.")
    else {
      val comp = Completer.from(client.execCommands)
      window.mini.read("Command:", "", commandRing, comp).onSuccess { cmd =>
        client.execCommand(cmd)
              .onSuccess(obj => window.emitStatus(s"Executed: $cmd"))
              .onFailure(window.exec.handleError)
      }
    }
  }

  @Fn("""Shows the symbols (functions, methods, classes, etc.) that enclose the current point.""")
  def showEnclosers () :Unit = enclosers(view.point()).onSuccess(encs => {
    val popbuf = Buffer.scratch("*popup*")
    var lastRow = 0
    for (enc <- encs) {
      var start = LSP.fromPos(enc.sigRange.getStart)
      // sometimes there are multiple enclosers on the same line, since we include the entire line, just skip over additional enclosers from the same line, we already got 'em
      if (start.row != lastRow) {
        popbuf.insertLine(popbuf.start, buffer.line(start))
        lastRow = start.row
      }
    }
    view.popup() = Popup.buffer(popbuf, Popup.UpRight(view.point()))
  })

  // @Fn("""Queries for a type (completed by the project's Codex) then queries for a
  //        member of that type and visits it.""")
  // def codexVisitTypeMember () :Unit = codexRead("Type:", Kind.TYPE) { df =>
  //   val mems = df.members.toSeq
  //   if (mems.isEmpty) visit(df) // if the def has no members, just jump to it
  //   else {
  //     val comp = new Completer[Def]() {
  //       def complete (glob :String) = Future.success(Completion(glob, mems, true)(_.globalRef.id))
  //       // take them to the type if they don't make any attempt to select a member
  //       override def commit (comp :Completion[Def], curval :String) =
  //         if (curval == "") Some(df) else super.commit(comp, curval)
  //     }
  //     window.mini.read("Member:", "", _memHistory.getOrElseUpdate(df, new Ring(8)), comp).
  //       onSuccess(visit)
  //   }
  // }
  // private val _memHistory = MMap[Def,Ring]()

  @Fn("Displays all uses of the symbol at the point in a separate buffer.")
  def findUses () :Unit = {
    val doc = LSP.docId(view.buffer)
    val pos = LSP.toPos(view.point())
    var name = wordAt(view.point())
    var req = new ReferenceParams(doc, pos, ReferenceContext());
    val initState = project.bufferState("lang-find-uses", LangFindUsesConfig.Context(name, req), client)
    window.focus.visit(project.createBuffer(s"*find-uses: ${name}*", initState))
  }

  @Fn("""Displays all diagnostics known for this project in a separate buffer, including
         diagnostics for files that aren't currently open in any buffer.""")
  def viewDiagnostics () :Unit = {
    val initState = project.bufferState(
      "lang-diagnostics", LangDiagnosticsConfig.Context(project.name, client.allDiagnostics))
    window.focus.visit(project.createBuffer(s"*diagnostics: ${project.name}*", initState))
  }

  @Fn("Debug document symbols")
  def debugDocSymbols () :Unit = {
    val docId = LSP.docId(view.buffer)
    val sparams = new DocumentSymbolParams(docId)
    LSP.adapt(textSvc.documentSymbol(sparams), window.exec).onSuccess {
      case null => println("null?")
      case syms if (syms.isEmpty) => println("no symbols?")
      case syms =>
        for (sym <- syms.asScala) println(sym)
    }
  }

  def enclosers (loc :Loc) :Future[Seq[Symbol]] = {
    val docId = LSP.docId(view.buffer)
    val sparams = new DocumentSymbolParams(docId)
    LSP.adapt(textSvc.documentSymbol(sparams), window.exec).map {
      case null => Seq()
      case syms if (syms.isEmpty) => Seq()
      case syms =>
        // convert from wonky "list of eithers" to two separate lists; we'll only have one kind of
        // symbol or the other but lsp4j's "translation" of LSP's "type" is a minor disaster
        @nowarn val sis = Seq.newBuilder[SymbolInformation]
        val dss = Seq.newBuilder[DocumentSymbol]
        syms.asScala map(LSP.toScala) foreach {
          case Left(si) => sis += si
          case Right(ds) => dss += ds
        }

        if (dss.knownSize > 0) {
          def loopDS (dss :Iterable[DocumentSymbol], encs :List[DocumentSymbol]) :Seq[Symbol] =
            dss.find(ds => LSP.fromRange(ds.getRange).contains(loc)) match {
              case Some(ds) => loopDS(ds.getChildren.asScala, ds :: encs)
              case None => encs.map(Symbol(docId, _)).toSeq
            }
          loopDS(dss.result(), Nil)
        }
        // if we have sym infos then we don't have body ranges; so we just find the closest symbol
        // that starts before our location and for which the next symbol ends after our location,
        // then use 'container name' to reconstruct encloser chain...
        else if (sis.knownSize > 0) {
          @nowarn def symloc (si :SymbolInformation) = LSP.fromPos(si.getLocation.getRange.getStart)
          val (before, after) = sis.result().partition(si => symloc(si) <= loc)
          if (before.isEmpty) Seq()
          else {
            @nowarn def outers (si :SymbolInformation, encs :List[SymbolInformation]) :Seq[Symbol] =
              before.find(_.getName == si.getContainerName) match {
                case Some(osi) => outers(osi, si :: encs)
                case None => (si :: encs).reverse.map(Symbol.apply).toSeq
              }
            outers(before.last, Nil)
          }
        }
        else Seq()
    }
  }
}
