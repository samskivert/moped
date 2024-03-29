//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.Path
import scala.annotation.nowarn
import scala.collection.mutable.{Map => MMap}
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j._

import moped._
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

  private val commandRing = new Ring(8)
  private def renameHistory = wspace.historyRing("lang-rename")
  private def symbolHistory = wspace.historyRing("lang-symbol")
  private def wordAt (loc :Loc) :String =
    buffer.regionAt(loc, Chars.Word).map(_.asString).mkString

  // /** Used when highlighting uses in our buffer. */
  // val highlights = Value(Seq[Use]())
  // highlights.onChange { (nuses, ouses) =>
  //   ouses foreach upHighlight(false)
  //   nuses foreach upHighlight(true)
  //   window.visits() = new Visit.List("occurrence", nuses.toSeq.sortBy(_.offset).map(
  //     u => Visit(buffer.store, u.offset)))
  // }
  // private def upHighlight (on :Boolean)(use :Use) :Unit = {
  //   val start = buffer.loc(use.offset) ; val end = buffer.loc(use.offset+use.length)
  //   if (on) buffer.addTag(EditorConfig.matchStyle, start, end)
  //   else buffer.removeTag(EditorConfig.matchStyle, start, end)
  // }

  override def keymap = super.keymap.
    bind("describe-element",     "C-c C-d").
    bind("show-enclosers",       "S-C-c S-C-d").
    bind("goto-definition",      "M-.").
    bind("goto-type-definition", "M-/").
    bind("visit-symbol",         "C-c C-v").
    bind("visit-type",           "C-c C-k").
    bind("visit-func",           "C-c C-j").
    bind("visit-value",          "C-c C-h").
    bind("rename-element",       "C-c C-r").
    bind("find-uses",            "C-c C-f").
    bind("lang-exec-command",    "C-c C-l x")

  //   bind("describe-codex", "C-h c").

  //   bind("codex-visit-module", "C-c C-v C-m").
  //   bind("codex-visit-type",   "C-c C-v C-t").
  //   bind("codex-visit-func",   "C-c C-v C-f").
  //   bind("codex-visit-value",  "C-c C-v C-v").
  //   bind("codex-visit-super",  "C-c C-v C-s").

  //   bind("codex-summarize-module",   "C-c C-s C-m").
  //   bind("codex-summarize-type",     "C-c C-s C-t").
  //   bind("codex-summarize-encloser", "C-c C-z").

  //   bind("codex-summarize-type",    "C-c C-j").
  //   // bind("codex-visit-type-member", "C-c C-k").

  //   // bind("codex-describe-element",  "C-c C-d").
  //   bind("codex-summarize-element", "S-C-c S-C-d").
  //   bind("codex-debug-element",     "C-c S-C-d").

  //   // bind("codex-visit-element",     "M-.").
  //   bind("codex-highlight-element", "C-c C-h");

  // override def deactivate () :Unit = {
  //   super.deactivate()
  //   highlights() = Seq()
  // }

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
        val buffer = Buffer.scratch("*popup*")
        val wrapWidth = view.width()-4
        LSP.toScala(contents) match {
          case Left(segs) => for (seg <- segs.asScala) client.format(buffer, wrapWidth, seg)
          case Right(markup) => client.format(buffer, wrapWidth, markup)
        }
        view.popup() = Popup.buffer(buffer, Popup.UpRight(view.point()))
      }
    })
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

  import java.util.{List => JList}
  import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
  private def visitLocations (what :String)(
    locs :JEither[JList[? <: Location],JList[? <: LocationLink]]
  ) = {
    val visits = LSP.toScala(locs) match {
      case Left(locs) => locs.asScala.filter(_.getUri != null).map(client.visit(project, _))
      case Right(links) => links.asScala.filter(_.getTargetUri != null).map(client.visit(project, _))
    }
    if (visits.isEmpty) view.window.popStatus(s"Unable to locate $what.")
    else {
      window.visits() = Visit.List(what, visits.toSeq)
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

  private def renameElementAt (loc :Loc, newName :String) =
    client.serverCaps.flatMap(caps => {
      val canRename = Option(caps.getRenameProvider).map(LSP.toScala).map(_ match {
        case Left(bv) => bv.booleanValue
        case Right(opts) => true
      }) || false
      if (!canRename) abort("Language Server does not support rename refactoring.")

      val rparams = new RenameParams(LSP.docId(view.buffer), LSP.toPos(loc), newName)
      LSP.adapt(textSvc.rename(rparams), view.window.exec).map(edits => {
        val docChanges = edits.getDocumentChanges
        if (docChanges != null) {
          println(s"TODO(docChanges): $docChanges")
        }

        // TODO: resource changes...

        val changes = edits.getChanges
        if (changes == null) abort(s"No changes returned for rename (to $newName)")
        // def toEdit (edit :TextEdit) = Edit(LSP.fromRange(edit.getRange), edit.getNewText)
        changes.asScala.map((uri, edits) => new Renamer(LSP.toStore(uri)) {
          def validate (buffer :Buffer) :Unit = {} // LSP does not supply enough info to validate
          def apply (buffer :Buffer) = {
            val backToFront = edits.asScala.sortBy(e => LSP.fromPos(e.getRange.getStart)).reverse
            for (edit <- backToFront) buffer.replace(
              LSP.fromRange(edit.getRange), Seq(Line(edit.getNewText)))
          }
        }).toSeq
      })
    })

  @Fn("Renames all occurrences of the element at the point.")
  def renameElement () :Unit = {
    val loc = view.point()
    window.mini.read("New name:", wordAt(loc), renameHistory, Completer.none).
      flatMap(name => renameElementAt(loc, name)).
      onSuccess(renamers => {
        println(s"Renames $renamers")
        if (renamers.isEmpty) abort(
          "No renames returned for refactor. Is there an element at the point?")

        def doit (save :Boolean) = try {
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.validate(buffer)
          }
          renamers.foreach { renamer =>
            val buffer = project.pspace.wspace.openBuffer(renamer.store)
            renamer.apply(buffer)
            if (save) buffer.save()
          }
        } catch {
          case err :Throwable => window.exec.handleError(err)
        }

        // if there are occurrences outside the current buffer, confirm the rename
        if (renamers.size == 1 && renamers(0).store == view.buffer.store) doit(false)
        else window.mini.readYN(
          s"'Element occurs in ${renamers.size-1} source file(s) not including this one. " +
            "Undoing the rename will not be trivial, continue?").onSuccess { yes =>
          if (yes) doit(true)
        }
      }).
      onFailure(window.exec.handleError)
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

  // @Fn("""If called inside a method, visits the method it immediately overrides, if any.
  //        If called inside a class but not a method, visits the class's parent.""")
  // def codexVisitSuper () :Unit = onEncloser(view.point()) { df =>
  //   val rel = if (df.kind == Kind.FUNC) Relation.OVERRIDES else Relation.INHERITS
  //   val rels = df.relations(rel)
  //   if (rels.isEmpty) abort(s"No $rel found for '${df.name}'.")
  //   val ref = rels.iterator.next
  //   codex.resolve(project, ref) match {
  //     case None     => abort("Unable to resolve: $ref")
  //     case Some(df) => visit(df)
  //   }
  // }

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

  // @Fn("""Displays the documentation and signature for the element at the point in a separate buffer
  //        (rather than in a popup). This can be useful when the docs are very long, or you wish
  //        to search them, etc.""")
  // def codexSummarizeElement () :Unit = {
  //   onElemAt(view.point()) { (elem, loc, df) =>
  //     val info = codex.summarizeDef(view, codex.stores(project), df)
  //     val name = s"${df.name}:${df.qualifier}"
  //     val buf = project.createBuffer(name, project.codexBufferState("codex-info"))
  //     buf.delete(buf.start, buf.end)
  //     buf.append(info.lines)
  //     frame.visit(buf)
  //   }
  // }

  // @Fn("Displays debugging info for all elements on the current line.")
  // def codexShowLineElements () :Unit = {
  //   val loc = view.point()
  //   val elems = reqIndex.elements(loc.row).toSeq
  //   if (elems.isEmpty) abort("No Codex elements on current line.")
  //   view.popup() = Popup.text(elems map(_.toString), Popup.UpRight(loc))
  //   elems foreach println
  //   highlights() = elems collect {
  //     case use :Use => use
  //     case df  :Def => new Use(df.ref, df.kind, df.offset, df.name.length)
  //   }
  // }

  // @Fn("Displays debugging info for the Codex element at the point.")
  // def codexDebugElement () :Unit = onElemAt(view.point()) {
  //   (elem, loc, df) => view.popup() = codex.mkDebugPopup(df, loc)
  // }

  // @Fn("Displays debugging info for the Codex element enclosing the point.")
  // def codexDebugEncloser () :Unit = onEncloser(view.point()) {
  //   df => view.popup() = codex.mkDebugPopup(df, buffer.loc(df.offset))
  // }

  // @Fn("Highlights all occurrences of an element in the current buffer.")
  // def codexHighlightElement () :Unit = {
  //   onElemAt(view.point()) { (elem, loc, df) =>
  //     val bufSource = Codex.toSource(buffer.store)
  //     val dfRef = df.ref
  //     val usesMap = codex.store(project).usesOf(df).toMapV // source -> uses
  //     def mkUse (offset :Int) = new Use(dfRef, df.kind, offset, df.name.length)

  //     // create a set of all uses in this buffer, for highlighting
  //     val localUses = Seq.builder[Use]()
  //     usesMap.get(bufSource) foreach { offsets => localUses ++= offsets map mkUse }
  //     if (df.source() == bufSource) localUses += mkUse(df.offset)
  //     highlights() = localUses.build()

  //     // report the number of uses found in this buffer, and elsewhere in the project
  //     val count = usesMap.map((src, us) => if (src != bufSource) us.length else 0).fold(0)(_ + _)
  //     window.emitStatus(s"${highlights().size} occurrences in this buffer, $count in other files.")
  //   }
  // }

  @Fn("Displays all uses of the symbol at the point in a separate buffer.")
  def findUses () :Unit = {
    val doc = LSP.docId(view.buffer)
    val pos = LSP.toPos(view.point())
    var name = wordAt(view.point())
    var req = new ReferenceParams(doc, pos, ReferenceContext());
    val initState = project.bufferState("lang-find-uses", LangFindUsesConfig.Context(name, req), client)
    window.focus.visit(project.createBuffer(s"*find-uses: ${name}*", initState))
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
          loopDS(dss.result, Nil)
        }
        // if we have sym infos then we don't have body ranges; so we just find the closest symbol
        // that starts before our location and for which the next symbol ends after our location,
        // then use 'container name' to reconstruct encloser chain...
        else if (sis.knownSize > 0) {
          def symloc (si :SymbolInformation) = LSP.fromPos(si.getLocation.getRange.getStart)
          val (before, after) = sis.result.partition(si => symloc(si) <= loc)
          if (before.isEmpty) Seq()
          else {
            def outers (si :SymbolInformation, encs :List[SymbolInformation]) :Seq[Symbol] =
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
