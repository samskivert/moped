//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.net.URI
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j._

import moped._
import moped.util.Errors

/** Helpers for integrating with LSP servers. */
object Lang {

  /** Enumerates different kinds of [[Note]]s. */
  enum Severity {
    case Hint, Info, Warning, Error
  }

  /** Describes information about a region of code in a file. */
  case class Note (store :Store, region :Region, msg :String, sev :Severity) extends Visit {

    /** Formats the message for display in a popup. */
    def format (maxWidth :Int, msg :String) :Buffer = {
      val wrapped = ArrayBuffer[String]()
      msg.split(System.getProperty("line.separator")) foreach { line =>
        if (line.length <= maxWidth) wrapped += line
        else line.grouped(maxWidth) foreach { wrapped += _ }
      }
      val buffer = Buffer.scratch("*popup*")
      buffer.append(wrapped.map(Line.apply))
      buffer
    }

    override protected def go (window :Window) = {
      val view = window.focus.visitFile(store)
      view.point() = view.buffer.clamp(region.start)
      val pop = Popup.buffer(format(view.width()-2, msg), Popup.UpRight(view.point()))
      view.showPopup(if (sev == Severity.Error) pop.toError else pop)
    }
  }

  /** Partitions definitions into "kinds": modules, types, functions and values. */
  enum Kind {
    case Module, Type, Func, Value, Synthetic
  }

  def toK (kind :SymbolKind) = kind match {
    case SymbolKind.Array => Kind.Value
    case SymbolKind.Boolean => Kind.Value
    case SymbolKind.Class => Kind.Type
    case SymbolKind.Constant => Kind.Value
    case SymbolKind.Constructor => Kind.Func
    case SymbolKind.Enum => Kind.Type
    case SymbolKind.EnumMember => Kind.Value
    case SymbolKind.Event => Kind.Type
    case SymbolKind.Field => Kind.Value
    case SymbolKind.File => Kind.Type // temp: hack due to csharp-ls
    case SymbolKind.Function => Kind.Func
    case SymbolKind.Interface => Kind.Type
    case SymbolKind.Key => Kind.Value
    case SymbolKind.Method => Kind.Func
    case SymbolKind.Module => Kind.Module
    case SymbolKind.Namespace => Kind.Module
    case SymbolKind.Null => Kind.Value
    case SymbolKind.Number => Kind.Value
    case SymbolKind.Object => Kind.Value
    case SymbolKind.Operator => Kind.Func
    case SymbolKind.Package => Kind.Module
    case SymbolKind.Property => Kind.Value
    case SymbolKind.String => Kind.Value
    case SymbolKind.Struct => Kind.Type
    case SymbolKind.TypeParameter => Kind.Type
    case SymbolKind.Variable => Kind.Value
    case null => Kind.Value
  }

  /** Applies `edits` to `buffer`, back to front so that earlier edits don't invalidate the
    * positions of later ones. */
  def applyTextEdits (buffer :Buffer, edits :Seq[TextEdit]) :Unit = {
    // some servers (e.g. typescript-language-server, for edits that insert content into a brand
    // new file created earlier in the same WorkspaceEdit) signal "insert at the end of the
    // document" via a bogus negative line/character position rather than a real one; detect and
    // redirect that to the buffer's actual end rather than passing negative coordinates through
    def regionFor (edit :TextEdit) = {
      val r = LSP.fromRange(edit.getRange)
      if (r.start.row < 0 || r.end.row < 0) Region(buffer.end, buffer.end) else r
    }
    val backToFront = edits.map(e => (regionFor(e), e)).sortBy(_._1.start).reverse
    // newText frequently spans multiple lines (e.g. inserting a whole new import statement), so it
    // must be split into one Line per line, not jammed into a single Line with embedded newlines
    for ((region, edit) <- backToFront) buffer.replace(region, Line.fromText(edit.getNewText))
  }

  /** Returns the stores that would be touched by applying `edit`, without actually applying it.
    * Useful for confirming a refactor with the user before committing to it. */
  def editedStores (edit :WorkspaceEdit) :Seq[Store] = {
    val docChanges = edit.getDocumentChanges
    if (docChanges != null) docChanges.asScala.toSeq.map(LSP.toScala).map {
      case Left(tde) => LSP.toStore(tde.getTextDocument.getUri)
      case Right(op :CreateFile) => LSP.toStore(op.getUri)
      case Right(op :RenameFile) => LSP.toStore(op.getNewUri)
      case Right(op :DeleteFile) => LSP.toStore(op.getUri)
      case Right(op) => throw new IllegalArgumentException(s"Unknown resource operation: $op")
    }
    else Option(edit.getChanges).map(_.asScala.keys.map(LSP.toStore).toSeq) || Seq()
  }

  /** Applies `edit` (as received from `workspace/applyEdit`, a code action's `edit` field, or
    * `textDocument/rename`) to `wspace`. Text edits are applied to (and, if not yet open, opened
    * in) the relevant buffers, but those buffers are not saved; the caller decides whether/when to
    * do that. Resource operations (file create/rename/delete), by contrast, take effect on disk
    * immediately, as there's no "unsaved buffer" state for them to stage into.
    * @return the stores whose buffers were edited (and may now want saving); this does not include
    * stores that were merely created, renamed or deleted via a resource operation. */
  def applyWorkspaceEdit (wspace :Workspace, edit :WorkspaceEdit) :Seq[Store] = {
    val edited = Seq.newBuilder[Store]
    def applyChange (uri :String, edits :Seq[TextEdit]) :Unit = {
      val store = LSP.toStore(uri)
      applyTextEdits(wspace.openBuffer(store), edits)
      edited += store
    }
    val docChanges = edit.getDocumentChanges
    if (docChanges != null) docChanges.asScala.foreach(change => LSP.toScala(change) match {
      case Left(tde) => applyChange(tde.getTextDocument.getUri, tde.getEdits.asScala.toSeq)
      case Right(op :CreateFile) => createFile(op)
      case Right(op :RenameFile) => renameFile(wspace, op)
      case Right(op :DeleteFile) => deleteFile(wspace, op)
      case Right(op) => throw new IllegalArgumentException(s"Unknown resource operation: $op")
    })
    else Option(edit.getChanges).foreach(_.asScala.foreach((uri, edits) =>
      applyChange(uri, edits.asScala.toSeq)))
    edited.result()
  }

  private def uriPath (uri :String) = Paths.get(new URI(uri))
  private def flag (bv :JBoolean) = bv != null && bv.booleanValue

  private def createFile (op :CreateFile) :Unit = {
    val path = uriPath(op.getUri)
    val opts = op.getOptions
    val overwrite = opts != null && flag(opts.getOverwrite)
    val ignoreIfExists = opts != null && flag(opts.getIgnoreIfExists)
    if (!Files.exists(path)) {
      Files.createDirectories(path.getParent)
      Files.write(path, Array.emptyByteArray)
    }
    else if (overwrite) Files.write(path, Array.emptyByteArray)
    else if (!ignoreIfExists) throw Errors.feedback(s"File already exists: $path")
  }

  private def renameFile (wspace :Workspace, op :RenameFile) :Unit = {
    val oldPath = uriPath(op.getOldUri) ; val newPath = uriPath(op.getNewUri)
    val opts = op.getOptions
    val overwrite = opts != null && flag(opts.getOverwrite)
    val ignoreIfExists = opts != null && flag(opts.getIgnoreIfExists)
    if (Files.exists(newPath) && !overwrite && !ignoreIfExists)
      throw Errors.feedback(s"File already exists: $newPath")
    Files.createDirectories(newPath.getParent)
    wspace.buffers.find(_.store.file.contains(oldPath)) match {
      case Some(buf) => buf.saveTo(Store(newPath)) ; Files.deleteIfExists(oldPath)
      case None =>
        val copyOpts = if (overwrite) Seq(StandardCopyOption.REPLACE_EXISTING) else Seq()
        Files.move(oldPath, newPath, copyOpts*)
    }
  }

  private def deleteFile (wspace :Workspace, op :DeleteFile) :Unit = {
    val path = uriPath(op.getUri)
    wspace.buffers.find(_.store.file.contains(path)).foreach(_.kill())
    val opts = op.getOptions
    val ignoreIfNotExists = opts != null && flag(opts.getIgnoreIfNotExists)
    if (!Files.deleteIfExists(path) && !ignoreIfNotExists)
      throw Errors.feedback(s"File does not exist: $path")
  }

  case class Symbol (
    kind :SymbolKind, name :String, fqName :String, sig :String, uri :URI,
    range :Range, sigRange :Range
  ) {
    def sortKey = (symbolOrder.indexOf(kind), name)
  }

  object Symbol {
    @nowarn def apply (sym :SymbolInformation) :Symbol = Symbol(
      sym.getKind, sym.getName,
      sym.getContainerName match {
        case null => s"${fileForLoc(sym.getLocation)}:${sym.getName}"
        case cont => s"${cont}.${sym.getName}"
      },
      formatSym(sym),
      new URI(sym.getLocation.getUri),
      sym.getLocation.getRange,
      sym.getLocation.getRange)

    def apply (sym :WorkspaceSymbol) :Symbol = LSP.toScala(sym.getLocation) match {
      case Left(loc) => Symbol(
        sym.getKind, sym.getName, sym.getContainerName match {
          case null => s"${fileForLoc(loc)}:${sym.getName}"
          case cont => s"${cont}.${sym.getName}"
        },
        formatSym(sym), new URI(loc.getUri), loc.getRange, loc.getRange)
      case Right(wsloc) => Symbol(
        sym.getKind, sym.getName,
        sym.getContainerName match {
          case null => s"${fileForUri(wsloc.getUri)}:${sym.getName}"
          case cont => s"${cont}.${sym.getName}"
        },
        formatSym(sym),
        new URI(wsloc.getUri),
        new Range(new Position(0, 0), new Position(0, 0)),
        new Range(new Position(0, 0), new Position(0, 0)))
    }

    def apply (docId :TextDocumentIdentifier, sym :DocumentSymbol) :Symbol = Symbol(
      sym.getKind, sym.getName, sym.getName, sym.getDetail, new URI(docId.getUri), sym.getRange,
      sym.getSelectionRange)

    /** Formats a symbol name for use during completion. Moped convention is `name:qualifier`. */
    @nowarn private def formatSym (sym :SymbolInformation) = sym.getContainerName match {
      case null => s"${sym.getName}:${fileForLoc(sym.getLocation)} [${sym.getKind}]"
      case cont => s"${sym.getName}:${cont} [${sym.getKind}]"
    }

    /** Formats a symbol name for use during completion. Moped convention is `name:qualifier`. */
    private def formatSym (sym :WorkspaceSymbol) = sym.getContainerName match {
      case null => LSP.toScala(sym.getLocation) match {
        case Left(loc) => s"${sym.getName}:${fileForLoc(loc)} [${sym.getKind}]"
        case Right(wsloc) => s"${sym.getName}:${fileForUri(wsloc.getUri)} [${sym.getKind}]"
      }
      case cont => s"${sym.getName}:${cont} [${sym.getKind}]"
    }

    private def fileForUri (uri :String) = uri.substring(uri.lastIndexOf("/")+1)
    private def fileForLoc (loc :Location) =
    s"${fileForUri(loc.getUri)}@${loc.getRange.getStart.getLine}"
  }

  def symbolCompleter (client :LangClient, kind :Option[Kind]) = new Completer[Symbol] {
    override def minPrefix = 2
    def wspaceSvc = client.server.getWorkspaceService
    def complete (glob :String) = {
      @nowarn val res = wspaceSvc.symbol(new WorkspaceSymbolParams(glob))
      LSP.adapt(res, client.exec).map(LSP.toScala).map(_ match {
        case Left(res) => process(glob, res.asScala.map(Symbol.apply).toSeq)
        case Right(res) => process(glob, res.asScala.map(Symbol.apply).toSeq)
      })
    }
    private def process (glob :String, syms :Seq[Symbol]) = Completion(
      glob, syms.filter(checkKind).sortBy(_.sortKey), false)(_.sig)
    private def checkKind (sym :Symbol) :Boolean =
      kind.map(kk => kk == toK(sym.kind)) || true
  }

  // used to sort symbol completion results
  private val symbolOrder = Array(
    SymbolKind.File,

    SymbolKind.Class,
    SymbolKind.Interface,
    SymbolKind.Struct,
    SymbolKind.Enum,

    SymbolKind.Module,
    SymbolKind.Namespace,
    SymbolKind.Object,
    SymbolKind.Package,

    SymbolKind.Constructor,
    SymbolKind.Method,
    SymbolKind.Function,
    SymbolKind.Operator,
    SymbolKind.Property,
    SymbolKind.Field,
    SymbolKind.EnumMember,

    SymbolKind.TypeParameter,
    SymbolKind.Array,
    SymbolKind.Boolean,
    SymbolKind.Constant,
    SymbolKind.Event,
    SymbolKind.Key,
    SymbolKind.Null,
    SymbolKind.String,
    SymbolKind.Variable,
  )
}
