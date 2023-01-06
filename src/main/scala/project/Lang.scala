//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.net.URI
import java.nio.file.Path
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

  /** Handles a rename refactoring in a single store. */
  abstract class Renamer (val store :Store) {
    /** Validates that this renamer can be applied to `buffer` (checking that its renames match
      * up with the buffer as expected).
      * @throw FeedbackException if something doesn't line up.
      */
    def validate (buffer :Buffer) :Unit
    /** Applies the rename to `buffer` (which will correspond to this renamer's `store`). */
    def apply (buffer :Buffer) :Unit
  }

  case class Symbol (
    kind :SymbolKind, name :String, fqName :String, sig :String, loc :LSP.URILoc
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
      LSP.URILoc(sym.getLocation))

    def apply (sym :WorkspaceSymbol) :Symbol = Symbol(
      sym.getKind, sym.getName,
      sym.getContainerName match {
        case null => LSP.toScala(sym.getLocation) match {
          case Left(loc) => s"${fileForLoc(loc)}:${sym.getName}"
          case Right(wsloc) => s"${fileForUri(wsloc.getUri)}:${sym.getName}"
        }
        case cont => s"${cont}.${sym.getName}"
      },
      formatSym(sym),
      LSP.URILoc(sym.getLocation))

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
