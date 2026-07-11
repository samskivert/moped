//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import moped._
import moped.grammar.Matcher

import org.treesitter._

object Sitter {
  case class Scopes (scopes :List[String])

  /** Maps tree-sitter's UTF-8 byte offsets (how every `TSNode` position is expressed) back to
    * Moped's UTF-16 character offsets (how `Buffer`/`Loc` address text, matching how JVM
    * `String`s are indexed). For pure-ASCII text these coincide, but the moment a buffer contains
    * any multi-byte character (smart quotes, em-dashes, emoji, etc.) before a given point,
    * tree-sitter's byte offsets diverge from Moped's character offsets from there on, so this
    * conversion is needed everywhere a `TSNode`'s position is turned into a `Loc`. */
  private class ByteOffsets (source :String) {
    // byteAt(i) is the UTF-8 byte offset corresponding to character index i (0 to source.length)
    private val byteAt = new Array[Int](source.length+1)
    locally {
      var ii = 0 ; var byteOff = 0
      while (ii < source.length) {
        val cp = source.codePointAt(ii)
        val cc = Character.charCount(cp)
        byteAt(ii) = byteOff
        if (cc == 2) byteAt(ii+1) = byteOff // mid-surrogate; never a real node boundary
        byteOff += (
          if (cp < 0x80) 1 else if (cp < 0x800) 2 else if (cp < 0x10000) 3 else 4)
        ii += cc
      }
      byteAt(source.length) = byteOff
    }

    /** Converts a tree-sitter (UTF-8) byte offset to a Moped (UTF-16) character offset. Assumes
      * `byteOffset` falls exactly on a character boundary, which tree-sitter node positions
      * always do. */
    def toChar (byteOffset :Int) :Int = {
      var lo = 0 ; var hi = byteAt.length-1
      while (lo < hi) {
        val mid = (lo+hi)/2
        if (byteAt(mid) < byteOffset) lo = mid+1 else hi = mid
      }
      lo
    }
  }

  /** Returns the number of UTF-8 bytes needed to encode `cs`. */
  private def utf8Length (cs :CharSequence) :Int = {
    var ii = 0 ; var bytes = 0
    while (ii < cs.length) {
      val cp = Character.codePointAt(cs, ii)
      bytes += (if (cp < 0x80) 1 else if (cp < 0x800) 2 else if (cp < 0x10000) 3 else 4)
      ii += Character.charCount(cp)
    }
    bytes
  }

  /** Loads a tree-sitter grammar built by the `buildTreeSitterGrammars` sbt task (see
    * build.sbt) for a language that has no published tree-sitter-ng Java artifact (e.g. Swift,
    * Prisma). `name` is the grammar's build name (e.g. "swift") and `symbol` is the exported C
    * symbol from its generated parser.c (e.g. "tree_sitter_swift"). */
  def loadNative (name :String, symbol :String) :TSLanguage = {
    val ext = System.getProperty("os.name") match {
      case n if n.startsWith("Mac") => "dylib"
      case n if n.startsWith("Windows") => "dll"
      case _ => "so"
    }
    val fileName = s"libtree-sitter-$name.$ext"
    val path = nativeLibCandidates(fileName).find(Files.exists(_)).getOrElse(throw
      new IllegalStateException(
        s"Missing native tree-sitter grammar library: $fileName\n" +
        "Run `sbt buildTreeSitterGrammars` (or just `sbt compile`) to build it."))
    TSLanguage.load(path.toString, symbol)
  }

  // candidate locations for a built native grammar library, most-likely-first:
  //  - alongside whatever jar/classes this code itself loaded from: covers both the staged/
  //    packaged distribution (where build.sbt's Universal/mappings puts native libs in the same
  //    "lib" dir as the jars) and a jpackage'd .app (same layout, see macapp/create.sh)
  //  - native/build relative to the cwd: covers `sbt run`/`sbt test` during development, where
  //    code runs from an exploded classes dir, not a jar, and cwd is the project root
  private def nativeLibCandidates (fileName :String) :Seq[java.nio.file.Path] = {
    val besideCode = try {
      val codeLoc = Paths.get(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
      val dir = if (Files.isRegularFile(codeLoc)) codeLoc.getParent else codeLoc
      Some(dir.resolve(fileName))
    } catch { case _ :Throwable => None }
    val devBuild = Paths.get(System.getProperty("user.dir"), "native", "build", fileName)
    besideCode.toSeq :+ devBuild
  }
}

type Styler = (List[String]) => String
type Syntaxer = (List[String]) => Syntax

/** Uses tree-sitter to parse the code in a buffer and apply scopes based on the results of the
  * parse. Then also listens for modifications to the buffer and updates the scopings to
  * accommodate the changes.
  *
  * @param procs a list of processors that will be applied first to the whole buffer, then to any
  * parts of the buffer that are rescoped due to the buffer being edited.
  */
class Sitter (
  lang :TSLanguage, buf :Buffer, stylers :Map[String, Styler], syntaxers :Map[String, Syntaxer]
) {

  /** Returns the scope names applied to `loc` in outer- to inner-most order. */
  def scopesAt (loc :Loc) :List[String] =
    buf.tagAt(classOf[Sitter.Scopes], loc).map(_.scopes).getOrElse(Nil).reverse

  /** Re-matches and re-faces the entire buffer. */
  def rethinkBuffer () :Unit = reparseAndRestyle(force = true, 0, buf.lines.size)

  /** Re-matches and re-faces the region from line `from` to line `to` (non-inclusive). */
  def rethinkRegion (from :Int, to :Int) :Unit = reparseAndRestyle(force = true, from, to)

  /** Re-matches and re-faces the region from line `from` to line `to` (non-inclusive). Tree-sitter
    * always parses the entire buffer text, so (unlike the TextMate/NDF `Scoper`) there's no
    * "isolated, no inherited state" parse mode to speak of; this is equivalent to `rethinkRegion`
    * and exists for interface parity with `Scoper`. */
  def rethinkIsolatedRegion (from :Int, to :Int) :Unit = rethinkRegion(from, to)

  /** Connects this sitter to `buf`, using `didInvoke` to batch refacing. */
  def connect (buf :RBuffer, didInvoke :SignalV[String]) :this.type = {
    assert(this.buf eq buf)
    // listen for changes to the buffer and incrementally inform our tree of edits as they happen
    buf.edited.onValue(processEdit)
    // when a fn completes, reparse (incrementally) and refresh styling for whatever tree-sitter
    // reports as actually changed
    didInvoke.onEmit(processRethinks())
    // parse and face the whole buffer for the first time
    reparseAndRestyle(force = true, 0, buf.lines.size)
    this
  }

  override def toString = s"Sitter($lang, $buf)"

  protected def isModeStyle (style :String) = style `startsWith` "code"

  private val parser = { val p = new TSParser() ; p.setLanguage(lang) ; p }

  // the tree matching the buffer's current content, once we've parsed at least once. Kept alive
  // and incrementally updated (via TSTree.edit, in processEdit) as edits arrive, so that the next
  // reparse can reuse tree-sitter's unchanged subtrees instead of parsing from scratch.
  private var curTree :TSTree = null

  // `TSPoint`'s row is a plain line count (unaffected by UTF-8/UTF-16 encoding, since newlines are
  // always single bytes), but its column is formally a UTF-8 byte offset within the row, whereas
  // `loc.col` is a UTF-16 character offset. We deliberately don't convert it: Sitter never reads
  // `TSNode`/`TSRange`'s column (only `getRow`, e.g. in `reparseAndRestyle`), and computing a true
  // byte column for the *old* (pre-edit) end of a delete/transform would require reconstructing
  // buffer content that no longer exists. Byte-accurate offsets are still supplied for the fields
  // that are actually depended on: the linear `start`/`oldEnd`/`newEnd` byte offsets below.
  private def toPoint (loc :Loc) = new TSPoint(loc.row, loc.col)

  // the number of characters spanned by `lines`, per Moped's convention (used by Buffer.offset and
  // Loc.+) that a line separator consumes a single character
  private def textLength (lines :Iterable[LineV]) :Int =
    lines.map(_.length).sum + math.max(0, lines.size-1)

  // the number of UTF-8 bytes spanned by `lines`; mirrors `textLength`, but tree-sitter's
  // `TSInputEdit` byte-offset fields need a byte count here, not a character count
  private def utf8TextLength (lines :Iterable[LineV]) :Int =
    lines.map(Sitter.utf8Length).sum + math.max(0, lines.size-1)

  // converts `loc` (which must currently exist in `buf`) into a UTF-8 byte offset; mirrors
  // `Buffer.offset`'s character-offset computation, since `TSInputEdit`'s linear fields are byte
  // offsets but `Buffer` only speaks UTF-16 character offsets
  private def byteOffset (loc :Loc) :Int = {
    @tailrec def go (row :Int, off :Int) :Int =
      if (row < 0) off else go(row-1, Sitter.utf8Length(buf.line(row))+1+off)
    go(loc.row-1, 0) + Sitter.utf8Length(buf.line(loc.row).view(0, loc.col))
  }

  private def toInputEdit (edit :Buffer.Edit) :TSInputEdit = edit match {
    case Buffer.Insert(start, end) =>
      val sb = byteOffset(start)
      new TSInputEdit(sb, sb, byteOffset(end), toPoint(start), toPoint(start), toPoint(end))
    case Buffer.Delete(start, end, deleted) =>
      // `end` (== start + deleted) is the *old* (pre-delete) end of the removed text; the buffer
      // has already been mutated by the time this fires, so we can't ask it where `end` used to
      // be, we have to compute the old byte offset from `deleted`'s own length instead
      val sb = byteOffset(start)
      new TSInputEdit(
        sb, sb + utf8TextLength(deleted), sb, toPoint(start), toPoint(end), toPoint(start))
    case Buffer.Transform(start, end, orig) =>
      // transforms are length-preserving (`end` is defined as `start + orig`, and per Transform's
      // own undo(), `[start,end)` remains valid in the buffer after the transform is applied), so
      // the old and new end of the edit are the same position
      val sb = byteOffset(start) ; val eb = byteOffset(end)
      new TSInputEdit(sb, eb, eb, toPoint(start), toPoint(end), toPoint(end))
  }

  // informs our live tree of edits as they happen so the next reparse can be incremental; edits
  // must be applied to the tree in the order they occur, which is exactly the order in which
  // `buf.edited` notifies us, so we can simply apply each one as it arrives
  private def processEdit (edit :Buffer.Edit) :Unit =
    if (curTree != null) curTree.edit(toInputEdit(edit))

  // reparses (incrementally, per the edits noted since the last parse) and refaces only the rows
  // that tree-sitter reports actually changed as a result
  private def processRethinks () :Unit = reparseAndRestyle(force = false, 0, 0)

  // reparses the buffer, reusing `curTree` (if we have one) so that tree-sitter can avoid
  // re-parsing subtrees unaffected by the edits applied to it since the last parse. If `force` is
  // true (or this is our first ever parse), styling is refreshed for all of `[restyleFrom,
  // restyleTo)`; otherwise only the rows tree-sitter reports as changed between the old and new
  // trees are refaced, which is the common (and cheap) case for routine edit-driven reparses.
  private def reparseAndRestyle (force :Boolean, restyleFrom :Int, restyleTo :Int) :Unit = {
    val source = Line.toText(buf.lines)
    val oldTree = curTree
    val newTree = parser.parseString(oldTree, source)
    // tree-sitter reports node positions as UTF-8 byte offsets; Buffer only speaks UTF-16
    // character offsets, so we need this mapping to translate node positions back to Locs
    val byteOffsets = new Sitter.ByteOffsets(source)
    try {
      if (force || oldTree == null) restyle(newTree, byteOffsets, restyleFrom, restyleTo)
      else TSTree.getChangedRanges(oldTree, newTree).foreach { range =>
        restyle(newTree, byteOffsets, range.getStartPoint.getRow, range.getEndPoint.getRow+1)
      }
    } finally {
      if (oldTree != null) oldTree.close()
      curTree = newTree
    }
  }

  // clears old styles/scopes from rows [from,to) and reapplies styling for any tree node whose
  // span overlaps those rows; nodes entirely outside the range (and their children, which can
  // never extend beyond their parent) are skipped
  private def restyle (
    tree :TSTree, byteOffsets :Sitter.ByteOffsets, from :Int, to :Int
  ) :Unit = if (to > from) {
    val start = Loc(from, 0) ; val until = Loc(to, 0)
    buf.removeTags(classOf[String], isModeStyle, start, until)
    buf.removeTags(classOf[Sitter.Scopes], _ => true, start, until)

    def process (node :TSNode, scopes :List[String]) :Unit = {
      val sloc = buf.loc(byteOffsets.toChar(node.getStartByte))
      val eloc = buf.loc(byteOffsets.toChar(node.getEndByte))
      if (sloc.row < to && eloc.row >= from) {
        stylers.get(node.getType).flatMap(styler => Option(styler(scopes))).
          foreach(style => buf.addStyle(style, sloc, eloc))
        syntaxers.get(node.getType).flatMap(taxer => Option(taxer(scopes))).
          foreach(tax => buf.setSyntax(tax, sloc, eloc))
        val nscopes = node.getType :: scopes
        if (node.getChildCount > 0) {
          for (ii <- 0 until node.getChildCount) process(node.getChild(ii), nscopes)
        }
        // uncomment to add scopes to every node; costly but needed when setting up a new language
        // else buf.addTag(Sitter.Scopes(nscopes), sloc, eloc)
      }
    }
    process(tree.getRootNode, Nil)
  }
}
