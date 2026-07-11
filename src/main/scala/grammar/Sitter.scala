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
  def rethinkBuffer () :Unit = cascadeRethink(0, buf.lines.size, 0, true)

  /** Re-matches and re-faces the region from line `from` to line `to` (non-inclusive). */
  def rethinkRegion (from :Int, to :Int) :Unit = cascadeRethink(from, to, 0, true)

  /** Re-matches and re-faces the region from line `from` to line `to` (non-inclusive) as if it
    * were the only contents of the buffer. The first line of the region is treated as having no
    * inherited scoping state. Used for highlighting code embedded in Markdown, etc. */
  def rethinkIsolatedRegion (from :Int, to :Int) :Unit = cascadeRethink(from, to, from, true)

  /** Connects this sitter to `buf`, using `didInvoke` to batch refacing. */
  def connect (buf :RBuffer, didInvoke :SignalV[String]) :this.type = {
    assert(this.buf eq buf)
    // listen for changes to the buffer and note the region that needs rethinking
    buf.edited.onValue(processEdit)
    // when a fn completes, rethink any changes we noted during edit notifications
    didInvoke.onEmit(processRethinks())
    // compute states for all of the starting rows (TODO: turn this into something that happens
    // lazily the first time a line is made visible...)
    cascadeRethink(0, buf.lines.size, 0, false)
    this
  }

  override def toString = s"Sitter(${lang}, $buf)"

  private var rethinkStart = Int.MaxValue
  private var rethinkEnd = -1

  private def processEdit (edit :Buffer.Edit) = edit match {
    case Buffer.Insert(start, end) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, end.row)
    case Buffer.Delete(start, end, _) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, start.row)
    case Buffer.Transform(start, end, _) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, end.row)
  }

  // private def processRethinks () = try {
  //   if (rethinkEnd >= rethinkStart) {
  //     var row = rethinkStart ; val end = rethinkEnd
  //     while (row <= end) { setState(row, rethink(row, 0)) ; row += 1 }
  //     cascadeRethink(row, buf.lines.size, 0, false)
  //     rethinkStart = Int.MaxValue
  //     rethinkEnd = -1
  //   }
  // } catch {
  //   case e :Throwable =>
  //     println(s"Rethink choked (for $this)")
  //     e.printStackTrace()
  // }
  private def processRethinks () = rethinkBuffer() // TEMP

  // private def rethink (row :Int, firstRow :Int) :Span.State = {
  //   // println(s"RETHINK $row ${buf.lines(row)}")
  //   val pstate = if (row == firstRow) topState else curState(row-1)
  //   val state = pstate.continue(buf.lines(row))
  //   state.apply(procs, buf, row)
  //   state
  // }

  protected def isModeStyle (style :String) = style `startsWith` "code"
  private val allStyles = stylers.values.toSet

  // rethinks row; if end of row state changed, rethinks the next row as well; &c
  private def cascadeRethink (row :Int, stopRow :Int, firstRow :Int, force :Boolean) :Unit = {
    // TEMP rethink whole buffer
    val parser = new TSParser()
    parser.setLanguage(lang)
    try {
      val source = Line.toText(buf.lines)
      val tree = parser.parseString(null, source)
      try {
        buf.removeTags(classOf[String], isModeStyle, buf) // TOOD: row/stopRow
        buf.removeTags(classOf[Sitter.Scopes], _ => true, buf) // TOOD: row/stopRow
        def process (node :TSNode, scopes :List[String]) :Unit = {
          val sloc = buf.loc(node.getStartByte)
          val eloc = buf.loc(node.getEndByte)
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
        process(tree.getRootNode, Nil)
      } finally tree.close()
    } finally {
      parser.close()
    }

    // if (row < stopRow) {
    //   try {
    //     val ostate = curState(row) ; val nstate = rethink(row, firstRow)
    //     setState(row, nstate)
    //     if (ostate == null || force || (ostate nequiv nstate)) cascadeRethink(
    //       row+1, stopRow, firstRow, force)
    //   } catch {
    //     case ex :Exception =>
    //       println(s"Cascade rethink died [row=$row, stop=$stopRow, first=$firstRow, force=$force]")
    //       ex.printStackTrace()
    //   }
    // }
  }
}
