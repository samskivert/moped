//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import moped._
import moped.grammar.Matcher

import ch.usi.si.seart.treesitter._

object Sitter {
  private var loaded = false

  def loadLibrary () = {
    if (!loaded) LibraryLoader.load()
    loaded = true
  }

  case class Scopes (scopes :List[String])
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
  lang :Language, buf :Buffer, stylers :Map[String, Styler], syntaxers :Map[String, Syntaxer]
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

  protected def isModeStyle (style :String) = style startsWith "code"
  private val allStyles = stylers.values.toSet

  // rethinks row; if end of row state changed, rethinks the next row as well; &c
  private def cascadeRethink (row :Int, stopRow :Int, firstRow :Int, force :Boolean) :Unit = {
    // TEMP rethink whole buffer
    val parser = Parser.getFor(lang)
    try {
      val source = Line.toText(buf.lines)
      val tree = parser.parse(source)

      // val states = buf.lines.map(_ => new Span.State(Nil))
      // def process (node :Node, scopes :List[String]) :Unit = {
      //   val children = node.getChildCount
      //   if (children == 0) {
      //     val sloc = buf.loc(node.getStartByte)
      //     val eloc = buf.loc(node.getEndByte)
      //     if (sloc.row != eloc.row) println("Dropping multi-line terminal node ${node.getType}")
      //     else {
      //       states(sloc.row).spans += Span(nscopes, sloc.col, eloc.col)
      //     }
      //   } else for (ii <- 0 until node.getChildCount) process(node.getChild(ii), nscopes)
      // }
      // process(tree.getRootNode, Nil)
      // buf.lines.indices.foreach(row => {
      //   val state = states(row)
      //   setState(row, state)
      //   state.apply(procs, buf, row)
      // })
      buf.removeTags(classOf[String], isModeStyle, buf) // TOOD: row/stopRow
      buf.removeTags(classOf[Sitter.Scopes], _ => true, buf) // TOOD: row/stopRow
      def process (node :Node, scopes :List[String]) :Unit = {
        val sloc = buf.loc(node.getStartByte)
        val eloc = buf.loc(node.getEndByte)
        stylers.get(node.getType).foreach(styler => buf.addStyle(styler(scopes), sloc, eloc))
        syntaxers.get(node.getType).foreach(taxer => buf.setSyntax(taxer(scopes), sloc, eloc))
        val nscopes = node.getType :: scopes
        if (node.getChildCount > 0) {
          for (ii <- 0 until node.getChildCount) process(node.getChild(ii), nscopes)
        }
        else buf.addTag(Sitter.Scopes(nscopes), sloc, eloc)
      }
      process(tree.getRootNode, Nil)

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
