//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.snippet

import java.lang.{StringBuffer => JStringBuilder}
import java.util.regex.{Pattern, Matcher}
import scala.collection.mutable.{ArrayBuffer, Builder}

import moped._
import moped.util.Chars

/** Tags a primary hole in a snippet. */
case class Hole (loc :Loc, deflen :Int, mirrors :Seq[Loc])
// TODO: reify mirrors, and include transforms

/** Metadata for a snippet. */
case class Snippet (
  /** A user friendly name for this snippet. */
  name :String,
  /** The trigger strings for this snippet. */
  triggers :Set[String],
  /** The syntax predicate for this snippet. */
  syntaxes :Syntax => Boolean,
  /** The activation line constraints. */
  lmode :Snippet.LineMode,
  /** The raw template for this snippet. */
  template :Seq[String]) {

  /** A fn that decides whether a match should be aborted because it looks like the snippet has
    * already been expanded. */
  lazy val stop :(LineV, Int) => Boolean = {
    val first = template.head
    val start = first.indexOf("$") match {
      case -1 => first
      case nn => first.substring(0, nn)
    }
    val offs = triggers.map(start.indexOf).filter(_ >= 0)
    if (offs.isEmpty) (line :LineV, start :Int) => false
    else {
      val m = moped.Matcher.exact(start)
      (line :LineV, start :Int) => offs.exists(off => start >= off && line.matches(m, start-off))
    }
  }

  /** Returns true if this snippet can be activated on `line` where the trigger matched at column
    * `start` and the point is at `pos`. */
  def canActivate (line :LineV, start :Int, pos :Int) :Boolean =
    lmode.canActivate(line, start, pos) && !stop(line, start)

  /** Inserts this snippet into `buffer` at `loc`. Returns the list of holes in the inserted
    * snippet. The final hole will be the "exit" point of the snippet (indicated by `$0` in the raw
    * template).
    */
  def insert (buffer :Buffer, loc :Loc) :(Seq[Hole], Loc) = {
    // we delay parsing until insertion because it's not that bad to parse one snippet just before
    // we insert it, but parsing hundreds of snippets at startup would be troublesome
    val holes = Seq.newBuilder[(Int,Int,Loc)]
    var end = loc ; var exit = Loc.None
    val iter = template.iterator ; while (iter.hasNext) {
      val text = iter.next
      val m = Snippet.HolePat.matcher(text) ; var pos = 0
      var lb :Line.Builder = null
      while (m.find(pos)) {
        val mpos = m.start ; val pre = text.substring(pos, mpos)
        if (lb == null) lb = Line.builder(pre)
        else lb += pre
        val g2 = m.group(2) ; val g3 = m.group(3)
        val id = (if (g2 == null) g3 else g2).toInt
        // if the id is zero, then this is the exit point of the snippet
        if (id == 0) exit = end + (0, lb.length)
        else {
          // if we have group(4) then it's a hole with default, otherwise its just a hole
          val defstr = m.group(4)
          def add (str :String, len :Int) :Unit = {
            holes += ((id, len, end + (0, lb.length)))
            lb += str
          }
          if (defstr == null) add("", 0) else add(defstr, defstr.length)
        }
        pos = m.end
      }
      val line = if (lb == null) Line(text) else lb.append(text.substring(pos)).build()
      end = buffer.insert(end, line)
      // if this is the last line and the exit is at the end of this line, then don't insert a
      // final line break; otherwise do
      if (iter.hasNext || exit != end) end = buffer.split(end)
    }

    // if there was no $0, put one at the end of the snippet
    if (exit == Loc.None) exit = end

    // now group the holes by id and figure out which are mirrors
    val hmap = holes.result.groupBy(_._1)
    val sb = Seq.newBuilder[Hole] // (hmap.size+1)
    hmap foreach { (_, holes) =>
      // determine which is the primary hole:
      // either the one with the default value, or the first one in the list
      val main = holes.find(_._2 > 0) getOrElse holes.head
      sb += Hole(main._3, main._2, holes.filterNot(_ == main).map(_._3))
    }
    sb += Hole(exit, 0, Seq()) // add the faux "exit" hole
    (sb.result, end)
  }

  override def toString = s"Snippet($name, $triggers)\n${template.mkString("\n")}"
}

object Snippet {

  /** Contains the data from a `.snip` file. */
  case class Source (name :String, includes :Set[String], snippets :Seq[Snippet])

  /** Models line-related restrictions on when a snippet can be activated. */
  sealed trait LineMode {
    def canActivate (line :LineV, start :Int, pos :Int) :Boolean
  }
  case object Alone extends LineMode {
    override def canActivate (line :LineV, start :Int, pos :Int) =
      line.indexOf(Chars.isNotWhitespace) == start && EOL.canActivate(line, start, pos)
  }
  case object EOL extends LineMode {
    override def canActivate (line :LineV, start :Int, pos :Int) =
      line.indexOf(Chars.isNotWhitespace, pos) == -1
  }
  case object Inline extends LineMode {
    override def canActivate (line :LineV, start :Int, pos :Int) = true
  }

  private val IncludeKey = "%include:"
  private val NameKey  = "%name:"
  private val KeysKey  = "%keys:"
  private val SynsKey  = "%syns:"
  private val LineKey  = "%line:"
  private val AllSyntaxes = (_ :Syntax) => true

  // example .snip file
  // %include: c-like java-like
  // %name: if else block
  // %keys: ife ifel
  // %syns: default
  // %line: eol
  // if (${1:condition}) $2 else $3
  // (optional blank line)
  // %name: ...

  /** Parses `lines`, which ostensibly represent the contents of a `.snip` file.
    * @return the set of includes specified in the file, if any.
    */
  def parse (lines :Iterable[String], into :Builder[Snippet, Seq[Snippet]]) :Set[String] = {
    val includes = Set.newBuilder[String]
    var ll = lines.toList ; while (!ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) ll = parseSnippet(ll, into += _)
      else {
        if (tline startsWith IncludeKey) includes ++= getval(tline, IncludeKey).split(" ")
        else if ({ val tt = tline.trim ; (tt startsWith "#") || (tt.length == 0)}) () // skip
        ll = ll.tail
      }
    }
    includes.result
  }

  private def getval (line :String, key :String) = line.substring(key.length).trim

  private val HolePat = Pattern.compile("""\$(([0-9]+)|\{([0-9]+):([^}]+)\})""")

  private[snippet] def parseSnippet (lines :List[String], recv :Snippet => Unit) = {
    var ll = lines // we'll move the line cursor along as we parse

    var name = ""
    var triggers = Set[String]()
    var syntaxes = AllSyntaxes
    var line :LineMode = Alone
    val bbuf = ArrayBuffer[String]()

    def parseSyntaxes (syns :String) = AllSyntaxes // TODO
    def parseLine (line :String) :LineMode = line.toLowerCase match {
      case "eol"    => EOL
      case "inline" => Inline
      case _ => Alone
    }

    // first parse the parameters
    var cont = true ; while (cont && !ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) name = getval(tline, NameKey)
      else if (tline startsWith KeysKey) triggers = Set.from(getval(tline, KeysKey).split(" "))
      else if (tline startsWith SynsKey) syntaxes = parseSyntaxes(getval(tline, SynsKey))
      else if (tline startsWith LineKey) line = parseLine(getval(tline, LineKey))
      // else complain if line looks like %foo:?
      else cont = false
      if (cont) ll = ll.tail
    }

    // next parse the body
    cont = true ; while (cont && !ll.isEmpty) {
      val tline = ll.head
      if (tline startsWith NameKey) cont = false
      else bbuf += ll.head
      if (cont) ll = ll.tail
    }
    // if the last line of the snippet is blank, remove it
    if (bbuf.last.length == 0) bbuf.dropRightInPlace(1)

    // TODO: validate things?

    // finally create a snippet and pass it to the receiver fn
    recv(Snippet(name, triggers, syntaxes, line, bbuf.toSeq))

    ll
  }
}
