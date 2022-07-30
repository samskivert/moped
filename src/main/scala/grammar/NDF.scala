//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import com.google.common.base.Charsets
import com.google.common.io.Resources
import java.io.{PrintStream}
import java.net.URL
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable.{Builder, ListBuffer}
import scala.jdk.CollectionConverters._

import moped._

/** Provides a simple nested dictionaries format for grammars. */
object NDF {

  /** Simplifies the process of writing NDF. */
  class Writer (out :PrintStream, depth :Int) {
    private val prefix = " " * depth
    private def escape (text :String) = text.replace("\n", "\\\n")

    /** Emits the supplied key/value pair at the current depth. */
    def emit (key :String, value :String) :Unit = {
      emitln(s"$key: ${escape(value)}")
    }

    /** Emits the supplied key/value pair iff `value` is defined. */
    def emit (key :String, value :Option[String]) :Unit = {
      if (value.isDefined) emit(key, value.get)
    }

    /** Emits the key for a nested dictionary and returns a writer that can be used to write the
      * contents of said dictionary. */
    def nest (key :String) :Writer = {
      emitln(s"$key:")
      new Writer(out, depth+1)
    }

    private def emitln (text :String) :Unit = {
      out.print(prefix)
      out.println(text)
    }
  }

  /** Contains data read from an NDF file. */
  sealed trait Entry {
    def key :String
    def dump (indent :String) :Unit
  }
  case class StrEnt (key :String, value :String) extends Entry {
    override def dump (indent :String) = println(s"$indent$key: $value")
  }
  case class DicEnt (key :String, values :Seq[Entry]) extends Entry {
    override def dump (indent :String) :Unit = {
      println(s"$indent$key:")
      values foreach { _.dump(s"$indent ") }
    }
  }

  /** Reads the contents of `file`, which must be in NDF format. */
  def read (file :Path) :Seq[Entry] = {
    read(List() ++ Files.readAllLines(file).asScala)
  }

  /** Reads the contents of `in`, which must be in NDF format. Closes `in` on completion. */
  def read (url :URL) :Seq[Entry] = {
    read(List() ++ Resources.readLines(url, Charsets.UTF_8).asScala)
  }

  /** Parses `lines` into a nested dictionary. */
  def read (lines :List[String]) :Seq[Entry] = {
    @tailrec def unbreak (ls :List[String], acc :ListBuffer[String]) :List[String] = ls match {
      case Nil => acc.toList
      case h :: t =>
        def merge (l1 :String, l2 :String) = l1.dropRight(1) + "\n" + l2
        if (t.isEmpty) { acc += h ; acc.toList }
        else if (h endsWith "\\") unbreak(merge(h, t.head) :: t.tail, acc)
        else { acc += h ; unbreak(t, acc) }
    }
    def split (line :String, pre :String) = line.indexOf(':') match {
      case -1 => Array(line) // invalid
      case ii =>
        val key = line.substring(pre.length, ii)
        if (ii == line.length-1) Array(key, "")
        else if (line.charAt(ii+1) != ' ') Array(line) // invalid
        else Array(key, line.substring(ii+2))
    }
    def read (lns :List[String], acc :Builder[Entry, Seq[Entry]], pre :String) :List[String] = lns match {
      case Nil => Nil
      case h :: t if (!h.startsWith(pre)) => lns
      case line :: rest => read(split(line, pre) match {
        case Array(key, value) =>
          if (value.length > 0) {
            acc += StrEnt(key, value)
            rest
          } else {
            val nacc = Seq.newBuilder[Entry]
            try read(rest, nacc, pre + " ")
            finally acc += DicEnt(key, nacc.result())
          }
        case s =>
          // complain and skip this line
          println(s"Invalid line: $line (prefix='$pre', split=${s.toList})")
          rest
      }, acc, pre)
    }
    val acc = Seq.newBuilder[Entry]
    read(unbreak(lines.filterNot(_.trim.startsWith("#")), ListBuffer[String]()), acc, "")
    acc.result()
  }
}
