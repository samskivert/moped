//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.io.{File, InputStream, PrintStream}
import java.net.URL
import java.nio.file.Path
import java.util.HashMap
import scala.collection.SortedSet

import moped._
import moped.grammar.Matcher

/** Contains the data for a TextMate language grammar. Certain elements are omitted as they do not
  * map directly to the way Moped handles languages. Generally one would create a language mode in
  * Moped, and then make use of the TextMate grammar to handle syntax highlighting and other
  * mechanisms.
  */
abstract class Grammar (
  val name               :String,
  val scopeName          :String,
  val foldingStartMarker :Option[String],
  val foldingStopMarker  :Option[String]
) {
  val repository :Map[String,Rule]
  val patterns   :List[Rule]

  override def toString = s"Grammar[$name, $scopeName]"

  /** Prints a debug representation of this grammar to `out`. */
  def print (out :PrintStream) :Unit = {
    val w = new NDF.Writer(out, 0)
    w.emit("name", name)
    w.emit("scopeName", scopeName)
    w.emit("foldStart", foldingStartMarker)
    w.emit("foldStop", foldingStopMarker)
    val rw = w.nest("repository")
    repository.toSeq.sortBy(_._1) foreach { case (k, v) => v.print(rw.nest(k)) }
    val pw = w.nest("patterns")
    patterns foreach { _.print(pw) }
  }

  /** Returns all scope names used by all rules in this grammar. */
  def scopeNames () :SortedSet[String] = {
    val names = SortedSet.newBuilder[String]
    repository.values foreach { _.collectNames(names) }
    patterns foreach { _.collectNames(names) }
    names.result()
  }

  /** A rule which includes a group of rules from the repository. */
  protected def include (group :String) :Rule = new Rule.Include(group)

  /** A rule that just contains more rules. This usually only shows up in the repository. */
  protected def rules (id :String, rules :List[Rule]) :Rule = new Rule.Container(id, rules)

  /** A rule which matches a regular expression on a single line, optionally assigning a name to the
    * entire regexp match, and to invidual capture groups identified by the regexp. */
  protected def single (pattern :String, name :Option[String] = None,
                        captures :List[(Int,String)] = Nil) :Rule = new Rule.Single(
    pattern, name, captures)

  /** A rule which matches a `begin` regular expression and an `end` regular expression (which may be
    * one separate lines), and which activates a new collection of rules when matching the code in
    * between `begin` and `end`. */
  protected def multi (begin :String, beginCaptures :List[(Int,String)] = Nil,
                       end :String, endCaptures :List[(Int,String)] = Nil,
                       name :Option[String] = None, contentName :Option[String] = None,
                       patterns :List[Rule] = Nil) :Rule = new Rule.Multi(
    begin, beginCaptures, end, endCaptures, name, contentName, patterns)

  /** A multiline rule which applies the same `captures` to the `begin` and `end` patterns. */
  protected def multi (begin :String, end :String, captures :List[(Int,String)],
                       name :Option[String], contentName :Option[String],
                       patterns :List[Rule]) :Rule = multi(
    begin, captures, end, captures, name, contentName, patterns)

  /** Useful for generating multiple rules that are variations on a theme. */
  protected def map (names :String*)(fn :String => Rule) :List[Rule] = List() ++ names map fn
}

object Grammar {
  import com.dd.plist._

  /** Used to compile grammars and resolve references to included grammars. */
  class Compiler (val grammar :Grammar, log :Logger, lookup :String => Compiler) {
    private val cache = new HashMap[String, List[Matcher]]()
    grammar.repository foreach { (k, v) => cache.put(k, v.compile(k, include)) }
    private def langPart (ref :String) = ref.indexOf('#') match {
      case -1 => ref
      case ii => ref.substring(0, ii)
    }

    lazy val matchers :List[Matcher] = grammar.patterns.flatMap(_.compile("patterns", include))

    def include (ref :String) :List[Matcher] = ref match {
      case "$self" => matchers
      case "$base" => matchers
      case group if (group `startsWith` "#") => cache.get(group `substring` 1) match {
        case null => log.log(s"Unknown include [grammar=${grammar.name}, group=$group]") ; Nil
        case ms   => ms
      }
      case lang => lookup(langPart(lang)) match {
        case null => log.log(s"Unknown include [grammar=${grammar.name}, lang=$lang]") ; Nil
        case comp => ref.indexOf('#') match {
          case -1 => comp.matchers
          case ii => comp.include(ref.substring(ii+1))
        }
      }
    }
  }

  /** Parses a `tmLanguage` grammar file which should be in NDF format. */
  def parseNDF (file :Path) :Grammar = NDFGrammar.toGrammar(NDF.read(file))
  /** Parses a `tmLanguage` grammar description which should be in NDF format. */
  def parseNDF (url :URL) :Grammar = NDFGrammar.toGrammar(NDF.read(url))

  /** Parses a `tmLanguage` grammar file which should be in plist XML format. */
  def parsePlist (file :File) :Grammar = PlistGrammar.parse(file)
  /** Parses a `tmLanguage` grammar description, which should be in plist XML format. */
  def parsePlist (in :InputStream) :Grammar = PlistGrammar.parse(in)

  /** Compiles a list of grammars and creates a scoper with them. For testing grammars. */
  def testScoper (grammars :Seq[Grammar], buffer :Buffer, procs :List[Selector.Processor]) = {
    val byScope = new HashMap[String, Grammar.Compiler]()
    val log = new Logger() {
      def log (msg :String) = println(msg)
      def log (msg :String, exn :Throwable) :Unit = { println(msg) ; exn.printStackTrace(System.err) }
    }
    val comps = grammars.map(g => new Grammar.Compiler(g, log, byScope.get))
    comps.foreach { c => byScope.put(c.grammar.scopeName, c) }
    new Scoper(comps.last.grammar, Matcher.first("patterns", comps.last.matchers), buffer, procs)
  }
}
