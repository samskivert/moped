//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped.{Matcher, _}
import moped.code._
import moped.grammar._
import moped.util.{Chars, Errors}

@Major(name="java",
       tags=Array("code", "project", "java"),
       pats=Array(".*\\.java"),
       ints=Array("java"),
       desc="A major mode for editing Java language source code.")
class JavaMode (env :Env) extends GrammarCodeMode(env) {

  override def configDefs = JavaConfig :: super.configDefs

  override def langScope = "source.java"

  override def keymap = super.keymap.
    bind("import-type",       "C-c C-i")
  // bind("method-override",   "C-c C-m C-o").
  // bind("method-implement", "C-c C-m C-i")

  override def createIndenter () = new BlockIndenter(config, Seq(
    // bump extends/implements in two indentation levels
    BlockIndenter.adjustIndentWhenMatchStart(Matcher.regexp("(extends|implements)\\b"), 2),
    // align changed method calls under their dot
    new BlockIndenter.AlignUnderDotRule(),
    // handle javadoc and block comments
    new BlockIndenter.BlockCommentRule(),
    // handle indenting switch statements properly
    new BlockIndenter.SwitchRule() {
      override def indentCaseBlocks = config(JavaConfig.indentCaseBlocks)
    },
    // handle continued statements, with some special sauce for : after case
    new BlockIndenter.CLikeContStmtRule()
  ))

  override val commenter = new Commenter() {
    override def linePrefix = "//"
    override def blockOpen = "/*"
    override def blockClose = "*/"
    override def blockPrefix = "*"
    override def docOpen = "/**"
  }

  @Fn("Queries for a type (completed by the analyzer) and adds an import for it.")
  def importType () = {
    // TODO!
    // val intel = Intel.apply(buffer())
    // window().mini().read("Type:", wordAt(view().point().get()), wspace().historyRing("java-type"),
    //                      intel.symbolCompleter(Option.some(Kind.TYPE))).onSuccess(sym -> {
    //   ImportUtil.insertImport(buffer(), intel.fqName(sym))
    // })
  }

  /** Returns the "word" at the specified location in the buffer. */
  protected def wordAt (loc :Loc) =
    buffer.regionAt(loc, Chars.Word).map(line => line.asString).mkString
}

/** Configuration for java-mode. */
object JavaConfig extends Config.JavaDefs {

  @Var("If true, cases inside switch blocks are indented one step.")
  val indentCaseBlocks = key(false)
}

@Plugin
class JavaGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map(
    "source.java" -> "grammar/Java.ndf",
    "text.html.javadoc" -> "grammar/JavaDoc.ndf")

  override def effacers = List(
    // Java code colorizations
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("storage.type.java", typeStyle), // TODO: handle match-specificity (drop .java)
    effacer("storage.type.generic", typeStyle),
    effacer("storage.type.primitive", typeStyle),
    effacer("storage.type.object", typeStyle), // meh, colors array []s same as type...
    effacer("storage.type.annotation", preprocessorStyle),
    effacer("storage.modifier.java", keywordStyle),
    effacer("storage.modifier.package", moduleStyle),
    effacer("storage.modifier.extends", keywordStyle),
    effacer("storage.modifier.implements", keywordStyle),
    effacer("storage.modifier.import", typeStyle),

    effacer("entity.name.type.class", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function.java", functionStyle),

    effacer("variable.language", keywordStyle),
    effacer("variable", variableStyle),

    // Javadoc colorizations
    effacer("markup.underline", preprocessorStyle),
    effacer("markup.raw.code", preprocessorStyle),

    // HTML in Javadoc colorizations
    effacer("entity.name.tag", constantStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )
}

object ImportUtil {
  import scala.collection.mutable.{Buffer => SeqBuffer}

  val importM = Matcher.regexp("^import ")
  val packageM = Matcher.regexp("^package ")
  val firstDefM = Matcher.regexp("\\b(class|interface|@interface|enum)\\b")

  class ImportGroup (val firstRow :Int, val lines :Seq[String]) {
    var longestPrefix = Completer.longestPrefix(lines)

    def matchLength (newImport :String) = Completer.sharedPrefix(longestPrefix, newImport).length
    def lastRow = firstRow + lines.size

    def insert (buffer :Buffer, newImport :String, newLine :Line) = {
      var insertIdx = lines.indexWhere(ll => newImport.compareTo(ll) > 0)
      var newRow = firstRow + (if (insertIdx < 0) lines.size else insertIdx)
      buffer.insert(Loc.apply(newRow, 0), Seq(newLine, Line.Empty));
    }

    override def toString = s"[row=$firstRow, longPre=$longestPrefix, lines=$lines]"
  }

  /**
   * Finds the appropriate line on which to insert an import for {@code fqName} and inserts it. If
   * the imports are grouped by whitespace, this function first searches for the most appropriate
   * group, then inserts the import alphabetically therein. If no appropriate group can be found, a
   * new group is created. If no imports exist at all, the import is added after the package
   * statement (or the start of the buffer if no package statement can be found).
   */
  def insertImport (buffer :Buffer, fqName :String) :Unit = {
    val newImport = s"import $fqName;"
    val newLine = Line.apply(newImport)

    // first figure out where we're going to stop looking (at the first class, etc.)
    val firstDef = buffer.findForward(firstDefM, buffer.start, buffer.end)
    val stopRow = if (firstDef == Loc.None) buffer.lines.size else firstDef.row

    // parse all of the imports into groups
    val groups = SeqBuffer[ImportGroup]()
    val imports = SeqBuffer[String]()
    var firstRow = 0 ; var row = 0
    while (row < stopRow) {
      var line = buffer.line(row)
      if (line.matches(importM, 0)) {
        if (imports.isEmpty) firstRow = row
        imports.append(line.asString)
      } else if (!imports.isEmpty) {
        groups.append(new ImportGroup(firstRow, imports.toSeq))
        imports.clear()
      }
      row += 1
    }
    // if the crazy programmer has imports jammed up against the first class decl,
    // be sure to handle that case
    if (!imports.isEmpty) groups.append(new ImportGroup(firstRow, imports.toSeq))

    // if we have at least one group, find the one that has the longest shared prefix with our to be
    // inserted import and most likely insert our new import therein
    if (!groups.isEmpty) {
      val best = groups.maxBy(_.matchLength(newImport))
      // if we already have this import, then report that to the user
      if (best.lines.contains(newImport)) throw Errors.feedback(fqName + " already imported.")
      // make sure we either match at least one package level of our best group, or that the group
      // itself is a hodge-podge (its longest prefix does not contain one package level)
      val sharedPrefix = Completer.sharedPrefix(newImport, best.longestPrefix)
      if (sharedPrefix.contains(".") || !best.longestPrefix.contains(".")) {
        best.insert(buffer, newImport, newLine)
        return
      }
      // otherwise fall through and create a new group
    }

    // create a new group before the first group which sorts alphabetically after our import
    groups.find(_.lines.head.compareTo(newImport) >= 0) match {
      case Some(group) =>
        buffer.insert(Loc.apply(group.firstRow, 0), Seq(newLine, Line.Empty, Line.Empty))
      case None =>
        // if we haven't matched yet, insert after the last group, or if we have no last group,
        // after the package statement, and if we have no package statement, then at the very start
        val newRow = if (groups.isEmpty) findPackageRow(buffer, stopRow) else groups.last.lastRow
        buffer.insert(Loc.apply(newRow, 0),
                      if (newRow > 0) Seq(Line.Empty, newLine, Line.Empty)
                      else Seq(newLine, Line.Empty))
    }
  }

  private def findPackageRow (buffer :Buffer, stopRow :Int) =
    buffer.lines.take(stopRow).indexWhere(ll => ll.matches(packageM, 0)) match {
      case -1 => 0
      case ii => ii+1
    }
}
