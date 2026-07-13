//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import scala.annotation.nowarn

import org.eclipse.lsp4j.{MarkedString, MarkupContent}
import org.eclipse.lsp4j.jsonrpc.messages.Either

import moped._
import moped.code.CodeConfig
import moped.grammar.GrammarService
import moped.util.Filler

/** General purpose formatting of LSP hover/completion text (plain text, markdown, and fenced code
  * blocks) into a `Buffer`, with appropriate styling applied.
  *
  * This also handles reflowing overly-wide code lines: the language server protocol has no notion
  * of "pretty print this to N columns", so a language server's hover/signature text can come back
  * with a line arbitrarily wider than the view (a classic case is a long function signature, e.g.
  * a heavily overloaded DOM method).
  */
object Format {

  /** Formats (and styles) a `text` block, appending it to `buffer`. */
  def format (buffer :Buffer, wrapWidth :Int, text :String) :Buffer = {
    text.split(System.getProperty("line.separator")) foreach { line =>
      if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
      val filler = new Filler(wrapWidth)
      filler.append(line)
      val start = buffer.end
      val end = buffer.insert(start, filler.toLines)
      buffer.addStyle(CodeConfig.docStyle, start, end)
    }
    buffer
  }

  /** Formats a marked `code` block, appending it to `buffer`. */
  @nowarn def format (
    buffer :Buffer, wrapWidth :Int, code :MarkedString, grammarSvc :GrammarService
  ) :Buffer = formatCode(
    buffer, wrapWidth, unescapeHtml(code.getValue), "source." + code.getLanguage, grammarSvc)

  /** Formats `markup`, appending it to `buffer`. */
  def format (
    buffer :Buffer, wrapWidth :Int, markup :MarkupContent, grammarSvc :GrammarService
  ) :Buffer = markup.getKind match {
    case "markdown"      => formatMarkdown(buffer, wrapWidth, markup.getValue, grammarSvc)
    case _ /*plaintext*/ => format(buffer, wrapWidth, markup.getValue)
  }

  /** Formats `either` a text or code block, appending it to `buffer`. A bare string here is a
    * `MarkedString`, which per the LSP spec is markdown text, not plain text; some servers (e.g.
    * java-language-server) use this older, pre-`MarkupContent` hover format for markdown docs. */
  @nowarn def format (
    buffer :Buffer, wrapWidth :Int, either :Either[String, MarkedString], grammarSvc :GrammarService
  ) :Buffer = LSP.toScala(either) match {
    case Left(text) => formatMarkdown(buffer, wrapWidth, text, grammarSvc)
    case Right(mark) => format(buffer, wrapWidth, mark, grammarSvc)
  }

  // some servers (e.g. java-language-server) emit raw HTML entities in hover markdown, even inside
  // fenced code blocks where they're presumably meant to render as literal characters (we've seen
  // `&nbsp;` used this way to keep an argument list from wrapping); decode the common ones so hover
  // text doesn't show literal entity text
  private val htmlEntities = Seq(
    "&nbsp;" -> " ", "&amp;" -> "&", "&lt;" -> "<", "&gt;" -> ">", "&quot;" -> "\"", "&#39;" -> "'")
  private def unescapeHtml (text :String) :String =
    htmlEntities.foldLeft(text) { case (t, (ent, ch)) => t.replace(ent, ch) }

  /** Formats a markdown `text` block, appending it to `buffer`. */
  def formatMarkdown (
    buffer :Buffer, wrapWidth :Int, rawText :String, grammarSvc :GrammarService
  ) :Buffer = {
    val text = unescapeHtml(rawText)
    def format (
      buffer :Buffer, wrapWidth :Int, lines :Seq[String], iter :Int
    ) :Unit = {
      if (lines.size > 0) {
        val open = lines.indexWhere(l => l.startsWith("```"), 0)
        if (open == -1) formatMarkdownText(buffer, wrapWidth, lines.mkString("\n"), grammarSvc)
        else {
          val close = lines.indexWhere(l => l.startsWith("```"), open+1)
          if (close == -1) formatMarkdownText(buffer, wrapWidth, lines.mkString("\n"), grammarSvc)
          else {
            if (open > 0) formatMarkdownText(
              buffer, wrapWidth, lines.take(open).mkString("\n"), grammarSvc)
            val code = lines.slice(open+1, close).mkString("\n")
            val scope = lines(open).substring(3).trim()
            if (iter > 0) { buffer.split(buffer.end) ; buffer.split(buffer.end) }
            formatCode(buffer, wrapWidth, code, if (scope == "") "text" else s"source.$scope",
                       grammarSvc)
            val rest = lines.drop(close+1)
            if (rest.size > 0) {
              if (iter > 0) { buffer.split(buffer.end) ; buffer.split(buffer.end) }
              format(buffer, wrapWidth, rest, iter+1)
            }
          }
        }
      }
    }
    format(buffer, wrapWidth, Seq.from(text.split("\n")), 0)
    buffer
  }

  /** Formats a chunk of non-fenced-code markdown `text`, wrapping it to `wrapWidth` and
    * highlighting it with the same grammar `markdown-mode` uses, so that inline constructs
    * (code spans, emphasis, links, headers, etc.) are styled the way they are in a `.md` buffer.
    * The whole chunk is given a baseline "doc comment" style (the same one applied to plain,
    * non-markdown hover text) so that prose isn't rendered in the buffer's plain text color;
    * markdown-specific styles are layered on top of (not instead of) that baseline. */
  private def formatMarkdownText (
    buffer :Buffer, wrapWidth :Int, text :String, grammarSvc :GrammarService
  ) :Unit = {
    text.split(System.getProperty("line.separator")) foreach { line =>
      if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
      val filler = new Filler(wrapWidth)
      filler.append(line)
      val start = buffer.end
      val end = buffer.insert(start, filler.toLines)
      buffer.addStyle(CodeConfig.docStyle, start, end)
      grammarSvc.scoper(buffer, "text.html.markdown").foreach(
        _.rethinkIsolatedRegion(start.row, end.row+1))
    }
  }

  /** Formats a styled `code` block using TextMate `scope`, appending it to `buffer`. Lines wider
    * than `wrapWidth` are reflowed first (see [[reflowCode]]) so that, e.g., a hover popup showing
    * an overload signature doesn't just run off the edge of the view. */
  def formatCode (
    buffer :Buffer, wrapWidth :Int, code :String, scope :String, grammarSvc :GrammarService
  ) :Buffer = {
    if (buffer.lines.length > 0 && buffer.lines.last.length > 0) buffer.split(buffer.end)
    val start = buffer.end
    val end = buffer.insert(start, Line.fromText(reflowCode(code, wrapWidth)))
    grammarSvc.scoper(buffer, scope).foreach(_.rethinkIsolatedRegion(start.row, end.row+1))
    buffer
  }

  /** Reflows `code` so that no line exceeds `wrapWidth` characters (a no-op if `wrapWidth <= 0`).
    * Lines that look like a call/definition signature (`prefix(arg1, arg2, ...)suffix`) are broken
    * after the opening paren, one argument per line, since that's how a human would reformat them
    * for a C-like language; anything else (or a signature-shaped line that still doesn't fit once
    * broken up) is wrapped generically, breaking at spaces where possible and hard-breaking
    * otherwise. */
  def reflowCode (code :String, wrapWidth :Int) :String =
    if (wrapWidth <= 0) code else code.split("\n", -1).map(reflowLine(_, wrapWidth)).mkString("\n")

  private def reflowLine (line :String, wrapWidth :Int) :String =
    if (line.length <= wrapWidth) line
    else reflowSignature(line, wrapWidth) getOrElse reflowGeneric(line, wrapWidth)

  // wraps `line` at spaces where possible, hard-breaking a single overlong "word" if need be; the
  // fallback for lines that don't look like (or don't benefit from being treated as) a signature
  private def reflowGeneric (line :String, wrapWidth :Int) :String = {
    val lead = line.takeWhile(_ == ' ')
    val filler = new Filler(math.max(1, wrapWidth-lead.length))
    filler.append(line.trim)
    filler.filled.map(lead + _).mkString("\n")
  }

  // TypeScript (and some other servers') hover signatures are conventionally prefixed with a bare
  // parenthesized tag, e.g. "(method) Foo.bar(...)" or "(property) baz: ...", which isn't the
  // signature's own parameter list; skip it so we don't mistake it for one
  private val leadingTag = """^\(\w+\)\s+""".r

  // attempts to reformat a signature-shaped line (`prefix(arg1, arg2, ...)suffix`) by breaking
  // after the opening paren and putting one argument per line; None if the line has no parens, or
  // breaking it up wouldn't actually help (no arguments to spread across lines)
  private def reflowSignature (line :String, wrapWidth :Int) :Option[String] = {
    val searchFrom = leadingTag.findPrefixMatchOf(line).map(_.end) getOrElse 0
    val open = line.indexOf('(', searchFrom)
    if (open < 0) None else matchingParen(line, open+1) match {
      case -1 => None
      case close =>
        val args = splitTopLevel(line.substring(open+1, close), ',').map(_.trim).filter(_.nonEmpty)
        if (args.isEmpty) None else {
          val lead = line.takeWhile(_ == ' ')
          val argLead = lead + "    "
          val prefixLine = reflowGeneric(line.substring(0, open+1), wrapWidth)
          val argLines = args.init.map(a => reflowGeneric(argLead+a, wrapWidth)+",") :+
            reflowGeneric(argLead+args.last, wrapWidth)
          val suffixLine = reflowGeneric(lead+line.substring(close), wrapWidth)
          Some((prefixLine +: argLines :+ suffixLine).mkString("\n"))
        }
    }
  }

  // finds the index of the ')' matching the '(' whose contents start at `from`, or -1 if unmatched
  private def matchingParen (s :String, from :Int) :Int = {
    var depth = 1 ; var ii = from
    while (ii < s.length && depth > 0) {
      s.charAt(ii) match {
        case '(' => depth += 1
        case ')' => depth -= 1
        case _ =>
      }
      ii += 1
    }
    if (depth == 0) ii-1 else -1
  }

  // splits `s` on `sep`, ignoring occurrences nested inside (), [], {} or <> (so that e.g. a
  // callback parameter's own comma-separated argument list doesn't get split as if it were one of
  // our top-level arguments); treats '>' as a closer only when it's not part of an arrow ('=>'), so
  // that arrow function types don't get misread as closing a generic
  private def splitTopLevel (s :String, sep :Char) :Seq[String] = {
    val parts = Seq.newBuilder[String]
    var depth = 0 ; var start = 0 ; var ii = 0
    while (ii < s.length) {
      s.charAt(ii) match {
        case '(' | '[' | '{' | '<' => depth += 1
        case ')' | ']' | '}' => depth -= 1
        case '>' if ii == 0 || s.charAt(ii-1) != '=' => depth -= 1
        case c if c == sep && depth == 0 => parts += s.substring(start, ii) ; start = ii+1
        case _ =>
      }
      ii += 1
    }
    parts += s.substring(start)
    parts.result()
  }

  /** Formats a `docs` string, appending to `buffer`. May contain newlines. */
  def formatDocs (buffer :Buffer, wrapWidth :Int, docs :String) :Buffer = format(buffer, wrapWidth, docs)

  /** Formats `either` a text or markup block, appending it to `buffer`. */
  def formatDocs (
    buffer :Buffer, wrapWidth :Int, either :Either[String, MarkupContent], grammarSvc :GrammarService
  ) :Buffer = LSP.toScala(either) match {
    case Left(text) => format(buffer, wrapWidth, text)
    case Right(mark) => format(buffer, wrapWidth, mark, grammarSvc)
  }

  /** Formats a `detail` string into a signature (to be shown next to the completion text).
    * Any newlines must be removed. Truncated (with an ellipsis) if overly long, since this is
    * meant to fit on one line alongside the completion's label. */
  def formatSig (detail :String) :LineV = {
    val flat = Filler.flatten(detail)
    val MaxLen = 60
    Line.apply(if (flat.length <= MaxLen) flat else flat.take(MaxLen-1) + "…")
  }
}
