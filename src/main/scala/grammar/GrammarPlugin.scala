//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import java.io.FileNotFoundException
import java.net.URL
import moped._

/** A plugin that provides a particular TextMate grammar for use by [[GrammarCodeMode]] and
  * anything else that wants to style text. Should be tagged as `textmate-grammar`. */
abstract class GrammarPlugin extends AbstractPlugin {

  /** The grammars provided by this plugin: scope name -> grammar resource path. */
  def grammars :Map[String,String]

  /** Used to map grammar scopes to Moped effacers (styles). */
  def effacers :List[Selector.Fn] = Nil

  /** Used to map grammar scopes to Moped syntaxes. */
  def syntaxers :List[Selector.Fn] = Nil

  /** Parses and returns the grammar for `scopeName`, which must be a key in [[grammars]]. */
  def grammar (scopeName :String) :Grammar = Grammar.parseNDF({
    val path = grammars(scopeName)
    val url = getClass.getClassLoader.getResource(path)
    if (url == null) throw new FileNotFoundException(path) else url
  })

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that
    * applies `cssClass` to buffer spans matched by the selector. */
  protected def effacer (selector :String, cssClass :String) :Selector.Fn =
    new Selector.Fn(Selector.parse(selector)) {
      def apply (buf :Buffer, start :Loc, end :Loc) :Unit = buf.addStyle(cssClass, start, end)
      override def toString =  s"'${this.selector}' => $cssClass"
    }

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that
    * applies `syntax` to buffer spans matched by the selector. */
  protected def syntaxer (selector :String, syntax :Syntax) :Selector.Fn =
    new Selector.Fn(Selector.parse(selector)) {
      def apply (buf :Buffer, start :Loc, end :Loc) :Unit = buf.setSyntax(syntax, start, end)
      override def toString =  s"'${this.selector}' => $syntax"
    }
}
