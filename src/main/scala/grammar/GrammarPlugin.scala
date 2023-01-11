//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.io.FileNotFoundException
import java.net.URL
import moped._

/** A plugin that provides a particular TextMate grammar for use by [[GrammarCodeMode]] and
  * anything else that wants to style text. */
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

  /** Compiles `seldef` into a TextMate grammar selector and pairs it with a function that
    * applies `cssClass` to buffer spans matched by the selector. */
  protected def effacer (seldef :String, cssClass :String) :Selector.Fn =
    new Selector.Fn(Selector.parse(seldef)) {
      def apply (buf :Buffer, start :Loc, end :Loc) :Unit = buf.addStyle(cssClass, start, end)
      override def toString =  s"'$selector' => $cssClass"
    }

  /** Compiles `seldef` into a TextMate grammar selector and pairs it with a function that
    * applies `syntax` to buffer spans matched by the selector. */
  protected def syntaxer (seldef :String, syntax :Syntax) :Selector.Fn =
    new Selector.Fn(Selector.parse(seldef)) {
      def apply (buf :Buffer, start :Loc, end :Loc) :Unit = buf.setSyntax(syntax, start, end)
      override def toString =  s"'$selector' => $syntax"
    }
}
