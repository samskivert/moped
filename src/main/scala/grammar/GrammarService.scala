//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import moped._

@Service(name="textmate-grammar", impl="grammar.GrammarManager",
         desc="Provides a database of TextMate grammars for syntax highlighting.")
trait GrammarService {

  /** Returns the grammar for `langScope`, if one is available. */
  def grammar (langScope :String) :Option[Grammar]

  /** Removes any cached compiled grammar for `langScope`.
    * Used to force reload of a grammar when debugging it during mode development. */
  def resetGrammar (langScope :String) :Unit

  /** Creates a [[Scoper]] for `buffer` using `langScope` to identify the main grammar.
    * @return the scoper or `None` if no grammar is available for `langScope`. */
  def scoper (buffer :Buffer, langScope :String) :Option[Scoper] =
    scoper(buffer, langScope, info => List(info.effacers, info.syntaxers).
      flatMap(sels => if (sels.isEmpty) None else Some(new Selector.Processor(sels))))

  /** Creates a [[Scoper]] for `buffer` using `langScope` to identify the main grammar.
    * @param mkProcs a function to create custom line processors given the plugin metadata.
    * @return the scoper or `None` if no grammar is available for `langScope`. */
  def scoper (buffer :Buffer, langScope :String,
              mkProcs :GrammarInfo => List[Selector.Processor]) :Option[Scoper]
}
