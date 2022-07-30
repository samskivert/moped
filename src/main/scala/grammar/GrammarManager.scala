//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import java.util.HashMap
import moped._
import moped.grammar.Matcher

class GrammarManager (msvc :MetaService) extends AbstractService with GrammarService {

  private def repo (scopeName :String) :GrammarInfo = scopeName match {
    case "source.ndf" => new NDFGrammarInfo()
    case "source.scala" => new ScalaGrammarInfo()
    case _ => null
  }

  private val comps = new HashMap[String,Grammar.Compiler]()
  private def compiler (scope :String) :Grammar.Compiler =
    Mutable.getOrPut(comps, scope, repo(scope) match {
      case null => null
      case plugin => new Grammar.Compiler(plugin.grammar, msvc.log, compiler)
    })

  override def didStartup () :Unit = {}
  override def willShutdown () :Unit = {}

  override def grammar (langScope :String) :Option[Grammar] =
    Option(compiler(langScope)).map(_.grammar)

  override def resetGrammar (langScope :String) :Unit = comps.remove(langScope)

  override def scoper (buffer :Buffer, langScope :String,
                       mkProcs :GrammarInfo => List[Selector.Processor]) :Option[Scoper] =
    for (info <- Option(repo(langScope)) ; comp = compiler(langScope))
    yield new Scoper(comp.grammar, Matcher.first(langScope, comp.matchers), buffer, mkProcs(info))
}
