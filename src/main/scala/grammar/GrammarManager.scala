//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.util.HashMap
import moped._
import moped.grammar.Matcher

class GrammarManager (msvc :MetaService) extends AbstractService with GrammarService {

  private val plugins = new HashMap[String, GrammarPlugin]()
  for (plugin <- msvc.resolvePlugins(classOf[GrammarPlugin])) {
    plugin.grammars.keySet foreach { scope => plugins.put(scope, plugin) }
  }

  private val comps = new HashMap[String,Grammar.Compiler]()
  private def compiler (scope :String) :Grammar.Compiler =
    Mutable.getOrPut(comps, scope, plugins.get(scope) match {
      case null => null
      case plugin => new Grammar.Compiler(plugin.grammar(scope), msvc.log, compiler)
    })

  override def didStartup () :Unit = {}
  override def willShutdown () :Unit = {}

  override def grammar (langScope :String) :Option[Grammar] =
    Option(compiler(langScope)).map(_.grammar)

  override def resetGrammar (langScope :String) :Unit = comps.remove(langScope)

  override def scoper (buffer :Buffer, langScope :String,
                       mkProcs :GrammarPlugin => List[Selector.Processor]) :Option[Scoper] =
    for (plugin <- Option(plugins.get(langScope)) ; comp = compiler(langScope))
    yield new Scoper(comp.grammar, Matcher.first(langScope, comp.matchers), buffer, mkProcs(plugin))
}
