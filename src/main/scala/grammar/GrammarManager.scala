//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import java.util.HashMap
import moped._
import moped.grammar.Matcher

class GrammarManager (
  msvc :MetaService, psvc :PluginService
) extends AbstractService with GrammarService {

  private val plugins = new HashMap[String,GrammarPlugin]()
  private def pluginAdded (plugin :GrammarPlugin) = plugin.grammars.keySet foreach {
    scope => plugins.put(scope, plugin) }
  private def pluginRemoved (plugin :GrammarPlugin) = plugin.grammars.keySet foreach {
    scope => if (plugins.get(scope) == plugin) plugins.remove(scope) }

  private val plugset = psvc.resolvePlugins[GrammarPlugin]("textmate-grammar")
  plugset.plugins foreach pluginAdded
  plugset.added onValue pluginAdded
  plugset.removed onValue pluginRemoved

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
