//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.snippet

import java.nio.file.Path
import moped._

/** Provides access to the database of snippets to snippet modes. */
@Service(name="snippet", impl="snippet.SnippetManager", desc="Manages snippet database.")
trait SnippetService {

  /** Returns all snippets that are applicable to `mode`, from highest to lowest precedence.
    * @param scope the config scope for the buffer using snippets.
    */
  def resolveSnippets (mode :String, scope :Config.Scope) :Seq[Snippet]

  /** Flushes cached snippets for the specified `mode` in the specified config root. */
  def flushSnippets (mode :String, root :Path) :Unit
}
