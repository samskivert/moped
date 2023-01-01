//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.{Files, Path}
import java.util.function.Consumer
import moped._
import moped.util.{Close, MoreFiles}

/** Creates clients for language servers. */
abstract class LangPlugin extends AbstractPlugin {

  /** The set of file suffixes handled by this language server.
    * @param root the root of the project in which the langserver will operate. */
  def suffs (root :Project.Root) :Set[String]

  /** Returns whether this language server can be activated in the supplied project `root`. If a
    * language server requires configuration files to exist, this is the place to check.
    * @param root the root of the project for which a langserver is sought. */
  def canActivate (root :Project.Root) :Boolean

  /** Whether or not the clients created by this plugin are specific to the particular module via
    * which they are instantiated (`true`) or whether they can serve requests for all modules that
    * share the same project `root` (`false`). */
  def moduleSpecific :Boolean = false

  /** Creates a language client for the supplied `project`.
    * @param project the project requesting a langserver. */
  def createClient (project :Project) :Future[LangClient]
}
