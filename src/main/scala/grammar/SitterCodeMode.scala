//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import moped._
import moped.code.CodeMode
import moped.util.Errors

import ch.usi.si.seart.treesitter._

/** Extends [[CodeMode]] with support for using TreeSitter parsers for code highlighting. Code
  * major modes which intend to use TreeSitter parsers for code highlighting and other purposes
  * should extend this class rather than [[CodeMode]].
  */
abstract class SitterCodeMode (env :Env) extends CodeMode(env) {
  Sitter.loadLibrary()

  /** The TreeSitter language id for this mode. */
  def langId :Language

  /** Used to map tree-sitter node types to Moped buffer styles. */
  def styles :Map[String, Styler] = Map()

  /** Used to map tree-sitter node types to Moped syntaxes. */
  def syntaxes :Map[String, Syntaxer] = Map()

  /** Handles parsing the buffer and applying styles and syntaxes. */
  val sitter = Sitter(langId, buffer, styles, syntaxes).connect(buffer, disp.didInvoke)

  protected def always (style :String) = (scopes :List[String]) => style

  override def keymap = super.keymap.
    bind("show-scopes", "M-A-p") // TODO: also M-PI?

  @Fn("Displays the TreeSitter node scopes at the point.")
  def showScopes () :Unit = {
    val ss = sitter.scopesAt(view.point())
    val text = if (ss.isEmpty) List("No scopes.") else ss
    view.popup() = Popup.text(text, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () :Unit = sitter.rethinkBuffer()
}
