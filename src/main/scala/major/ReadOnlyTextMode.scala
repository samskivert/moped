//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

/** A base class for modes that want to use {@code TextMode} styles, but which are read-only. This
  * extends [[ReadingMode]] rather than [[EditingMode]]. */
abstract class ReadOnlyTextMode (env :Env) extends ReadingMode(env) {

  override def stylesheets = stylesheetURL("/text.css") :: super.stylesheets
}
