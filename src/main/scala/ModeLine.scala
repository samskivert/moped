//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** Displays mode summary information to the user. The default information includes:
  * ` - BufferName.ext  L#  [majormode]` and modes can augment the modeline with additional text.
  * This should be used sparingly, as the mode line is not infinite in length.
  */
trait ModeLine {

  /** Adds a label displaying the current value of `value` to the mode line.
    *
    * @param value the reactive value to be displayed.
    * @param tooltip a tooltip to be displayed to the user when they hover over the datum.
    *
    * @return an [[Closeable]] which will remove the datum from the modeline when closed.
    * A mode is advised to `note()` this closeable so that it will automatically be removed if
    * the mode is deactivated. Fire and forget!
    */
  def addDatum (value :ValueV[String], tooltip :ValueV[String]) :Closeable
}

object ModeLine {

  /** A modeline that ignores the caller. Used in situations where a mode has no associated mode
    * linee (like minibuffer modes) or when testing.
    */
  val Noop = new ModeLine() {
    override def addDatum (value :ValueV[String], tooltip :ValueV[String]) = Closeable.Noop
  }
}
