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

  /** Adds a datum to the mode line whose text is rendered as a sequence of independently styled
    * segments (e.g. a warning count in one color immediately followed by an error count in
    * another), rather than a single uniformly styled string as with [[addDatum]]. An empty
    * sequence of segments renders as nothing, so the datum can disappear entirely when there's
    * nothing to show.
    *
    * @param value the reactive sequence of segments to be displayed.
    * @param tooltip a tooltip to be displayed to the user when they hover over the datum.
    * @param onClick if supplied, invoked when the user clicks the datum; a hand cursor is shown
    * over the datum in that case, to signal that it's clickable.
    *
    * @return an [[Closeable]] which will remove the datum from the modeline when closed.
    * A mode is advised to `note()` this closeable so that it will automatically be removed if
    * the mode is deactivated. Fire and forget!
    */
  def addStyledDatum (
    value :ValueV[Seq[ModeLine.Segment]], tooltip :ValueV[String],
    onClick :Option[() => Unit] = None
  ) :Closeable
}

object ModeLine {

  /** A single run of styled text within a datum added via [[ModeLine.addStyledDatum]].
    * @param style a CSS style class to apply to this run (e.g. `EditorConfig.warnStyle`), or the
    * empty string to use the mode line's default text styling. */
  case class Segment (text :String, style :String = "")

  /** A modeline that ignores the caller. Used in situations where a mode has no associated mode
    * linee (like minibuffer modes) or when testing.
    */
  val Noop = new ModeLine() {
    override def addDatum (value :ValueV[String], tooltip :ValueV[String]) = Closeable.Noop
    override def addStyledDatum (
      value :ValueV[Seq[Segment]], tooltip :ValueV[String], onClick :Option[() => Unit]
    ) = Closeable.Noop
  }
}
