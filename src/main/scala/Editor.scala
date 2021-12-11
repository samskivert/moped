//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.util.HashMap

/** An abstraction over the OS clipboard. */
trait Clipboard {
  /** Reads the contents of the clipboard as a string, if available. */
  def read :Option[String]
  /** Updates the contents of the clipboard with `text`. */
  def set (text :String) :Unit
}

/** Provides access to data and services encapsulated by the editor. Not much is global to the
  * entire editor, most runtime state is divided into workspaces, and then further into windows,
  * frames and buffers. However, certain things (like the kill ring) are truly global.
  */
trait Editor {

  /** An event bus for editor-wide event dispatch. */
  val events :EventBus = new EventBus()

  /** A reactive mapping of editor-wide state. */
  val state :RState = new RState()

  /** Provides global config data. Keys are defined in [[EditorConfig]]. */
  def config :Config

  /** Enables running code on background or UI threads. */
  def exec :Executor

  /** Provides access to the OS clipoard. */
  def clipboard :Clipboard

  /** Displays the supplied URL in the user's preferred web browser. */
  def showURL (url :String) :Unit

  /** The ring in which killed regions are stored. */
  def killRing = Mutable.getOrPut(
    Rings(state), "kill", new KillRing(this, config(EditorConfig.killRingSize)))

  /** The ring in which killed rectangles are stored. */
  def rectKillRing = Mutable.getOrPut(
    Rings(state), "rect-kill", new Ring(config(EditorConfig.killRingSize)))

  // TODO: def workspaces :SeqV[Workspace]?

  /** A signal emitted when a workspace is opened. */
  def workspaceOpened :SignalV[Workspace]
}

/** Defines global configurables. */
object EditorConfig extends Config.Defs {

  @Var("""The default x position of editor views, in pixels.
          -1 indicates that the view should be centered in the screen.""")
  val viewLeft = key(-1)
  @Var("""The default y position of editor views, in pixels.
          -1 indicates that the view should be centered in the screen.""")
  val viewTop = key(-1)

  @Var("The default width of editor views, in characters.")
  val viewWidth = key(100)
  @Var("The default height of editor views, in characters.")
  val viewHeight = key(40)

  @Var("The number of entries retained by the kill ring.")
  val killRingSize = key(40)

  @Var("The number of entries retained in most minibuffer histories.")
  val historySize = key(40)

  /** The default CSS class name for text. */
  val textStyle = "textFace"
  /** The CSS class name for the active region face. */
  val regionStyle = "regionFace"
  /** The CSS class name for `warn` face. */
  val warnStyle = "warnFace"
  /** The CSS class name for `error` face. */
  val errorStyle = "errorFace"

  /** The CSS class name for (non-semantic) bold text. */
  val boldStyle = "boldFace"
  /** The CSS class name for (non-semantic) italicized text. */
  val italicStyle = "italicFace"
  /** The CSS class name for (non-semantic) underlined text. */
  val underlineStyle = "underlineFace"
  /** The CSS class name for (non-semantic) struck-through text. */
  val strikeStyle = "strikeFace"

  /** The CSS style applied to search matches. */
  val matchStyle = "matchFace"
  /** The CSS style applied to the active search match. */
  val activeMatchStyle = "activeMatchFace"
}
