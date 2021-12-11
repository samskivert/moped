//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

object TextConfig extends Config.Defs {

  /** The CSS style applied to `header` lines. */
  val headerStyle = "textHeaderFace"
  /** The CSS style applied to `subHeader` lines. */
  val subHeaderStyle = "textSubHeaderFace"
  /** The CSS style applied to `section` lines. */
  val sectionStyle = "textSectionFace"
  /** The CSS style applied to `list` lines. */
  val listStyle = "textListFace"
  /** The CSS style applied to highlighted prefixes. */
  val prefixStyle = "textPrefixFace"
  /** The CSS style applied to links. */
  val linkStyle = "textLinkFace"
  /** The CSS style applied to references. */
  val refStyle = "textRefFace"
}

@Major(name="text", tags=Array("text"), desc="""
  A major mode for editing plain text. This is used when no more specific mode can be found.
""")
class TextMode (env :Env) extends EditingMode(env) {

  override def configDefs = TextConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/text.css") :: super.stylesheets
}
