//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.util.Arrays

/** Models a limited quantity of syntax information on a per-character basis in a buffer.
  * This makes life easier for various code-grokking routines, which need to know if they're
  * looking at a comment, or a string literal, or actual code.
  *
  * A mode can make use of custom syntax instances if desired, but most general purpose code should
  * base behavior on the results of the syntax methods rather than object identity.
  */
abstract class Syntax {

  /** Returns true if this syntax represents a comment of some sort. */
  def isComment :Boolean

  /** Returns true if this syntax represents a char, string or integer literal. */
  def isLiteral :Boolean

  /** Returns true if this syntax is code (i.e. not a comment and not a literal). */
  def isCode :Boolean = !isComment && !isLiteral

  /** Returns true if this syntax matches the properties of `other`. */
  def matches (other :Syntax) :Boolean =
    (isComment == other.isComment) && (isLiteral == other.isLiteral)
}

/** Various standard syntax singletons. */
object Syntax {

  /** The default syntax. Interpreted as actual code. */
  val Default = new Syntax {
    def isComment = false
    def isLiteral = false
    override def toString = "Default"
  }

  /** A singleton [[Syntax]] instance for tagging line comments. */
  val LineComment = new Syntax {
    def isComment = true
    def isLiteral = false
    override def toString = "LineComment"
  }

  /** A singleton [[Syntax]] instance for tagging block comments. */
  val BlockComment = new Syntax {
    def isComment = true
    def isLiteral = false
    override def toString = "BlockComment"
  }

  /** A singleton [[Syntax]] instance for tagging doc comments. */
  val DocComment = new Syntax {
    def isComment = true
    def isLiteral = false
    override def toString = "DocComment"
  }

  /** A singleton [[Syntax]] instance for tagging string literals. */
  val StringLiteral = new Syntax {
    def isComment = false
    def isLiteral = true
    override def toString = "StringLiteral"
  }

  /** A singleton [[Syntax]] instance for tagging heredoc (multiline string) literals. */
  val HereDocLiteral = new Syntax {
    def isComment = false
    def isLiteral = true
    override def toString = "HereDocLiteral"
  }

  /** A singleton [[Syntax]] instance for tagging character literals. */
  val CharLiteral = new Syntax {
    def isComment = false
    def isLiteral = true
    override def toString = "CharLiteral"
  }

  /** A singleton [[Syntax]] instance for tagging non-string, non-char literals (like integer or
    * floating point literals). */
  val OtherLiteral = new Syntax {
    def isComment = false
    def isLiteral = true
    override def toString = "OtherLiteral"
  }

  /** Creates a syntax array of `length`, filled with `syn`. */
  def mkArray (length :Int, syn :Syntax) :Array[Syntax] = {
    val syns = new Array[Syntax](length)
    Arrays.fill(syns.asInstanceOf[Array[Object]], syn)
    syns
  }
}
