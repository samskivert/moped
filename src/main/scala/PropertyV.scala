//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** Provides a read-only view of a property. This is useful if you want to expose reading of a
  * [[ValueV]] without providing the ability to register reactions.
  */
trait PropertyV[+T] {

  /** Returns the current value of this property. */
  def get :T

  /** Returns the current value of this property. This is a synonym for [[get]] so that one can use
    * Scala's special apply syntax (e.g. `myprop()` instead of `myprop.get`). */
  def apply () :T
}
