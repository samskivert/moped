//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** Models a read-write property. This is useful if you want to expose reading and writing of a
  * [[Value]] without providing the ability to register reactions.
  */
trait Property[T] extends PropertyV[T] {

  /** Updates the value of this property. */
  def update (value :T) :T
}
