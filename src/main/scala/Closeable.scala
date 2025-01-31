//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** A "refinement" of [[java.lang.AutoCloseable]] that does not throw exceptions on [[close]]. */
trait Closeable extends AutoCloseable {

  /** Performs the deferred close operation. */
  def close () :Unit
}

object Closeable {

  /** A closeable that does nothing. Simplifies situations where you close an old closeable and
    * replace it with a new one. */
  val Noop :Closeable = new Closeable() {
    override def close () :Unit = {}
  }

  /** Creates a [[Closeable]] that invokes `thunk` when `close` is called. */
  def apply[U] (thunk : => U) = new Closeable() {
    def close () = thunk
  }
}
