//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.util.concurrent.Executor

/** A signal that emits events of type `T`. */
class Signal[T] extends SignalV[T] {

  /** Causes this signal to emit the supplied event to connected slots.
    * @throws $EXNDOC */
  def emit (event :T) = notifyEmit(event)
}

/** Helper methods for signals. */
object Signal {

  /** Creates a signal instance. */
  def apply[T] () = new Signal[T]

  /** Creates a signal instance which dispatches events via `exec`. */
  def apply[T] (exec :Executor) = new Signal[T]() {
    override def emit (event :T) = exec.execute(new Runnable() {
      override def run () = notifyEmit(event)
    })
  }
}
