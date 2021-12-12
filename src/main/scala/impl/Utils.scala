//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import moped._

/** Utilities used by the Moped implementation code. */
object Utils {

  def safeSignal[T] (log :Logger) :Signal[T] = new Signal[T]() {
    override def emit (value :T) :Unit = try {
      super.emit(value)
    } catch {
      case t :Throwable => log.log(s"Signal.emit failure [value=$value]", t)
    }
  }
}
