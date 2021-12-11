//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.util.concurrent.Executor

/**
 * Extends the [[Executor]] API with delayed scheduling.
 */
trait Scheduler extends Executor {

  /** Schedules `op` to run as soon as possible. */
  def execute (op :Runnable) :Unit

  /** Schedules `op` to run after `delay` milliseconds have elapsed.
    * @return a [Closeable] via which to cancel the operation (if it has not already executed). */
  def schedule (delay :Long, op :Runnable) :Closeable

  // TODO: do we want schedulePeriodically?
}
