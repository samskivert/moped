//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** Moped services must extend this class so that they can be notified of lifecycle events. */
abstract class AbstractService {

  /** A callback invoked when a service is first started by Moped. */
  def didStartup () :Unit

  /** A callback invoked when a service is about to be shutdown by Moped. */
  def willShutdown () :Unit
}
