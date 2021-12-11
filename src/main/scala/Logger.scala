//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

/** An injectable service that can be used for debug logging.
  *
  * User directed feedback should be delivered via [[Editor.emitStatus]], but internal logging
  * which is mainly for developer or end-user debugging can be sent here. It will be sent to the
  * `*messages*` buffer and when Moped is run in development mode, will also be logged to the
  * console.
  */
trait Logger {

  /** Records `msg` to the log. */
  def log (msg :String) :Unit

  /** Records `msg` and the stack trace for `exn` to the log. */
  def log (msg :String, exn :Throwable) :Unit
}
