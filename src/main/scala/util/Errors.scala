//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.util

import java.io.{PrintWriter, StringWriter}
import moped._

/** Error reporting utilities. */
object Errors {

  /** An exception reported to provide error feedback rather than indicate catastrophic failure.
    * [[Window.emitError]] will report such exceptions to the user but not dump their stack trace
    * for debugging.
    */
  class FeedbackException (msg :String) extends RuntimeException(msg)

  /** Returns true if `t` is a feedback exception. */
  def isFeedback (t :Throwable) :Boolean = t.isInstanceOf[FeedbackException]

  /** Creates an exception to report the supplied error message. */
  def feedback (msg :String) = new FeedbackException(msg)

  /** Creates a [[Future]] which will fail with a the feedback message `msg`. */
  def futureFeedback[T] (msg :String) :Future[T] = Future.failure(feedback(msg))

  /** Converts `exn`'s stack trace into a string. */
  def stackTraceToString (exn :Throwable) :String = {
    val trace = new StringWriter()
    exn.printStackTrace(new PrintWriter(trace))
    trace.toString
  }

  /** Converts `exn`'s stack trace into lines. */
  def stackTraceToLines (exn :Throwable) :Seq[LineV] = Line.fromTextNL(stackTraceToString(exn))
}
