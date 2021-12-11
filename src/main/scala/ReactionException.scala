//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.io.{PrintStream, PrintWriter}

/** Communicates failures from one or more reactors. */
class ReactionException extends RuntimeException {

  override def getMessage = {
    val buf = new StringBuilder
    val sup = getSuppressed
    for (failure <- sup) {
      if (buf.length > 0) buf.append(", ")
      buf.append(failure.getClass.getName).append(": ").append(failure.getMessage)
    }
    s"${sup.length} failures: $buf"
  }

  override def fillInStackTrace () :Throwable = this // no stack trace here
}
