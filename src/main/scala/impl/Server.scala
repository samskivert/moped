//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import java.net.ServerSocket
import scala.io.Source

/** Hosts a simple server on localhost:[[Moped.Port]]. Currently accepts only a single command:
  * `open PATH` which causes `PATH` to be opened in the most recently used editor on the currently
  * active desktop (if Moped can figure out what desktop is active).
  */
class Server (app :Moped) extends Thread {
  setDaemon(true)

  override def run () :Unit = {
    val port = Moped.Port
    try {
      val ssock = new ServerSocket(port)
      app.logger.log(s"Listening for commands on localhost:$port")
      while (true) {
        val csock = ssock.accept()
        try Source.fromInputStream(csock.getInputStream(), "UTF-8").getLines foreach process
        finally csock.close()
      }

    } catch {
      case e :Exception => app.logger.log(s"Failed to bind to $port", e)
    }
  }

  private def process (cmd :String) :Unit = {
    if (cmd `startsWith` "open ") onMainThread {
      app.wspMgr.visit(app.wspMgr.resolve(cmd.substring(5).trim))
    }
    else app.logger.log(s"Unknown command: '$cmd'")
  }
}
