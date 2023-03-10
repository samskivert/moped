//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.util

import java.io._
import java.nio.file.Path

import scala.jdk.CollectionConverters._
import collection.mutable.ArrayBuffer

import moped._

/** Utilities for execing a process and obtaining its output. */
object Process {

  /** Communicates a process's exit code. */
  case class Result (exitCode :Int, stdout :Seq[String], stderr :Seq[String])

  /** Execs `cmd`, accumulates stdout and stderr into buffers until the proces completes and then
    * returns everything along with the process exit code.
    *
    * @param exec the Moped executor to use for background process handling.
    * @param cwd the directory in which to run the process, or null for whatever happens to be the
    * current working directory (i.e. it shouldn't matter). Yeah, null, sue me.
    * @param maxLines the maximum number of lines to accumulate for stdout or stderr. This avoids
    * craziness if the process decides to spew a million lines of output. The trimmed output will
    * contain the first maxLines/2 and the last maxLines/2 of output with a counter in the middle
    * of the number of omitted lines.
    */
  def exec (exec :Executor, cmd :Iterable[String], cwd :Path = null,
            maxLines :Int = 500) :Future[Result] = {
    val pb = new ProcessBuilder(cmd.toList.asJava)
    if (cwd != null) pb.directory(cwd.toFile)
    val proc = pb.start()
    val out = new RollingBuffer(maxLines)
    val err = new RollingBuffer(maxLines)
    var outReader = new Thread() {
      override def run () = out.slurp(proc.getInputStream)
    }
    outReader.start()
    var errReader = new Thread() {
      override def run () = err.slurp(proc.getErrorStream)
    }
    errReader.start()
    val result = exec.uiPromise[Result];
    exec.runInBG(() => {
      var exitCode = proc.waitFor()
      outReader.join()
      errReader.join()
      result.succeed(Result(exitCode, out.toSeq, err.toSeq))
    })
    result
  }

  class RollingBuffer (var maxLines :Int) {
    private final val BlockSize = 64
    private val header = ArrayBuffer[String]()
    private val footers = ArrayBuffer[ArrayBuffer[String]]()
    private var lineCount = 0

    def slurp (in :InputStream) :Unit = {
      try {
        val bin = new BufferedReader(new InputStreamReader(in))
        var line :String = null
        while ({ line = bin.readLine ; line } != null) append(line)
        bin.close()
      } catch {
        case e :Throwable =>
          var sout = new StringWriter
          e.printStackTrace(new PrintWriter(sout))
          append(e.getMessage)
          sout.toString.split(System.getProperty("line.separator")).foreach(append)
      }
    }

    def toSeq :Seq[String] = {
      if (lineCount > maxLines) {
        val footerCount = (BlockSize * (footers.size-1)) + footers.last.size
        header += s"...${lineCount-maxLines} omitted..."
        header ++= footers.head.drop(footerCount - maxLines/2)
        footers.drop(1).foreach(header ++= _)
      } else {
        footers.foreach(header ++= _)
      }
      header.toSeq
    }

    def append (line :String) :Unit = {
      if (lineCount < maxLines/2) header += line
      else {
        if (footers.isEmpty || footers.last.size == BlockSize) {
          if (BlockSize * (footers.size-1) >= maxLines/2) footers.dropInPlace(1)
          footers += ArrayBuffer[String]()
        }
        footers.last += line
      }
      lineCount += 1
    }
  }
}
