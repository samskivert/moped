//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths, Path}
import moped._

/** Routines for dealing with very dumb config files that are just blank line separated blocks of
  * otherwise uninterpreted text.
  */
object ConfigFile {

  /** Reads `path` into a list of lists of strings. */
  def read (path :Path) :Seq[Seq[String]] = {
    val groups = Seq.newBuilder[Seq[String]]
    val lines = Seq() ++ Files.readAllLines(path).asScala
    var start = 0
    var end = 0
    while (end < lines.size) {
      if (lines(end).length == 0) {
        if (end - start > 0) {
          groups += lines.slice(start, end)
        }
        start = end+1
      }
      end += 1
    }
    if (end - start > 0) {
      groups += lines.slice(start, end)
    }
    groups.result
  }

  /** Writes the config `data` to `path` in a way that can be recovered by [[readConfig]]. */
  def write (path :Path, data :Iterable[Seq[String]]) :Unit = {
    val out = Files.newBufferedWriter(path)
    var first = true
    for (group <- data) {
      if (first) first = false
      else out.newLine() // blank separator
      for (line <- group) {
        out.write(line)
        out.newLine()
      }
    }
    out.close()
  }

  /** Reads `path` into a list of lists of strings and then turns that into a map where the first
    * string in each block is the key and the value is the remaining strings in the block. */
  def readMap (path :Path) :Map[String,Seq[String]] = {
    val map = Map.newBuilder[String,Seq[String]]
    for (group <- read(path)) {
      map += (group(0) -> group.drop(1))
    }
    map.result
  }

  /** Helper that makes it easy to write files that can be read using [[readMap]]. */
  class WriteMap (path :Path) {
    private val out = Files.newBufferedWriter(path)
    private var first = true

    def write (key :String, data :Iterable[String]) :Unit = {
      if (first) first = false
      else out.newLine() // blank separator
      out.write(key)
      out.newLine()
      for (line <- data) {
        out.write(line)
        out.newLine()
      }
    }

    def close () :Unit = {
      out.close()
    }
  }
}
