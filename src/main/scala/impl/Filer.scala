//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.function.Consumer

import collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters._

import moped._

/** Helper routines for working with the file system. */
object Filer {

  /** Maintains a value that has a file backing store. */
  abstract class FileDB[T] (path :Path) {

    /** Returns the current value (loaded lazily). */
    def apply () :T = {
      if (_value == null) read()
      _value.nn
    }
    /** Replaces the current value with `value`, and [[write]]s it. */
    def update (value :T) :Unit = {
      _value = value
      write()
    }
    /** Updates the current value by applying `fn` to it. Then [[write]]s the result. */
    def update (fn :T => T) :Unit = update(fn(apply()))

    /** Reads the backing file into a new value. Replaces the current value therewith. */
    def read () :Unit = {
      val lines :Iterable[String] = try {
        if (Files.exists(path)) Files.readAllLines(path).asScala else Seq()
      } catch {
        case e :Exception => e.printStackTrace(System.err) ; Seq()
      }
      _value = decode(lines)
    }
    /** Writes the current value to the backing file. */
    def write () :Unit = try {
      if (_value != null) Files.write(path, encode(_value.nn).asJava, StandardCharsets.UTF_8)
    } catch {
      case e :Exception => e.printStackTrace(System.err)
    }

    protected def decode (lines :Iterable[String]) :T
    protected def encode (value :T) :Iterable[String]

    private var _value :T | Null = null
  }

  /** Creates a [[FileDB]] with the specified encoder and decoder fns. */
  def fileDB[T] (path :Path, dec :Iterable[String] => T, enc :T => Iterable[String]) =
    new FileDB[T](path) {
      protected def decode (lines :Iterable[String]) = dec(lines)
      protected def encode (value :T) = enc(value)
    }

  /** Ensures that `dir` exists and is a directory.
    * Terminates the editor with an error message on failure. */
  def requireDir (dir :Path) :Path = {
    if (!Files.exists(dir)) Files.createDirectory(dir)
    else if (Files.isDirectory(dir)) dir
    else fail(s"$dir should be a directory but is not.")
  }

  /** Applies `op` to all subdirectories, subsubdirectories, etc of `root`. If `op` returns false,
    * we descend into the directory, if it returns true we do not. */
  def descendDirs (root :Path)(op :Path => Boolean) :Unit = {
    val seen = MSet[Path]()
    def apply (dir :Path) :Unit = if (seen.add(dir)) Files.list(dir).forEach(new Consumer[Path] {
      def accept (p :Path) = if (Files.isDirectory(p)) { if (!op(p)) apply(p) }
    })
    apply(root)
  }

  /** Applies `op` to all files in `root` and in subdirectories (and subsubdirectories) thereof. */
  def descendFiles (root :Path)(op :Path => Unit) :Unit = {
    val seen = MSet[Path]()
    def apply (dir :Path) :Unit = if (seen.add(dir)) Files.list(dir).forEach(new Consumer[Path] {
      def accept (p :Path) = if (Files.isDirectory(p)) apply(p)
                             else op(p)
    })
    apply(root)
  }

  private def fail (msg :String) :Nothing = {
    System.err.println(s"$msg Moped cannot operate without this directory.")
    sys.exit(255)
  }
}
