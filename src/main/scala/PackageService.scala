//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import java.nio.file.Path
// import moped.util.BufferBuilder

/** Provides the ability to interact with the Moped Package Manager. */
@Service(name="package", impl="impl.PackageManager",
         desc="Provides the ability to interact with the Moped Package Manager.")
trait PackageService {

  /** Returns the directory in which extra code or data can be stored. LSP clients may need to
    * download extra bits to do their jobs, so they can ask for a directory in which to store said
    * bits via this method. */
  def extrasDir (id :String) :Path

  // /** Returns the directory in which the package with source `source` is installed.
  //   * @throws NoSuchElementException if no such package exists. */
  // def installDir (source :String) :Path

  // /** Returns the classpath for the package module with source `source`.
  //   * @throws NoSuchElementException if no such package exists. */
  // def classpath (source :String) :Seq[Path]

  // /** Adds debugging info on all loaded packages to `bb`. */
  // def describePackages (bb :BufferBuilder) :Unit
}
