//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.nio.file.Path
import moped._

/** Provides miscellaneous project services.
  * Chiefly project resolution, and some other global bits and bobs. */
@Service(name="project", impl="project.ProjectManager", autoLoad=true,
         desc="Provides miscellaneous project services.")
trait ProjectService {

  def pathsFor (store :Store) :Option[List[Path]]
  def findRoots (paths :List[Path]) :Seq[Project.Root]
  def resolveByPaths (paths :List[Path]) :Project.Root
  def resolveById (id :Project.Id) :Option[Project.Root]
  def unknownProject (ps :ProjectSpace) :Project
}
