//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import moped._
import moped.util.BufferBuilder
import java.nio.file.{Path, Paths, Files}

/** A component added to projects that have Java (or JVM-language) code. */
abstract class JavaComponent (project :Project) extends Project.Component {

  /** One or more directories or jar files that contain all classes provided by this project. */
  def classes :Seq[Path]
  /** The directories or jar files needed to compile this project. Note: this <em>should not</em>
    * contain the jar files or directories in {@link #classes}. */
  def buildClasspath :Seq[Path]
  /** The directories or jar files needed to run code in this project. Note: this <em>should</em>
    * contain the jar files and directories in {@link #classes}. */
  def execClasspath :Seq[Path]

  /** Adds any standard (Java) testing components to this project. This should be called after the
    * [[buildClasspath]] has been updated. */
  def addTesters () = {
    // JUnitTester.addComponent(project, this)
    // TestNGTester.addComponent(project, this)
  }

  override def describeSelf (bb :BufferBuilder) = {
    bb.addSubHeader("Java Info")
    bb.addSection("Classes:")
    classes.foreach(p => bb.add(p.toString()))
    bb.addSection("Build classpath:")
    buildClasspath.foreach(p => bb.add(p.toString()))
    bb.addSection("Exec classpath:");
    execClasspath.foreach(p => bb.add(p.toString()))
  }

  override def close () = {} // nada
}

/** Defines additional persistent data for a JVM language project. */
case class JavaMeta (classes :Seq[Path], buildClasspath :Seq[Path], execClasspath :Seq[Path])

  /** Handles reading and writing [[JavaMeta]]s. */
object JavaMetaMeta extends Project.MetaMeta[JavaMeta] {

  def zero (project :Project) = new JavaMeta(Seq(), Seq(), Seq())

  def read (in :Map[String,Seq[String]]) = new JavaMeta(
    in.apply("classes").map(pathFromString),
    in.apply("buildClasspath").map(pathFromString),
    in.apply("execClasspath").map(pathFromString)
  )

  def write (out :ConfigFile.WriteMap, meta :JavaMeta) = {
    out.write("classes", meta.classes.map(pathToString))
    out.write("buildClasspath", meta.buildClasspath.map(pathToString))
    out.write("execClasspath", meta.execClasspath.map(pathToString))
  }

  private def pathFromString (path :String) = Paths.get(path)
  private def pathToString (path :Path) = path.toString
}

/** A Java component that stores its metadata persistently. */
class JavaMetaComponent (project :Project) extends JavaComponent(project) {

  /** Tracks Java-specific project metadata. */
  val javaMetaV = project.metaValue("java-meta", JavaMetaMeta)

  override def classes = javaMetaV.get.classes
  override def buildClasspath = javaMetaV.get.buildClasspath
  override def execClasspath = javaMetaV.get.execClasspath
}
