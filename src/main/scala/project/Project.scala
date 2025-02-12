//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import com.google.common.collect.{HashMultimap, Multimap}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.security.MessageDigest
import java.util.{HashMap, TreeMap}
import java.util.function.Consumer
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import moped._
import moped.util.{BufferBuilder, Close, MoreFiles, Errors}

/** [[Project]]-related helper types &c. */
object Project {

  /** Represents different kinds of universal project identifiers. */
  sealed abstract class Id

  /** An id string to use for [[RepoId.repo]] for the Maven repository. */
  final val MavenRepo = "mvn"
  /** An id string to use for [[RepoId.repo]] for the Ivy repository. */
  final val IvyRepo = "ivy"

  /** Identifies a project via an artifact repository identifier.
    * The canonical example of this kind of ID is a Maven/Ivy style dependency. */
  case class RepoId (repo :String, groupId :String, artifactId :String,
                     version :String) extends Id

  /** Identifies a project via its version control system URL. Examples:
    * - `SrcURL(git, https://github.com/moped/maven-project.git)`
    * - `SrcURL(git, git@github.com:samskivert/samskivert.git)`
    * - `SrcURL(hg, https://ooo-maven.googlecode.com/hg/)`
    * - `SrcURL(svn, https://ooo-gwt-utils.googlecode.com/svn)`
    */
  case class SrcURL (vcs :String, url :String) extends Id

  /** Identifies a project by it's root + module. Used for intra-project depends. */
  case class RootId (path :Path, module :String) extends Id
  object RootId {
    def apply (root :Root) :RootId = RootId(root.path, root.module)
  }

  /** An id string to use for [[PlatformId.platform]] for the Java/JDK platform. Versions for this
    * platform should be of the form: `"6"`, `"7"`, `"8"`. */
  final val JavaPlatform = "jdk"

  /** Identifies a platform project. This is generally an implicit dependency added by the build
    * system, like a particular version of the JDK for a Java or Scala project, or a particular
    * version of the Ruby standard libraries for a Ruby project. */
  case class PlatformId (platform :String, version :String) extends Id

  /** Defines a project's root directory and any module tag that is needed to differentiate
    * multiple modules in the same root directory. Many build systems put main and test modules in
    * the same directory, and some (like SBT or Gradle) can have a whole tree of modules rooted in
    * a single directory and managed by a single build file. */
  case class Root (path :Path, module :String = "") {
    /** Returns a hash name for this root. Used for internal directory names. */
    def hashName :String = md5hex(toString)
    def toString (sep :String) = s"$path$sep$module"
    override def toString :String = toString(if (module.length == 0) "" else ":")
  }

  /** Returns the project configured for the supplied buffer.
    * If no project is configured in the buffer a feedback error is thrown. */
  def apply (buffer :Buffer) :Project = buffer.state.get[Project].getOrElse {
    throw Errors.feedback(s"No project configured in buffer")
  }

  /** Used to read and write project metadata. */
  trait MetaMeta[T] {
    /** The default meta for a freshly opened and totally unknown project. */
    def zero (project :Project) :T
    /** Reads a `T` from [[ConfigFile]] data. */
    def read (in :Map[String,Seq[String]]) :T
    /** Writes `meta` to `out`. */
    def write (out :ConfigFile.WriteMap, meta :T) :Unit
  }

  /** Defines the basic persistent metadata for a project. When a project is first resolved, the
    * metadata is quickly restored from a file. Project components may subsequently update that
    * metadata based on external project files (which could take a long time due to things like
    * Gradle or SBT initialization).
    *
    * Any time the project metadata changes, it's saved so that it can be rapdily read in again
    * next time we have a cold start. */
  case class Meta (val name :String, val ids :Set[Id])

  /** Handles reading and writing [[Meta]]s. */
  object Meta extends MetaMeta[Meta] {
    def zero (project :Project) = Meta(project.root.path.getFileName.toString, Set())
    def read (in :Map[String,Seq[String]]) :Meta = {
      val Seq(name) = in("name")
      val ids = in("ids").flatMap(Codec.readId).toSet
      Meta(name, ids)
    }
    def write (out :ConfigFile.WriteMap, meta :Meta) :Unit = {
      out.write("name", Seq(meta.name))
      out.write("ids", meta.ids.map(Codec.showId).toSeq)
    }
  }

  /** Identifies a component of a project. */
  abstract class Component extends AutoCloseable {

    /** Adds info on this project component to the project description buffer. */
    def describeSelf (bb :BufferBuilder) :Unit = {}

    /** Called when the project of which this component is a part is added to `buffer`. */
    def addToBuffer (buffer :RBuffer) :Unit = {}

    /** Releases any resources held by this component. */
    def close () :Unit = {}
  }

  private def md5hex (text :String) = toHex(digest.digest(text.getBytes))
  private def toHex (data :Array[Byte]) = {
    val cs = new Array[Char](data.length*2)
    val chars = Chars
    var in = 0 ; var out = 0 ; while (in < data.length) {
      val b = data(in).toInt & 0xFF
      cs(out) = chars(b/16)
      cs(out+1) = chars(b%16)
      in += 1 ; out += 2
    }
    new String(cs)
  }
  private val digest = MessageDigest.getInstance("MD5")
  private final val Chars = "0123456789ABCDEF"
}

/** Provides services for a particular project.
  * @param pspace the project space of which this project is a part.
  * @param root the directory in which this project is rooted. */
class Project (val pspace :ProjectSpace, val root :Project.Root) {
  import Project._

  private val components = new HashMap[Class[? <: Component],Component]()
  private val activeBuffers = ArrayBuffer[RBuffer]()
  private val bufferNotes = new TreeMap[Store, Value[Seq[Lang.Note]]]()

  /** Tracks the basic project metadata. This should only be updated by the project, but outside
    * parties may want to react to changes to it. */
  val metaV = metaValue("meta", Meta)
  // when metaV changes, update our status
  metaV.onEmit { updateStatus() }

  /** Indicates that this project should be omitted from lookup by name. */
  def isIncidental = false

  /** The name of this project. */
  def name :String = metaV().name

  /** Returns all identifiers known for this project. This may include `RepoId`, `SrcURL`, etc. */
  def ids :Set[Id] = metaV().ids

  /** Summarizes the status of this project. This is displayed in the modeline. */
  lazy val status :Value[(String,String)] = Value(makeStatus)

  /** A bag of closeables to be closed when this project is [[dispose]]d. */
  val toClose = Close.bag()

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Feedback messages (or errors) emitted on this project. These will be forwarded (by
    * project-mode) to any windows showing buffers to which this project is attached. */
  val feedback = Signal[Either[(String, Boolean), Throwable]](pspace.wspace.exec.ui)

  /** An executor that reports errors via this project's `feedback` signal. */
  val exec = pspace.wspace.exec.handleErrors(err => feedback.emit(Right(err)))

  /** The meta service, for easy access. */
  def metaSvc :MetaService = pspace.msvc

  /** Returns the file named `name` in this project's metadata directory. */
  def metaFile (name :String) :Path = {
    metaDir.resolve(name)
  }

  /** Briefly displays a status message to the user.
    * @param ephemeral if false, the status message will also be appended to the `*messages*`
    * buffer; if true, it disappears forever in a poof of quantum decoherence. */
  def emitStatus (msg :String, ephemeral :Boolean = false) = feedback.emit(Left(msg, ephemeral))

  /** Reports an unexpected error to the user.
    * The message will also be appended to the `*messages*` buffer. */
  def emitError (err :Throwable) :Unit = feedback.emit(Right(err))

  /** Returns of the stores for which notes have been provided, naturally ordered. */
  def noteStores :Seq[Store] =
    Seq() ++ bufferNotes.entrySet.asScala.filterNot(_.getValue.get.isEmpty).map(_.getKey)

  /** Returns the analyzer notes for the buffer identified by `store`. */
  def notes (store :Store) :Value[Seq[Lang.Note]] =
    Mutable.getOrPut(bufferNotes, store, Value(Seq[Lang.Note]()))

  /** Adds this project to `buffer`'s state. Called by [[ProjectSpace]] whenever a buffer is
    * created (and only after this project has reported itself as ready).
    *
    * By default adds this project to the buffer, but a project may which to inspect the path being
    * edited in the buffer and add a different project instead. */
  def addToBuffer (buffer :RBuffer) :Unit = {
    buffer.state[Project]() = this
    import Config.Scope
    buffer.state[Scope]() = Scope("project", metaDir, buffer.state.get[Scope])

    // tell our components that we've been added
    components.values.asScala.foreach { _.addToBuffer(buffer) }

    // note that we've been added to this buffer
    activeBuffers += buffer
    buffer.killed.onEmit { activeBuffers -= buffer }
  }

  /** Creates the buffer state for a buffer with mode `mode` and mode arguments `args`, which is
    * configured to be a part of this project. */
  def bufferState (mode :String, args :Any*) :List[State.Init[?]] = List(
    State.init(Mode.Hint(mode, args*)),
    State.init(classOf[Project], this))

  /** Creates a simple buffer configured to be part of this project. A buffer with the same name
    * will be reused. This is useful for incidental buffers related to the project like compiler
    * output, test output, etc. */
  def createBuffer (name :String, mode :String, args :Any*) :Buffer =
    pspace.wspace.createBuffer(Store.scratch(name, root.path), bufferState(mode, args*), true)

  /** Creates a simple buffer configured to be part of this project. A buffer with the same name
    * will be reused. This is useful for incidental buffers related to the project like compiler
    * output, test output, etc. */
  def createBuffer (name :String, initState :List[State.Init[?]]) :Buffer =
    pspace.wspace.createBuffer(Store.scratch(name, root.path), initState, true)

  /** Returns a buffer to which incidental log output relating to this project can be sent
    * (compiler output, test output, etc.). */
  def logBuffer :Buffer = createBuffer(s"*$name:log*", "log")

  /** Appends `msg` to this project's [[logBuffer]]. This method can be called from any thread, but
    * is a bit expensive. Append to [[logBuffer]] yourself if you have a lot of logging to do and
    * know you're on the UI thread. */
  def log (msg :String) :Unit = exec.runOnUI {
    logBuffer.append(Line.fromTextNL(msg))
  }

  /** Visits a buffer containing a description of this project. */
  def visitDescription (window :Window) :Unit = {
    val buf = createBuffer(s"*project:${name}*", "help")
    val bb = new BufferBuilder(window.focus.geometry.width-1)
    describeSelf(bb)
    window.focus.visit(bb.applyTo(buf))
  }

  /** Emits a description of this project to `bb`. The default project adds basic metadata, and
    * derived project implementations undoubtedly have useful things to add. */
  def describeSelf (bb :BufferBuilder) :Unit = {
    bb.addHeader(name)
    bb.addBlank()
    describeMeta(bb)

    val notes = bufferNotes.entrySet.asScala.filter(_.getValue.get.size > 0)
    if (!notes.isEmpty) {
      bb.addSection("Notes")
      // TODO: make the buffer name a link to the buffer
      bb.addKeysValues(notes.map(ent => (s"${ent.getKey.name}: ", s"${ent.getValue.get.size}")))
    }

    // add info on our helpers
    components.values.asScala.foreach { _.describeSelf(bb) }
  }

  protected def describeMeta (bb :BufferBuilder) :Unit = {
    val info = Seq.newBuilder[(String,String)]
    info += ("Impl: " -> getClass.getName)
    info += ("Root: " -> root.path.toString)
    ids.foreach { id => info += ("ID: " -> id.toString) }
    bb.addKeysValues(info.result)
  }

  /** Instructs the project to update its status info. This is generally called by project helpers
    * that participate in the modeline info. */
  def updateStatus () :Unit = status() = makeStatus

  // getters for various well-known project components
  def files :Filer = component[Filer] || DefaultFiler

  // this is only a project component to separate the code (and to participate in addToBuffer and
  // describeSelf component hooks), it's not provided or customized by a project resolver
  val lang :LangClient.Component =
    addComponent(classOf[LangClient.Component], new LangClient.Component(this))

  /** Closes any open resources maintained by this project and prepares it to be freed. This
    * happens when this project's owning workspace is disposed. */
  def dispose () :Unit = {
    println(s"$this disposing")
    try toClose.close()
    catch {
      case e :Throwable => log.log("$this dispose failure", e)
    }
    components.values.asScala.foreach(_.close())
    components.clear()
  }

  /** Returns the component for the specified type-key, or `None` if no component is registered. */
  def component[C <: Component] (cclass :Class[C]) :Option[C] =
    Option(components.get(cclass).asInstanceOf[C])

  /** A `component` variant that uses class tags to allow usage like: `component[Foo]`. */
  def component[C <: Component] (implicit tag :ClassTag[C]) :Option[C] =
    component(tag.runtimeClass.asInstanceOf[Class[C]])

  /** Returns whether a `cclass` component has been added to this project. */
  def hasComponent[C <: Component] (cclass :Class[C]) :Boolean = components.containsKey(cclass)

  /** Registers `comp` with this project. If a component of the same type-key is already registered
    * it will be closed and replaced with `comp`. Components will also be closed when this project
    * is disposed.
    */
  def addComponent[C <: Component] (cclass :Class[C], comp :C) :C = {
    assert(comp != null)
    val oldComp = components.get(cclass)
    if (oldComp != null) oldComp.close()
    components.put(cclass, comp)
    // tell this component about buffers to which we're already added
    activeBuffers foreach comp.addToBuffer
    comp
  }

  /** Creates a meta-value with storage in this project's meta directory. */
  def metaValue[T] (id :String, metameta :MetaMeta[T]) :Value[T] = {
    val confFile = metaFile(id + ".conf")
    val value = Value(metameta.zero(this))
    if (Files.exists(confFile)) try {
      value() = metameta.read(ConfigFile.readMap(confFile))
    } catch {
      case t :Throwable =>
        pspace.wspace.exec.handleError(
          new Exception(s"Failed to read meta: '$confFile' (project: ${root.path})", t))
        pspace.wspace.exec.runInBG(Files.delete(confFile))
    }
    value.onValue { nvalue =>
      val out = new ConfigFile.WriteMap(confFile)
      metameta.write(out, nvalue)
      out.close()
    }
    value
  }

  override def toString = s"$name (${root.path})"

  protected def log = metaSvc.log

  /** Returns the directory in which this project will store metadata. */
  private[project] def metaDir = {
    val dir = pspace.metaDir(this)
    if (!Files.exists(dir)) Files.createDirectories(dir)
    dir
  }

  /** Populates our status line (`sb`) and status line tooltip (`tb`) strings. */
  protected def makeStatus (sb :StringBuilder, tb :StringBuilder) :Unit = {
  }
  private def makeStatus :(String,String) = {
    val sb = new StringBuilder("(").append(name)
    val tb = new StringBuilder("Current project: ").append(name)
    makeStatus(sb, tb)
    (sb.append(")").toString, tb.toString)
  }

  lazy val DefaultFiler = {
    def isZip (name :String) = (name `endsWith` ".zip") || (name `endsWith` ".jar")
    if (isZip(root.path.getFileName.toString)) new ZipFiler(Seq(root.path))
    else new DirectoryFiler(this, Ignorer.stockIgnores)
  }
}
