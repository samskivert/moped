//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import scala.jdk.CollectionConverters.*

import java.nio.file.{Path, Paths, Files}
import java.net.{URL, URI}
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.jsonrpc.services.JsonDelegate
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment
import org.eclipse.lsp4j.services.LanguageServer

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import moped._
import moped.util.{Errors, Fetcher}

@Plugin
class EclipseRootPlugin extends RootPlugin.File(".project")

object JDTLS {

  // from whence we download the Eclipse JDT language server
  val JdtFile = "jdt-language-server-latest.tar.gz"
  val JdtUrl = new URI(s"https://download.eclipse.org/jdtls/snapshots/$JdtFile").toURL

  /** Downloads and unpacks the JDTLS, if needed. */
  def resolve (metaSvc :MetaService, root :Project.Root) :Future[Path] = {
    val pkgSvc = metaSvc.service[PackageService]
    val extrasDir = pkgSvc.extrasDir("eclipse-extras")
    val jdtlsDir = extrasDir.resolve("eclipse-jdt-ls")
    if (Files.exists(jdtlsDir)) Future.success(jdtlsDir)
    else {
      val jdtPath = extrasDir.resolve(JdtFile)
      Fetcher.fetch(metaSvc.exec, JdtUrl, jdtPath, pct => {
        metaSvc.log.log(s"Downloading $JdtFile: $pct%")
      }).map(targz => {
        metaSvc.log.log(s"Unpacking $JdtFile...")
        val jdtlsTmp = Files.createTempDirectory(extrasDir, "jdtls")
        try {
          untargz(targz, jdtlsTmp)
          deleteAll(jdtlsDir)
          Files.move(jdtlsTmp, jdtlsDir)
          jdtlsDir
        } finally {
          Files.deleteIfExists(targz)
          deleteAll(jdtlsTmp)
        }
      })
    }
  }

  /** Determines the path to the launcher jar in a JDTLS installation. */
  def launcherJar (jdtls :Path) :Option[Path] =
    Files.list(jdtls.resolve("plugins")).filter(path => {
      val name = path.getFileName.toString
      name.startsWith("org.eclipse.equinox.launcher_") && name.endsWith(".jar")
    }).iterator().asScala.find(_ => true) // hacky `firstOption`

  /** Unpacks the .tar.gz file at `path` into the `into` directory. */
  def untargz (path :Path, into :Path) :Unit = {
    using(new GzipCompressorInputStream(Files.newInputStream(path))) { gzin =>
      val tin = new TarArchiveInputStream(gzin)
      var entry = tin.getNextTarEntry
      while (entry != null) {
        if (!entry.isDirectory) {
          val file = into.resolve(Paths.get(entry.getName))
          Files.createDirectories(file.getParent)
          Files.copy(tin, file)
        }
        entry = tin.getNextTarEntry
      }
    }
  }

  /** Deletes a file or empty directory, setting its write permissions if necessary. */
  private def safeDelete (path :Path) = {
    if (!Files.isWritable(path)) path.toFile().setWritable(true)
    Files.delete(path)
  }

  /** Deletes {@code dir} and all of its contents. */
  private def deleteAll (dir :Path) :Unit = {
    if (!Files.exists(dir)) return; // our job is already done
    import java.nio.file.{SimpleFileVisitor, FileVisitResult}
    Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
      import java.nio.file.attribute.BasicFileAttributes
      override def visitFile (file :Path, attrs :BasicFileAttributes) = {
        if (!attrs.isDirectory()) safeDelete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory (dir :Path, exn :java.io.IOException) = {
        if (exn != null) throw exn
        safeDelete(dir);
        FileVisitResult.CONTINUE
      }
    });
  }
}

@Plugin
class EclipseLangPlugin extends LangPlugin {

  override def suffs (root :Project.Root) = Set("java", "scala") // TODO: others?
  override def canActivate (root :Project.Root) = Files.exists(root.path.resolve(PROJECT_FILE))

  override def createClient (proj :Project) = JDTLS.resolve(proj.metaSvc, proj.root).map(
    jdtls => new EclipseLangClient(proj, serverCmd(proj, jdtls)))

  private final val PROJECT_FILE = ".project"

  /** Constructs the command line to invoke the JDT LS daemon. */
  private def serverCmd (proj :Project, jdtls :Path) = {
    val osName = System.getProperty("os.name")
    val configOS = if (osName.equalsIgnoreCase("linux")) "linux"
                   else if (osName.startsWith("Windows")) "win"
                   else "mac"

    val launcherJar = JDTLS.launcherJar(jdtls).getOrElse(() => {
      throw Errors.feedback("Can't find launcher jar in " + jdtls)
    })
    val configDir = jdtls.resolve("config_" + configOS)
    val dataDir = proj.metaFile("eclipse-jdt-ls")

    Seq("java",
        "-Declipse.application=org.eclipse.jdt.ls.core.id1",
        "-Dosgi.bundles.defaultStartLevel=4",
        "-Declipse.product=org.eclipse.jdt.ls.core.product",
        "-noverify",
        "-Xmx1G",
        "-XX:+UseG1GC",
        "-XX:+UseStringDeduplication",
        "-jar", launcherJar.toString,
        "-configuration", configDir.toString,
        "-data", dataDir.toString)
  }
}

case class StatusReport (message :String, typ :String)

class EclipseLangClient (proj :Project, cmd :Seq[String]) extends LangClient(proj, cmd, None) {

  override def name = "Eclipse"
  override def langServerClass = classOf[EclipseLangServer]

  /** Fetches the contents for a "synthetic" location, one hosted by the language server. */
  override def fetchContents (uri :URI, exec :Executor) = {
    try {
      val docId = new TextDocumentIdentifier(uri.toString())
      return LSP.adapt(getJavaExtensions.classFileContents(docId), exec)
    } catch {
      case e :Exception => throw new RuntimeException(e.getMessage, e)
    }
  }

  override def modeFor (loc :URI) = "java"

  private def getJavaExtensions = server.asInstanceOf[EclipseLangServer].getJavaExtensions

  /** Notifies us of the JDT LS status. (Eclipse JDT LS extension). */
  @JsonNotification("language/status")
  def statusNotification (report :StatusReport) = messages.emit(name + ": " + report.message)

  // TODO: tweak the stuff we get back from JDT-LS to make it nicer
}

enum BuildWorkspaceStatus { case FAILED, SUCCEED, WITH_ERROR, CANCELLED }

@JsonSegment("java")
trait JavaExtensions {

  @JsonRequest
  def classFileContents (documentUri :TextDocumentIdentifier) :CompletableFuture[String]

  @JsonNotification
  def projectConfigurationUpdate (documentUri :TextDocumentIdentifier) :Unit

  @JsonRequest
  def buildWorkspace (forceReBuild :Boolean) :CompletableFuture[BuildWorkspaceStatus]
}

/** Augments the standard {@code LanguageServer} interface with Eclipse JDT LS extensions. */
trait EclipseLangServer extends LanguageServer {

  /** Extensions provided by the Eclipse Language Server. */
  @JsonDelegate
  def getJavaExtensions :JavaExtensions
}
