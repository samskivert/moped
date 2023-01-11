//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.snippet

import java.nio.file.{Path, Files}
import java.util.HashSet
import scala.jdk.CollectionConverters._

import moped._

/** Implements [[SnippetService]]. Keeps track of stuff. */
class SnippetManager (msvc :MetaService, editor :Editor)
    extends AbstractService with SnippetService {

  private val userSnipCache = Mutable.cacheMap[(String,Path),Seq[Snippet]]((readDirSnips _).tupled)
  private val SnippetsDir = "Snippets"

  override def didStartup () :Unit = {}
  override def willShutdown () :Unit = {}

  override def resolveSnippets (mode :String, scope :Config.Scope) = {
    val snips = Seq.newBuilder[Snippet]
    // first look through all the config directories, adding any snippets from there
    scope.toList.map(_.root.resolve(SnippetsDir)) foreach { dir =>
      snips ++= userSnipCache.get((mode, dir))
    }
    // TODO: add snips from any registered "snippet database" directories
    snips.result
  }

  override def flushSnippets (mode :String, root :Path) :Unit = {
    userSnipCache.invalidate((mode, root.resolve(SnippetsDir)))
  }

  private def readDirSnips (mode :String, dir :Path) = readSnips(mode) { name =>
    val path = dir.resolve(s"$name.snip")
    if (Files.exists(path)) Some(Files.readAllLines(path).asScala) else None
  }

  private def readSnips (mode :String)(reader :String => Option[Iterable[String]]) = {
    val snips = Seq.newBuilder[Snippet]
    val seen = new HashSet[String]()
    def add (name :String) :Unit = if (seen.add(name)) reader(name) foreach { lines =>
      val incls = Snippet.parse(lines, snips)
      incls foreach add
    }
    add(mode)
    snips.result
  }
}
