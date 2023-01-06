//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j._

import moped._
import moped.major.ReadingMode

object LangFindUsesConfig extends Config.Defs {

  /** The CSS style applied to file paths. */
  val pathStyle = "usePathStyle"

  /** The CSS style applied to line numbers. */
  val lineNoStyle = "useLineNoStyle"

  /** The CSS style applied to uses. */
  val matchStyle = EditorConfig.matchStyle // standard matchStyle

  case class Context (name :String, req :ReferenceParams)
}

@Major(name="lang-find-uses", tags=Array("project"),
       desc="""A major mode that displays all known uses of a symbol.""")
class LangFindUsesMode (env :Env, ctx :LangFindUsesConfig.Context, client :LangClient) extends ReadingMode(env) {
  import LangFindUsesConfig._

  def project = Project(buffer)
  // import project.pspace

  override def configDefs = LangFindUsesConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/lang.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("visit-use", "ENTER");

  private val noUse = Visit.Tag(new Visit() {
    protected def go (window :Window) = window.popStatus("No use on the current line.")
  })

  @Fn("Visits the use on the current line.")
  def visitUse () :Unit = {
    buffer.line(view.point()).lineTag(noUse)(window)
  }

  var visitList :Visit.List = _

  // look up our uses in the background and append them to the buffer
  if (buffer.start == buffer.end) {
    println(s"Finding uses of ${ctx.name}")
    LSP.adapt(client.server.getTextDocumentService.references(ctx.req), project.exec).onSuccess {
      res => showLocations(res.asScala) }
  }
  // reinstate our visit list if our buffer is already generated
  else if (visitList != null) window.visits() = visitList

  private def showLocations (locs :Iterable[Location]) :Unit = {
    val visits = Seq.newBuilder[Visit]
    val lines = Seq.newBuilder[Line]

    for ((uri, uriLocs) <- locs.groupBy(_.getUri)) {
      lines += Line.builder(uri).withStyle(pathStyle, 0, uri.length).build()
      var store = LSP.toStore(uri)
      var line = 0
      store.read(Store.reader { (data, start, end, offset) =>
        for (uriLoc <- uriLocs ; locStart = uriLoc.getRange.getStart
             if (locStart.getLine == line)) {
          val fileoff = offset + locStart.getCharacter
          val visit = Visit(store, fileoff)
          val locEnd = uriLoc.getRange.getEnd // TODO: multiline?
          lines += Line.builder(data, start, end).
            withStyle(matchStyle, locStart.getCharacter, locEnd.getCharacter).
            withLineTag(Visit.Tag(visit)).
            build()
          visits += visit
        }
        line += 1
      })
    }

    buffer append lines.result
    buffer split buffer.end

    window.exec.runOnUI {
      visitList = new Visit.List("use", visits.result)
      window.visits() = visitList
      view.point() = Loc.Zero
    }
  }

  private def addLocation (loc :Location) :Unit = {

  }
}
