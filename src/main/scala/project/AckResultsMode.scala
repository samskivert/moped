//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.project

import java.util.regex.Pattern
import scala.annotation.tailrec
import moped._
import moped.major.ReadingMode
import moped.util.{Errors, SubProcess}

/** Provides configuration for [[AckResultsMode]]. */
object AckResultsConfig {

  /** The CSS style applied to file paths. */
  val pathStyle = "ackPathStyle"

  /** The CSS style applied to line numbers. */
  val lineNoStyle = "ackLineNoStyle"

  /** The CSS style applied to matches. */
  val matchStyle = EditorConfig.matchStyle // standard matchStyle
}

@Major(name="ack-results", tags=Array("ack"),
       desc="Displays `ack` results and allows navigation therethrough")
class AckResultsMode (env :Env, opts :AckConfig.Opts) extends ReadingMode(env) {
  import AckConfig._
  import AckResultsConfig._

  val pspace = ProjectSpace(wspace)

  override def configDefs = AckConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/ack.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("visit-match", "ENTER");

  private val noMatch = Visit.Tag(new Visit() {
    override protected def go (window :Window) = window.popStatus("No match on the current line.")
  })

  @Fn("Visits the match on the current line.")
  def visitMatch () :Unit = {
    buffer.line(view.point()).lineTag(noMatch).visit(window)
  }

  private val NumLineP = Pattern.compile("""(\d+):(.*)""")
  private val FileNumLineP = Pattern.compile("""([^:]+):(\d+):(.*)""")
  private val TermM = try Matcher.regexp(opts.term) catch {
    case e :Exception => { window.exec.handleError(e) ; Matcher.exact("ERROR") }
  }

  private def refresh () :Unit = {
    buffer.delete(buffer.start, buffer.end)
    val cmd = Seq("ack") ++ opts.opts ++ Seq("--nocolor", "--nopager", "-x", opts.term)
    env.log.log(cmd.mkString(" "))

    import SubProcess._
    val events = Signal[Event](window.exec.ui)
    events.onValue(new (Event => Unit) {
      val visits = Seq.newBuilder[Visit]
      var file = ""

      def apply (event :Event) = event match {
        case Output(text, _)   => if (text.length > 0) process(text)
        case Complete(isErr)   => if (!isErr) finish()
        case Failure(cause, _) => buffer.append(Line.fromTextNL(Errors.stackTraceToString(cause)))
      }

      def process (text :String) :Unit = {
        val lb = Line.builder(text)

        def xFile (start :Int, end :Int) = {
          lb.withStyle(pathStyle, start, end)
          text.substring(start, end)
        }
        def xLineNo (start :Int, end :Int) = {
          lb.withStyle(lineNoStyle, start, end)
          text.substring(start, end).toInt-1
        }

        val m = NumLineP.matcher(text) // '(num):(line)'
        if (m.matches) {
          append(xLineNo(m.start(1), m.end(1)), lb.build(), m.start(2))

        } else {
          val m = FileNumLineP.matcher(text) // '(file):(num):(line)'
          if (m.matches) {
            file = xFile(m.start(1), m.end(1))
            append(xLineNo(m.start(2), m.end(2)), lb.build(), m.start(3))

          } else {
            // if it's neither 'num:line' or 'file:num:line' then it's 'file'
            file = xFile(0, text.length)
            buffer.split(buffer.insert(buffer.end, lb.build()))
          }
        }
      }

      def append (lineNo :Int, line :Line, mstart :Int) :Unit = {
        // append the line (and a newline) to the buffer
        val loc = buffer.end
        buffer.split(buffer.insert(loc, line))

        // style the matches in the line and add visits for them
        var ii = line.indexOf(TermM, mstart) ; var first = true
        while (ii != -1) {
          val visit = Visit(Store(file), Loc(lineNo, ii-mstart))
          visits += visit
          // if this is the first match, tag the line with its visit
          if (first) {
            buffer.setLineTag(loc, Visit.Tag(visit))
            first = false
          }

          val end = ii+TermM.matchLength
          buffer.addStyle(matchStyle, loc.atCol(ii), loc.atCol(end))
          ii = line.indexOf(TermM, end)
        }
      }

      def finish () :Unit = {
        window.visits() = new Visit.List("match", visits.result()) {
          override def things = "matches"
        }
      }
    })

    val proc = new SubProcess(Config(cmd.toArray), events)
    // pass the files in the project to ack individually; this allows us to leverage the filtering
    // done by projects which know about which files to ignore
    val ps = opts.scope match {
      case PScope => Seq(Project(buffer))
      case WScope => pspace.allProjects.map(i => pspace.projectFor(i._1))
    }
    ps foreach(_.files.onFiles(f => proc.send(f.toString)))
    proc.close()
  }

  refresh() // run the search for the first time
}
