//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.snippet

import java.nio.file.Files
import java.util.HashSet
import moped._
import moped.code.CodeMode
import moped.util.{Anchor, Close}

object SnippetConfig extends Config.Defs {

  /** The CSS style applied to the hole being currently edited. */
  val activeHoleStyle = "activeHoleFace"
}

@Minor(name="snippet", tags=Array("text", "code"),
       desc="Provides support for insertion and population of text/code snippets.")
class SnippetMode (env :Env, major :MajorMode) extends MinorMode(env) {
  import SnippetConfig._
  private val snipsvc = env.msvc.service[SnippetService]

  override def configDefs = SnippetConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/snippet.css") :: super.stylesheets

  override def keymap = super.keymap.
    bind("expand-or-next-snippet", "TAB").
    bind("prev-snippet", "S-TAB");

  case class Trigger (m :Matcher, trig :String, snip :Snippet)
  lazy val trigs :Seq[Trigger] = {
    val bb = Seq.newBuilder[Trigger]
    val seenTrigs = new HashSet[String]()
    for (snip <- snipsvc.resolveSnippets(major.name, env.configScope) ; trig <- snip.triggers) {
      // if we've already seen a trigger, don't add it when we see it again later; our snippet
      // sources are returned in order of precedence, so we want the first one we see
      if (seenTrigs.add(trig)) bb += Trigger(Matcher.exact(trig), trig, snip)
    }
    bb.result.sortBy(-_.trig.length) // sort by longest to shortest trigger
  }

  /** An activated snippet plus a bunch of useful metadata. */
  class ActiveSnip (holes :Seq[Hole], initStart :Loc, initEnd :Loc) {
    val anchors = new Anchor.Set(buffer)

    /** Tracks the start and end of the entire snippet. */
    val region = Anchor.Region(anchors.add(initStart).bindLeft, anchors.add(initEnd))

    /** Tracks the bounds of each hole and mirror in the snippet, and other bits. */
    class ActiveHole (hole :Hole) {
      /** The region that bounds this hole, automatically adjusted for edits. */
      val region = Anchor.Region(anchors.add(hole.loc), anchors.add(hole.loc + (0, hole.deflen)))

      /** Anchor regions for all of this hole's mirrors (start, end). */
      val amirrors = hole.mirrors.map(l => Anchor.Region(anchors.add(l), anchors.add(l)))

      /** Tracks whether this hole still contains its default text. */
      var isDefault = (hole.deflen > 0)

      /** Like [[Region.contains]] except that the loc *at* our end position is considered
        * contained, which is not true for `Region.contains`. This is what we want when deciding
        * whether or not the point is currently "in" this hole. */
      def containsP (loc :Loc) = (region.start <= loc) && (region.end >= loc)

      def init () :Unit = {
        // if we have a default value and mirrors, stuff our default value into our mirrors
        if (isDefault && !amirrors.isEmpty) {
          amirrors foreach { _.bindOut }
          updateMirrors()
          amirrors foreach { _.bindIn }
        }
      }

      def activate () :Unit = {
        // while we're active, our regions bind out (they expand due to edits on their edges)
        region.bindOut
        buffer.addTag(activeHoleStyle, region)
        amirrors foreach { _.bindOut }
        amirrors foreach { buffer.addTag(activeHoleStyle, _) }
        moveTo()
      }
      def deactivate () :Unit = {
        // while we're inactive, our regions bind in (no expand due to edits on edges)
        region.bindIn
        amirrors foreach { _.bindIn }
        buffer.removeTag(activeHoleStyle, region)
        amirrors foreach { buffer.removeTag(activeHoleStyle, _) }
      }

      def moveTo () :Unit = {
        if (!containsP(view.point())) {
          view.point() = if (isDefault) region.start else region.end
          // if we're in column zero, automatically indent
          if (view.point().col == 0) afterEdit { ifCode(_.reindentAtPoint()) }
        }
      }

      def updateMirrors () :Unit = {
        val current = buffer.region(region)
        disableOnEdit = true
        try amirrors foreach { mr => buffer.replace(mr, current) }
        finally disableOnEdit = false
      }

      override def toString = s"H:$region:$isDefault"
    }
    val aholes = holes map { new ActiveHole(_) }

    private var ahole :ActiveHole = null
    def activeHole = ahole
    def activeHole_= (hole :ActiveHole) = if (hole != ahole) {
      if (ahole != null) ahole.deactivate()
      ahole = hole
      if (hole != null) hole.activate()
    }

    // used to disable our buffer listener when we're updating mirrors or doing other buffer
    // changes to which we don't want to react
    private var disableOnEdit = false

    // set ourselves as the active snippet
    // println(s"activating $this")
    activeSnip = this
    // fill in the mirrors for all holes with defval+mirrors
    aholes foreach { _.init() }
    // indent our inserted snippet lines
    ifCode { code =>
      // TODO: reindent starting line?
      var loc = region.start.nextStart ; val end = region.end ; while (loc < end) {
        if (buffer.line(loc).length > 0) code.reindent(loc)
        loc = loc.nextStart
      }
    }

    // wire up our buffer listeners and advance to the first hole
    val toClose = Close.bag()
    toClose += buffer.edited `onValue` onEdit
    toClose += view.point `onValue` checkSwitchHole
    activateHole(0) // this will deactivate this snippet if hole 0 is the exit hole

    def deactivate () :Unit = {
      // println(s"deactivating $this")
      toClose.close()
      activeHole = null
    }

    def nextHole () :Unit = {
      // if the point is before the currently active hole, just move to the active hole
      val ah = activeHole
      if (view.point() < ah.region.start) ah.moveTo()
      else activateHole(aholes.indexOf(activeHole)+1)
    }

    def prevHole () :Unit = {
      val hidx = aholes.indexOf(activeHole)
      if (hidx > 0) activeHole = aholes(hidx-1)
    }

    private def activateHole (hidx :Int) :Unit = {
      if (hidx < aholes.size-1) activeHole = aholes(hidx)
      // if we're on the penultimate hole, then the next hole is the exit and deactivates us
      else {
        deactivateSnippet()
        view.point() = aholes(hidx).region.end
        // if the end is inside the snippet, indent
        if (view.point() < region.end) ifCode { _.reindentAtPoint() }
      }
    }

    // if the point moves outside the active hole, look to see if it moved into another hole, and
    // if so, make that hole active; NOTE: we also don't do this check if listening is disabled
    private def checkSwitchHole (loc :Loc) :Unit = if (!activeHole.containsP(loc)) afterEdit {
      if (activeSnip == this) { // make sure we don't come in too late... blah
        val hidx = aholes.indexWhere(_.containsP(loc))
        if (hidx >= 0) activateHole(hidx)
      }
    }

    // ignore edits if we're currently twiddling the buffer programmatically
    private def onEdit (edit :Buffer.Edit) = if (!disableOnEdit) edit match {
      case Buffer.Insert(istart, iend) =>
        // if they edit outside the bounds of the active hole, deactivate the snippet; they
        // better be done because we can't be keeping track of all that craziness
        val ah = activeHole
        if (!ah.containsP(istart)) deactivateSnippet()
        else {
          // tag the insert with the active hole style
          buffer.addTag(activeHoleStyle, istart, iend)

          // if this hole currently contains its default value, queue it up to be cleared (we
          // can't clear it immediately because we're processing a buffer edit right now)
          if (ah.isDefault) {
            // replace the whole hole region with this insertion; that will remove the default and
            // replace it with just what was inserted
            ah.isDefault = false
            // trim the default text in the didInvoke hook so that its combined with the current
            // edit for undo purposes; we can't do it now because we're dispatching an edit
            afterEdit {
              // delete any default text from the end of the insert onward
              if (iend != ah.region.end) {
                disableOnEdit = true
                try buffer.delete(iend, ah.region.end)
                finally disableOnEdit = false
              }
              activeHole.updateMirrors()
            }
          }
          // otherwise, update our mirrors after this edit commits
          else afterEdit { ah.updateMirrors() }
        }

      case Buffer.Delete(dstart, dend, deleted) =>
        // if this delete is not inside a current hole, then deactivate
        if (!aholes.exists(_.containsP(dstart))) deactivateSnippet()
        // otherwise, update our mirrors after this edit commits
        else afterEdit { activeHole match {
          case null => println("Urk, post-delete-mirror update too late")
          case hole => hole.updateMirrors()
        }}

      case _ => // don't care about transforms
    }

    override def toString = s"Snip:$region:$aholes"
  }

  // invokes `action` after the current fn is finished dispatching; this is often necessary when we
  // want to change the buffer but we're reacting to a buffer change (it's not legal to change the
  // buffer during that time); using disp.didInvoke means our changes are bundled up with whatever
  // changes were part of the executing fn, which is generally what we want for sensible undo
  private def afterEdit[U] (action : =>U) :Unit = {
    if (disp.curFn != null) (disp.didInvoke onEmit { action }).once()
    else window.exec.runOnUI(action)
  }

  /** The currently active snippet, if any. */
  var activeSnip :ActiveSnip = null
  def reqActiveSnip = if (activeSnip == null) abort("No active snippet.") else activeSnip

  @Fn("""If there is no active snippet, attemps to expand the snippet at the point.
         If there is an active snippet, advances to the next hole in the snippet.""")
  def expandOrNextSnippet () :Boolean = activeSnip match {
    case null => expandSnippet()
    case snip => snip.nextHole() ; true
  }

  @Fn("Expands the snippet at the point, if any.")
  def expandSnippet () :Boolean = {
    // TODO: make this more efficient?
    // (build trie from reversed trigger strings, match backward char by char)
    val p = view.point() ; val pcol = p.col ; val line = buffer.line(p)
    val iter = trigs.iterator ; while (iter.hasNext) {
      val t = iter.next() ; val tlen = t.trig.length ; val tcol = pcol-tlen
      if ((tlen <= pcol) && line.matches(t.m, tcol) && t.snip.canActivate(line, tcol, pcol)) {
        startSnippet(t.snip, p.atCol(tcol), p)
        return true
      }
    }
    false
  }

  @Fn("""Moves to the next hole in the active snippet. If the point is currently on the last
         hole, it will be moved to the snippet's "exit" and the snippet will be deactivated.""")
  def nextSnippet () :Unit = reqActiveSnip.nextHole()

  @Fn("""Moves to the previous hole in the active snippet.
         NOOPs if the point is already on the first hole in the snippet.""")
  def prevSnippet () :Boolean = if (activeSnip == null) false else {
    activeSnip.prevHole()
    true
  }

  @Fn("""Deactivates the active snippet.""")
  def deactivateSnippet () :Unit = if (activeSnip != null) {
    activeSnip.deactivate()
    activeSnip = null
  }

  @Fn("Visits a snippets definition file.")
  def editSnippets () :Unit = {
    window.mini.read("Mode:", major.name, snippetNameHistory, Completer.none) onSuccess { name =>
      val scopes = env.configScope.toList ; val comp = Completer.from(scopes)(_.name)
      window.mini.read("Scope:", scopes.head.name, configScopeHistory, comp) onSuccess { scope =>
        val file = scope.root.resolve("Snippets").resolve(s"$name.snip")
        val view = frame.visit(wspace.openBuffer(Store(file)))
        // if this snippets file doesn't exist, insert some boilerplate
        if (!Files.exists(file)) view.buffer.append(
          Seq(s"# $name snippets",
              "#",
              "# Use M-x insert-stock-snippets to populate this buffer with defaults.",
              "# Use M-x describe-mode to read more about the .snip format.",
              "", "").map(Line.apply))
      }
    }
  }

  @Fn("Displays the template and other info for a particular snippet.")
  def describeSnippet () :Unit = {
    val comp = Completer.from(trigs)(_.trig)
    window.mini.read("Trigger:", "", snippetTriggerHistory, comp) onSuccess { trig =>
      val snip = trig.snip
      val info = Seq(s"Name: ${snip.name}",
                     s"Triggers: ${snip.triggers.mkString(" ")}") ++ snip.template
      // TODO: syntaxes, line constraints
      view.popup() = Popup.text(info, Popup.UpRight(view.point()))
    }
  }

  private def startSnippet (snip :Snippet, start :Loc, pos :Loc) :Unit = {
    deactivateSnippet()                           // deactivate any previous snippet
    buffer.delete(start, pos)                     // delete the trigger
    val (holes, end) = snip.insert(buffer, start) // insert the snippet text
    new ActiveSnip(holes, start, end)             // and start the heavy machinery
  }

  private val computeIndent = major match {
    case code :CodeMode => (row :Int) => " " * code.computeIndent(row)
    case _              => (row :Int) => ""
  }

  private def ifCode (fn :CodeMode => Unit) :Unit = major match {
    case code :CodeMode => fn(code)
    case _ => // oh well, nada
  }

  private def add (loc :Loc, snipLoc :Loc) :Loc = snipLoc.row match {
    case 0 => loc.atCol(loc.col + snipLoc.col)
    case n => Loc(loc.row + snipLoc.row, snipLoc.col)
  }

  private def configScopeHistory = wspace.historyRing("config-scope")
  private def snippetNameHistory = wspace.historyRing("snippet-name")
  private def snippetTriggerHistory = wspace.historyRing("snippet-trigger")
}
