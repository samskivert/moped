//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.util.regex.{Pattern => JPattern, Matcher => JMatcher}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import moped._

/** Represents a matched span of text on a line.
  * @param scopes the list of scopes applicable to this span of text (innermost to outermost).
  * @param start the offset in the line at which the span starts.
  * @param end the offset in the line at which the span ends.
  */
case class Span (scopes :List[String], start :Int, end :Int) {
  def contains (pos :Int) = start <= pos && pos < end
  override def toString = s"$start-$end " + scopes.mkString(" ")
}

/** Matches some number of grammar rules against a particular line. */
abstract class Matcher {

  /** The id of the pattern from which this matcher was compiled. For debugging. */
  def id :String

  /** Applies this matcher to the supplied `line` at `start`. Matched spans are added to `spans`.
    * @return `-1` if no matches were made, the offset following the last match otherwise. */
  def apply (state :Matcher.State, line :LineV, start :Int) :Int

  /** Converts this matcher to a string for debugging purposes. */
  def show (expand :Set[String], depth :Int) :String

  protected def nest (depth :Int, msg :String) = ("." * depth) + msg
}

object Matcher {

  final val Debug = false

  /** Used to match a single line and retain the matcher state at the end of the line.
    * @param matchers the current stack of active matchers.
    * @param scopes the current stack of active scopes. */
  class State (var matchers :List[Matcher], var scopes :List[String])
      extends Line.Tag with Cloneable {

    /** The spans matched on this line. */
    val spans = ArrayBuffer[Span]()

    /** A set of matchers to be skipped because they loop. */
    var skips = Set[Matcher]()

    /** Continues matching with `line` (which should be the line immediately following ours). This
      * state serves as the starting point and a new state is returned which represents the state
      * of the matcher at the end of `line`. */
    def continue (line :LineV) :State = new State(matchers, scopes).process(line)

    /** Applies `procs` to the spans matched on this line. */
    def apply (procs :List[Selector.Processor], buf :Buffer, row :Int) :Unit = {
      val sp = spans ; val ll = sp.length
      var pp = procs ; while (!pp.isEmpty) {
        val proc = pp.head ; proc.onBeforeLine(buf, row)
        var ii = 0 ; while (ii < ll) { proc.apply(buf, row, sp(ii)); ii += 1 }
        pp = pp.tail
      }
    }

    /** Returns the scopes at `pos` on this line. */
    def scopesAt (pos :Int) :List[String] = spans.find(_.contains(pos)).map(_.scopes) getOrElse Nil

    /** Genertes a debug representation of our scopes. */
    def showScopes :Seq[String] = spans.map(_.toString).toSeq

    /** Returns true if this matcher is NOT equivalent to `other`. */
    def nequiv (other :State) :Boolean = matchers != other.matchers || scopes != other.scopes

    // from Line.Tag; we don't want to be copied around
    override def ephemeral = true

    override def toString = "Matcher.State(" +
      s"scopes=${scopes.mkString(";")}, spans=${spans.length}, skips=${skips.size})\n  " +
      matchers.mkString("\n  ")

    //
    // implementation details

    private var lastPos = 0

    def onMatch (didAdvance :Boolean) :Unit = {
      // if we advanced the scan position, clear the skipped matchers set
      if (didAdvance && !skips.isEmpty) skips = Set()
    }

    def pushScope (pos :Int, scope :String) :Unit = {
      if (Debug) debug(s"Push $pos $scope")
      if (pos > lastPos) {
        spans += Span(scopes, lastPos, pos)
        lastPos = pos
      }
      scopes = scope :: scopes
    }
    def pushScope (pos :Int, scope :Option[String]) :Unit =
      if (scope.isDefined) pushScope(pos, scope.get)

    def popScope (pos :Int, scope :String) :Unit = {
      if (Debug) debug(s"Pop $pos $scope")
      if (pos > lastPos) {
        spans += Span(scopes, lastPos, pos)
        lastPos = pos
      }
      if (scopes.head != scope) new IllegalStateException(s"Wanted to pop $scope, have $scopes")
      scopes = scopes.tail
    }
    def popScope (pos :Int, scope :Option[String]) :Unit =
      if (scope.isDefined) popScope(pos, scope.get)

    private def process (line :LineV) :State = {
      if (Debug) debug("Scopes: " + this.scopes.mkString(" "))
      val last = line.length
      // first apply our matches to the line, accumulating scoping steps
      @inline @tailrec def loop (start :Int, prevEnd :Int) :Unit =
        if (start <= last) matchers.head.apply(this, line, start) match {
          // if we match nothing, advance one character and try again
          case  -1 => loop(start+1, prevEnd)
          // otherwise, keep going from the end of the match
          case end => loop(end, end) // TODO: check for loopus infinitus?
        }
      loop(0, 0)
      if (last > lastPos) spans += Span(scopes, lastPos, last)
      this
    }
  }

  /** A sentinel used to denote that a line's state has not yet been computed. */
  val NoState = new State(Nil, Nil)

  /** Handles matching a pattern and applying a set of captures. */
  class Pattern (regexp :String, p :JPattern, captures :List[(Int,String)]) {
    // we use transparent bounds so that things like word boundary detection works properly even
    // when we restrict a pattern to a sub-region of a string
    private val m = p.matcher("").useTransparentBounds(true)
    private val fullLine = regexp `startsWith` "^"

    def apply (line :LineV, start :Int) :Boolean = if (fullLine && start != 0) false else {
      m.reset(line)
      m.region(start, line.length)
      m.lookingAt
    }

    def capture (state :Matcher.State, start :Int) :Int = {
      // turn the captures into a list of matches
      case class Match (name :String, start :Int, end :Int) {
        def push () = state.pushScope(start, name)
        def pop () = state.popScope(end, name)
      }
      val ms = new ArrayBuffer[Match](captures.size)
      var cs = captures ; while (!cs.isEmpty) {
        val group = cs.head._1 ; val name = cs.head._2
        try {
          val mstart = m.start(group)
          if (mstart >= 0) ms += Match(name, mstart, m.end(group))
        } catch {
          case e :Exception =>
            println(s"Capture failure [re=$regexp, grp=$group, name=$name]: ${e.getMessage}")
        }
        cs = cs.tail
      }
      // now emit push/pops for those matches, in proper nesting order
      def emit (ii :Int) :Unit = if (ii < ms.length) {
        val m = ms(ii)
        m.push()
        // if the next match is "inside" this one, we need to "nest" its push/pops inside ours
        if (ii < ms.length-1 && ms(ii+1).start < m.end) { emit(ii+1) ; m.pop() }
        else { m.pop() ; emit(ii+1) }
      }
      emit(0)
      m.end
    }

    override def toString = s"'$p'${captures.map(_._1).mkString}"
  }

  /** Returns a matcher that applies `matchers` in turn, stopping when one of them matches
    * something. */
  def first (_id :String, matchers :List[Matcher]) = new Matcher() {
    def id = _id
    def apply (state :Matcher.State, line :LineV, start :Int) =
      applyFirst(matchers, state, line, start)
    def show (expand :Set[String], depth :Int) = showAt(matchers, expand, depth)
    override def toString = s"First(${matchers.mkString(", ")})"
  }

  /** Returns a pattern that matches `regexp` and names groups per `captures`. */
  def pattern (source :Rule, regexp :String, captures :List[(Int,String)]) :Pattern = try {
    // captures have to be in ascending order, so we sort them to be sure
    new Pattern(regexp, JPattern.compile(regexp), captures.sortBy(_._1))
  } catch {
    case e :Exception =>
      println(s"Error compiling '$regexp': ${e.getMessage}")
      println(s"Source rule:")
      source.print(new NDF.Writer(System.out, 0), false)
      new Pattern("NOMATCH", JPattern.compile("NOMATCH"), captures)
  }

  @tailrec private def applyFirst (ms :List[Matcher], state :State, line :LineV, start :Int) :Int =
    if (ms.isEmpty) -1
    else if (state.skips(ms.head)) applyFirst(ms.tail, state, line, start)
    else ms.head.apply(state, line, start) match {
      case  -1 => applyFirst(ms.tail, state, line, start)
      case end => /*println(s"Match: ${ms.head} at $start");*/ state.onMatch(start < end) ; end
    }

  private def showAt (ms :List[Matcher], expand :Set[String], depth :Int) =
    if (ms.isEmpty) "" else ms.map(_.show(expand, depth)).mkString("\n", "\n", "")

  class Deferred (group :String, incFn :String => List[Matcher]) extends Matcher {
    def id = group
    def apply (state :Matcher.State, line :LineV, start :Int) =
      applyFirst(incFn(group), state, line, start)
    def show (expand :Set[String], depth :Int) =
      nest(depth, toString) + showAt(
        if (expand(group)) incFn(group) else List(), expand - group, depth+1)
    override def toString = s"Deferred($group)"
  }

  /** A matcher that matches a regexp in a single line. */
  class Single (var id :String, pattern :Pattern) extends Matcher {
    def apply (state :Matcher.State, line :LineV, start :Int) =
      if (pattern.apply(line, start)) {
        if (Debug) debug(s"Matched '$id' $start $pattern '$line'")
        val end = pattern.capture(state, start)
        // we're not going to change the matcher stack, so if we somehow match a zero-length span,
        // we can't claim that match because otherwise will end up right back here matching again
        // in ye olde infinite loop; so instead we ignore this degenerate match; write better
        // regular expressions people!
        if (end == start) -1 else end
      } else -1
    def show (expand :Set[String], depth :Int) = nest(depth, toString)
    override def toString = s"Single($pattern)"
  }

  /** A matcher that matches a begin regexp, then applies a set of nested matchers until an end
    * regexp is seen (potentially on a new line). */
  class Multi (var id :String, open :Pattern, close :Pattern, name :Option[String],
               contentName :Option[String], contentMatchers :List[Matcher]) extends Matcher {

    private val contentEnd = new Matcher() {
      def id = s"${Multi.this.id}:close"
      def apply (state :Matcher.State, line :LineV, start :Int) = {
        applyFirst(contentMatchers, state, line, start) match {
          case -1 =>
            if (close.apply(line, start)) {
              state.popScope(start, contentName)
              val end = close.capture(state, start)
              if (Debug) debug(s"Matched '$id' [start=$start end=$end close=$close] '$line'")
              if (state.matchers.head != this) throw new IllegalStateException(
                s"Want to pop $this but see ${state.matchers.head}")
              state.matchers = state.matchers.tail
              state.popScope(end, name)
              // if we "matched" a multi-pattern but did not actually advance the scan position,
              // then we may be about to enter an infinite loop (go grammar author!); nip that in
              // the bud by adding this matcher to a skips set which will cause it to be skipped
              // until we next advance the scan position
              if (start == end) state.skips += Multi.this
              end
            } else -1
          case end => end
        }
      }
      def show (expand :Set[String], depth :Int) = showAt(contentMatchers, expand, depth)
      override def toString = s"MultiClose($open, $close, $name, $contentName)"
    }

    def apply (state :Matcher.State, line :LineV, start :Int) =
      if (open.apply(line, start)) {
        if (Debug) debug(s"Matched '${name || id}' [start=$start open=$open " +
                         s"cms=${contentMatchers.length} cname=$contentName] '$line'")
        state.pushScope(start, name)
        val contentStart = open.capture(state, start)
        state.matchers = contentEnd :: state.matchers
        state.pushScope(contentStart, contentName)
        contentStart
      } else {
        // if (Debug) debug(s"Did not match '${name || id}' @ $start '${line.toString.substring(start)}'")
        -1
      }

    def show (expand :Set[String], depth :Int) =
      nest(depth, toString) + showAt(contentMatchers, expand, depth+1)

    override def toString = s"Multi($open, $close, $name, $contentName)"
  }

  private def debug (msg :String) = println(msg)
}
