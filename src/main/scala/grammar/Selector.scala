//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import moped._

/** Matches scopes. */
abstract class Selector {

  /** Calls `onMatch` with `fn`, the matched scope and match depth if `this` matches `scopes`.
    * @return true if onMatch was called, false otherwise. */
  def checkMatch (scopes :List[String], fn :Selector.Fn, onMatch :Selector.OnMatchFn) :Boolean
}

object Selector {

  type OnMatchFn = (Selector.Fn,String,Int) => Unit

  /** A hook that applies a fn to spans which match a scope selector. */
  abstract class Fn (val selector: Selector) {
    /** Called for every span that matches our `selector`. */
    def apply (buf :Buffer, start :Loc, end :Loc) :Unit
  }

  /** A helper class for using a group of selectors to process a buffer. The canonical use is to use
    * a collection of selector to style mappings (i.e. a TextMate theme) to apply style classes to
    * the appropriate regions of the buffer.
    */
  class Processor (sels :List[Fn]) {

    /** Called before applying this processor to any spans on `row` in `buf`. */
    def onBeforeLine (buf :Buffer, row :Int) :Unit = {
      // nada by default
    }

    // NOTE: the code in apply and addMatch is called a bajillion times per buffer (dozens of times
    // on every span of every line in the entire buffer); so we go to great pains to avoid creating
    // unnecessary garbage and generally to be as efficient as possible; hence all the mutable
    // variables and manual for loops (thanks for making that painful Scala)

    /** Applies this processor to the `span` on `row` in `buf`. */
    def apply (buf :Buffer, row :Int, span :Span) :Unit = {
      val start = Loc(row, span.start) ; val end = Loc(row, span.end)
      // check all of our selectors and accumulate all of them that match (the addMatch fn below
      // will handle keeping the most specific matches based on match depth and matched scope
      // prefix length)
      var ss = sels ; while (!ss.isEmpty) {
        ss.head.selector.checkMatch(span.scopes, ss.head, addMatchFn)
        ss = ss.tail
      }
      // if we have no matches, let the processor do special "unmatched" processing
      if (_curfns.isEmpty) onUnmatched(buf, start, end)
      // otherwise go through and apply all of the matched styling fns
      else {
        var ii = 0 ; val ll = _curfns.size
        while (ii < ll) { _curfns(ii).apply(buf, start, end) ; ii += 1 }
        _curdepth = -1
        _curfns.clear()
        _curmstrs.clear()
      }
    }

    /** Called for spans that did not match any of our supplied selectors. This is useful, for
      * example, for clearing styles from unmatched spans which may have previously matched.
      */
    protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) :Unit = {
    }

    private def addMatch (fn :Fn, mstr :String, mdepth :Int) :Unit = {
      // if our depth is greater than the existing matches, clear them out and add ourself
      if (mdepth > _curdepth) {
        _curfns.clear()
        _curmstrs.clear()
        _curdepth = mdepth
      }
      // if any existing match subsumes us, then don't add ourselves;
      // contrarily if we subsume any existing matches, remove them
      var ii = 0 ; var ll = _curmstrs.size ; while (ii < ll) {
        val exmstr = _curmstrs(ii)
        // if it starts with us, then we're subsumed; abort abort!
        if (exmstr startsWith mstr) return
        // if we start with it, then it's subsumed
        if (mstr startsWith exmstr) {
          _curfns.remove(ii) ; _curmstrs.remove(ii)
          ii -= 1 ; ll -= 1
        }
        ii += 1
      }
      _curfns += fn
      _curmstrs += mstr
    }

    private val _curfns = ArrayBuffer[Fn]()
    private val _curmstrs = ArrayBuffer[String]()
    private var _curdepth = -1
    private[this] val addMatchFn = addMatch(_, _, _)
  }

  /** Parses a selector description.
    * @see http://manual.macromates.com/en/scope_selectors.html */
  def parse (selstr :String) :Selector = selstr split(",") map(_.trim) match {
    case Array(selstr) => parse0(selstr)
    case selstrs => new Any(List.from(selstrs).map(parse0))
    null
  }

  private def parse0 (selstr :String) :Selector = selstr.indexOf("-") match {
    case -1 => new Path(List.from(selstr.split(" ")).map(_.trim).reverse)
    case ii => new Exclude(parse0(selstr.substring(0, ii)), parse0(selstr.substring(ii+1)))
  }

  private class Any (sels :List[Selector]) extends Selector {
    override def checkMatch (scopes :List[String], fn :Fn, onMatch :OnMatchFn) = {
      var matched = false
      var ss = sels ; while (!ss.isEmpty) {
        if (ss.head.checkMatch(scopes, fn, onMatch)) matched = true
        ss = ss.tail
      }
      matched
    }
    override def toString = sels.mkString(", ")
  }

  private class Exclude (want :Selector, dontWant :Selector) extends Selector {
    override def checkMatch (scopes :List[String], fn :Fn, onMatch :OnMatchFn) = {
      if (dontWant.checkMatch(scopes, fn, NoopOnMatch)) false
      else want.checkMatch(scopes, fn, onMatch)
    }
    override def toString = s"$want - $dontWant"
  }

  private class Path (pres :List[String]) extends Selector {
    override def checkMatch (scopes :List[String], fn :Fn, onMatch :OnMatchFn) :Boolean = {
      var ss = scopes ; var ps = pres ; var depth = -1
      while (!ss.isEmpty && !ps.isEmpty) {
        if (ss.head startsWith ps.head) {
          if (depth == -1) depth = ss.length
          if (ps.tail.isEmpty) {
            onMatch(fn, ps.head, depth)
            return true
          }
          ps = ps.tail
        }
        ss = ss.tail
      }
      false
    }
    override def toString = pres.reverse.mkString(" ")
  }

  private val NoopOnMatch :OnMatchFn = (_, _, _) => {}
}
