//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped

import org.junit._
import org.junit.Assert._

class BufferTest {

  val text = Seq("This is some text. Text is very interesting.",
                 "In this case we want to be sure the find fns work",
                 "and also don't match erroneously.")
  val buffer = Buffer("test", text.map(Line.apply))

  @Test def testFindForward () :Unit = {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findForward(mp, buffer.start))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) :Unit = {
      assertEquals(mloc, buffer.findForward(mm, buffer.start))
      // make sure a search for "match" exactly on match does match
      assertEquals(mloc, buffer.findForward(mm, mloc))
      // make sure a search for "match" just after match doesn't match
      assertEquals(Loc.None, buffer.findForward(mm, mloc.nextC))
    }
    test(Matcher.exact("match"))
    test(Matcher.regexp("\\bmatch\\b"))
  }

  @Test def testFindBackward () :Unit = {
    val mp = Matcher.exact("peanut")
    assertEquals(Loc.None, buffer.findBackward(mp, buffer.end))

    val mloc = Loc(2, 15)
    def test (mm :Matcher) :Unit = {
      assertEquals(s"findBackward($mm) matches", mloc, buffer.findBackward(mm, buffer.end))
      // make sure a search for "match" exactly on match does not match (findBackward starts
      // immediately prior to `start`)
      assertEquals(s"findBackward($mm) at match doesn't match",
                   Loc.None, buffer.findBackward(mm, mloc))
      // make sure a search for "match" before match also doesn't match
      assertEquals(s"findBackward($mm) at match,prevC doesn't match",
                   Loc.None, buffer.findBackward(mm, mloc.prevC))
    }
    test(Matcher.exact("match"))
    test(Matcher.regexp("\\bmatch\\b"))
  }

  @Test def testNextPrevCharSurrogatePairs () :Unit = {
    // 😀 (U+1F600) is outside the Basic Multilingual Plane, so it's a UTF-16 surrogate pair (two
    // Java chars); "hi 😀 bye" is thus 9 buffer columns long, not 8
    val line = "hi 😀 bye"
    val buf = Buffer("test", Seq(Line(line)))
    assertEquals(9, buf.lineLength(0))

    val beforeEmoji = Loc(0, 3) // just before the emoji's high surrogate
    val afterEmoji = Loc(0, 5) // just after the emoji's low surrogate

    // stepping forward over the emoji lands after both surrogates in one hop, not in the middle
    assertEquals(afterEmoji, buf.nextChar(beforeEmoji))
    // and stepping backward from there lands right back before it
    assertEquals(beforeEmoji, buf.prevChar(afterEmoji))

    // plain ASCII characters still step by exactly one column, as before
    assertEquals(Loc(0, 1), buf.nextChar(Loc(0, 0)))
    assertEquals(Loc(0, 0), buf.prevChar(Loc(0, 1)))

    // stepping forward/backward at the very start/end of the buffer is a safe no-op
    assertEquals(buf.start, buf.prevChar(buf.start))
    assertEquals(buf.end, buf.nextChar(buf.end))
  }
}
