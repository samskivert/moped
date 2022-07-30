//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import org.junit.Assert._
import org.junit._
import moped._

class SelectorTest {

  @Test def testMatchDepth () :Unit = {
    val scopes = List("punctuation.definition.keyword.javadoc",
                      "keyword.other.documentation.directive.literal.javadoc",
                      "meta.directive.literal.javadoc",
                      "text.html",
                      "meta.documentation.comment.javadoc",
                      "comment.block.documentation.javadoc")

    val sel1 = Selector.parse("keyword")
    sel1.checkMatch(scopes, null, (fn, mstr, depth) => {
      assertEquals(5, depth)
      assertEquals("keyword", mstr)
    })
    val sel2 = Selector.parse("comment.block")
    sel2.checkMatch(scopes, null, (fn, mstr, depth) => {
      assertEquals(1, depth)
      assertEquals("comment.block", mstr)
    })
  }
}
