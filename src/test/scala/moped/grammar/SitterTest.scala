//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import org.junit.Assert._
import org.junit.{Test, Before}
import moped._

import ch.usi.si.seart.treesitter._

object Hack {
  var libLoaded = false
}

class SitterTest {

  @Before def loadLibrary () :Unit = {
    if (!Hack.libLoaded) {
      LibraryLoader.load()
      Hack.libLoaded = true
    }
  }

  @Test def testPython () :Unit = {
    val parser = Parser.getFor(Language.PYTHON)
    try {
      val source = "def foo(bar, baz):\n  print(bar)\n  print(baz)"
      val tree = parser.parse(source)
      val root = tree.getRootNode()
      // dump(source, root)
      assertEquals(1, root.getChildCount())
      assertEquals("module",  root.getType)
      assertEquals(0, root.getStartByte)
      assertEquals(44,  root.getEndByte)
      val function = root.getChild(0)
      assertEquals("function_definition", function.getType);
      assertEquals(5,  function.getChildCount)
    } finally {
      parser.close()
    }
  }

  val swift = """func fibonacci(n: Int) -> Int {
                |    let square_root_of_5 = sqrt(5.0)
                |    let p = (1 + square_root_of_5) / 2
                |    let q = 1 / p
                |    return Int((pow(p,CDouble(n)) + pow(q,CDouble(n))) / square_root_of_5 + 0.5)
                |}
                |
                |for i in 1...30 {
                |    println(fibonacci(i))
                |}""".stripMargin

  @Test def testSwift () :Unit = {
    val parser = Parser.getFor(Language.SWIFT)
    try {
      val tree = parser.parse(swift)
      val root = tree.getRootNode()
      // dump(swift, root)
      assertEquals("source_file",  root.getType)
      assertEquals(2, root.getChildCount())
      val function = root.getChild(0)
      assertEquals("function_declaration", function.getType);
      val for_loop = root.getChild(1)
      assertEquals("for_statement", for_loop.getType);
    } finally {
      parser.close()
    }
  }

  def dump (source :String, node :Node, indent :String = "") :Unit = {
    val text = source.substring(node.getStartByte, node.getEndByte)
    println(s"$indent$text :: ${node.getType}")
    val nindent = s"$indent  "
    for (ii <- 0 until node.getChildCount) {
      dump(source, node.getChild(ii), nindent)
    }
  }
}
