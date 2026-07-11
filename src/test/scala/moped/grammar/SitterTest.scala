//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import org.junit.Assert._
import org.junit.Test
import moped._

import org.treesitter._

class SitterTest {

  @Test def testPython () :Unit = {
    val parser = new TSParser()
    parser.setLanguage(new TreeSitterPython())
    try {
      val source = "def foo(bar, baz):\n  print(bar)\n  print(baz)"
      val tree = parser.parseString(null, source)
      try {
        val root = tree.getRootNode()
        // dump(source, root)
        assertEquals(1, root.getChildCount())
        assertEquals("module",  root.getType)
        assertEquals(0, root.getStartByte)
        assertEquals(44,  root.getEndByte)
        val function = root.getChild(0)
        assertEquals("function_definition", function.getType);
        assertEquals(5,  function.getChildCount)
      } finally tree.close()
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
    val parser = new TSParser()
    parser.setLanguage(Sitter.loadNative("swift", "tree_sitter_swift"))
    try {
      val tree = parser.parseString(null, swift)
      try {
        val root = tree.getRootNode()
        // dump(swift, root)
        assertEquals("source_file",  root.getType)
        assertEquals(2, root.getChildCount())
        val function = root.getChild(0)
        assertEquals("function_declaration", function.getType);
        val for_loop = root.getChild(1)
        assertEquals("for_statement", for_loop.getType);
      } finally tree.close()
    } finally {
      parser.close()
    }
  }

  val prismaSchema = """datasource db {
                       |  provider = "postgresql"
                       |  url      = env("DATABASE_URL")
                       |}
                       |
                       |model User {
                       |  id    Int     @id @default(autoincrement())
                       |  email String  @unique
                       |  name  String?
                       |}""".stripMargin

  @Test def testPrisma () :Unit = {
    val parser = new TSParser()
    parser.setLanguage(Sitter.loadNative("prisma", "tree_sitter_prisma"))
    try {
      val tree = parser.parseString(null, prismaSchema)
      try {
        val root = tree.getRootNode()
        // dump(prismaSchema, root)
        assertEquals("program",  root.getType)
        assertEquals(2, root.getChildCount())
        val datasource = root.getChild(0)
        assertEquals("datasource_declaration", datasource.getType)
        val model = root.getChild(1)
        assertEquals("model_declaration", model.getType)
      } finally tree.close()
    } finally {
      parser.close()
    }
  }

  @Test def testIncrementalEditing () :Unit = {
    val buffer = moped.impl.BufferImpl.scratch("test.py")
    buffer.append(Seq(
      Line("# leading comment"),
      Line("def foo():"),
      Line("    pass")))
    // consume the initial insert edit the buffer emits for the append() above, so it doesn't
    // confuse the Sitter we're about to connect (which isn't listening yet anyway, but this
    // mirrors how a real buffer is populated before a mode attaches to it)

    val stylers = Map[String, Styler]("comment" -> (_ => "cmt"))
    val syntaxers = Map[String, Syntaxer]()
    val sitter = new Sitter(new TreeSitterPython(), buffer, stylers, syntaxers)
    val didInvoke = Signal[String]()
    sitter.connect(buffer, didInvoke)

    def hasCmt (loc :Loc) = buffer.stylesAt(loc).contains("cmt")

    // initial parse should have styled the leading comment
    assertTrue(hasCmt(Loc(0, 2)))
    assertFalse(hasCmt(Loc(1, 2))) // "def foo():" is not a comment

    // insert a new comment-only line between the def and the pass, and a trailing comment on the
    // last line; this exercises multiple edits arriving before a single rethink
    buffer.insertLine(Loc(2, 0), Line("    # inner comment"))
    buffer.insert(buffer.end, Line("  # trailing"))
    didInvoke.emit("did-invoke") // trigger the batched rethink, as a real mode dispatch would

    // the newly inserted comment should now be styled...
    assertTrue(hasCmt(Loc(2, 6)))
    assertTrue(hasCmt(buffer.end.atCol(buffer.end.col-2)))
    // ...and the original comment should still be styled (proving the incremental restyle didn't
    // clobber/lose styling outside the edited rows)
    assertTrue(hasCmt(Loc(0, 2)))
    // and the non-comment lines should still not be styled as comments
    assertFalse(hasCmt(Loc(1, 2)))
    assertFalse(hasCmt(Loc(3, 4)))
  }

  def dump (source :String, node :TSNode, indent :String = "") :Unit = {
    val text = source.substring(node.getStartByte, node.getEndByte)
    println(s"$indent$text :: ${node.getType}")
    val nindent = s"$indent  "
    for (ii <- 0 until node.getChildCount) {
      dump(source, node.getChild(ii), nindent)
    }
  }
}
