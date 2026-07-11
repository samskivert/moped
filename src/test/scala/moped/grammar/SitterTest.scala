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

  def dump (source :String, node :TSNode, indent :String = "") :Unit = {
    val text = source.substring(node.getStartByte, node.getEndByte)
    println(s"$indent$text :: ${node.getType}")
    val nindent = s"$indent  "
    for (ii <- 0 until node.getChildCount) {
      dump(source, node.getChild(ii), nindent)
    }
  }
}
