//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import org.junit.Assert._
import org.junit.Test
import moped._

import org.treesitter._

class SitterTest {

  @Test def testNonAsciiOffsets () :Unit = {
    // a UTF-8 multi-byte character (an em-dash: 3 bytes in UTF-8, 1 UTF-16 code unit) earlier in
    // the buffer used to throw off every subsequent tree-sitter node position, since
    // node.getStartByte/getEndByte are UTF-8 byte offsets but Buffer only understands UTF-16
    // character offsets (see Sitter.ByteOffsets, which fixes this)
    val buffer = moped.impl.BufferImpl.scratch("test.ts")
    buffer.append(Seq(
      Line("// em—dash comment"),
      Line("const after = 2;")))

    val stylers = Map[String, Styler]("identifier" -> (_ => "var"))
    val syntaxers = Map[String, Syntaxer]()
    val sitter = new Sitter(new org.treesitter.TreeSitterTypescript(), buffer, stylers, syntaxers)
    sitter.connect(buffer, Signal[String]())

    // "after" occupies row 1, columns [6,11); were byte/char offsets confused, the style would
    // land two columns later (the em-dash's extra UTF-8 bytes), instead styling "ter = " partially
    assertFalse(buffer.stylesAt(Loc(1, 5)).contains("var")) // space just before "after"
    assertTrue(buffer.stylesAt(Loc(1, 6)).contains("var")) // 'a' of "after"
    assertTrue(buffer.stylesAt(Loc(1, 10)).contains("var")) // 'r' of "after"
    assertFalse(buffer.stylesAt(Loc(1, 11)).contains("var")) // space just after "after"
  }

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

  val typescript = """export class Widget extends Base implements Named {
                     |  private count: number = 0;
                     |
                     |  get size(): number {
                     |    return this.count;
                     |  }
                     |}
                     |
                     |interface Named { name: string }
                     |""".stripMargin

  @Test def testTypeScript () :Unit = {
    val parser = new TSParser()
    parser.setLanguage(new org.treesitter.TreeSitterTypescript())
    try {
      val tree = parser.parseString(null, typescript)
      try {
        val root = tree.getRootNode()
        // dump(typescript, root)
        assertEquals("program", root.getType)
        val exportStmt = root.getChild(0)
        assertEquals("export_statement", exportStmt.getType)
        val classDecl = exportStmt.getNamedChild(0)
        assertEquals("class_declaration", classDecl.getType)
        // confirm the class name node type that TypeScriptMode's styler keys off of
        assertEquals("type_identifier", classDecl.getChildByFieldName("name").getType)
        // confirm `extends Base` types Base as a plain identifier, not type_identifier (the
        // quirk TypeScriptMode's "extends_clause" case in its identifier styler exists for)
        val heritage = classDecl.getNamedChild(1)
        assertEquals("class_heritage", heritage.getType)
        val extendsClause = heritage.getNamedChild(0)
        assertEquals("extends_clause", extendsClause.getType)
        assertEquals("identifier", extendsClause.getChildByFieldName("value").getType)
      } finally tree.close()
    } finally parser.close()
  }

  @Test def testFunctionAndMethodCallScoping () :Unit = {
    val source = """function isChunkMismatchError(error: Error) {
  return error?.message?.includes("x") || error.name === 'ChunkLoadError';
}

export default function ErrorPage({ error }: { error: Error }) {
  posthog.capture('x', { error_message: error.message });
  window.location.reload();
}
"""
    val parser = new TSParser()
    parser.setLanguage(new org.treesitter.TreeSitterTsx())
    try {
      val tree = parser.parseString(null, source)
      try {
        val root = tree.getRootNode()
        // dump(source, root)

        // function declaration name: identifier under function_declaration (styled functionStyle)
        val decl = findFirst(root, "function_declaration").get
        assertEquals("identifier", decl.getChildByFieldName("name").getType)

        def propNamed (n :String) :TSNode = {
          def find (node :TSNode) :Option[TSNode] =
            if (node.getType == "property_identifier" &&
                source.substring(node.getStartByte, node.getEndByte) == n) Some(node)
            else (0 until node.getChildCount).view.flatMap(ii => find(node.getChild(ii))).headOption
          find(root).getOrElse(throw new AssertionError(s"couldn't find property_identifier '$n'"))
        }

        // plain property read `error.name` (used in a binary_expression, not called) must NOT be
        // treated as a method call: its member_expression's parent must not be call_expression
        val nameRead = propNamed("name")
        assertEquals("member_expression", nameRead.getParent.getType)
        assertNotEquals("call_expression", nameRead.getParent.getParent.getType)

        // `error?.message?.includes(...)`: "includes" is a method call (member_expression whose
        // parent is call_expression) via optional chaining; "message" along the way is not
        val includes = propNamed("includes")
        assertEquals("member_expression", includes.getParent.getType)
        assertEquals("call_expression", includes.getParent.getParent.getType)
        val message = propNamed("message")
        assertEquals("member_expression", message.getParent.getType)
        assertNotEquals("call_expression", message.getParent.getParent.getType)

        // `posthog.capture(...)`: same shape as above, without optional chaining
        val capture = propNamed("capture")
        assertEquals("member_expression", capture.getParent.getType)
        assertEquals("call_expression", capture.getParent.getParent.getType)

        // `window.location.reload()`: "reload" is the call; "location" (nested member access on
        // the way to it) is not
        val reload = propNamed("reload")
        assertEquals("member_expression", reload.getParent.getType)
        assertEquals("call_expression", reload.getParent.getParent.getType)
        val location = propNamed("location")
        assertEquals("member_expression", location.getParent.getType)
        assertNotEquals("call_expression", location.getParent.getParent.getType)
      } finally tree.close()
    } finally parser.close()
  }

  def findFirst (node :TSNode, typ :String) :Option[TSNode] =
    if (node.getType == typ) Some(node)
    else (0 until node.getChildCount).view.flatMap(ii => findFirst(node.getChild(ii), typ)).headOption

  @Test def testTsx () :Unit = {
    val source = """export const App = () => <div className="app">Hello</div>;"""
    val parser = new TSParser()
    parser.setLanguage(new org.treesitter.TreeSitterTsx())
    try {
      val tree = parser.parseString(null, source)
      try {
        val root = tree.getRootNode()
        // dump(source, root)
        assertEquals("program", root.getType)
        assertFalse(root.hasError())

        // confirm the node types/scoping that TypeScriptMode's JSX stylers key off of
        val jsxElement = findFirst(root, "jsx_element").get
        val openTag = jsxElement.getChildByFieldName("open_tag")
        assertEquals("jsx_opening_element", openTag.getType)
        assertEquals("identifier", openTag.getChildByFieldName("name").getType)
        val closeTag = jsxElement.getChildByFieldName("close_tag")
        assertEquals("jsx_closing_element", closeTag.getType)
        assertEquals("identifier", closeTag.getChildByFieldName("name").getType)

        val attr = findFirst(root, "jsx_attribute").get
        assertEquals("property_identifier", attr.getNamedChild(0).getType)
      } finally tree.close()
    } finally parser.close()
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
