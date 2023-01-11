//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.io.{File, StringReader}
import java.util.HashMap
import org.junit.Assert._
import org.junit.Test
import moped._
import moped.impl.BufferImpl

class GrammarTest {

  val JdBegin = "punctuation.definition.directive.begin.javadoc"
  val JdEnd   = "punctuation.definition.directive.end.javadoc"
  val JdKey   = "punctuation.definition.keyword.javadoc"

  val KeyDoc  = (kind :String) => s"keyword.other.documentation.$kind.javadoc"
  val KeyDir  = (kind :String) => s"keyword.other.documentation.directive.$kind.javadoc"

  val javaDoc = new Grammar(
    name      = "JavaDoc",
    scopeName = "text.html.javadoc",
    foldingStartMarker = Some("""/\*\*"""),
    foldingStopMarker  = Some("""\*\*/""")) {

    val repository = Map(
      "inline" -> rules(
        "inline",
        include("#invalid") ::
        include("#inline-formatting") ::
        // include("text.html.basic"),
        single("""((https?|s?ftp|ftps|file|smb|afp|nfs|(x-)?man|gopher|txmt):""" +
               """\/\/|mailto:)[-:@a-zA-Z0-9_.~%+\/?=&#]+(?<![.?:])""",
               name = Some("markup.underline.link")) ::
        Nil),

      "inline-formatting" -> rules(
        "inline-formatting",
        map("code", "literal") { kind =>
          multi(
            begin = s"(\\{)((\\@)$kind)",
            beginCaptures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey),
            end = """\}""",
            endCaptures = List(0 -> JdEnd),
            name = Some(s"meta.directive.$kind.javadoc"),
            contentName = Some(s"markup.raw.$kind.javadoc"))
        } ++ map("docRoot", "inheritDoc") { kind =>
          single("""(\{)((\@)$kind)(\})""",
                 name = Some(s"meta.directive.$kind.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey, 4 -> JdEnd))
        } ++ map("link", "linkplain") { kind =>
          single(s"(\\{)((\\@)$kind)(?:\\s+(\\S+?))?(?:\\s+(.+?))?\\s*(\\})",
                 name = Some(s"meta.directive.$kind.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey,
                                 4 -> s"markup.underline.$kind.javadoc",
                                 5 -> "string.other.link.title.javadoc",
                                 6 -> JdEnd))
        } ++ List(
          single("""(\{)((\@)value)\s*(\S+?)?\s*(\})""",
                 name = Some("meta.directive.value.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir("value"), 3 -> JdKey,
                                 4 -> "variable.other.javadoc", 5 -> JdEnd)))),

      "invalid" -> rules(
        "invalid",
        single("""^(?!\s*\*).*$\n?""", Some("invalid.illegal.missing-asterisk.javadoc")) :: Nil)
    )

    // we have to specify a return type here to work around scalac bug; meh
    val patterns :List[Rule] = List(
      multi(
        begin = """(/\*\*)\s*""",
        beginCaptures = List(1 -> "punctuation.definition.comment.begin.javadoc"),
        end = """\*/""",
        endCaptures = List(0 -> "punctuation.definition.comment.end.javadoc"),
        name = Some("comment.block.documentation.javadoc"),
        patterns = List(
          include("#invalid"),
          multi(
            begin = """\*\s*(?=\w)""",
            end = """(?=\s*\*\s*@)|(?=\s*\*\s*/)""",
            name = Some("meta.documentation.comment.javadoc"),
            contentName = Some("text.html"),
            patterns = List(include("#inline")))) ++
          map("param", "return", "throws", "exception", "author", "version", "see", "since",
              "serial", "serialField", "serialData", "deprecated") { kind =>
            multi(
              begin = s"\\*\\s*((\\@)$kind)",
              beginCaptures = List(1 -> KeyDoc(kind), 2 ->  JdKey),
              end = """(?=\s*\*\s*@)|(?=\s*\*\s*/)""",
              name = Some(s"meta.documentation.tag.$kind.javadoc"),
              contentName = Some("text.html"),
              patterns = List(include("#inline")))
          } ++ List(
            single("""\*\s*((\@)\S+)\s""", captures = List(1 -> KeyDoc("custom"), 2 -> JdKey)))))
  }

  def testBuffer (name :String, text :String) = BufferImpl(new TextStore(name, "", text))

  def assertScopesEqual (want :List[String], got :List[String]) :Unit = {
    if (want != got) {
      val fmt = s"%${want.map(_.length).max}s | %s"
      fail("Scope mismatch (want | got):\n" +
        want.zipAll(got, "", "").map(t => fmt.format(t._1, t._2)).mkString("\n"))
    }
  }

  val testJavaCode = Seq(
    //                1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/**",
    /* 3*/ " * This is some test Java code that we'll use to test {@code Grammar} and specifically",
    /* 4*/ " * the {@literal JavaDoc} grammar.",
    /* 5*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 6*/ " */",
    /* 7*/ "public class Test {",
    /* 8*/ "   /**",
    /* 9*/ "    * A constructor, woo!",
    /*10*/ "    * @param foo for fooing.",
    /*11*/ "    */",
    /*12*/ "   public Test () {}",
    /*13*/ "",
    /*14*/ "   /**",
    /*15*/ "    * A method. How exciting. Let's {@link Test} to something.",
    /*16*/ "    * @throws IllegalArgumentException if we feel like it.",
    /*17*/ "    */",
    /*18*/ "   @Deprecated(\"Use peanuts\")",
    /*19*/ "   public void test (int count) {}",
    /*20*/ "}").mkString("\n")

  val commentStart = List("text.html.javadoc",
                          "comment.block.documentation.javadoc",
                          "punctuation.definition.comment.begin.javadoc")
  val literalAt = List("text.html.javadoc",
                       "comment.block.documentation.javadoc",
                       "meta.documentation.comment.javadoc",
                       "text.html",
                       "meta.directive.literal.javadoc",
                       "keyword.other.documentation.directive.literal.javadoc",
                       "punctuation.definition.keyword.javadoc")
  val literalToken = List("text.html.javadoc",
                          "comment.block.documentation.javadoc",
                          "meta.documentation.comment.javadoc",
                          "text.html",
                          "meta.directive.literal.javadoc",
                          "keyword.other.documentation.directive.literal.javadoc")

  val log = new Logger() {
    def log (msg :String) = println(msg)
    def log (msg :String, exn :Throwable) :Unit = { println(msg) ; exn.printStackTrace(System.err) }
  }

  @Test def testJavaDocMatch () :Unit = {
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = Grammar.testScoper(Seq(javaDoc), buffer, Nil)
    scoper.rethinkBuffer()
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(4, 8)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(4, 9)))
  }

  val smallTestCode = Seq(
    //                1         2         3         4         5         6
    //      012345678901234567890123456789012345678901234567890123456789012345678
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/**", // 901234567890123456789012345678901234567890123456789012345678
    /* 3*/ " * Blah blah {@literal Grammar} blah blah {@code JavaDoc} grammar.",
    /* 4*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 5*/ " */").mkString("\n")

  def smallTestBits () = {
    val buffer = testBuffer("Test.java", smallTestCode)
    val didEdit = Signal[String]()
    val scoper = Grammar.testScoper(Seq(javaDoc), buffer, Nil).connect(buffer, didEdit)
    // do some precondition tests
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(3, 14)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(3, 15)))
    (buffer, didEdit, scoper)
  }

  @Test def testWordInsert () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.insert(Loc(3, 8), Line("blah "))
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(3, 19)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(3, 20)))
  }

  @Test def testNewlineInsert () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.insert(Loc(3, 0), Seq(Line(""), Line("")))
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(4, 14)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(4, 15)))
  }

  @Test def testRaggedInsert () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.insert(Loc(3, 0), Seq(Line(" "), Line(" ")))
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(4, 15)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(4, 16)))
  }

  @Test def testWordDelete () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.delete(Loc(3, 8), 5)
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(3, 9)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(3, 10)))
  }

  @Test def testNewlineDelete () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.delete(Loc(1, 0), Loc(2, 0))
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(1, 0)))
    assertScopesEqual(literalAt, scoper.scopesAt(Loc(2, 14)))
    assertScopesEqual(literalToken, scoper.scopesAt(Loc(2, 15)))
  }

  @Test def testEnclosingDelete () :Unit = {
    val (buffer, didEdit, scoper) = smallTestBits()
    buffer.delete(Loc(3, 13), Loc(3, 32))
    didEdit.emit("")
    assertScopesEqual(commentStart, scoper.scopesAt(Loc(2, 0)))
    val scopes = List("text.html.javadoc",
                      "comment.block.documentation.javadoc",
                      "meta.documentation.comment.javadoc",
                      "text.html")
    assertScopesEqual(scopes, scoper.scopesAt(Loc(3, 14)))
    assertScopesEqual(scopes, scoper.scopesAt(Loc(3, 15)))
  }

  @Test def testParse () :Unit = {
    val javaDocP = getClass.getClassLoader.getResourceAsStream("grammar/JavaDoc.tmLanguage")
    val javaP = getClass.getClassLoader.getResourceAsStream("grammar/Java.tmLanguage")
    val javaDoc = Grammar.parsePlist(javaDocP)
    val java = Grammar.parsePlist(javaP)
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = Grammar.testScoper(Seq(javaDoc, java), buffer, Nil)
    // println(scoper)
  }

  // @Test def testPrint () :Unit = {
  //   val javaDoc = Grammar.parsePlist(
  //     getClass.getClassLoader.getResourceAsStream("JavaDoc.tmLanguage"))
  //   javaDoc.print(System.out)
  // }
}
