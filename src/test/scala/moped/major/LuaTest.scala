//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import org.junit._
import org.junit.Assert._

import moped._
import moped.code._
import moped.lua._

class LuaTest {

  val testCode = Seq(
    "function foo ()",
    "if (fget(k,6)) then",
    "a.is_pickup=true",
    "end",
    "end",

    "if foo then",
    "if (fget(k,6)) then",
    "a.is_pickup=true",
    "end",
    "end",
  )

  @Test def testIndenter () :Unit = {
    val buf = Buffer.scratch("Test.lua")
    buf.append(testCode.map(Line.apply))
    val indenter = LuaIndenter.create(Config.testConfig)
    for (ll <- buf.lines.indices) {
      var indent = indenter.apply(buf, ll)
      var line = buf.lines(ll)
      println(line)
      println(s"> $indent")
      println(s"# ${line.lineTags.head}")
    }
  }
}
