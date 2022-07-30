//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import scala.jdk.CollectionConverters._
import com.eclipsesource.json._
import java.nio.file.{Files, Path, Paths}
import moped._

object JsonGrammar {

  implicit class Reader (obj :JsonObject) {
    def optValue (name :String) = Option(obj.get(name))
    def reqValue (name :String) = obj.get(name) match {
      case null => throw new Exception(s"Missing required property '$name'")
      case jval => jval
    }

    def reqString (name :String) = reqValue(name).asString
    def optString (name :String) = optValue(name).map(_.asString)
  }

  def parse (path :Path) = {
    val json = Json.parse(Files.newBufferedReader(path)).asObject
    new Grammar(json.reqString("name"), json.reqString("scopeName"), None, None) {
      val repository = Map() ++ json.reqValue("repository").asObject.asScala.map(
        mem => (mem.getName, parseRule(mem.getValue.asObject)))
      val patterns = List() ++ parseRules(json.get("patterns"))
    }
  }

  def parseRules (json :JsonValue) = json match {
    case null => Nil
    case arr :JsonArray => List() ++ arr.asArray.asScala.map(_.asObject).map(parseRule)
    case _ => warn(s"Invalid patterns: '$json'") ; Nil
  }

  def parseRule (obj :JsonObject) :Rule = {
    parseSingle(obj) orElse
    parseMulti(obj) orElse
    parseInclude(obj) orElse
    parseContainer(obj) getOrElse
    { warn(s"Invalid rule: '$obj'") ; Rule.Container("<error>", Nil) }
  }

  def parseSingle (obj :JsonObject) = obj.optString("match").map(
    m => Rule.Single(m, obj.optString("name"), parseCaps(obj.optValue("captures"))))
  def parseMulti (obj :JsonObject) = obj.optString("begin").map(
    begin => Rule.Multi(begin,
                        parseCaps(obj.optValue("beginCaptures") orElse obj.optValue("captures")),
                        obj.reqString("end"),
                        parseCaps(obj.optValue("endCaptures") orElse obj.optValue("captures")),
                        obj.optString("name"), None, // TODO: contentName?
                        parseRules(obj.get("patterns"))))
  def parseInclude (obj :JsonObject) = obj.optString("include").map(
    group => Rule.Include(group))
  def parseContainer (obj :JsonObject) = obj.optValue("patterns").map(
    pats => Rule.Container("patterns", parseRules(pats)))

  def parseCaps (json :Option[JsonValue]) :List[(Int,String)] = try json match {
    case Some(obj :JsonObject) => List() ++ obj.asScala.flatMap(
      mem => mem.getValue.asObject.get("name") match {
        case null => None
        case name => Some(mem.getName.toInt -> name.asString)
      })
    case _ => Nil
  } catch {
    case t :Throwable => warn(s"Invalid captures: '$json': $t") ; Nil
  }

  def warn (msg :String) = System.err.println(msg)
}
