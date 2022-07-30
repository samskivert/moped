//
// Moped TextMate Grammar - a library for using TextMate language grammars with Moped
// http://github.com/samskivert/moped-textmate-grammar/blob/master/LICENSE

package moped.grammar

import java.io.InputStream
import java.nio.file.Path
import moped._

object NDFGrammar {
  import NDF._

  def toGrammar (root :Seq[Entry]) :Grammar = {
    val (strs, dicts) = toMaps(root)
    val name = strs.get("name") || "unknown:name"
    val scopeName = strs.get("scopeName") || "unknown:scopeName"
    val foldStart = strs.get("foldStart")
    val foldStop  = strs.get("foldStop")

    // val fileTypes = rootDict.objectForKey("fileTypes")
    new Grammar(name, scopeName, foldStart, foldStop) {
      val repository = Map() ++ (dicts.get("repository") || Seq()).flatMap {
        case DicEnt(k, v) => Some(k -> new Rule.Container(k, v.flatMap(parseRule).toList))
        case e @ StrEnt(k, v) => println("Invalid repository entry: $e") ; None
      }
      val patterns = parseRules(dicts)
    }
  }

  def toMaps (es :Seq[Entry]) :(Map[String,String],Map[String,Seq[Entry]]) = {
    val (strs, dicts) = (Map.newBuilder[String,String], Map.newBuilder[String,Seq[Entry]])
    es foreach {
      case StrEnt(k, v)  => strs += (k -> v)
      case DicEnt(k, vs) => dicts += (k -> vs)
    }
    (strs.result(), dicts.result())
  }

  def parseInlineCaps (str :String) = {
    def parse (cap :String) = cap.indexOf("=") match {
      case -1 => println(s"Invalid capture: '$cap'") ; None
      case ii => Some(cap.substring(0, ii).toInt -> cap.substring(ii+1))
    }
    List.from(str.split("\\s+")).flatMap(parse)
  }
  def parseDictCaps (ents :Seq[Entry]) = ents.flatMap(ent => ent match {
    case StrEnt(k, v) => Some(k.toInt -> v)
    case DicEnt(k, vs) => println(s"Invalid capture: '$ent'") ; None
  }).toList
  def parseCaptures (
    name :String, strs :Map[String,String], dicts :Map[String,Seq[Entry]],
    defCaps :List[(Int,String)] = List()
  ) :List[(Int,String)] =
    strs.get(name).map(parseInlineCaps) orElse dicts.get(name).map(parseDictCaps) getOrElse defCaps

  def parseRules (dict :Map[String,Seq[Entry]]) :List[Rule] =
    (dict.get("patterns") || List()).flatMap(parseRule).toList

  def parseRule (data :Entry) :Option[Rule] = try data match {
    case StrEnt("include", target) => Some(new Rule.Include(target))
    case DicEnt("single", rdata) =>
      val (strs, dicts) = toMaps(rdata)
      Some(new Rule.Single(strs("pattern"), strs.get("name"), parseCaptures("caps", strs, dicts)))
    case DicEnt("multi", rdata) =>
      val (strs, dicts) = toMaps(rdata)
      val begin = strs.get("begin") getOrElse {
        throw new Exception(s"Rule missing begin: $rdata")
      }
      val caps = parseCaptures("caps", strs, dicts)
      val beginCaps = parseCaptures("bcaps", strs, dicts, caps)
      val end = strs.get("end") getOrElse {
        throw new Exception(s"Rule missing end: $rdata")
      }
      val endCaps = parseCaptures("ecaps", strs, dicts, caps)
      Some(new Rule.Multi(begin, beginCaps, end, endCaps, strs.get("name"),
                          strs.get("contentName"), parseRules(dicts)))
    case _ => println(s"Invalid rule data: $data") ; None

  } catch {
    case e :Exception =>
      System.err.println(s"Rule parse failure: $data")
      e.printStackTrace(System.err)
      None
  }
}
