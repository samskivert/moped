//
// Moped - a scalable editor extensible via JVM languages
// http://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.io.PrintStream
import scala.collection.SortedSet
import scala.collection.mutable.Builder

import moped._
import moped.grammar.Matcher

/** Models a single TextMate grammar rule. Instead of having one giant intertwingled mess, we model
  * grammar rules with a variety of subclasses, each of which handles a particular common case.
  * This means that there are various "expressible" grammar rules which we cannot model, but those
  * are nonsensical and ideally do not show up in the wild.
  */
abstract class Rule {

  /** Compiles this rule into a sequence of matchers. */
  def compile (id :String, incFn :String => List[Matcher]) :List[Matcher]

  /** Prints a debug representation of this rule. */
  def print (out :NDF.Writer, children :Boolean = true) :Unit

  /** Adds any scope names matched by this rule to `names`. */
  def collectNames (names :Builder[String, SortedSet[String]]) :Unit = {}
}

object Rule {

  case class Include (group :String) extends Rule {
    override def compile (id :String, incFn :String => List[Matcher]) = List(
      new Matcher.Deferred(group, incFn))
    override def print (out :NDF.Writer, children :Boolean) = out.emit("include", group)
  }

  case class Container (id :String, patterns :List[Rule]) extends Rule {
    override def compile (id :String, incFn :String => List[Matcher]) =
      patterns.flatMap(_.compile(id, incFn)) // TODO: add index
    override def print (out :NDF.Writer, children :Boolean) = patterns.foreach(_.print(out))
    override def collectNames (names :Builder[String, SortedSet[String]]) =
      patterns.foreach(_.collectNames(names))
  }

  case class Single (pattern :String, name :Option[String], captures :List[(Int,String)])
      extends Rule {
    override def compile (id :String, incFn :String => List[Matcher]) = {
      val caps = name.map(n => (0 -> n) :: captures).getOrElse(captures)
      List(new Matcher.Single(id, Matcher.pattern(this, pattern, caps)))
    }
    override def print (out :NDF.Writer, children :Boolean) = {
      val w = out.nest("single")
      w.emit("name", name)
      w.emit("pattern", pattern)
      if (!captures.isEmpty) w.emit("caps", fmt(captures))
    }
    override def collectNames (names :Builder[String, SortedSet[String]]) :Unit = {
      name.foreach(names += _)
      names ++= captures.map(_._2)
    }
  }

  case class Multi (begin :String, beginCaptures :List[(Int,String)],
                    end :String, endCaptures :List[(Int,String)],
                    name :Option[String], contentName :Option[String],
                    patterns :List[Rule]) extends Rule {
    override def compile (id :String, incFn :String => List[Matcher]) = List(
      new Matcher.Multi(id, Matcher.pattern(this, begin, beginCaptures),
                        Matcher.pattern(this, end, endCaptures),
                        name, contentName, patterns.flatMap(_.compile(id, incFn))))
    override def print (out :NDF.Writer, children :Boolean) :Unit = {
      val w = out.nest("multi")
      w.emit("name", name)
      w.emit("contentName", contentName)
      w.emit("begin", begin)
      if (!beginCaptures.isEmpty) w.emit("bcaps", fmt(beginCaptures))
      w.emit("end", end)
      if (!endCaptures.isEmpty) w.emit("ecaps", fmt(endCaptures))
      if (children && !patterns.isEmpty) {
        val pw = w.nest("patterns")
        patterns.foreach(_.print(pw))
      }
    }
    override def collectNames (names :Builder[String, SortedSet[String]]) :Unit = {
      name.foreach(names += _)
      contentName.foreach(names += _)
      names ++= beginCaptures.map(_._2)
      names ++= endCaptures.map(_._2)
    }
  }

  private def fmt (caps :List[(Int,String)]) = caps.map(c => s"${c._1}=${c._2}").mkString(" ")
}
