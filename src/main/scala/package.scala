//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

import java.{util => ju}
import java.util.{function => juf}

package object moped {

  type uV = scala.annotation.unchecked.uncheckedVariance
  type tailrec = scala.annotation.tailrec

  // for great Java interop
  type JBoolean   = java.lang.Boolean
  type JByte      = java.lang.Byte
  type JShort     = java.lang.Short
  type JCharacter = java.lang.Character
  type JInteger   = java.lang.Integer
  type JLong      = java.lang.Long
  type JFloat     = java.lang.Float
  type JDouble    = java.lang.Double

  type JIterator[+A]  = ju.Iterator[A @uV]
  type JIterable[+A]  = java.lang.Iterable[A @uV]
  type JStringBuilder = java.lang.StringBuilder

  // TODO: make these aliases if SI-8079 is ever fixed; sigh
  trait JConsumer[-T] extends juf.Consumer[T @uV]
  trait JPredicate[-T] extends juf.Predicate[T @uV]
  trait JFunction[-T,+R] extends juf.Function[T @uV, R @uV]

  // a Scala reimplementation of Java 8's try-with-resources
  @inline def using[R <: AutoCloseable, T] (rsrc :R)(block :(R => T)) :T = {
    var exn :Throwable = null
    try block(rsrc)
    catch { case t :Throwable => exn = t ; throw t }
    finally {
      if (exn == null) rsrc.close()
      else try rsrc.close()
      catch { case t :Throwable => exn.addSuppressed(t) }
    }
  }

  extension [A] (opt: Option[A])
    def || (v : => A): A = opt getOrElse v

}
