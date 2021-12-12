//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import java.lang.reflect.Field
import scala.collection.mutable.{Map => MMap}
import moped._
import moped.util.Errors

abstract class ModeResolver (msvc :MetaService, window :Window, frame :Window#Frame) {

  /** Returns the names of all known modes, major if `major`, minor if not. */
  def modes (major :Boolean) :Set[String] = Set()

  /** Returns the names of all minor modes that match `tags`. */
  def tagMinorModes (tags :Seq[String]) :Set[String] = Set()

  /** Resolves and instantiates the major mode `mode` with the supplied environment. */
  def resolveMajor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    args :List[Any]) :MajorMode =
    resolve(mode, view, mline, disp, args, requireMajor(mode))

  /** Resolves and instantiates the minor mode `mode` with the supplied environment. */
  def resolveMinor (mode :String, view :BufferViewImpl, mline :ModeLine, disp :DispatcherImpl,
                    major :MajorMode, args :List[Any]) :Option[MinorMode] = {
    val modeClass = requireMinor(mode)
    val modeAnn = modeClass.getAnnotation(classOf[Minor])
    // ensure that all of the state classes required by this mode are available
    val stateKeys = view.buffer.state.keys
    if (Seq.from(modeAnn.stateTypes) forall stateKeys) Some(
      resolve(mode, view, mline, disp, major :: args, modeClass))
    else None
  }

  protected def locate (major :Boolean, mode :String) :Class[_]
  protected def configScope :Config.Scope
  protected def injectInstance[T] (clazz :Class[T], args :List[Any]) :T

  private def requireMajor (mode :String) = reqType(mode, classOf[MajorMode])
  private def requireMinor (mode :String) = reqType(mode, classOf[MinorMode])
  private def reqType[T] (mode :String, mclass :Class[T]) = {
    val isMajor = mclass == classOf[MajorMode]
    val clazz = locate(isMajor, mode)
    if (mclass.isAssignableFrom(clazz)) clazz.asInstanceOf[Class[T]]
    else throw new IllegalArgumentException(s"$mode ($clazz) is not a ${mclass.getSimpleName}.")
  }

  private def resolve[T] (mode :String, vw :BufferViewImpl, mln :ModeLine, dsp :DispatcherImpl,
                          args :List[Any], modeClass :Class[T]) :T = {
    val envargs = new Env {
      val msvc = ModeResolver.this.msvc
      val frame = ModeResolver.this.frame
      val window = ModeResolver.this.window
      val view = vw
      val mline = mln
      val disp = dsp
      def configScope = ModeResolver.this.configScope
    } :: args
    injectInstance(modeClass, envargs)
  }
}
