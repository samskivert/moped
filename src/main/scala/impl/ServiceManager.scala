//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import moped._

class ServiceManager (app :Moped) extends AbstractService with MetaService {

  private var services = Mutable.cacheMap { (iclass :Class[?]) =>
    startService(iclass.asInstanceOf[Class[AbstractService]]) }

  // we provide MetaService, so stick ourselves in the cache directly; meta!
  services.put(getClass, this)
  // wire the workspace and package managers up directly as well
  services.put(app.pkgMgr.getClass, app.pkgMgr)
  services.put(app.wspMgr.getClass, app.wspMgr)

  override def metaFile (name :String) = app.pkgMgr.metaDir.resolve(name)
  override def log = app.logger
  override def exec = app.exec
  override def service[T] (clazz :Class[T]) :T = resolveService(clazz).asInstanceOf[T]
  override def process[P] (thunk: => P) :Pipe[P] = new Plumbing(exec.bg, thunk)

  override def injectInstance[T] (clazz :Class[T], args :List[Any]) :T = {
    def fail (t :Throwable) = throw new InstantiationException(
      s"Unable to inject $clazz [args=$args]").initCause(t)
    try {
      // println(s"Creating instance of ${clazz.getName}")
      val ctor = clazz.getConstructors match {
        case Array(ctor) => ctor
        case ctors       => throw new IllegalArgumentException(
          s"${clazz.getName} must have only one constructor [has=${ctors.mkString(", ")}]")
      }

      // match the args to the ctor parameters; the first arg of the desired type is used and then
      // removed from the arg list, so we can request multiple args of the same type as long as
      // they're in the correct order
      var remargs = ArrayBuffer[Any]()
      remargs ++= args
      remargs ++= List(log, exec, app)
      val params = ctor.getParameterTypes.map { p => remargs.find(p.isInstance) match {
        case Some(arg) => remargs -= arg ; arg
        case None =>
          if (p.getName.endsWith("Service")) resolveService(p)
          else throw new InstantiationException(
            s"Unable to resolve mode arg: $p (args: $args, remargs: $remargs")
      }}
      ctor.newInstance(params.asInstanceOf[Array[Object]]*).asInstanceOf[T]
    } catch {
      case ite :InvocationTargetException => fail(ite.getCause)
      case t :Throwable => fail(t)
    }
  }

  override def resolvePlugins[T <: AbstractPlugin] (clazz :Class[T], args :List[Any]) :Seq[T] = {
    import org.reflections.scanners.Scanners._
    var classes = app.pkgMgr.reflections.get(SubTypes.of(clazz).asClass()).asScala.
      filter(_.getAnnotation(classOf[Plugin]) != null)
    classes.map(pc => injectInstance(pc, args).asInstanceOf[T]).toSeq
  }

  override def didStartup () :Unit = {} // unused
  override def willShutdown () :Unit = {} // unused

  // iterates over all known services and resolves any that are marked `autoLoad`; this is called
  // after the editor is fully initialized but before it loads its starting buffers; we can't do
  // this during our constructor because of initialization inter-depends with plugin manager
  def resolveAutoLoads () :Unit = app.pkgMgr.autoLoadServices.foreach(resolveService)

  def shutdown () :Unit = {
    services.asMap.values.forEach { svc =>
      try svc.willShutdown()
      catch {
        case ex :Throwable => app.logger.log(s"Failed to shutdown $svc", ex)
      }
    }
  }

  private def resolveService (sclass :Class[?]) :AbstractService = {
    if (!sclass.getName.endsWith("Service")) throw new InstantiationException(
      s"Service classes must be named FooService: $sclass")
    else if (sclass.getName == "moped.MetaService") return this
    else if (sclass.getName == "moped.PackageService") return app.pkgMgr
    else app.pkgMgr.service(sclass.getName) match {
      case None       => throw new InstantiationException(s"Missing implementation: $sclass")
      case Some(impl) => services.get(impl)
    }
  }

  private def startService (iclass :Class[AbstractService]) :AbstractService = {
    val svc = injectInstance(iclass, Nil)
    svc.didStartup()
    svc
  }
}
