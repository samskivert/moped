//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.net.ServerSocket
import java.nio.file.Paths
import java.util.concurrent.{Callable, ExecutionException, FutureTask, TimeUnit}

import javafx.application.Platform
import javafx.embed.swing.SwingFXUtils
import javafx.geometry.Rectangle2D
import javafx.scene.SnapshotParameters
import javafx.scene.input.{MouseButton, MouseEvent}
import javax.imageio.ImageIO

import moped._

/** Hosts a simple server on localhost:[[Moped.Port]]. Accepts newline-terminated commands, one per
  * line. `open` is fire-and-forget (no response); the rest are debugging/automation hooks that
  * write a single response line (`ok`, or `error: ...`) back before reading the next command:
  *
  *   - `open PATH`: opens `PATH` in the most recently used editor on the currently active desktop.
  *   - `invoke FN`: invokes the named `Fn` (e.g. `forward-char`, `backward-char`) against some open
  *     window's focused frame, as if triggered by a keybinding.
  *   - `type TEXT`: inserts `TEXT` at the point of some open window's focused buffer, as if typed.
  *   - `point`: reports the point (as `row,col`) of some open window's focused buffer.
  *   - `mark`: reports the mark (as `row,col`, or `none`) of some open window's focused buffer.
  *   - `line ROW`: reports the text of line `ROW` (0-indexed) of some open window's focused buffer.
  *   - `click X Y [COUNT]`: simulates a primary-button mouse press at pixel position `(X, Y)`
  *     (relative to the buffer content, i.e. the same coordinate space `screenshot` captures) in
  *     some open window's focused frame. `COUNT` (default 1) is the click count, i.e. `2` for a
  *     double-click, `3` for a triple-click.
  *   - `drag X Y`: simulates dragging the (already-pressed, per `click`) mouse to `(X, Y)`.
  *   - `release X Y`: simulates releasing the mouse button at `(X, Y)`.
  *   - `screenshot PATH`: renders some open window to a PNG file at `PATH`.
  *   - `screenshot PATH X Y W H`: as above, but cropped to the pixel region `[X,Y,X+W,Y+H)`.
  *
  * The last seven exist to let an external script drive the editor and inspect its rendered output
  * (e.g. for debugging) without needing OS-level input-injection or screen-capture permissions,
  * since the screenshot is rendered in-process via JavaFX's own snapshot mechanism.
  */
class Server (app :Moped) extends Thread {
  setDaemon(true)

  override def run () :Unit = {
    val port = Moped.Port
    try {
      val ssock = new ServerSocket(port)
      app.logger.log(s"Listening for commands on localhost:$port")
      while (true) {
        val csock = ssock.accept()
        try {
          val in = new BufferedReader(new InputStreamReader(csock.getInputStream(), "UTF-8"))
          val out = new PrintWriter(new OutputStreamWriter(csock.getOutputStream(), "UTF-8"), true)
          var line = in.readLine()
          while (line != null) {
            process(line).foreach(out.println)
            line = in.readLine()
          }
        } finally csock.close()
      }
    } catch {
      case e :Exception => app.logger.log(s"Failed to bind to $port", e)
    }
  }

  // processes one command, returning a response line to write back to the client (`open`, which
  // needs/has no response, returns None)
  private def process (cmd :String) :Option[String] = {
    def arg (prefix :String) = cmd.substring(prefix.length).trim
    try cmd match {
      case c if c `startsWith` "open " =>
        onMainThread { app.wspMgr.visit(app.wspMgr.resolve(arg("open "))) }
        None
      case c if c `startsWith` "invoke " => Some(onUIBlocking {
        app.wspMgr.anyWindow match {
          case Some(win) =>
            if (win.focusedDispatcher.invoke(arg("invoke "))) "ok" else "error: no such fn"
          case None => "error: no open window"
        }
      })
      case c if c `startsWith` "type " => Some(onUIBlocking {
        focusedFrame match {
          case Some(f) if f.view != null =>
            f.view.buffer.insert(f.view.point(), Line(arg("type "))) ; "ok"
          case Some(_) => "error: no buffer"
          case None => "error: no open window"
        }
      })
      case "point" => Some(onUIBlocking {
        focusedFrame match {
          case Some(f) if f.view != null =>
            val p = f.view.point() ; s"${p.row},${p.col}"
          case Some(_) => "error: no buffer"
          case None => "error: no open window"
        }
      })
      case "mark" => Some(onUIBlocking {
        focusedFrame match {
          case Some(f) if f.view != null =>
            f.view.buffer.mark.map(p => s"${p.row},${p.col}").getOrElse("none")
          case Some(_) => "error: no buffer"
          case None => "error: no open window"
        }
      })
      case c if c `startsWith` "line " => Some(onUIBlocking {
        focusedFrame match {
          case Some(f) if f.view != null =>
            val row = arg("line ").toInt
            if (row < 0 || row >= f.view.buffer.lines.size) "error: no such line"
            else f.view.buffer.lines(row).asString
          case Some(_) => "error: no buffer"
          case None => "error: no open window"
        }
      })
      case c if c `startsWith` "click " => Some(onUIBlocking {
        withXY(arg("click "), MouseEvent.MOUSE_PRESSED)(
          (win, mev) => win.focusedArea.mousePressed(mev))
      })
      case c if c `startsWith` "drag " => Some(onUIBlocking {
        withXY(arg("drag "), MouseEvent.MOUSE_DRAGGED)((win, mev) => win.focusedArea.mouseDragged(mev))
      })
      case c if c `startsWith` "release " => Some(onUIBlocking {
        withXY(arg("release "), MouseEvent.MOUSE_RELEASED)((win, mev) => win.focusedArea.mouseReleased(mev))
      })
      case c if c `startsWith` "screenshot " => Some(onUIBlocking(doScreenshot(arg("screenshot "))))
      case _ => Some(s"error: unknown command '$cmd'")
    } catch {
      case e :ExecutionException => Some(s"error: ${e.getCause}")
      case e :Throwable => Some(s"error: $e")
    }
  }

  // finds the focused frame of some open window; must be called on the JavaFX application thread
  private def focusedFrame = app.wspMgr.anyWindow.map(_.focus)

  // parses "X Y [COUNT]" out of `argStr` and, if there's an open window, invokes `fn` with it and
  // a synthetic MouseEvent of type `evType` at that position; used by `click`/`drag`/`release`
  private def withXY (
    argStr :String, evType :javafx.event.EventType[MouseEvent]
  )(fn :(WindowImpl, MouseEvent) => Unit) :String = {
    app.wspMgr.anyWindow match {
      case Some(win) =>
        argStr.split(" ").filter(_.nonEmpty) match {
          case Array(x, y) => fn(win, mouseEvent(evType, x.toDouble, y.toDouble, 1)) ; "ok"
          case Array(x, y, count) =>
            fn(win, mouseEvent(evType, x.toDouble, y.toDouble, count.toInt)) ; "ok"
          case _ => "error: usage: X Y [COUNT]"
        }
      case None => "error: no open window"
    }
  }

  // builds a synthetic MouseEvent at (x, y); passed directly to BufferArea.mousePressed/
  // mouseDragged/mouseReleased (bypassing the real event-dispatch chain, since we're calling it
  // programmatically), which only read getX/getY (and, for mousePressed, getClickCount), so the
  // other fields are inert placeholders
  private def mouseEvent (
    evType :javafx.event.EventType[MouseEvent], x :Double, y :Double, clickCount :Int
  ) :MouseEvent = new MouseEvent(
    evType, x, y, x, y, MouseButton.PRIMARY, clickCount,
    false, false, false, false, // shift/control/alt/meta down
    true, false, false, // primary/middle/secondary button down
    false, false, false, // synthesized, popupTrigger, stillSincePress
    null)

  // must be called on the JavaFX application thread
  private def doScreenshot (spec :String) :String = app.wspMgr.anyWindow match {
    case None => "error: no open window"
    case Some(win) =>
      val params = new SnapshotParameters()
      spec.split(" ").filter(_.nonEmpty) match {
        case Array(path) =>
          ImageIO.write(SwingFXUtils.fromFXImage(win.snapshot(params, null), null),
                        "png", Paths.get(path).toFile)
          "ok"
        case Array(path, x, y, w, h) =>
          params.setViewport(new Rectangle2D(x.toDouble, y.toDouble, w.toDouble, h.toDouble))
          ImageIO.write(SwingFXUtils.fromFXImage(win.snapshot(params, null), null),
                        "png", Paths.get(path).toFile)
          "ok"
        case _ => "error: usage: screenshot PATH [X Y W H]"
      }
  }

  // runs `op` on the JavaFX application thread and blocks (with a timeout) for its result; the
  // debug/automation commands above need this since they must respond only once `op` completes
  private def onUIBlocking[T] (op : =>T) :T = {
    val task = new FutureTask(new Callable[T]() { def call () = op })
    Platform.runLater(task)
    task.get(5, TimeUnit.SECONDS)
  }
}
