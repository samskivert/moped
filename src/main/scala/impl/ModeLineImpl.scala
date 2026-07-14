//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import scala.jdk.CollectionConverters._
import javafx.scene.{Cursor, Node}
import javafx.scene.control.Label
import javafx.scene.control.Tooltip
import javafx.scene.layout.HBox
import javafx.scene.text.{Text, TextFlow}
import moped._

class ModeLineImpl extends HBox(8) with ModeLine {
  getStyleClass.add("modeLine")
  setMaxWidth(Double.MaxValue)

  def addDatum (value :ValueV[String], tooltip :ValueV[String]) :Closeable = {
    val label = new Label()
    val vconn = value `onValueNotify`(label.setText)
    val tconn = tooltip `onValueNotify`(tt => label.setTooltip(new Tooltip(tt)))
    getChildren.add(label)
    Closeable({
      getChildren.remove(label)
      vconn.close()
      tconn.close()
    })
  }

  def addStyledDatum (
    value :ValueV[Seq[ModeLine.Segment]], tooltip :ValueV[String],
    onClick :Option[() => Unit] = None
  ) :Closeable = {
    val flow = new TextFlow()
    val tt = new Tooltip()
    Tooltip.install(flow, tt)
    onClick.foreach { cb =>
      flow.setCursor(Cursor.HAND)
      flow.setOnMouseClicked(_ => cb())
    }
    def rebuild (segs :Seq[ModeLine.Segment]) :Unit = flow.getChildren.setAll(segs.map { seg =>
      val text = new Text(seg.text)
      if (!seg.style.isEmpty) text.getStyleClass.add(seg.style)
      text :Node
    }.asJava)
    val vconn = value `onValueNotify` rebuild
    val tconn = tooltip `onValueNotify` tt.setText
    getChildren.add(flow)
    Closeable({
      getChildren.remove(flow)
      vconn.close()
      tconn.close()
    })
  }
}
