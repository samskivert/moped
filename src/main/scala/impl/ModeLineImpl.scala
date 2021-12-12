//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import javafx.scene.control.Label
import javafx.scene.control.Tooltip
import javafx.scene.layout.HBox
import moped._

class ModeLineImpl extends HBox(8) with ModeLine {
  getStyleClass.add("modeLine")
  setMaxWidth(Double.MaxValue)

  def addDatum (value :ValueV[String], tooltip :ValueV[String]) :Closeable = {
    val label = new Label()
    val vconn = value onValueNotify(label.setText)
    val tconn = tooltip onValueNotify(tt => label.setTooltip(new Tooltip(tt)))
    getChildren.add(label)
    Closeable({
      getChildren.remove(label)
      vconn.close()
      tconn.close()
    })
  }
}
