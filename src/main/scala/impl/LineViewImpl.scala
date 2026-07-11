//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

import collection.mutable.ArrayBuffer
import collection.{Seq => SeqV}

import javafx.application.Platform
import javafx.geometry.VPos
import javafx.scene.Node
import javafx.scene.text.{Font, Text, TextFlow, FontSmoothingType}

import moped._

class LineViewImpl (_line :LineV) extends TextFlow with LineView {

  override def line = _line
  private var _valid = false

  // fontProperty.bind(ctrl.fontProperty)
  // fillProperty.bind(textFill)
  // impl_selectionFillProperty().bind(highlightTextFill)

  // TODO: this seems like it should alleviate the "shifting" issue when we break up the text on a
  // line differently due to changing style runs, but it doesn't... sigh
  // setSnapToPixel(false)

  /** Returns the x position of character at the specified column. This measures the actual
    * rendered width of `[0, col)` using `font` rather than assuming every column is the same
    * width: many characters (most CJK ideographs/kana, fullwidth forms, emoji, etc.) render wider
    * than a normal fixed-width cell, but not necessarily at any consistent multiple of it (it
    * depends on the font), so a real text measurement is needed to stay lined up with them. */
  def charX (col :Int, font :Font) :Double = {
    // TODO: handle tabs, other funny business?
    val text = _line.sliceString(0, math.min(col, _line.length))
    getLayoutX + LineViewImpl.measureWidth(text, font)
  }

  /** Updates this line to reflect the supplied style change. */
  def onStyle (loc :Loc) :Unit = invalidate()

  /** Marks this line view as invalid, clearing its children. */
  def invalidate () :Unit = if (_valid) {
    _valid = false
    // if we're not visible, remove our children now to free up memory
    if (!isVisible) getChildren.clear()
    else Platform.runLater(new Runnable() {
      override def run () = validate()
    })
  }

  /** Validates this line, rebuilding its visualization. This is called when the line becomes
    * visible. Non-visible lines defer visualization rebuilds until they become visible. */
  def validate () :Unit = if (!_valid) {
    // go through the line and add all of the styled line fragments
    class Adder extends Function3[SeqV[Tag[String]],Int,Int,Unit]() {
      private val kids = ArrayBuffer[Node]()
      private var last :Int = 0

      def add (start :Int, end :Int, styles :SeqV[Tag[String]]) :Unit = {
        var text = _line.sliceString(start, end)
        assert(end > start)
        val nlidx = text.indexOf('\n')
        if (nlidx != -1) {
          new Exception(s"Text cannot have newlines: $text").printStackTrace(System.err);
          text = text.substring(0, nlidx)
        }
        val tnode = new FillableText(text)
        tnode.setFontSmoothingType(FontSmoothingType.LCD)
        val sc = tnode.getStyleClass
        sc.add("textFace")
        styles.foreach(t => sc.add(t.tag))
        tnode.setTextOrigin(VPos.TOP)
        kids += tnode.fillRect
        kids += tnode
      }

      def apply (cls :SeqV[Tag[String]], start :Int, end :Int) :Unit = {
        // if we skipped over any unstyled text, add it now
        if (start > last) add(last, start, Seq.empty)
        add(start, end, cls)
        last = end
      }

      def finish () :Unit = {
        // if there's trailing unstyled text, add that
        if (last < _line.length) add(last, _line.length, Seq.empty)
        if (!kids.isEmpty) getChildren.addAll(kids.toArray*)
      }
    }
    _valid = true // mark ourselves valid now to avoid looping if freakoutery
    getChildren.clear()
    val adder = new Adder()
    _line.visitTags(classOf[String])(adder)
    adder.finish()
  }

  override def layoutChildren () :Unit = {
    super.layoutChildren()
    val iter = getChildren.iterator
    while (iter.hasNext) iter.next match {
      case ft :FillableText => ft.layoutRect()
      case _ => // nada
    }
  }

  override def toString = s"$line:${_valid}"
}

object LineViewImpl {
  // a scratch (never-added-to-a-scene) Text node reused to measure rendered string widths; JavaFX
  // computes a Text node's layout bounds from its text+font alone, no live scene graph needed
  private val measurer = new Text()

  private def measureWidth (text :String, font :Font) :Double = {
    measurer.setText(text)
    measurer.setFont(font)
    measurer.getLayoutBounds.getWidth
  }
}
