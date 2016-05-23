/**
 * The MIT License (MIT) Copyright (c) 2016 Istvan Bartha
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package forms

import scala.util._

import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import scalatags.JsDom._
import scalatags.JsDom.all._

trait Widgets {

  def handle(elem: HTMLSelectElement) = new Handle[String] {
    def get = Success(elem.value)
    def set(t: String) = elem.value = t
    def notify(f: Try[String] => Unit) = elem.onchange = (e: Event) => f(get)
  }

  def handle(elem: HTMLTextAreaElement) = new Handle[String] {
    def get = Success(elem.value)
    def set(t: String) = elem.value = t
    def notify(f: Try[String] => Unit) = elem.oninput = (e: Event) => f(get)
  }

  def handle(elem: HTMLInputElement) = new Handle[String] {
    def get = Success(elem.value)
    def set(t: String) = elem.value = t
    def notify(f: Try[String] => Unit) = elem.oninput = (e: Event) => f(get)
  }

  def textForm(attributes: Modifier*) = new Factory[String] {
    def make = {
      val inputE = textarea(attributes).render
      (handle(inputE), new ViewHandle {
        val anchor = inputE
        def enable(b: Boolean) = inputE.disabled = !b
        def flash(s: String) = ()
      })
    }
  }

  def selectForm(values: List[(String, String)], attributes: Modifier*) = new Factory[String] {
    def make = {
      val inputE = select(attributes)(values.map(x => option(value := x._1)(x._2))).render
      (handle(inputE), new ViewHandle {
        val anchor = inputE
        def enable(b: Boolean) = inputE.disabled = !b
        def flash(s: String) = ()
      })
    }
  }

  def checkbox(attributes: Modifier*) = new Factory[Boolean] {
    def make = {
      val inputE = input(attributes, `type` := "checkbox").render
      val handle = new Handle[Boolean] {
        def get = Success(inputE.checked)
        def set(t: Boolean) = inputE.checked = t
        def notify(f: Try[Boolean] => Unit) = inputE.onchange = (e: Event) => f(get)
      }
      (handle, new ViewHandle {
        val anchor = inputE
        def enable(b: Boolean) = inputE.disabled = !b
        def flash(s: String) = ()
      })
    }
  }

  def inputForm(attributes: Modifier*) = new Factory[String] {
    def make = {
      val inputE = input(attributes).render
      (handle(inputE), new ViewHandle {
        val anchor = inputE
        def enable(b: Boolean) = inputE.disabled = !b
        def flash(s: String) = ()
      })
    }
  }

  def tableForm[T](
    tableTag: TypedTag[HTMLElement],
    tbodyTag: TypedTag[HTMLElement],
    addButtonTag: TypedTag[HTMLElement] = button(`type` := "button", "Add row"),
    removeButtonTag: TypedTag[HTMLElement] = button(`type` := "button", "Remove")
  )(implicit f: Factory[T]) = new Factory[Seq[T]] {
    def make = {
      val c = tbodyTag.render

      val (h1, row1) = f.make
      val buf = scala.collection.mutable.Buffer(h1 -> row1)

      def currentValue = Try(buf.map(_._1.get.get))

      var notifyFun: Option[Try[Seq[T]] => Unit] = None

      val removeButton = removeButtonTag(onclick := { (e: Event) =>

        c
          .childNodes
          .toList
          .zipWithIndex
          .find(_._1 == row1)
          .map(_._2)
          .foreach(i => buf.remove(i))

        c.removeChild(row1.anchor)
      }).render
      row1.anchor.appendChild(td(`style` := "vertical-align: middle !important;")(removeButton).render)
      c.appendChild(row1.anchor)

      val buttons = scala.collection.mutable.Buffer(removeButton)

      val b = addButtonTag(onclick := { (e: Event) =>

        val (h, n) = f.make
        val removeButton = removeButtonTag(onclick := { (e: Event) =>
          c.childNodes.toList.zipWithIndex.find(_._1 == n.anchor).map(_._2).foreach(i => buf.remove(i))
          c.removeChild(n.anchor)
          notifyFun.foreach(t => t(currentValue))
        }).render
        n.anchor.appendChild(td(removeButton).render)
        buf.append(h -> n)
        c.appendChild(n.anchor)
        buttons.append(removeButton)
        notifyFun.foreach(t => h.notify(k => t(currentValue)))

      }).render
      buttons.append(b)

      val h = new Handle[Seq[T]] {
        def get = currentValue
        def set(t: Seq[T]) = {
          buf.clear
          c.childNodes.foreach(ch => c.removeChild(ch))
          t.foreach { t =>
            val (h, n) = f.make
            val removeButton = removeButtonTag(onclick := { (e: Event) =>
              c.childNodes.toList.zipWithIndex.find(_._1 == n.anchor).map(_._2).foreach(i => buf.remove(i))
              c.removeChild(n.anchor)
              notifyFun.foreach(t => t(currentValue))
            }).render
            h.set(t)
            n.anchor.appendChild(td(removeButton).render)
            buf.append(h -> n)
            c.appendChild(n.anchor)
            buttons.append(removeButton)
          }
        }
        def notify(f: Try[Seq[T]] => Unit) = {
          notifyFun = Some(f)
          buf.foreach(h => h._1.notify(t => f(get)))
        }
      }

      val vn = new ViewHandle {
        val anchor = tableTag(c)(tfoot(b)).render
        def enable(b: Boolean) = {
          buf.foreach(_._2.enable(b))
          buttons.foreach { button =>

            button.disabled = (!b)

            if (b) button.style.pointerEvents = "auto"
            else button.style.pointerEvents = "none"

          }
        }
        def flash(s: String) = ()
      }
      (h, vn)
    }
  }

}
