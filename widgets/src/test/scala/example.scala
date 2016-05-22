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
import forms._

import scala.scalajs.js
import js.annotation._
import scalatags.JsDom._
import scalatags.JsDom.all._
import scala.util._

import org.scalajs.dom

object Example extends js.JSApp {

  def main(): Unit = {
    val factory =
      UIKit.form.zip(
        UIKit.tableForm(
          List("h1", "h2", "h3"),
          tr.zip(
            td(`class` := "uk-width-1-10").wrap(inputForm(`type` := "text")),
            td(`class` := "uk-width-9-10").wrap(inputForm(`type` := "text", `class` := "uk-width-1-1")),
            td.wrap(selectForm(List("1" -> "11", "2" -> "22")))
          )
        ),
        UIKit.formControl("Label1")(textForm(`class` := "uk-width-1-1")),
        UIKit.formControl("Label2")(textForm(`class` := "uk-width-1-1")),
        UIKit.formControl("Label3")(checkbox(`class` := "uk-width-1-1"))
      )

    val (handle, node) = factory.make

    handle.set((List(("a", "b", "1"), ("a", "b", "1")), "c", "d", true))

    dom.document.body.appendChild(div(`class` := "uk-container")(node).render)

    handle.notify {
      case Success(x) =>
        println(x)
    }

  }

}
