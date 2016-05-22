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

import org.scalajs.dom
import scalatags.JsDom._
import scalatags.JsDom.all._

object UIKit {

  val form = all.form(`class` := "uk-form uk-form-horizontal")

  def formControl(modifiers: Modifier*) = WrappedFactory((e: dom.raw.Node) =>
    div(`class` := "uk-form-row uk-width-large-1-1")(
      label(`class` := "uk-form-label")(modifiers),
      div(`class` := "uk-form-controls")(e)
    ).render)

  def tableForm[T](
    headers: Seq[String],
    widget: Factory[T]
  ) = formControl("Table1")(
    forms.tableForm(
      table(`class` := "uk-table uk-table-condensed uk-table-striped")(
        thead(headers.map(x => th(x)): _*)
      ),
      tbody(),
      span(`class` := "uk-icon-hover uk-icon-plus"),
      span(`class` := "uk-icon-hover uk-icon-remove")
    )(widget)
  )

}
