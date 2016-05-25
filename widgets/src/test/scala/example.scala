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
import scala.scalajs.js
import scala.util._

import forms._
import org.scalajs.dom
import scalatags.JsDom.all._

object Example extends js.JSApp {

  case class BB(f1: String, f2: String, f3: Int)
  case class AA(
    f1: Seq[BB],
    f2: Int,
    f3: String,
    f4: Boolean,
    f5: Any
  )

  def main(): Unit = {

    val factoryB: Factory[Seq[BB]] =
      UIKit.formControl("Table 1")(
        UIKit.tableForm(
          List("h1", "h2", "h3"),
          tr.zip(
            td(`class` := "uk-width-1-10").wrap(inputForm(`type` := "text")),
            td(`class` := "uk-width-9-10").wrap(inputForm(`type` := "text", `class` := "uk-width-1-1")),
            td.wrap(
              UIKit.flash(
                selectForm(List("1" -> "11", "2" -> "22", "c" -> "invalid"))
              )
            ).map(_.toInt)(_.toString).flashOnError
          ).map((BB.apply _).tupled)(b => (BB.unapply(b).get))
        )
      ).flashOnError

    val choiceFactory = choice(
      "1" -> checkbox(),
      "2" -> textForm()
    )(_ match {
        case _: String => "2"
        case _: Boolean => "1"
      })(selectForm(List("1" -> "1", "2" -> "2")))

    val factory: Factory[AA] =
      UIKit.alert(h4(`class` := "uk-alert-danger"))(
        UIKit.form.zip(
          factoryB,
          UIKit.formControl("Label1")(textForm(`class` := "uk-width-1-1")).map(_.toInt)(_.toString).flashOnError,
          UIKit.formControl("Label2")(textForm(`class` := "uk-width-1-1")),
          UIKit.formControl("Label3")(checkbox(`class` := "uk-width-1-1")),
          UIKit.formControl("Label4")(choiceFactory)
        ).map(tuple => AA(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))(aa => (aa.f1, aa.f2, aa.f3, aa.f4, aa.f5))
      ).flashOnError

    val (handle, viewhandle) = factory.make

    handle.set(AA(List(BB("a", "b", 1), BB("a", "b", 2)), 1, "d", true, false))

    dom.document.body.appendChild(div(`class` := "uk-container")(viewhandle.anchor).render)

    dom.document.body.appendChild(
      div(
      button(
        `type` := "button",
        onclick := { (e: dom.Event) =>
          viewhandle.enable(false)
        },
        "disable"
      ),
      button(
        `type` := "button",
        onclick := { (e: dom.Event) =>
          viewhandle.enable(true)
        },
        "enable"
      ),
      button(
        `type` := "button",
        onclick := { (e: dom.Event) =>
          println(handle.get)
        },
        "print"
      )
    ).render
    )

    handle.notify {
      case Success(x) =>
        println(x)
      case Failure(e) => {
        println(e)
      }
    }

  }

}
