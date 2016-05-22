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

import org.scalajs.dom.raw._

trait Handle[T] {
  def get: Try[T]
  def set(t: T): Unit
  def notify(f: Try[T] => Unit): Unit
}

trait ViewHandle {
  def enable(b: Boolean): Unit
  def flash(s: String): Unit
  val anchor: HTMLElement
}

trait Factory[T] { self =>
  def make: (Handle[T], ViewHandle)
  def attach(c: Node): Handle[T] = {
    val (h, n) = make
    c.appendChild(n.anchor)
    h
  }
  def map[K](f: T => K)(g: K => T) = new Factory[K] {
    def make = {
      val (h1, n) = self.make
      val h2 = new Handle[K] {
        def get = h1.get.map(f)
        def set(k: K) = h1.set(g(k))
        def notify(h: Try[K] => Unit) = h1.notify((t: Try[T]) => h(t.map(f)))
      }
      (h2, n)
    }
  }
  def wrap(f: ViewHandle => ViewHandle) = new Factory[T] {
    def make = {
      val (h, n) = self.make
      (h, f(n))
    }
  }
  def flashOnChange(fun: (Try[T], ViewHandle) => Unit) = new Factory[T] {
    def make = {
      val (h1, n) = self.make
      h1.notify((t: Try[T]) => fun(t, n))
      val h2 = new Handle[T] {
        def get = h1.get
        def set(t: T) = h1.set(t)
        def notify(handler: Try[T] => Unit) = h1.notify { (t: Try[T]) =>
          handler(t)
          fun(t, n)
        }
      }
      (h2, n)
    }
  }
  def flashOnError = flashOnChange { (t: Try[T], n: ViewHandle) =>
    t match {
      case Failure(e) => n.flash(e.getMessage)
      case Success(_) => n.flash("")
    }
  }
}

case class WrappedFactory(f: ViewHandle => ViewHandle) {
  def apply[T](fac: Factory[T]) = fac.wrap(f)
}
