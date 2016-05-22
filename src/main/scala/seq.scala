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

import org.scalajs.dom.raw.Node
import scala.util._

trait Seqs {
  def sequence[T](combine: Seq[Node] => Node)(f: Seq[Factory[T]]) = new Factory[Seq[T]] {
    def make = {
      val (hs, ns) = f.map(_.make).unzip

      val h = new Handle[Seq[T]] {
        def enable(b: Boolean) = hs.foreach(_.enable(b))
        def get = Try(hs.map(_.get.get))
        def set(t: Seq[T]) = hs.zip(t).foreach(x => x._1.set(x._2))
        def notify(f: Try[Seq[T]] => Unit) = hs.foreach(h => h.notify(t => f(get)))
      }
      (h, combine(ns))
    }
  }

}
