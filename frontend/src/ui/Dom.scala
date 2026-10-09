/*
 * Copyright 2026 Salar Rahmanian
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softinio.scalapkg.ui

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Dispatcher
import org.scalajs.dom

import scala.scalajs.js

/** Small DOM helpers for islands that enhance server-rendered markup rather
  * than render it themselves.
  */
object Dom:

  /** Runs `handler` for every `event` on `target` while the resource is open.
    */
  def listen(target: dom.EventTarget, event: String)(
      handler: dom.Event => IO[Unit]
  ): Resource[IO, Unit] =
    on[dom.Event](target, event)(d => e => d.unsafeRunAndForget(handler(e)))

  /** Like `listen`, for events the page may take over: `claim` decides
    * synchronously, so the default (following a link, submitting a form, typing
    * the key) is prevented before the browser acts on it. Calling
    * preventDefault inside the IO is too late, as the IO runs after dispatch.
    */
  def intercept[E <: dom.Event](target: dom.EventTarget, event: String)(
      claim: E => Option[IO[Unit]]
  ): Resource[IO, Unit] =
    on[E](target, event)(d =>
      e =>
        claim(e).foreach { action =>
          e.preventDefault()
          d.unsafeRunAndForget(action)
        }
    )

  private def on[E <: dom.Event](target: dom.EventTarget, event: String)(
      listener: Dispatcher[IO] => js.Function1[E, Unit]
  ): Resource[IO, Unit] =
    Dispatcher.sequential[IO].flatMap { d =>
      val f: js.Function1[E, Unit] = listener(d)
      Resource.make(IO(target.addEventListener(event, f)))(_ =>
        IO(target.removeEventListener(event, f))
      )
    }

  def byId(id: String): Option[dom.Element] =
    Option(dom.document.getElementById(id))

  /** The nearest ancestor-or-self of the event target matching `selector`. */
  def closest(e: dom.Event, selector: String): Option[dom.Element] =
    e.target match
      case el: dom.Element => Option(el.closest(selector))
      case _               => None

  /** GETs `url` as text; fails on any status that is not 2xx. */
  def fetchText(url: String): IO[String] =
    IO.fromPromise(IO(dom.fetch(url))).flatMap { resp =>
      if resp.ok then IO.fromPromise(IO(resp.text()))
      else IO.raiseError(new RuntimeException(s"$url returned ${resp.status}"))
    }

  /** Whether the event comes from a text field, where key shortcuts must not
    * fire.
    */
  def isTyping(e: dom.Event): Boolean =
    e.target match
      case _: dom.HTMLInputElement | _: dom.HTMLTextAreaElement |
          _: dom.HTMLSelectElement =>
        true
      case el: dom.HTMLElement => el.isContentEditable
      case _                   => false
