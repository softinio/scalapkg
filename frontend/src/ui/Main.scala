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

import calico.syntax.*
import calico.unsafe.given
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import com.softinio.scalapkg.domain.*
import fs2.concurrent.SignallingRef
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom

import scala.concurrent.duration.*
import scala.scalajs.js

/** Entry point: mounts the islands found on the page and wires the page-wide
  * behaviour (theme toggle, `/` shortcut, package links opening the drawer,
  * back/forward).
  */
object Main:

  def main(args: Array[String]): Unit =
    program.useForever.unsafeRunAndForget()

  private def program: Resource[IO, Unit] =
    for
      current <- SignallingRef[IO].of(Option.empty[PackageSummary]).toResource
      // the search query string currently shown in #results
      shown <- Ref[IO].of(dom.window.location.search).toResource
      events <- SearchEvents()
      nav = Navigation(current, shown, events)
      _ <- themeToggle
      _ <- slashShortcut
      _ <- nav.interceptPackageLinks
      _ <- Dom.listen(dom.window, "popstate")(_ => nav.onPopState)
      islands <- Resource.eval(IO {
        val nodes = dom.document.querySelectorAll("[data-island]")
        (0 until nodes.length).toList.map(nodes(_))
      })
      _ <- islands.traverse_(mount(_, nav))
      // a search page rendered by the server counts like a swapped-in one
      _ <- events.resultsShown.toResource
    yield ()

  private def mount(el: dom.Element, nav: Navigation): Resource[IO, Unit] =
    (Option(el.getAttribute("data-island")), el, el.parentNode) match
      case (Some("search"), form: dom.HTMLFormElement, _) =>
        form.querySelector("#search-input") match
          case input: dom.HTMLInputElement => SearchIsland(form, input, nav)
          case _                           => Resource.unit
      case (Some("package-drawer"), _, dialog: dom.HTMLDialogElement) =>
        nav.mountDrawer(el, dialog)
      case _ => Resource.unit

  private def themeToggle: Resource[IO, Unit] =
    Dom.byId("theme-toggle") match
      case None      => Resource.unit
      case Some(btn) =>
        Dom.listen(btn, "click") { _ =>
          IO {
            val dark = dom.document.documentElement.classList.toggle("dark")
            try
              dom.window.localStorage
                .setItem("theme", if dark then "dark" else "light")
            catch case _: Throwable => ()
          }
        }

  private def slashShortcut: Resource[IO, Unit] =
    Dom.intercept[dom.KeyboardEvent](dom.document, "keydown") { key =>
      if key.key == "/" && !key.ctrlKey && !key.metaKey && !Dom.isTyping(key)
      then
        Dom.byId("search-input").collect { case input: dom.HTMLInputElement =>
          IO {
            input.focus()
            input.select()
          }
        }
      else None
    }

/** Keeps the URL, the results region and the drawer in step. */
final class Navigation(
    current: SignallingRef[IO, Option[PackageSummary]],
    shown: Ref[IO, String],
    val events: SearchEvents
):

  private def path: String = dom.window.location.pathname
  private def isPackagePath: Boolean = path.startsWith("/p/")

  /** The package JSON `open` stored with the current history entry. */
  private def statePackage: Option[String] =
    (dom.window.history.state: Any) match
      case s: String => Some(s)
      case _         => None

  /** Opens a package's drawer, adding a history entry for its URL. */
  def open(p: PackageSummary): IO[Unit] =
    IO {
      dom.window.history.pushState(p.asJson.noSpaces, "", p.ref.path)
      countPageView(p.ref.path)
    } *> current.set(Some(p))

  /** Called when the drawer closes (close button, Esc, backdrop). */
  def closed: IO[Unit] =
    current.set(None) *> IO {
      if isPackagePath then
        if statePackage.isDefined then dom.window.history.back()
        else dom.window.history.replaceState(null, "", "/")
    }

  def onPopState: IO[Unit] =
    if isPackagePath then
      statePackage.flatMap(decode[PackageSummary](_).toOption) match
        case Some(p) => current.set(Some(p))
        case None    => IO(dom.window.location.reload())
    else current.set(None) *> refreshResultsIfChanged

  private def refreshResultsIfChanged: IO[Unit] =
    val search = dom.window.location.search
    shown.get.flatMap { last =>
      if search == last then IO.unit
      else if search.isEmpty then IO(dom.window.location.reload())
      else swap(search, push = false)
    }

  /** Replaces #results with the server-rendered fragment for `search` (a
    * `?q=...` string) and updates the address bar.
    */
  def swap(search: String, push: Boolean): IO[Unit] =
    Dom.byId("results") match
      case None          => IO(dom.window.location.assign(s"/$search"))
      case Some(results) =>
        Dom
          .fetchText(s"/fragments/search$search")
          .flatMap { html =>
            IO {
              results.innerHTML = html
              val url =
                if search.isEmpty then "/" else s"/$search"
              if push then dom.window.history.pushState(null, "", url)
              else dom.window.history.replaceState(null, "", url)
              countPageView(url)
            } *> shown.set(search) *> events.resultsShown
          }
          .handleErrorWith(_ => IO(dom.window.location.assign(s"/$search")))

  /** Package links carry their summary as JSON in data-pkg; open the drawer
    * instead of loading /p/... (ctrl/cmd-click still opens a new tab). Every
    * click on one, new tab or not, goes to `events`.
    */
  def interceptPackageLinks: Resource[IO, Unit] =
    Dom.listen(dom.document, "click") { e =>
      Dom.closest(e, "a[data-pkg]").traverse_(events.linkClicked)
    } *>
      Dom.intercept[dom.MouseEvent](dom.document, "click") { me =>
        if me.button != 0 || me.metaKey || me.ctrlKey || me.shiftKey || me.altKey
        then None
        else
          Dom
            .closest(me, "a[data-pkg]")
            .flatMap(a =>
              decode[PackageSummary](a.getAttribute("data-pkg")).toOption
            )
            .map(open)
      }

  /** Mounts the drawer into the server-rendered <dialog>. A /p/... page passes
    * the package in data-props so the drawer opens straight away.
    */
  def mountDrawer(
      el: dom.Element,
      dialog: dom.HTMLDialogElement
  ): Resource[IO, Unit] =
    val initial = Option(el.getAttribute("data-props"))
      .flatMap(decode[PackageSummary](_).toOption)
    val closeDialog = IO(if dialog.open then dialog.close())
    for
      _ <- Resource.eval(IO { el.innerHTML = "" })
      _ <- Drawer(current, closeDialog, events.drawerAction).renderInto(
        el.asInstanceOf[fs2.dom.Node[IO]]
      )
      _ <- Dom.listen(dialog, "close")(_ =>
        current.get.flatMap(c => closed.whenA(c.isDefined))
      )
      // a click on the backdrop lands on the dialog itself
      _ <- Dom.listen(dialog, "click") { e =>
        if e.target == dialog then closeDialog else IO.unit
      }
      _ <- current.discrete
        .foreach {
          case Some(_) =>
            IO(if !dialog.open then dialog.showModal())
          case None => closeDialog
        }
        .compile
        .drain
        .background
      _ <- Resource.eval(initial.traverse_(p => current.set(Some(p))))
    yield ()

  private def countPageView(path: String): Unit =
    val gc = dom.window.asInstanceOf[js.Dynamic].goatcounter
    if !js.isUndefined(gc) && !js.isUndefined(gc.count) then
      gc.count(js.Dynamic.literal(path = s"${dom.window.location.host}$path"))

/** Search as you type: debounces the input and swaps in the results fragment;
  * facet, sort and pager links inside #results swap too.
  */
object SearchIsland:

  def apply(
      form: dom.HTMLFormElement,
      input: dom.HTMLInputElement,
      nav: Navigation
  ): Resource[IO, Unit] =
    def searchFor(q: String): String =
      val params = new dom.URLSearchParams(dom.window.location.search)
      if q.trim.isEmpty then params.delete("q") else params.set("q", q.trim)
      params.delete("page")
      val s = params.toString
      if s.isEmpty then "" else s"?$s"

    for
      typed <- SignallingRef[IO].of(input.value).toResource
      _ <- Dom.listen(input, "input")(_ =>
        typed.set(input.value) *> nav.events.typed
      )
      _ <- Dom.intercept(form, "submit")(_ =>
        Some(nav.swap(searchFor(input.value), push = true))
      )
      _ <- Dom.byId("results").traverse_ { results =>
        Dom.intercept[dom.MouseEvent](results, "click") { me =>
          Dom.closest(me, "a[data-swap]") match
            case Some(a: dom.HTMLAnchorElement)
                if me.button == 0 && !me.metaKey && !me.ctrlKey =>
              Some(nav.swap(a.search, push = true))
            case _ => None
        }
      }
      _ <- typed.discrete.changes
        .drop(1) // the initial value is already rendered
        .debounce(250.millis)
        .switchMap(q => fs2.Stream.eval(nav.swap(searchFor(q), push = false)))
        .compile
        .drain
        .background
    yield ()
