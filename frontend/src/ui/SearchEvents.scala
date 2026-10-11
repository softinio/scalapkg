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

/** Sends [[SearchEvent]]s to `POST /events`: a search once its results have
  * stayed on screen for a moment (so the half-typed queries of search as you
  * type are skipped), a result opened from it, and the links followed and
  * snippets copied in the drawer it opened. The ids are random; nothing
  * identifies the visitor.
  */
final class SearchEvents private (
    session: String,
    // the search shown in #results; `recorded` once its event is sent
    shown: Ref[IO, Option[SearchEvents.Shown]],
    // bumped whenever results are shown, so a search settles once it stops
    changes: SignallingRef[IO, Int],
    // the search result the open drawer came from
    origin: Ref[IO, Option[SearchEvents.Origin]]
):
  import SearchEvents.*

  /** Called once #results holds new search results (not the landing lists). */
  def resultsShown: IO[Unit] =
    IO(readResults).flatMap {
      case None                => shown.set(None)
      case Some((total, keys)) =>
        newId.flatMap(id =>
          shown.set(
            Some(Shown(id, currentQuery, total, keys, recorded = false))
          )
        ) *> changes.update(_ + 1)
    }

  /** Called on every keystroke in the search box: the results on screen no
    * longer match what is typed, so they are not recorded, and the next ones
    * settle only once typing stops.
    */
  def typed: IO[Unit] = shown.set(None) *> changes.update(_ + 1)

  /** Records the shown search if it has not been yet. */
  private def recordShown: IO[Unit] =
    shown
      .modify {
        case Some(s) if !s.recorded => (Some(s.copy(recorded = true)), Some(s))
        case other                  => (other, None)
      }
      .flatMap(_.traverse_ { s =>
        send(
          SearchEvent(
            EventKind.Search,
            Some(s.id),
            Some(session),
            query = Some(s.query),
            total = Some(s.total),
            results = s.keys.take(SearchEvent.maxResults)
          )
        )
      })

  /** A click on a package link: from a search result it records an `open` and
    * remembers the result for what the drawer records next.
    */
  def linkClicked(link: dom.Element): IO[Unit] =
    val pkg = decode[PackageSummary](link.getAttribute("data-pkg")).toOption
    val card = Option(link.closest("#results li[data-key]"))
    (shown.get, IO(card.map(c => (c.getAttribute("data-key"), indexOf(c)))))
      .flatMapN {
        case (Some(s), Some((key, index))) =>
          val o = Origin(
            s.id,
            key,
            (s.query.page - 1) * SearchQuery.pageSize + index + 1
          )
          recordShown *> origin.set(Some(o)) *>
            outcome(o, EventKind.Open, pkg.map(p => s"${p.groupId}:${p.name}"))
        case _ => origin.set(None)
      }

  /** A link followed or a snippet copied in the drawer; recorded only when the
    * drawer was opened from a search result.
    */
  def drawerAction(kind: EventKind, detail: String): IO[Unit] =
    origin.get.flatMap(_.traverse_(outcome(_, kind, Some(detail))))

  private def outcome(o: Origin, kind: EventKind, detail: Option[String]) =
    send(
      SearchEvent(
        kind,
        Some(o.searchId),
        Some(session),
        result = Some(o.key),
        position = Some(o.position),
        detail = detail
      )
    )

  private def settle: Resource[IO, Unit] =
    changes.discrete
      .debounce(settleAfter)
      .evalMap(_ => recordShown)
      .compile
      .drain
      .background
      .void

object SearchEvents:

  private val settleAfter = 1500.millis

  private final case class Shown(
      id: String,
      query: SearchQuery,
      total: Int,
      keys: List[String],
      recorded: Boolean
  )

  private final case class Origin(searchId: String, key: String, position: Int)

  def apply(): Resource[IO, SearchEvents] =
    for
      session <- Resource.eval(sessionId)
      shown <- Resource.eval(Ref[IO].of(Option.empty[Shown]))
      changes <- Resource.eval(SignallingRef[IO].of(0))
      origin <- Resource.eval(Ref[IO].of(Option.empty[Origin]))
      events = new SearchEvents(session, shown, changes, origin)
      _ <- events.settle
    yield events

  /** The total and result keys in #results, if it shows search results. */
  private def readResults: Option[(Int, List[String])] =
    Dom.byId("results").flatMap { results =>
      Option(results.querySelector("[data-total]"))
        .flatMap(_.getAttribute("data-total").toIntOption)
        .map { total =>
          val cards = results.querySelectorAll("li[data-key]")
          (
            total,
            (0 until cards.length).toList.map(cards(_).getAttribute("data-key"))
          )
        }
    }

  private def indexOf(card: dom.Element): Int =
    val cards = dom.document.querySelectorAll("#results li[data-key]")
    (0 until cards.length).indexWhere(cards(_) == card).max(0)

  private def currentQuery: SearchQuery =
    val params = new dom.URLSearchParams(dom.window.location.search)
    SearchQuery.fromParams(
      List("q", "platform", "scala", "sort", "page")
        .map(k => k -> params.getAll(k).toSeq)
        .toMap
    )

  private def newId: IO[String] = IO {
    val crypto = js.Dynamic.global.crypto
    if !js.isUndefined(crypto) && !js.isUndefined(crypto.randomUUID) then
      crypto.randomUUID().asInstanceOf[String]
    else
      (1 to 4)
        .map(_ => (math.random() * 0x7fffffff).toLong.toHexString)
        .mkString("-")
  }

  // one id per browser tab, kept across page loads; a fresh one if storage
  // is blocked
  private def sessionId: IO[String] =
    newId.flatMap { fresh =>
      IO {
        try
          Option(dom.window.sessionStorage.getItem("scalapkg-session"))
            .getOrElse {
              dom.window.sessionStorage.setItem("scalapkg-session", fresh)
              fresh
            }
        catch case _: Throwable => fresh
      }
    }

  // sendBeacon queues the request even when the page is being left
  private def send(event: SearchEvent): IO[Unit] =
    IO {
      try dom.window.navigator.sendBeacon("/events", event.asJson.noSpaces)
      catch case _: Throwable => false
    }.void
