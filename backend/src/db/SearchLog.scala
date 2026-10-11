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

package com.softinio.scalapkg.db

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Queue
import com.softinio.scalapkg.domain.SearchEvent
import com.softinio.scalapkg.domain.SearchQuery
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.OffsetDateTime
import java.time.ZoneOffset
import scala.concurrent.duration.*

/** Where a logged search event came from. */
enum EventSource derives CanEqual:
  case Web, Mcp

  def param: String = toString.toLowerCase

/** Records searches and their outcomes (table `search_events`). Recording never
  * waits on the database: events go to a bounded queue that a background fiber
  * writes in batches, and when the queue is full (a flood, or a database that
  * is down) new events are dropped. Losing a few is fine for statistics;
  * slowing a search down is not.
  */
trait SearchLog:
  def record(source: EventSource, event: SearchEvent): IO[Unit]

object SearchLog:

  /** An event as queued, stamped when it was recorded rather than written. */
  final case class Entry(
      at: OffsetDateTime,
      source: EventSource,
      event: SearchEvent
  )

  val noop: SearchLog = (_, _) => IO.unit

  /** Writes queued events with `write`, up to `batch` at a time and at least
    * every `every`; a failed write is logged and its batch dropped.
    */
  def queued(
      write: List[Entry] => IO[Unit],
      capacity: Int = 10000,
      batch: Int = 500,
      every: FiniteDuration = 5.seconds
  )(using log: Logger[IO]): Resource[IO, SearchLog] =
    for
      queue <- Resource.eval(
        Queue.bounded[IO, Entry](capacity)
      )
      _ <- fs2.Stream
        .fromQueueUnterminated(queue)
        .groupWithin(batch, every)
        .evalMap(chunk =>
          write(chunk.toList).handleErrorWith(e =>
            log.warn(e)(s"dropped ${chunk.size} search events")
          )
        )
        .compile
        .drain
        .background
    yield (source, event) =>
      IO.realTimeInstant.flatMap(now =>
        queue.tryOffer(Entry(now.atOffset(ZoneOffset.UTC), source, event)).void
      )

  /** Inserts events into `search_events`. */
  def writer(
      sessions: Resource[IO, Session[IO]]
  ): List[Entry] => IO[Unit] =
    events =>
      if events.isEmpty then IO.unit
      else
        val rows = row.values.list(events.size)
        sessions
          .use(
            _.execute(
              sql"""INSERT INTO search_events
                      (at, source, kind, search_id, session, query, filters,
                       total, results, result, position, detail)
                    VALUES $rows""".command
            )(events.map(toRow))
          )
          .void

  /** Deletes events older than `keep`; the line for the job log. */
  def prune(
      sessions: Resource[IO, Session[IO]],
      keep: FiniteDuration
  ): IO[String] =
    sessions
      .use(
        _.execute(
          sql"""DELETE FROM search_events
                WHERE at < now() - make_interval(secs => $float8)""".command
        )(keep.toSeconds.toDouble)
      )
      .map {
        case skunk.data.Completion.Delete(n) => s"deleted $n old search events"
        case other                           => s"pruned search events: $other"
      }

  private type Row = (
      OffsetDateTime,
      String,
      String,
      Option[String],
      Option[String],
      Option[String],
      List[String],
      Option[Int],
      List[String],
      Option[String],
      Option[Int],
      Option[String]
  )

  private val row: Codec[Row] =
    timestamptz *: text *: text *: text.opt *: text.opt *: text.opt *:
      IngestRepo.textList *:
      int4.opt *: IngestRepo.textList *: text.opt *: int4.opt *: text.opt

  private def toRow(entry: Entry): Row =
    val e = entry.event
    (
      entry.at,
      entry.source.param,
      e.kind.param,
      e.searchId,
      e.session,
      e.query.map(_.q.trim).filter(_.nonEmpty),
      e.query.toList.flatMap(filters),
      e.total,
      e.results,
      e.result,
      e.position,
      e.detail
    )

  /** The query's parameters other than the text, as `key=value`. */
  def filters(q: SearchQuery): List[String] =
    q.params.collect { case (k, v) if k != "q" => s"$k=$v" }
