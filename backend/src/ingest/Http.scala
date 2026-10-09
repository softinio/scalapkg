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

package com.softinio.scalapkg.ingest

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import org.http4s.ProductId
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`User-Agent`
import org.typelevel.log4cats.LoggerFactory

import scala.concurrent.duration.*

/** The HTTP client the ingestion jobs share, built once in `Main.run`. */
object Http:

  val userAgent: `User-Agent` =
    `User-Agent`(
      ProductId("scalapkg"),
      List(org.http4s.ProductComment("+https://pkg.scalanews.net"))
    )

  // 429 and 5xx are the usual "slow down"/transient answers; Maven Central's
  // Cloudflare front sends 403 when rate limiting, so retry that too
  private val retriableStatus = Set(403, 429, 500, 502, 503, 504)

  // a server asking us to come back later than this is a quota, not a blip:
  // give up and let the job's next scheduled run try again
  private val maxRetryAfter = 5.minutes

  private def retryAfter(
      resp: org.http4s.Response[IO]
  ): Option[FiniteDuration] =
    resp.headers
      .get[org.http4s.headers.`Retry-After`]
      .flatMap(_.retry.toOption)
      .map(_.seconds)

  private val policy: RetryPolicy[IO] = RetryPolicy[IO](
    RetryPolicy.exponentialBackoff(maxWait = 1.minute, maxRetry = 4),
    (_, result) =>
      result match
        case Right(resp) =>
          retriableStatus.contains(resp.status.code) &&
          retryAfter(resp).forall(_ <= maxRetryAfter)
        case Left(_) => true
  )

  /** Retries that honour Retry-After, for requests that are safe to repeat. */
  def retrying(client: Client[IO])(using LoggerFactory[IO]): Client[IO] =
    Retry(policy)(client)

  /** Ember client with our User-Agent, without retries. */
  def client(using LoggerFactory[IO]): Resource[IO, Client[IO]] =
    EmberClientBuilder
      .default[IO]
      // no overall deadline: the full Maven index is a multi-GB stream; a
      // stalled connection is caught by the idle timeout instead
      .withTimeout(Duration.Inf)
      .withIdleConnectionTime(2.minutes)
      .build
      .map(base => Client[IO](req => base.run(req.putHeaders(userAgent))))

/** Spaces calls to one host at least `interval` apart, across fibers. */
final class Throttle private (
    interval: FiniteDuration,
    last: AtomicCell[IO, Long]
):
  /** Waits for this call's slot, then runs `io`; only the starts are spaced, so
    * calls can still overlap.
    */
  def apply[A](io: IO[A]): IO[A] =
    last.evalUpdate { prev =>
      IO.monotonic.flatMap { now =>
        val wait = prev + interval.toMillis - now.toMillis
        IO.sleep(wait.millis).whenA(wait > 0) *> IO.monotonic.map(_.toMillis)
      }
    } *> io

object Throttle:
  def apply(interval: FiniteDuration): IO[Throttle] =
    AtomicCell[IO].of(Long.MinValue / 2).map(new Throttle(interval, _))
