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
import com.softinio.scalapkg.DbConfig
import dumbo.Dumbo
import dumbo.logging.Implicits.console as dumboConsoleLogger
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import skunk.Session

import scala.concurrent.duration.FiniteDuration

/** Postgres connection pool and schema migrations, both set up once in
  * `Main.run` before any route or job uses the database.
  */
object Database:

  private given Tracer[IO] = Tracer.noop[IO]
  private given Meter[IO] = Meter.noop[IO]

  /** A pool of up to 10 skunk sessions, over the unix socket in `unixSocketDir`
    * when it is set, else over TCP. With `statementTimeout`, Postgres cancels
    * any statement that runs longer (the web pool sets one; ingestion, whose
    * bulk writes can run long, does not).
    */
  def pool(
      config: DbConfig,
      statementTimeout: Option[FiniteDuration] = None
  ): Resource[IO, Resource[IO, Session[IO]]] =
    val base = Session
      .Builder[IO]
      .withDatabase(config.database)
    val withConn = config.unixSocketDir match
      case Some(dir) => base.withUnixSocketDirectory(dir)
      case None      => base.withHost(config.host).withPort(config.port)
    val withAuth = config.password match
      case Some(pw) => withConn.withUserAndPassword(config.user, pw)
      case None     => withConn.withUser(config.user)
    val withTimeout = statementTimeout.fold(withAuth)(t =>
      withAuth.withConnectionParameters(
        withAuth.connectionParameters + ("statement_timeout" -> t.toMillis.toString)
      )
    )
    withTimeout.pooled(max = 10)

  // NOTE: every new migration file must also be added to this list
  /** Applies pending migrations with Dumbo; fails if an applied migration's
    * checksum changed, so applied files are never edited: a schema change is a
    * new file.
    */
  def migrate(sessions: Resource[IO, Session[IO]])(using
      Logger[IO]
  ): IO[Unit] =
    Dumbo
      .withResources[IO](
        List(
          dumbo.ResourceFilePath("/db/migration/V1__init.sql"),
          dumbo.ResourceFilePath("/db/migration/V2__search_events.sql"),
          dumbo.ResourceFilePath("/db/migration/V3__project_moves.sql")
        )
      )
      .withSession(sessions)(using cats.effect.Sync[IO], dumboConsoleLogger)
      .runMigration
      .flatMap { result =>
        Logger[IO].info(
          s"Database migrations applied: ${result.migrationsExecuted}"
        )
      }
