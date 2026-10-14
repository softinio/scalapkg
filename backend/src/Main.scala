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

package com.softinio.scalapkg

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.softinio.scalapkg.db.Database
import com.softinio.scalapkg.db.IngestRepo
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.PackageReport
import com.softinio.scalapkg.db.ReportResult
import com.softinio.scalapkg.db.SearchGate
import com.softinio.scalapkg.db.SearchLog
import com.softinio.scalapkg.db.SiteStatus
import com.softinio.scalapkg.db.StatusRepo
import com.softinio.scalapkg.depsdev.DepsDev
import com.softinio.scalapkg.domain.ApiError
import com.softinio.scalapkg.domain.ArtifactVersion
import com.softinio.scalapkg.domain.LiveInfo
import com.softinio.scalapkg.embed.Embedder
import com.softinio.scalapkg.github.GitHub
import com.softinio.scalapkg.html.Pages
import com.softinio.scalapkg.ingest.*
import com.softinio.scalapkg.maven.MavenIndex
import com.softinio.scalapkg.maven.MavenRepo
import com.softinio.scalapkg.routes.*
import com.softinio.scalapkg.summary.Anthropic
import io.circe.syntax.*
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Retry-After`
import org.http4s.server.Router
import org.http4s.server.middleware.GZip
import org.typelevel.ci.CIString
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import skunk.exception.PostgresErrorException

import scala.concurrent.duration.*

/** Server entry point: loads `AppConfig`, migrates the database, rebuilds the
  * search documents, starts the ingestion jobs that are configured and serves
  * HTTP.
  */
/** What the status page and the report form need: the status query, the report
  * check, how often queued reports are checked, and the per-visitor limit on
  * reports.
  */
final case class ReportsApi(
    status: IO[SiteStatus],
    submit: PackageReport => IO[ReportResult],
    interval: FiniteDuration,
    limit: RateLimit
)

object Main extends IOApp.Simple:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  /** `Implementation-Version` from the jar manifest, else `dev`; logged on
    * start and reported in the MCP `serverInfo`.
    */
  val version: String =
    Option(getClass.getPackage.getImplementationVersion).getOrElse("dev")

  /** All routes: assets, then `/api` and `/mcp` behind `rateLimit`, `/events`
    * behind `eventLimit` and `/status/report` behind `reportLimit`, then the
    * HTML pages; anything unmatched gets the 404 page. A search refused by the
    * [[SearchGate]] or cancelled by the statement timeout answers 503 with
    * `Retry-After`. Responses are gzipped.
    */
  def httpApp(
      baseUrl: String,
      devAssetsDir: Option[String],
      repo: PackageRepo,
      live: ArtifactVersion => IO[LiveInfo],
      rateLimit: RateLimit,
      eventLimit: RateLimit,
      log: SearchLog,
      reports: ReportsApi
  ): HttpApp[IO] =
    val assets = Assets(devAssetsDir)
    val public = PublicRoutes(repo, baseUrl)
    val status = StatusRoutes(reports.status, reports.interval, baseUrl)
    val report =
      ReportRoutes(reports.submit, reports.status, reports.interval, baseUrl)
    val limited = Router(
      "/api" -> rateLimit(ApiRoutes(repo).routes <+> status.api),
      "/mcp" -> rateLimit(Mcp(repo, live, version, log).routes),
      "/events" -> eventLimit(EventRoutes(log).routes),
      "/status/report" -> reports.limit(report.routes)
    )
    val routes = assets.routes <+> limited <+> public.routes <+> status.page
    // the Scala.js bundle and the HTML compress well (~900 KB -> ~200 KB JS)
    GZip(
      Kleisli(req =>
        routes
          .run(req)
          .getOrElseF(Html.notFound(Pages.notFound(baseUrl)))
          .recoverWith { case e if overloaded(e) => busy }
      )
    )

  // 57014: query_canceled, which is what statement_timeout raises
  private def overloaded(e: Throwable): Boolean = e match
    case SearchGate.Busy           => true
    case p: PostgresErrorException => p.code == "57014"
    case _                         => false

  private val busy =
    ServiceUnavailable(ApiError("busy, try again in a moment").asJson)
      .map(
        _.putHeaders(
          `Retry-After`.unsafeFromLong(2),
          // never let the edge keep serving a moment's overload
          Header.Raw(CIString("Cache-Control"), "no-store")
        )
      )

  private def jobs(
      client: Client[IO],
      depsDev: DepsDev,
      ingest: IngestRepo,
      config: IngestConfig,
      scheduler: Scheduler,
      reports: Reports,
      reportsConfig: ReportsConfig
  )(using Logger[IO]): IO[Unit] =
    val maven =
      MavenIndex(client, config.indexCacheDir.map(fs2.io.file.Path(_)))
    val sync = IndexSync(maven, ingest, config.bootstrapChunks)
    val enrich = Enrich(depsDev, ingest)
    val fresh = Freshness(depsDev, ingest, config.freshnessMaxPackages)
    val deps = DepsJob(depsDev, ingest)
    val firstReleases = FirstReleases(depsDev, ingest)
    // Central rebuilds the index about every two weeks and rate-limits
    // downloads from it per IP: once a day is plenty
    (Option
      .when(config.indexSyncEnabled)(
        scheduler.every("index-sync", 10.seconds, 24.hours)(sync.run)
      )
      .toList ++ List(
      scheduler.every("enrich", 1.minute, 15.minutes)(enrich.run),
      scheduler.every("freshness", 2.hours, 24.hours)(fresh.run),
      // new versions bring new requirements; the first run reads them all
      scheduler.every("deps", 2.minutes, 1.hour)(deps.run),
      scheduler.every("first-releases", 3.minutes, 5.minutes)(
        firstReleases.run
      ),
      scheduler.every("reports", 5.minutes, reportsConfig.interval)(
        reports.run(reportsConfig.maxPerRun)
      )
    )).parSequence_

  /** Runs `job` in the background when it is set up, else logs why it is off.
    */
  private def optional(job: Option[IO[Unit]], off: String)(using
      Logger[IO]
  ): Resource[IO, Unit] =
    job.fold(Resource.eval(Logger[IO].info(off)))(_.background.void)

  def run: IO[Unit] =
    Slf4jLogger.create[IO].flatMap { case given Logger[IO] =>
      AppConfig.load.flatMap { config =>
        val resources =
          for
            sessions <- Database.pool(config.db)
            _ <- Resource.eval(Database.migrate(sessions))
            // pages and the API get their own pool, whose statement timeout
            // bounds each query; ingestion's long bulk writes use `sessions`
            web <- Database.pool(config.db, statementTimeout = Some(2.seconds))
            gate <- Resource.eval(
              SearchGate(2 * Runtime.getRuntime.availableProcessors)
            )
            plain <- Http.client
            client = Http.retrying(plain)
            throttle <- Resource.eval(Throttle(250.millis))
            depsDev = DepsDev(client, DepsDev.api, throttle)
            embedder <- config.search.modelDir
              .traverse(dir => Embedder.load(java.nio.file.Path.of(dir)))
            embed <- Resource.eval(embedder.traverse(_.queryVectors))
            repo = PackageRepo.skunk(
              web,
              embed = embed,
              maxDistance = config.search.semanticMaxDistance,
              gate = Some(gate)
            )
            live <- Resource.eval(depsDev.cachedLive(1.hour))
            ingest = IngestRepo(sessions)
            _ <- Resource.eval(ingest.rebuildSearchDocs)
            rateLimit <- Resource.eval(RateLimit(60, 1.minute))
            // a settled search at most every couple of seconds, plus clicks
            eventLimit <- Resource.eval(RateLimit(60, 1.minute))
            searchLog <- SearchLog.queued(SearchLog.writer(sessions))
            statusRepo = StatusRepo(sessions)
            scheduler = Scheduler(statusRepo)
            _ <- scheduler
              .every("prune", 10.minutes, 24.hours)(
                (
                  SearchLog.prune(sessions, 90.days),
                  statusRepo.prune(90.days)
                ).mapN((a, b) => s"$a, $b")
              )
              .background
            // a few reports a minute per visitor; the deps.dev checks behind
            // them are also capped for everyone together (Reports)
            reportLimit <- Resource.eval(RateLimit(5, 1.minute))
            budget <- Resource.eval(Budget(config.reports.livePerHour, 1.hour))
            slots <- Resource.eval(Semaphore[IO](2))
            reports = Reports(
              depsDev,
              MavenRepo(client, MavenRepo.central),
              ingest,
              statusRepo,
              budget,
              slots,
              config.reports.interval
            )
            _ <- optional(
              Option.when(config.ingest.enabled)(
                jobs(
                  client,
                  depsDev,
                  ingest,
                  config.ingest,
                  scheduler,
                  reports,
                  config.reports
                )
              ),
              "Maven/deps.dev ingestion disabled"
            )
            _ <- optional(
              config.search.githubToken.map(token =>
                Throttle(1.second).flatMap(t =>
                  scheduler.every("github", 30.seconds, 1.hour)(
                    GithubSync(GitHub(client, token.value, t), ingest).run
                  )
                )
              ),
              "no GitHub token: topics and READMEs off"
            )
            _ <- optional(
              config.search.summaries.map(s =>
                scheduler.every("summaries", 3.minutes, 30.minutes)(
                  SummaryJob(
                    Anthropic(plain, s.apiKey.value, s.model),
                    ingest,
                    afterGithub = config.search.githubToken.isDefined
                  ).run
                )
              ),
              "no Anthropic API key: project summaries off"
            )
            _ <- optional(
              embedder.map(e =>
                scheduler.every("embed", 45.seconds, 10.minutes)(
                  EmbedJob(e, ingest).run
                )
              ),
              "no embedding model: semantic search off"
            )
            server <- EmberServerBuilder
              .default[IO]
              .withHost(config.http.host)
              .withPort(config.http.port)
              .withHttpApp(
                httpApp(
                  config.baseUrl,
                  config.devAssetsDir,
                  repo,
                  live,
                  rateLimit,
                  eventLimit,
                  searchLog,
                  ReportsApi(
                    StatusRepo(web).status,
                    reports.submit,
                    config.reports.interval,
                    reportLimit
                  )
                )
              )
              .build
          yield server

        resources.use { server =>
          Logger[IO].info(
            s"scalapkg $version listening on ${server.address}"
          ) *>
            IO.never
        }
      }
    }
