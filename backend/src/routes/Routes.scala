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

package com.softinio.scalapkg.routes

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import com.softinio.scalapkg.db.EventSource
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.PackageReport
import com.softinio.scalapkg.db.ReportOutcome
import com.softinio.scalapkg.db.ReportReason
import com.softinio.scalapkg.db.ReportResult
import com.softinio.scalapkg.db.SearchLog
import com.softinio.scalapkg.db.SiteStatus
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.html.Feed
import com.softinio.scalapkg.html.Pages
import com.softinio.scalapkg.html.ReportForm
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.headers.`Retry-After`
import org.typelevel.ci.CIString
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.ZoneOffset
import scala.concurrent.duration.*
import scala.util.Try

/** Builds text responses with the plain string encoder, explicitly: files that
  * import the circe entity encoders would otherwise JSON-quote the body.
  */
object Html:
  private def withType(status: Status, body: String, mt: MediaType) =
    IO.pure(
      Response[IO](status)
        .withEntity(body)(using EntityEncoder.stringEncoder)
        .withContentType(headers.`Content-Type`(mt, Charset.`UTF-8`))
    )

  def ok(body: String): IO[Response[IO]] =
    withType(Status.Ok, body, MediaType.text.html)

  def notFound(body: String): IO[Response[IO]] =
    withType(Status.NotFound, body, MediaType.text.html)

  def badRequest(body: String): IO[Response[IO]] =
    withType(Status.BadRequest, body, MediaType.text.html)

  /** For answers to one visitor's form, which no cache may keep. */
  val noStore: Header.Raw = Header.Raw(CIString("Cache-Control"), "no-store")

  def atom(body: String): IO[Response[IO]] =
    withType(Status.Ok, body, MediaType.application.`atom+xml`)

  def plain(body: String): IO[Response[IO]] =
    withType(Status.Ok, body, MediaType.text.plain)

  /** Lets browsers and the Cloudflare edge reuse a response for `seconds`.
    * Pages carry no per-visitor content, so they can be cached publicly.
    */
  def cacheControl(seconds: Int): Header.Raw =
    Header.Raw(CIString("Cache-Control"), s"public, max-age=$seconds")

  def cached(seconds: Int)(resp: IO[Response[IO]]): IO[Response[IO]] =
    resp.map(_.putHeaders(cacheControl(seconds)))

/** The HTML site: home and search (`/`), the results fragment the search island
  * swaps in, `/p/{groupId}/{name}` deep links, `/new`, `/about`, the Atom feed
  * and `robots.txt`. Not rate-limited.
  */
final class PublicRoutes(repo: PackageRepo, baseUrl: String):

  private val landing: IO[Pages.Landing] =
    (repo.newProjects(10), repo.newest(10), repo.mostStarred(10))
      .parMapN(Pages.Landing.apply)

  private def isLanding(q: SearchQuery): Boolean =
    q.isEmpty && q.sort == Sort.Relevance && q.page == 1

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ GET -> Root =>
      val query = SearchQuery.fromParams(req.multiParams)
      if isLanding(query) then
        Html.cached(600)(
          landing.flatMap(l =>
            Html.ok(Pages.home(Pages.Home.Lists(l), baseUrl))
          )
        )
      else
        Html.cached(60)(
          repo
            .search(query)
            .flatMap(r => Html.ok(Pages.home(Pages.Home.Results(r), baseUrl)))
        )

    // results region only, swapped in by the search island
    case req @ GET -> Root / "fragments" / "search" =>
      Html.cached(60)(
        repo
          .search(SearchQuery.fromParams(req.multiParams))
          .flatMap(r => Html.ok(Pages.resultsFrag(r).render))
      )

    case GET -> Root / "p" / groupId / name =>
      repo.get(PackageRef(groupId, name)).flatMap {
        case Some(pkg) =>
          Html.cached(600)(
            landing.flatMap(l =>
              Html.ok(Pages.home(Pages.Home.Lists(l, Some(pkg)), baseUrl))
            )
          )
        case None => Html.notFound(Pages.notFound(baseUrl))
      }

    case req @ GET -> Root / "new" =>
      val page = req.params.get("page").flatMap(_.toIntOption).getOrElse(1)
      repo.newProjects(PackageRepo.maxNewProjects).flatMap { ps =>
        if page < 1 || page > math.max(1, Pages.pageCount(ps.size)) then
          Html.notFound(Pages.notFound(baseUrl))
        else Html.cached(600)(Html.ok(Pages.newProjects(ps, page, baseUrl)))
      }

    case GET -> Root / "about" =>
      Html.cached(3600)(Html.ok(Pages.about(baseUrl)))

    case GET -> Root / "feed.xml" =>
      for
        now <- IO.realTimeInstant
        since = OffsetDateTime.ofInstant(now, ZoneOffset.UTC).minusDays(14)
        releases <- repo.releases(since, 100)
        updated = releases.headOption.map(_.releasedAt).getOrElse(now.toString)
        resp <- Html.cached(600)(
          Html.atom(Feed.atom(baseUrl, releases, updated))
        )
      yield resp

    case GET -> Root / "robots.txt" =>
      Html.cached(86400)(
        Html.plain(
          "User-agent: *\nDisallow: /fragments/\nDisallow: /api/\nDisallow: /mcp\n"
        )
      )
  }

/** Read-only public JSON API under /api/v1. */
final class ApiRoutes(repo: PackageRepo):

  private val cors = Header.Raw(CIString("Access-Control-Allow-Origin"), "*")

  private def json[A: io.circe.Encoder](a: A, seconds: Int): IO[Response[IO]] =
    Ok(a.asJson).map(_.putHeaders(cors, Html.cacheControl(seconds)))

  private def badRequest(msg: String): IO[Response[IO]] =
    BadRequest(ApiError(msg).asJson).map(_.putHeaders(cors))

  /** `search`, `packages/{groupId}/{name}`, `new-projects` (`limit` 1..100,
    * default 20) and `releases` (`limit` 1..500, default 100; `since` defaults
    * to 7 days ago). A bad parameter gets a 400 with an `ApiError`.
    */
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ GET -> Root / "v1" / "search" =>
      repo.search(SearchQuery.fromParams(req.multiParams)).flatMap(json(_, 60))

    case GET -> Root / "v1" / "packages" / groupId / name =>
      repo.get(PackageRef(groupId, name)).flatMap {
        case Some(p) => json(PackageResponse.of(p), 300)
        case None    =>
          NotFound(ApiError(s"no package $groupId:$name").asJson)
            .map(_.putHeaders(cors))
      }

    case req @ GET -> Root / "v1" / "new-projects" =>
      val limit = req.params.get("limit").flatMap(_.toIntOption).getOrElse(20)
      if limit < 1 || limit > 100 then badRequest("limit must be 1..100")
      else repo.newProjects(limit).flatMap(ps => json(ps, 300))

    case req @ GET -> Root / "v1" / "releases" =>
      val params = req.params
      val limit = params.get("limit").flatMap(_.toIntOption).getOrElse(100)
      val since = params.get("since") match
        case Some(s) =>
          ApiRoutes
            .parseSince(s)
            .toRight(s"since must be an ISO-8601 timestamp, got $s")
        case None => Right(OffsetDateTime.now(ZoneOffset.UTC).minusDays(7))
      since match
        case Left(err)                            => badRequest(err)
        case Right(_) if limit < 1 || limit > 500 =>
          badRequest("limit must be 1..500")
        case Right(at) =>
          repo
            .releases(at, limit)
            .flatMap(rs =>
              json(ReleasesResponse(PackageRepo.isoString(at), rs), 300)
            )
  }

object ApiRoutes:

  /** A `since` argument, for the API and MCP alike: an ISO-8601 timestamp, or a
    * date, read as midnight UTC.
    */
  def parseSince(s: String): Option[OffsetDateTime] =
    Try(OffsetDateTime.parse(s))
      .orElse(Try(Instant.parse(s).atOffset(ZoneOffset.UTC)))
      .orElse(Try(LocalDate.parse(s).atStartOfDay().atOffset(ZoneOffset.UTC)))
      .toOption

/** `POST /events`: search events from the islands (see [[SearchEvent]]). The
  * body is read only up to `EventRoutes.maxBody` bytes and checked before it is
  * queued; a malformed event gets 400, an accepted one 204 (whether or not the
  * queue had room for it).
  */
final class EventRoutes(log: SearchLog):

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] { case req @ POST -> Root =>
    req.body
      .take(EventRoutes.maxBody + 1L)
      .compile
      .to(Array)
      .flatMap { bytes =>
        Option
          .when(bytes.length <= EventRoutes.maxBody)(bytes)
          .flatMap(b => decode[SearchEvent](String(b, UTF_8)).toOption)
          .flatMap(EventRoutes.validated)
          .fold(BadRequest())(e =>
            log.record(EventSource.Web, e) *> NoContent()
          )
      }
  }

object EventRoutes:
  val maxBody = 4096
  private val maxText = 200
  private val id = "[A-Za-z0-9-]{8,64}".r

  /** The event if it is well formed, its query normalised the way a search
    * request's would be.
    */
  def validated(e: SearchEvent): Option[SearchEvent] =
    val query =
      e.query.map(q => SearchQuery.fromParams(q.params.groupMap(_._1)(_._2)))
    val shape = e.kind match
      case EventKind.Search =>
        query.isDefined && e.total.exists(_ >= 0) &&
        e.result.isEmpty && e.position.isEmpty
      case _ =>
        e.query.isEmpty && e.total.isEmpty && e.results.isEmpty &&
        e.result.isDefined &&
        e.position.exists(p =>
          p >= 1 && p <= SearchQuery.maxPage * SearchQuery.pageSize
        )
    Option.when(
      shape &&
        e.searchId.exists(id.matches) && e.session.exists(id.matches) &&
        e.results.size <= SearchEvent.maxResults &&
        (e.results ++ e.result ++ e.detail).forall(_.length <= maxText)
    )(e.copy(query = query))

/** `/status`, the state of the index and its jobs, and the same as JSON at
  * `/api/v1/status` (mounted under the rate-limited `/api`).
  */
final class StatusRoutes(
    status: IO[SiteStatus],
    reportsInterval: FiniteDuration,
    baseUrl: String
):

  private val cors = Header.Raw(CIString("Access-Control-Allow-Origin"), "*")

  val page: HttpRoutes[IO] = HttpRoutes.of[IO] { case GET -> Root / "status" =>
    Html.cached(60)(
      (status, IO.realTimeInstant).flatMapN((s, now) =>
        Html.ok(
          Pages.status(s, now, ReportForm.empty, None, reportsInterval, baseUrl)
        )
      )
    )
  }

  val api: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "v1" / "status" =>
      status.flatMap(s =>
        Ok(s.asJson).map(_.putHeaders(cors, Html.cacheControl(60)))
      )
  }

/** `POST /status/report`: the report form on `/status` (works without JS). The
  * body is read only up to `ReportRoutes.maxBody` bytes. A bad form gets the
  * status page again with the error (400); a good one the outcome of `submit`.
  * A filled-in honeypot field gets a plausible answer and nothing else.
  */
final class ReportRoutes(
    submit: PackageReport => IO[ReportResult],
    status: IO[SiteStatus],
    reportsInterval: FiniteDuration,
    baseUrl: String
):

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] { case req @ POST -> Root =>
    req.body
      .take(ReportRoutes.maxBody + 1L)
      .compile
      .to(Array)
      .flatMap { bytes =>
        val fields = Option
          .when(bytes.length <= ReportRoutes.maxBody)(bytes)
          .flatMap(b =>
            UrlForm.decodeString(Charset.`UTF-8`)(String(b, UTF_8)).toOption
          )
          .map(_.values.view.mapValues(_.headOption.getOrElse("")).toMap)
          .getOrElse(Map.empty)
        val form = ReportForm(
          fields.getOrElse("group", ""),
          fields.getOrElse("artifact", ""),
          fields.getOrElse("reason", ReportReason.Missing.param),
          fields.getOrElse("note", "")
        )
        ReportRoutes.parse(form) match
          case Left(error) =>
            (status, IO.realTimeInstant)
              .flatMapN((s, now) =>
                Html.badRequest(
                  Pages
                    .status(s, now, form, Some(error), reportsInterval, baseUrl)
                )
              )
              .map(_.putHeaders(Html.noStore))
          case Right(report) =>
            val result =
              if fields.get("website").exists(_.nonEmpty) then
                IO.pure(
                  ReportResult(ReportOutcome.Pending, "Thanks, it is queued.")
                )
              else submit(report)
            result
              .flatMap(r => Html.ok(Pages.reportResult(report, r, baseUrl)))
              .map(_.putHeaders(Html.noStore))
      }
  }

object ReportRoutes:
  val maxBody = 4096
  val maxNote = 500
  private val maxId = 100

  private val groupId = "[A-Za-z0-9_-]+(\\.[A-Za-z0-9_-]+)*".r
  private val artifactId = "[A-Za-z0-9][A-Za-z0-9_.-]*".r

  /** The report the form describes, or what is wrong with it. An artifact with
    * a Scala suffix (`zio-http_3`) is reported as that artifact, anything else
    * as a package name (`zio-http`).
    */
  def parse(form: ReportForm): Either[String, PackageReport] =
    val group = form.group.trim
    val artifact = form.artifact.trim
    val note = form.note.trim
    for
      _ <- Either.cond(
        group.length <= maxId && groupId.matches(group),
        (),
        "Enter a groupId such as dev.zio."
      )
      _ <- Either.cond(
        artifact.length <= maxId && artifactId.matches(artifact) &&
          !artifact.contains(".."),
        (),
        "Enter an artifactId such as zio-http_3, or a package name such as zio-http."
      )
      reason <- ReportReason
        .parse(form.reason)
        .toRight("Choose what is wrong.")
      _ <- Either.cond(
        note.length <= maxNote,
        (),
        s"Keep the note under $maxNote characters."
      )
    yield
      val parsed = ArtifactIds.parse(artifact)
      PackageReport(
        group,
        parsed.as(artifact),
        parsed.fold(artifact)(_.name),
        reason,
        Option(note).filter(_.nonEmpty)
      )

/** Fixed-window per-client request limit for the API and MCP endpoints. The
  * client is the X-Real-IP address (nginx overwrites it) or the peer, never
  * X-Forwarded-For, whose first entry the client chooses.
  */
final class RateLimit private (
    limit: Int,
    window: FiniteDuration,
    // the current window's start and each client's count in it
    state: Ref[IO, (Long, Map[String, Int])]
):
  private def clientOf(req: Request[IO]): String =
    req.headers
      .get(CIString("X-Real-IP"))
      .map(_.head.value)
      .orElse(req.remoteAddr.map(_.toString))
      .getOrElse("unknown")

  // counts every request it sees, so mount it only on the limited prefixes
  def apply(routes: HttpRoutes[IO]): HttpRoutes[IO] = Kleisli { req =>
    OptionT.liftF(IO.monotonic.map(_.toMillis)).flatMap { now =>
      val key = clientOf(req)
      val windowStart = now - now % window.toMillis
      OptionT
        .liftF(state.modify { (w, m) =>
          // a new window starts every count afresh
          val counts = if w == windowStart then m else Map.empty
          val count = counts.getOrElse(key, 0) + 1
          ((windowStart, counts.updated(key, count)), count)
        })
        .flatMap { count =>
          if count > limit then
            val retryIn = (windowStart + window.toMillis - now) / 1000 + 1
            OptionT.liftF(
              TooManyRequests(ApiError("rate limit exceeded").asJson)
                .map(_.putHeaders(`Retry-After`.unsafeFromLong(retryIn)))
            )
          else routes(req)
        }
    }
  }

object RateLimit:
  def apply(limit: Int, window: FiniteDuration): IO[RateLimit] =
    Ref
      .of[IO, (Long, Map[String, Int])]((0L, Map.empty))
      .map(new RateLimit(limit, window, _))

/** Serves the bundled static assets (CSS, JS, fonts) from the classpath,
  * falling back to a local directory during development so
  * `mill -w frontend.fastLinkJS` output is picked up without re-packaging.
  */
final class Assets(devAssetsDir: Option[String]):

  private given org.typelevel.log4cats.LoggerFactory[IO] =
    Slf4jFactory.create[IO]

  // URLs carry ?v=<deploy tag>, so assets can be cached for a long time
  private val cacheHeader =
    Header.Raw(CIString("Cache-Control"), "public, max-age=31536000, immutable")

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> "assets" /: rest =>
      val name = rest.segments.map(_.encoded).mkString("/")
      if name.contains("..") then NotFound()
      else
        val fromDev = devAssetsDir
          .map(dir =>
            StaticFile.fromPath[IO](fs2.io.file.Path(dir) / name, Some(req))
          )
          .getOrElse(OptionT.none[IO, Response[IO]])
        fromDev
          .orElse(
            StaticFile.fromResource[IO](s"/public/assets/$name", Some(req))
          )
          .map(_.putHeaders(cacheHeader))
          .getOrElseF(NotFound())
  }
