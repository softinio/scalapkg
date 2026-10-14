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

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import com.softinio.scalapkg.db.*
import com.softinio.scalapkg.db.EventSource
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.SearchGate
import com.softinio.scalapkg.db.SearchLog
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.routes.RateLimit
import io.circe.Json
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString

import java.time.OffsetDateTime
import scala.concurrent.duration.*

class RoutesSuite extends CatsEffectSuite {

  private val baseUrl = "https://pkg.example.test"

  private val cats = PackageSummary(
    "org.typelevel",
    "cats-core",
    "2.13.0",
    Some("2025-01-20T10:00:00Z"),
    List("jvm:3", "jvm:2.13", "sjs1:3", "native0.5:3"),
    List("MIT"),
    deprecated = false,
    Some(
      ProjectInfo(
        "github.com/typelevel/cats",
        Some("Lightweight, modular, and extensible library for functional programming."),
        Some(5470),
        Some(1243),
        Some(279),
        None,
        Some("https://typelevel.org/cats/"),
        Some(5.6)
      )
    )
  )

  private val plugin = PackageSummary(
    "org.scalameta",
    "sbt-scalafmt",
    "2.5.4",
    Some("2025-02-01T00:00:00Z"),
    List("sbt1:2.12"),
    List("Apache-2.0"),
    deprecated = false,
    None
  )

  private val all = List(cats, plugin)

  /** Filters in memory, enough to exercise routing and rendering. */
  private object repo extends PackageRepo:
    def search(q: SearchQuery): IO[SearchResponse] =
      // "busy" stands for a search the SearchGate refuses
      if q.q == "busy" then IO.raiseError(SearchGate.Busy) else
      // "boom" stands for a search that fails inside the repository
      if q.q == "boom" then IO.raiseError(new NoSuchElementException("boom"))
      else search0(q)
    private def search0(q: SearchQuery): IO[SearchResponse] =
      // "redis" stands for a query nothing is about: everything is related
      val related = q.q == "redis"
      val hits = all.filter(p =>
        (q.q.isEmpty || related || p.name.contains(q.q.toLowerCase)) &&
          (q.platforms.isEmpty ||
            p.parsedTargets.exists(t => q.platforms.contains(t.platform.facet)))
      )
      val results = hits.map(p =>
        SearchHit(
          p.project.map(_.id).getOrElse(s"maven:${p.groupId}"),
          p.name,
          p.latestReleaseAt,
          NonEmptyList.one(p),
          1
        )
      )
      IO.pure(
        SearchResponse(q, results.size, results, Facets(Map("jvm" -> 1, "sbt" -> 1), Map("3" -> 1)), related)
      )
    def get(ref: PackageRef): IO[Option[PackageSummary]] =
      IO.pure(all.find(_.ref == ref))
    // cats-core dropped Scala 2.12 after 2.10.0
    def targetVersions(ref: PackageRef): IO[Map[Target, String]] =
      IO.pure(
        all.find(_.ref == ref).toList.flatMap(p =>
          p.parsedTargets.map(_ -> p.latestVersion) ++
            Target.parseKey("jvm:2.12").filter(_ => p == cats).map(_ -> "2.10.0")
        ).toMap
      )
    def newest(limit: Int): IO[List[PackageSummary]] = IO.pure(all.take(limit))
    def mostStarred(limit: Int): IO[List[PackageSummary]] = IO.pure(List(cats))
    def newProjects(limit: Int): IO[List[NewProject]] =
      // sbt-scalafmt, then enough older ones for a second page of /new
      IO.pure(
        (NewProject(plugin, "2026-09-01T00:00:00Z") ::
          List.tabulate(24)(i => NewProject(plugin.copy(name = s"older-$i"), "2026-08-01T00:00:00Z")))
          .take(limit)
      )
    def releases(since: OffsetDateTime, limit: Int): IO[List[Release]] =
      IO.pure(
        List(Release("org.typelevel", "cats-core", "2.13.0", "2025-01-20T10:00:00Z", cats.targets, cats.project))
      )

  private val live: ArtifactVersion => IO[LiveInfo] =
    _ => IO.pure(LiveInfo(None, Some(Nil), None))

  private val siteStatus = SiteStatus(
    Some(IndexState(940, OffsetDateTime.parse("2026-10-03T04:00:00Z"), None, Some(941), Some(OffsetDateTime.parse("2026-10-02T04:00:00Z")))),
    List(JobRun("enrich", None, None, Some(OffsetDateTime.parse("2026-10-03T05:00:00Z")), Some("enriched 3 artifacts"), None)),
    Backlog(3, 0, 0, 10, 20),
    ReportCounts(1, 2)
  )

  private def app(
      limit: Int = 100,
      log: SearchLog = SearchLog.noop,
      submit: PackageReport => IO[ReportResult] = _ => IO.pure(ReportResult(ReportOutcome.Pending, "queued"))
  ): IO[HttpApp[IO]] =
    for
      rl <- RateLimit(limit, 1.minute)
      el <- RateLimit(limit, 1.minute)
      reportLimit <- RateLimit(limit, 1.minute)
    yield Main.httpApp(baseUrl, None, repo, live, rl, el, log, ReportsApi(IO.pure(siteStatus), submit, 6.hours, reportLimit))

  /** A SearchLog that keeps what it is given, in order. */
  private def recording: IO[(SearchLog, IO[List[(EventSource, SearchEvent)]])] =
    Ref[IO].of(Vector.empty[(EventSource, SearchEvent)]).map(ref =>
      ((source, e) => ref.update(_ :+ (source, e)), ref.get.map(_.toList))
    )

  private def post(app: HttpApp[IO], uri: Uri, body: String): IO[(Status, String)] =
    app
      .run(
        Request[IO](Method.POST, uri)
          .withEntity(body)
          .withContentType(headers.`Content-Type`(MediaType.application.json))
      )
      .flatMap(r => r.as[String].map((r.status, _)))

  private def get(uri: Uri, limit: Int = 100): IO[(Status, String, Headers)] =
    app(limit).flatMap(_.run(Request[IO](Method.GET, uri))).flatMap(r =>
      r.as[String].map((r.status, _, r.headers))
    )

  private def mcp(body: String): IO[(Status, String)] =
    app().flatMap(
      _.run(
        Request[IO](Method.POST, uri"/mcp")
          .withEntity(body)
          .withContentType(headers.`Content-Type`(MediaType.application.json))
      )
    ).flatMap(r => r.as[String].map((r.status, _)))

  test("new projects API lists projects with their first release") {
    for
      (ok, body, _) <- get(uri"/api/v1/new-projects")
      (bad, _, _) <- get(uri"/api/v1/new-projects?limit=0")
    yield
      assertEquals(ok, Status.Ok)
      assert(body.contains("\"firstReleaseAt\":\"2026-09-01T00:00:00Z\""))
      assert(body.contains("sbt-scalafmt"))
      assertEquals(bad, Status.BadRequest)
  }

  test("landing page lists new and starred packages") {
    get(uri"/").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      assert(body.startsWith("<!DOCTYPE html>"))
      assert(body.contains("New releases"))
      assert(body.contains("Most starred"))
      assert(body.contains("New projects"))
      assert(body.contains("href=\"/p/org.scalameta/sbt-scalafmt\""))
      assert(body.contains("Sep 1, 2026"))
      assert(body.contains("data-island=\"search\""))
      assert(body.contains("data-island=\"package-drawer\""))
    }
  }

  test("every new project is listed on /new, linked from the landing page") {
    for
      (_, home, _) <- get(uri"/")
      (status, body, headers) <- get(uri"/new")
    yield
      assert(home.contains("href=\"/new\""), home)
      assertEquals(status, Status.Ok)
      assert(body.contains("<h1"), body)
      assert(body.contains("href=\"/p/org.scalameta/sbt-scalafmt\""), body)
      assert(body.contains("Sep 1, 2026"), body)
      assert(body.contains("Page 1 of 2"), body)
      assert(body.contains("href=\"/new?page=2\""), body)
      assertEquals(headers.get(CIString("Cache-Control")).map(_.head.value), Some("public, max-age=600"))
  }

  test("/new pages through the new projects 20 at a time") {
    for
      (status, body, _) <- get(uri"/new?page=2")
      (past, _, _) <- get(uri"/new?page=3")
      (zero, _, _) <- get(uri"/new?page=0")
    yield
      assertEquals(status, Status.Ok)
      assert(body.contains("Page 2 of 2"), body)
      assert(body.contains("older-23"), body)
      assert(!body.contains("sbt-scalafmt"), body)
      assert(body.contains("href=\"/new\""), body)
      assertEquals((past, zero), (Status.NotFound, Status.NotFound))
  }

  test("pages load GoatCounter, settings before count.js") {
    get(uri"/about").map { (_, body, _) =>
      val settings = body.indexOf("window.goatcounter")
      val counter = body.indexOf("https://wisdom.softinio.com/count.js")
      assert(settings >= 0, "the path-prefix settings must be on the page")
      assert(counter >= 0, "count.js must be on the page")
      assert(
        settings < counter,
        "settings must run before count.js reads window.goatcounter"
      )
      assert(
        body.contains("data-goatcounter=\"https://wisdom.softinio.com/count\"")
      )
    }
  }

  test("footer credits the author") {
    get(uri"/").map { (_, body, _) =>
      assert(body.contains("href=\"https://www.softinio.com\""))
      assert(body.contains("Salar Rahmanian"))
    }
  }

  test("search renders results with facets and package links") {
    get(uri"/?q=cats&platform=jvm").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      assert(body.contains("href=\"/p/org.typelevel/cats-core\""))
      assert(body.contains("Scala.js"))
      assert(body.contains("data-pkg="))
      assert(!body.contains("sbt-scalafmt"))
      assert(body.contains("noindex"))
    }
  }

  test("results nothing is about are labelled as related") {
    for
      (_, related, _) <- get(uri"/?q=redis")
      (_, matches, _) <- get(uri"/?q=cats")
    yield
      assert(related.contains("No project is described as"))
      assert(related.contains("cats-core"))
      assert(!matches.contains("No project is described as"))
  }

  test("results fragment is only the results region") {
    get(uri"/fragments/search?q=sbt").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      assert(!body.contains("<html"))
      assert(body.contains("sbt-scalafmt"))
    }
  }

  test("package deep link opens the drawer with the package") {
    get(uri"/p/org.typelevel/cats-core").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      assert(body.contains("org.typelevel:cats-core"))
      assert(body.contains("data-props=\"{"))
      assert(body.contains("&quot;cats-core&quot;"))
      assert(body.contains("<noscript>"))
    }
  }

  test("unknown package and unknown path get the 404 page") {
    for
      (s1, _, _) <- get(uri"/p/nope/nope")
      (s2, body, _) <- get(uri"/nope")
    yield
      assertEquals(s1, Status.NotFound)
      assertEquals(s2, Status.NotFound)
      assert(body.contains("Page not found"))
  }

  test("API search returns JSON with CORS") {
    get(uri"/api/v1/search?q=cats").map { (status, body, hdrs) =>
      assertEquals(status, Status.Ok)
      val json = parse(body).toOption.get
      assertEquals(json.hcursor.get[Int]("total"), Right(1))
      assertEquals(
        hdrs.get(org.typelevel.ci.CIString("Access-Control-Allow-Origin")).map(_.head.value),
        Some("*")
      )
    }
  }

  test("API package includes install snippets") {
    get(uri"/api/v1/packages/org.scalameta/sbt-scalafmt").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      assert(body.contains("addSbtPlugin"), body)
    }
  }

  test("API releases validates its parameters") {
    for
      (ok, body, _) <- get(uri"/api/v1/releases?since=2025-01-01T00:00:00Z")
      (date, _, _) <- get(uri"/api/v1/releases?since=2025-01-01")
      (bad, _, _) <- get(uri"/api/v1/releases?since=yesterday")
      (badLimit, _, _) <- get(uri"/api/v1/releases?limit=0")
    yield
      assertEquals(ok, Status.Ok)
      assert(body.contains("cats-core"))
      // a plain date, as the MCP tool accepts
      assertEquals(date, Status.Ok)
      assertEquals(bad, Status.BadRequest)
      assertEquals(badLimit, Status.BadRequest)
  }

  test("API requests over the limit get 429") {
    app(limit = 2).flatMap { a =>
      val req = Request[IO](Method.GET, uri"/api/v1/search?q=cats")
      for
        r1 <- a.run(req)
        r2 <- a.run(req)
        r3 <- a.run(req)
        page <- a.run(Request[IO](Method.GET, uri"/"))
      yield
        assertEquals(r1.status, Status.Ok)
        assertEquals(r2.status, Status.Ok)
        assertEquals(r3.status, Status.TooManyRequests)
        // pages are not rate limited
        assertEquals(page.status, Status.Ok)
    }
  }

  test("a client-chosen X-Forwarded-For does not reset the limit") {
    app(limit = 1).flatMap { a =>
      def req(ip: String) =
        Request[IO](Method.GET, uri"/api/v1/search?q=cats")
          .putHeaders(Header.Raw(CIString("X-Forwarded-For"), ip))
      for
        r1 <- a.run(req("10.0.0.1"))
        r2 <- a.run(req("10.0.0.2"))
      yield
        assertEquals(r1.status, Status.Ok)
        assertEquals(r2.status, Status.TooManyRequests)
    }
  }

  test("clients nginx tells apart (X-Real-IP) are limited separately") {
    app(limit = 1).flatMap { a =>
      def req(ip: String) =
        Request[IO](Method.GET, uri"/api/v1/search?q=cats")
          .putHeaders(Header.Raw(CIString("X-Real-IP"), ip))
      for
        a1 <- a.run(req("10.0.0.1"))
        b1 <- a.run(req("10.0.0.2"))
        a2 <- a.run(req("10.0.0.1"))
      yield
        assertEquals(a1.status, Status.Ok)
        assertEquals(b1.status, Status.Ok)
        assertEquals(a2.status, Status.TooManyRequests)
    }
  }

  test("pages tell the edge how long to cache them; 404s are not cached") {
    for
      (_, _, home) <- get(uri"/")
      (_, _, search) <- get(uri"/?q=cats")
      (missing, _, notFound) <- get(uri"/p/no/such")
    yield
      assertEquals(home.get(CIString("Cache-Control")).map(_.head.value), Some("public, max-age=600"))
      assertEquals(search.get(CIString("Cache-Control")).map(_.head.value), Some("public, max-age=60"))
      assertEquals(missing, Status.NotFound)
      assertEquals(notFound.get(CIString("Cache-Control")), None)
  }

  test("a search refused for load answers 503 with Retry-After") {
    for
      (api, apiBody, apiHeaders) <- get(uri"/api/v1/search?q=busy")
      (page, _, _) <- get(uri"/?q=busy")
    yield
      assertEquals(api, Status.ServiceUnavailable)
      assert(apiBody.contains("busy"), apiBody)
      assert(apiHeaders.get(CIString("Retry-After")).isDefined)
      assertEquals(page, Status.ServiceUnavailable)
  }

  test("Atom feed lists releases") {
    get(uri"/feed.xml").map { (status, body, hdrs) =>
      assertEquals(status, Status.Ok)
      assert(body.contains("<feed xmlns=\"http://www.w3.org/2005/Atom\">"))
      assert(body.contains("org.typelevel:cats-core 2.13.0"))
      assert(hdrs.get[headers.`Content-Type`].exists(_.mediaType == MediaType.application.`atom+xml`))
    }
  }

  test("MCP initialize, notification and tools/list") {
    for
      (s1, init) <- mcp("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"1"}}}""")
      (s2, _) <- mcp("""{"jsonrpc":"2.0","method":"notifications/initialized"}""")
      (s3, list) <- mcp("""{"jsonrpc":"2.0","id":2,"method":"tools/list"}""")
    yield
      assertEquals(s1, Status.Ok)
      assert(init.contains("\"protocolVersion\":\"2025-06-18\""), init)
      assertEquals(s2, Status.Accepted)
      assertEquals(s3, Status.Ok)
      val names = parse(list).toOption.get.hcursor
        .downField("result").downField("tools").as[List[Json]].toOption.get
        .flatMap(_.hcursor.get[String]("name").toOption)
      assertEquals(names, List("search_packages", "get_package", "recent_releases", "install_snippet"))
  }

  test("MCP tools/call runs each tool") {
    def call(name: String, args: String) =
      mcp(s"""{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"$name","arguments":$args}}""")
        .map((_, body) => body)
    for
      search <- call("search_packages", """{"query":"cats"}""")
      pkg <- call("get_package", """{"group_id":"org.typelevel","name":"cats-core"}""")
      missing <- call("get_package", """{"group_id":"x","name":"y"}""")
      snippet <- call("install_snippet", """{"group_id":"org.typelevel","name":"cats-core","build_tool":"scala-cli"}""")
      for212 <- call("install_snippet", """{"group_id":"org.typelevel","name":"cats-core","build_tool":"sbt","scala_version":"2.12"}""")
      for211 <- call("install_snippet", """{"group_id":"org.typelevel","name":"cats-core","scala_version":"2.11"}""")
      recent <- call("recent_releases", """{"since":"2025-01-01"}""")
      unknown <- call("nope", "{}")
      failed <- call("search_packages", """{"query":"boom"}""")
    yield
      assert(search.contains("cats-core"), search)
      assert(pkg.contains("org.typelevel:cats-core_3"), pkg) // live lookup used the JVM Scala 3 artifact
      assert(missing.contains("\"isError\":true"), missing)
      assert(snippet.contains("using dep org.typelevel::cats-core"), snippet)
      assert(!snippet.contains("libraryDependencies"), snippet)
      // the last release for 2.12, not the latest
      assert(for212.contains("2.10.0") && !for212.contains("2.13.0"), for212)
      assert(!for212.contains("Mill"), for212)
      assert(for211.contains("\"isError\":true") && for211.contains("not published for Scala 2.11"), for211)
      assert(recent.contains("2.13.0"), recent)
      assert(unknown.contains("-32602"), unknown)
      // a failure inside a tool is a tool error, whatever its exception type
      assert(failed.contains("\"isError\":true"), failed)
  }

  test("MCP rejects invalid JSON and unknown methods") {
    for
      (s1, _) <- mcp("not json")
      (_, body) <- mcp("""{"jsonrpc":"2.0","id":9,"method":"resources/list"}""")
    yield
      assertEquals(s1, Status.BadRequest)
      assert(body.contains("-32601"))
  }

  test("results mark their total and each result's key for search events") {
    get(uri"/fragments/search?q=cats").map { (_, body, _) =>
      assert(body.contains("data-total=\"1\""), body)
      assert(body.contains("data-key=\"github.com/typelevel/cats\""), body)
    }
  }

  test("search events are checked, normalised and recorded") {
    val id = "0f6c2a9e-1b7d-4c55-9e21-3a8f5d0c7b64"
    val session = "6b1e4f0a-2c3d-4e5f-8a9b-0c1d2e3f4a5b"
    val search =
      s"""{"kind":"search","searchId":"$id","session":"$session",
         |"query":{"q":"  json  ","platforms":["jvm","bogus"],"scalaVersions":[],"sort":"relevance","page":1},
         |"total":42,"results":["github.com/circe/circe"],"result":null,"position":null,"detail":null}""".stripMargin
    val copy =
      s"""{"kind":"copy","searchId":"$id","session":"$session","query":null,"total":null,
         |"results":[],"result":"github.com/circe/circe","position":3,"detail":"sbt"}""".stripMargin
    val invalid = List(
      search.replace("\"search\"", "\"hover\""), // unknown kind
      search.replace(id, "short"), // not an id
      copy.replace("\"position\":3", "\"position\":null"), // an outcome needs its rank
      copy.replace("\"detail\":\"sbt\"", s"\"detail\":\"${"x" * 5000}\""), // too big
      "not json"
    )
    for
      (log, recorded) <- recording
      a <- app(log = log)
      (s1, _) <- post(a, uri"/events", search)
      (s2, _) <- post(a, uri"/events", copy)
      rejected <- invalid.traverse(b => post(a, uri"/events", b).map(_._1))
      events <- recorded
    yield
      assertEquals((s1, s2), (Status.NoContent, Status.NoContent))
      assertEquals(rejected.distinct, List(Status.BadRequest))
      assertEquals(events.map((source, e) => (source, e.kind)), List(EventSource.Web -> EventKind.Search, EventSource.Web -> EventKind.Copy))
      // the query is stored the way a search request would read it
      assertEquals(events.head._2.query.map(q => (q.q, q.platforms)), Some(("json", List("jvm"))))
  }

  test("MCP searches, package lookups and install snippets are recorded") {
    def call(a: HttpApp[IO], name: String, args: String) =
      post(a, uri"/mcp", s"""{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"$name","arguments":$args}}""")
    val pkg = """"group_id":"org.typelevel","name":"cats-core""""
    for
      (log, recorded) <- recording
      a <- app(log = log)
      _ <- call(a, "search_packages", """{"query":"cats"}""")
      _ <- call(a, "get_package", s"{$pkg}")
      _ <- call(a, "install_snippet", s"""{$pkg,"build_tool":"sbt"}""")
      _ <- call(a, "recent_releases", "{}")
      events <- recorded
    yield
      assert(events.forall(_._1 == EventSource.Mcp))
      val List(search, open, copy) = events.map(_._2): @unchecked
      assertEquals(search.kind, EventKind.Search)
      assertEquals(search.query.map(_.q), Some("cats"))
      assertEquals((search.total, search.results), (Some(1), List("github.com/typelevel/cats")))
      assertEquals((open.kind, open.result), (EventKind.Open, Some("org.typelevel:cats-core")))
      assertEquals((copy.kind, copy.detail), (EventKind.Copy, Some("sbt")))
  }

  private def report(a: HttpApp[IO], fields: (String, String)*): IO[(Status, String, Headers)] =
    a.run(Request[IO](Method.POST, uri"/status/report").withEntity(UrlForm(fields*)))
      .flatMap(r => r.as[String].map((r.status, _, r.headers)))

  /** A submit that keeps what it is given. */
  private def submitted: IO[(PackageReport => IO[ReportResult], IO[List[PackageReport]])] =
    Ref[IO].of(Vector.empty[PackageReport]).map(ref =>
      (
        r => ref.update(_ :+ r).as(ReportResult(ReportOutcome.Added, "Added newlib_3 1.0.0.")),
        ref.get.map(_.toList)
      )
    )

  test("the status page shows the index, the jobs and the report form") {
    get(uri"/status").map { (status, body, headers) =>
      assertEquals(status, Status.Ok)
      assert(body.contains("Index update 940"), body)
      assert(body.contains("newer update waiting"), body)
      assert(body.contains(">update 941</a>, published on"), body)
      assert(body.contains("Package details"), body)
      assert(body.contains("3 artifacts waiting"), body)
      assert(body.contains("""action="/status/report""""), body)
      // jobs that never ran here are left out
      assert(!body.contains("Project summaries"), body)
      assertEquals(headers.get(CIString("Cache-Control")).map(_.head.value), Some("public, max-age=60"))
    }
  }

  test("the status is served as JSON") {
    get(uri"/api/v1/status").map { (status, body, _) =>
      assertEquals(status, Status.Ok)
      val json = parse(body).getOrElse(fail(body))
      assertEquals(json.hcursor.downField("index").get[Int]("lastChunk"), Right(940))
      assertEquals(json.hcursor.downField("reports").get[Long]("pending"), Right(1L))
    }
  }

  test("a report names an artifact when it has a Scala suffix, else a package") {
    for
      (submit, seen) <- submitted
      a <- app(submit = submit)
      (status, body, headers) <- report(a, "group" -> " io.newlib ", "artifact" -> "newlib_3", "reason" -> "missing", "note" -> "")
      _ <- report(a, "group" -> "io.newlib", "artifact" -> "newlib", "reason" -> "version", "note" -> "stuck at 0.9")
      reports <- seen
    yield
      assertEquals(status, Status.Ok)
      assert(body.contains("Added newlib_3 1.0.0."), body)
      assert(body.contains("/p/io.newlib/newlib"), body)
      assertEquals(headers.get(CIString("Cache-Control")).map(_.head.value), Some("no-store"))
      assertEquals(
        reports,
        List(
          PackageReport("io.newlib", Some("newlib_3"), "newlib", ReportReason.Missing, None),
          PackageReport("io.newlib", None, "newlib", ReportReason.Version, Some("stuck at 0.9"))
        )
      )
  }

  test("a malformed report gets the form back with the error, and is not checked") {
    for
      (submit, seen) <- submitted
      a <- app(submit = submit)
      (badGroup, body, _) <- report(a, "group" -> "../etc", "artifact" -> "newlib_3", "reason" -> "missing")
      (badArtifact, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "a..b_3", "reason" -> "missing")
      (badReason, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "newlib_3", "reason" -> "spam")
      (longNote, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "newlib_3", "reason" -> "other", "note" -> "x" * 501)
      reports <- seen
    yield
      assertEquals(List(badGroup, badArtifact, badReason, longNote), List.fill(4)(Status.BadRequest))
      assert(body.contains("Enter a groupId"), body)
      // the visitor's input is shown again
      assert(body.contains("newlib_3"), body)
      assertEquals(reports, Nil)
  }

  test("a report with the hidden field filled in is answered but not checked") {
    for
      (submit, seen) <- submitted
      a <- app(submit = submit)
      (status, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "newlib_3", "reason" -> "missing", "website" -> "http://spam")
      reports <- seen
    yield
      assertEquals(status, Status.Ok)
      assertEquals(reports, Nil)
  }

  test("reports over the limit get 429") {
    app(limit = 1).flatMap { a =>
      for
        (r1, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "newlib_3", "reason" -> "missing")
        (r2, _, _) <- report(a, "group" -> "io.newlib", "artifact" -> "newlib_3", "reason" -> "missing")
      yield
        assertEquals(r1, Status.Ok)
        assertEquals(r2, Status.TooManyRequests)
    }
  }
}
