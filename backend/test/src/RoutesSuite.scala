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
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.SearchGate
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
    def newest(limit: Int): IO[List[PackageSummary]] = IO.pure(all.take(limit))
    def mostStarred(limit: Int): IO[List[PackageSummary]] = IO.pure(List(cats))
    def newProjects(limit: Int): IO[List[NewProject]] =
      IO.pure(List(NewProject(plugin, "2026-09-01T00:00:00Z")))
    def releases(since: OffsetDateTime, limit: Int): IO[List[Release]] =
      IO.pure(
        List(Release("org.typelevel", "cats-core", "2.13.0", "2025-01-20T10:00:00Z", cats.targets, cats.project))
      )

  private val live: ArtifactVersion => IO[LiveInfo] =
    _ => IO.pure(LiveInfo(None, Some(Nil), None))

  private def app(limit: Int = 100): IO[HttpApp[IO]] =
    RateLimit(limit, 1.minute).map(rl => Main.httpApp(baseUrl, None, repo, live, rl))

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
      recent <- call("recent_releases", """{"since":"2025-01-01"}""")
      unknown <- call("nope", "{}")
      failed <- call("search_packages", """{"query":"boom"}""")
    yield
      assert(search.contains("cats-core"), search)
      assert(pkg.contains("org.typelevel:cats-core_3"), pkg) // live lookup used the JVM Scala 3 artifact
      assert(missing.contains("\"isError\":true"), missing)
      assert(snippet.contains("using dep org.typelevel::cats-core"), snippet)
      assert(!snippet.contains("libraryDependencies"), snippet)
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
}
