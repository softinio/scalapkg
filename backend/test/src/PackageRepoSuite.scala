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

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import com.softinio.scalapkg.db.Database
import com.softinio.scalapkg.db.EventSource
import com.softinio.scalapkg.db.IngestRepo
import com.softinio.scalapkg.db.NewerVersion
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.PendingArtifact
import com.softinio.scalapkg.db.SearchLog
import com.softinio.scalapkg.db.SeenArtifact
import com.softinio.scalapkg.depsdev.DepsDev
import com.softinio.scalapkg.depsdev.VersionInfo
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.github.RepoText
import com.softinio.scalapkg.ingest.DepsJob
import com.softinio.scalapkg.ingest.Enrich
import com.softinio.scalapkg.ingest.FirstReleases
import com.softinio.scalapkg.ingest.Throttle
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import skunk.Session
import skunk.codec.all.*
import skunk.implicits.*

import java.time.Instant
import java.time.OffsetDateTime
import scala.concurrent.duration.*

/** The SQL behind search ranking and the home page lists, against a real
  * Postgres with pgvector. Each run recreates the `scalapkg_test` database (never
  * the dev database) and loads a small fixture in which each project stands
  * for a case the ranking has to get right. Runs only when
  * `SCALAPKG_TEST_DB_PORT` is set:
  *
  * {{{
  * SCALAPKG_TEST_DB_PORT=5432 ./mill backend.test.testOnly com.softinio.scalapkg.PackageRepoSuite
  * }}}
  */
class PackageRepoSuite extends CatsEffectSuite {

  private given Logger[IO] = NoOpLogger[IO]

  private val port = sys.env.get("SCALAPKG_TEST_DB_PORT").flatMap(_.toIntOption)
  private val testDb = "scalapkg_test"

  private def config(database: String) =
    DbConfig(
      "localhost",
      port.getOrElse(5432),
      sys.env.getOrElse("SCALAPKG_TEST_DB_USER", "scalapkg"),
      sys.env.get("SCALAPKG_TEST_DB_PASSWORD"),
      database,
      None
    )

  private final case class Repos(
      sessions: Resource[IO, Session[IO]],
      search: PackageRepo,
      ingest: IngestRepo
  )

  private val recreate: IO[Unit] =
    Database.pool(config("postgres")).use(_.use { s =>
      s.execute(sql"DROP DATABASE IF EXISTS #$testDb WITH (FORCE)".command) *>
        s.execute(sql"CREATE DATABASE #$testDb".command).void
    })

  private val repos: Resource[IO, Option[Repos]] =
    port.traverse { _ =>
      for
        _ <- Resource.eval(recreate)
        sessions <- Database.pool(config(testDb))
        _ <- Resource.eval(Database.migrate(sessions))
        _ <- Resource.eval(
          sessions.use(s => fixture.traverse_(stmt => s.execute(sql"#$stmt".command)))
        )
        ingest = IngestRepo(sessions)
        _ <- Resource.eval(ingest.rebuildSearchDocs)
      yield Repos(sessions, PackageRepo.skunk(sessions), ingest)
    }

  private val db = ResourceSuiteLocalFixture("db", repos)
  override def munitFixtures = List(db)

  private def withDb(body: Repos => IO[Unit]): IO[Unit] =
    IO(assume(port.isDefined, "set SCALAPKG_TEST_DB_PORT to run the database tests")) >>
      body(db().get)

  private def keys(r: SearchResponse): List[String] =
    r.results.map(_.key.stripPrefix("github.com/"))

  private def search(repos: Repos, q: String, sort: Sort = Sort.Relevance) =
    repos.search.search(SearchQuery(q, Nil, Nil, sort, 1))

  test("a query nothing is about is flagged as related results") {
    withDb { r =>
      for
        redis <- search(r, "redis")
        json <- search(r, "json")
      yield
        // only cachey's README mentions redis
        assertEquals(keys(redis), List("acme/cachey"))
        assert(redis.related)
        assert(!json.related)
    }
  }

  test("sorting by stars leaves out projects that match only through a module name") {
    withDb { r =>
      search(r, "json", Sort.Stars).map { res =>
        assert(keys(res).contains("circe/circe"), keys(res))
        // atlas is a time series database with an `atlas-json` module
        assert(!keys(res).contains("netflix/atlas"), keys(res))
      }
    }
  }

  test("a typo-only match ranks below a real word match, however popular") {
    withDb { r =>
      search(r, "scalafmt").map { res =>
        assertEquals(keys(res).headOption, Some("scalameta/sbt-scalafmt"), keys(res))
      }
    }
  }

  test("a project publishing for 2.13 and 3 ranks above a 2.13-only twin") {
    withDb { r =>
      // same text, stars and dependents; alpha would win the tie on its key
      search(r, "fastcodec").map { res =>
        assertEquals(keys(res).take(2), List("beta/fastcodec", "alpha/fastcodec"))
      }
    }
  }

  test("an established project named what was typed comes first") {
    withDb { r =>
      val unboosted = PackageRepo.skunk(
        r.sessions,
        ranking = PackageRepo.Ranking(exactNameStars = Int.MaxValue, exactNameDependents = Int.MaxValue)
      )
      def plain(q: String) = unboosted.search(SearchQuery(q, Nil, Nil, Sort.Relevance, 1))
      for
        zio <- search(r, "zio")
        zioPlain <- plain("zio")
        json <- search(r, "json")
        jsonPlain <- plain("json")
      yield
        assertEquals(keys(zio).headOption, Some("zio/zio"), keys(zio))
        // on relevance alone a project with zio all over its text wins
        assertNotEquals(keys(zioPlain).headOption, Some("zio/zio"), keys(zioPlain))
        // a tiny repo called json is not established, so nothing changes
        assertEquals(keys(json), keys(jsonPlain))
    }
  }

  test("a result opens the project's main module among those matching the query") {
    withDb { r =>
      search(r, "circe").map { res =>
        val circe = res.results.find(_.key == "github.com/circe/circe")
        // both modules match; circe-testing was released last
        assertEquals(circe.map(_.primary.name), Some("circe-core"), keys(res))
      }
    }
  }

  test("a project that moved to a new groupId is shown by its new coordinates") {
    withDb { r =>
      for
        starred <- r.search.mostStarred(50)
        newest <- r.search.newest(50)
        found <- search(r, "playframework")
      yield
        // the old group has more targets and dependents
        assertEquals(starred.find(_.name == "play").map(_.groupId), Some("org.playframework"))
        assertEquals(newest.find(_.name == "play").map(_.groupId), Some("org.playframework"))
        assertEquals(found.results.headOption.map(_.primary.groupId), Some("org.playframework"))
    }
  }

  test("a renamed repository's packages join the project it moved to") {
    withDb { r =>
      def skunks(res: SearchResponse) = keys(res).filter(_.endsWith("/skunk")).sorted
      for
        throttle <- Throttle(0.millis)
        depsDev = DepsDev(Client.fromHttpApp(HttpApp.notFound[IO]), Uri.unsafeFromString("http://deps.test"), throttle)
        before <- search(r, "skunk")
        _ <- r.ingest.saveGithub(
          List(RepoText("github.com/tpolecat/skunk", Nil, None, movedTo = Some("github.com/typelevel/skunk")))
        )
        after <- search(r, "skunk")
        // regrouping from the artifacts, which still name the old repository,
        // keeps the move
        _ <- Enrich(depsDev, r.ingest).rebuild(List(PackageRef("org.tpolecat", "skunk-macros")))
        project <- r.sessions.use(_.unique(sql"SELECT project_id FROM packages WHERE name = 'skunk-macros'".query(text)))
      yield
        assertEquals(skunks(before), List("tpolecat/skunk", "typelevel/skunk"))
        assertEquals(skunks(after), List("typelevel/skunk"))
        assertEquals(project, "github.com/typelevel/skunk")
    }
  }

  test("old names that moved to an unindexed repository become one result") {
    withDb { r =>
      def logkits(res: SearchResponse) = keys(res).filter(_.endsWith("/logkit")).sorted
      for
        before <- search(r, "logkit")
        _ <- r.ingest.saveGithub(
          List("github.com/old1/logkit", "github.com/old2/logkit")
            .map(RepoText(_, Nil, None, movedTo = Some("github.com/new/logkit")))
        )
        after <- search(r, "logkit")
      yield
        assertEquals(logkits(before), List("old1/logkit", "old2/logkit"))
        assertEquals(logkits(after), List("old1/logkit"))
    }
  }

  test("new releases leave out packages with no source repository") {
    withDb { r =>
      r.search.newest(50).map { pkgs =>
        // solo-lib is the newest release in the fixture, but deps.dev links
        // it to no repository, so there is nothing to describe it with
        assert(!pkgs.exists(_.name == "solo-lib"), pkgs.map(_.name))
        assert(pkgs.exists(_.groupId == "io.circe"), pkgs.map(_.name))
      }
    }
  }

  test("most starred shows each current project's main module") {
    withDb { r =>
      r.search.mostStarred(10).map { pkgs =>
        val names = pkgs.map(_.name)
        // neo4j's only Scala module is a 2.11 leftover
        assert(!names.contains("neo4j-cypher-compiler-2.2"), names)
        // depended on, not the module released last
        assert(names.contains("circe-core"), names)
        assert(!names.contains("circe-testing"), names)
        // not the benchmark module, though its name is shorter
        assert(names.contains("atlas-core"), names)
        // sbt plugins are 2.12 because sbt 1 is
        assert(names.contains("sbt-scalajs"), names)
      }
    }
  }

  test("new projects are those first released recently, newest first") {
    withDb { r =>
      r.search.newProjects(10).map { ps =>
        // sbt-testng only moved to new coordinates (repository from 2013),
        // undated has a module without a known first release, oldish is
        // outside the window
        assertEquals(ps.map(_.pkg.name), List("solo-lib", "fresh-core"))
      }
    }
  }

  test("an artifact's first release only ever moves earlier") {
    withDb { r =>
      val key = ArtifactKey("io.temp", "kept_3")
      val t1 = Instant.parse("2020-01-01T00:00:00Z")
      val t2 = Instant.parse("2021-01-01T00:00:00Z")
      for
        _ <- r.ingest.upsertSeen(List(SeenArtifact("io.temp", "kept_3", "kept", Target(Platform.Jvm, "3"), "1.0")))
        _ <- r.ingest.noteFirstReleases(List(key -> t2))
        _ <- r.ingest.noteFirstReleases(List(key -> t1))
        _ <- r.ingest.noteFirstReleases(List(key -> t2))
        first <- firstRelease(r, key)
      yield assertEquals(first.map(_.toInstant), Some(t1))
    }
  }

  test("first releases come from deps.dev; a failed lookup is retried later") {
    withDb { r =>
      val dated = ArtifactKey("io.temp", "dated_3")
      val failing = ArtifactKey("io.temp", "failing_3")
      val versions =
        """{"versions":[
          |  {"versionKey":{"version":"1.1"},"publishedAt":"2025-01-02T00:00:00Z"},
          |  {"versionKey":{"version":"1.0"},"publishedAt":"2024-01-02T00:00:00Z"}]}""".stripMargin
      val depsDevApp = HttpApp[IO] { req =>
        IO.pure(
          if req.uri.path.renderString.endsWith("io.temp%3Adated_3") ||
            req.uri.path.renderString.endsWith("io.temp:dated_3")
          then Response[IO](Status.Ok).withEntity(versions)
          else Response[IO](Status.InternalServerError)
        )
      }
      for
        throttle <- Throttle(0.millis)
        depsDev = DepsDev(Client.fromHttpApp(depsDevApp), Uri.unsafeFromString("http://deps.test"), throttle)
        _ <- r.ingest.upsertSeen(
          List(
            SeenArtifact("io.temp", "dated_3", "dated", Target(Platform.Jvm, "3"), "1.1"),
            SeenArtifact("io.temp", "failing_3", "failing", Target(Platform.Jvm, "3"), "1.0")
          )
        )
        _ <- FirstReleases(depsDev, r.ingest).run
        datedAt <- firstRelease(r, dated)
        failingAt <- firstRelease(r, failing)
        undated <- r.ingest.undatedArtifacts(100)
      yield
        assertEquals(datedAt.map(_.toInstant), Some(Instant.parse("2024-01-02T00:00:00Z")))
        assertEquals(failingAt, None)
        assert(undated.contains(failing), undated)
        assert(!undated.contains(dated), undated)
    }
  }

  test("the freshness check polls the artifact the dependency reader picks") {
    withDb { r =>
      for
        _ <- r.ingest.upsertSeen(
          List(
            SeenArtifact("com.alpha", "fastcodec_2.12", "fastcodec", Target(Platform.Jvm, "2.12"), "1.0.0"),
            SeenArtifact("com.alpha", "fastcodec_2.13", "fastcodec", Target(Platform.Jvm, "2.13"), "1.0.0")
          )
        )
        pkgs <- r.ingest.activePackages(1000)
      yield assertEquals(
        pkgs.filter(_.groupId == "com.alpha").map(DepsJob.representative(_).artifactId),
        List("fastcodec_2.13")
      )
    }
  }

  test("a freshness result never replaces a newer version recorded meanwhile") {
    withDb { r =>
      val jvm3 = Target(Platform.Jvm, "3")
      for
        _ <- r.ingest.upsertSeen(
          List(SeenArtifact("io.race", "raced_3", "raced", jvm3, "1.0"), SeenArtifact("io.race", "calm_3", "calm", jvm3, "1.0"))
        )
        _ <- r.ingest.upsertSeen(List(SeenArtifact("io.race", "raced_3", "raced", jvm3, "1.2")))
        _ <- r.ingest.advance(
          List(NewerVersion("io.race", "raced_3", "1.0", "1.1"), NewerVersion("io.race", "calm_3", "1.0", "1.1"))
        )
        latest <- r.ingest.latestVersions(List(ArtifactKey("io.race", "raced_3"), ArtifactKey("io.race", "calm_3")))
      yield assertEquals(latest, Map(ArtifactKey("io.race", "raced_3") -> "1.2", ArtifactKey("io.race", "calm_3") -> "1.1"))
    }
  }

  test("the web pool's statement timeout cancels a slow statement") {
    withDb { _ =>
      Database
        .pool(config(testDb), statementTimeout = Some(50.millis))
        .use(_.use(_.execute(sql"SELECT 1 FROM pg_sleep(1)".query(int4))).attempt)
        .map {
          case Left(e: skunk.exception.PostgresErrorException) =>
            // 57014 is what Main.httpApp turns into a 503
            assertEquals(e.code, "57014")
          case other => fail(s"expected a cancelled statement, got $other")
        }
    }
  }

  test("search events are stored with their filters and pruned after 90 days") {
    withDb { db =>
      val query = SearchQuery("json", List("jvm"), List("3"), Sort.Stars, 2)
      val now = OffsetDateTime.now()
      val events = List(
        SearchLog.Entry(now.minusDays(100), EventSource.Web, SearchEvent(EventKind.Search, Some("search-old"), Some("s"), query = Some(query), total = Some(7), results = List("a", "b"))),
        SearchLog.Entry(now, EventSource.Mcp, SearchEvent(EventKind.Copy, None, None, result = Some("org.typelevel:cats-core"), detail = Some("sbt")))
      )
      for
        _ <- db.sessions.use(_.execute(sql"TRUNCATE search_events".command))
        _ <- SearchLog.writer(db.sessions)(events)
        stored <- db.sessions.use(
          _.execute(
            sql"""SELECT source, kind, query, array_to_string(filters, ' '),
                         array_to_string(results, ' '), detail
                  FROM search_events ORDER BY kind DESC"""
              .query(text *: text *: text.opt *: text *: text *: text.opt)
          )
        )
        pruned <- SearchLog.prune(db.sessions, 90.days)
        left <- db.sessions.use(_.unique(sql"SELECT count(*) FROM search_events".query(int8)))
      yield
        assertEquals(
          stored,
          List(
            ("web", "search", Some("json"), "platform=jvm scala=3 sort=stars page=2", "a b", None),
            ("mcp", "copy", None, "", "", Some("sbt"))
          )
        )
        assertEquals(pruned, "deleted 1 old search events")
        assertEquals(left, 1L)
    }
  }

  test("deps.dev live lookups are cached only when every part succeeded") {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    val failFirst = new java.util.concurrent.atomic.AtomicBoolean(true)
    val app = HttpApp[IO] { req =>
      IO {
        calls.incrementAndGet()
        // the first version lookup (which carries the advisories) fails,
        // everything after succeeds
        if !req.uri.path.renderString.contains("dependencies") && failFirst.getAndSet(false) then
          Response[IO](Status.InternalServerError)
        else Response[IO](Status.NotFound)
      }
    }
    val a = ArtifactVersion("io.temp", "cached_3", "1.0")
    for
      throttle <- Throttle(0.millis)
      live <- DepsDev(Client.fromHttpApp(app), Uri.unsafeFromString("http://deps.test"), throttle).cachedLive(1.hour)
      _ <- live(a) // advisories failed: not cached
      afterFailure = calls.get
      _ <- live(a) // complete: cached
      afterSuccess = calls.get
      _ <- live(a) // served from the cache
    yield
      assert(afterSuccess > afterFailure, "a failed lookup must be asked again")
      assertEquals(calls.get, afterSuccess)
  }

  test("the weekly refresh skips projects no artifact links any more") {
    withDb { r =>
      r.ingest.projectsToRefresh(100).map { ids =>
        // the fixture links circe from an artifact, neo4j from none
        assert(ids.contains("github.com/circe/circe"), ids)
        assert(!ids.contains("github.com/neo4j/neo4j"), ids)
      }
    }
  }

  test("a package can link a project deps.dev just reported") {
    withDb { r =>
      val repo = "github.com/brand/new"
      val info = VersionInfo(None, Nil, false, Some(repo))
      for
        throttle <- Throttle(0.millis)
        depsDev = DepsDev(Client.fromHttpApp(HttpApp.notFound[IO]), Uri.unsafeFromString("http://deps.test"), throttle)
        _ <- r.ingest.upsertSeen(List(SeenArtifact("io.brand", "brand_3", "brand", Target(Platform.Jvm, "3"), "1.0")))
        _ <- r.ingest.saveDetails(List(PendingArtifact("io.brand", "brand_3", "brand", "1.0") -> info))
        _ <- Enrich(depsDev, r.ingest).rebuild(List(PackageRef("io.brand", "brand")))
        linked <- r.sessions.use(
          _.unique(sql"SELECT project_id, search IS NOT NULL FROM packages WHERE group_id = 'io.brand'".query(text.opt *: bool))
        )
      yield assertEquals(linked, (Some(repo), true))
    }
  }

  test("an interrupted deps.dev save leaves no artifact pointing at a missing project") {
    withDb { r =>
      val ok = VersionInfo(None, Nil, false, Some("github.com/brand/half"))
      val bad = VersionInfo(None, List("\u0000"), false, Some("github.com/brand/other"))
      for
        _ <- r.ingest.upsertSeen(List(SeenArtifact("io.brand", "half_3", "half", Target(Platform.Jvm, "3"), "1.0")))
        saved <- r.ingest
          .saveDetails(List(PendingArtifact("io.brand", "half_3", "half", "1.0") -> ok, PendingArtifact("io.brand", "x_3", "x", "1.0") -> bad))
          .attempt
        dangling <- r.sessions.use(
          _.unique(sql"SELECT count(*) FROM artifacts a LEFT JOIN projects pr ON pr.id = a.project_id WHERE a.project_id IS NOT NULL AND pr.id IS NULL".query(int8))
        )
      yield
        assert(saved.isLeft)
        assertEquals(dangling, 0L)
    }
  }

  private def firstRelease(r: Repos, key: ArtifactKey): IO[Option[OffsetDateTime]] =
    r.sessions.use(
      _.unique(
        sql"SELECT first_release_at FROM artifacts WHERE group_id = $text AND artifact_id = $text"
          .query(timestamptz.opt)
      )((key.groupId, key.artifactId))
    )

  // each project stands for one case the ranking has to get right
  private val fixture: List[String] = List(
    """INSERT INTO projects (id, description, stars, readme, repo_created_at, summary, summary_keywords) VALUES
      ('github.com/circe/circe', 'Yet another JSON library for Scala', 2500, NULL, now() - interval '12 years',
       'JSON encoding and decoding with automatic codec derivation.', '{json,codecs}'),
      ('github.com/netflix/atlas', 'In-memory dimensional time series database.', 3500, NULL, now() - interval '10 years', NULL, '{}'),
      ('github.com/neo4j/neo4j', 'Graph database', 17000, NULL, now() - interval '15 years', NULL, '{}'),
      ('github.com/acme/cachey', 'In-memory caching', 50, 'Cachey can also keep entries in redis.', now() - interval '3 years', NULL, '{}'),
      ('github.com/alpha/fastcodec', 'Binary codec toolkit', 100, NULL, now() - interval '3 years', NULL, '{}'),
      ('github.com/beta/fastcodec', 'Binary codec toolkit', 100, NULL, now() - interval '3 years', NULL, '{}'),
      ('github.com/scalameta/sbt-scalafmt', 'sbt plugin for Scalafmt', 400, NULL, now() - interval '8 years', NULL, '{}'),
      ('github.com/scala-js/scala-js', 'Scala.js, the Scala to JavaScript compiler', 4700, NULL, now() - interval '12 years', NULL, '{}'),
      ('github.com/new/fresh', 'A brand new effect library', 5, NULL, now() - interval '20 days', NULL, '{}'),
      ('github.com/sbt/sbt-testng', 'TestNG support for sbt', 30, NULL, now() - interval '12 years', NULL, '{}'),
      ('github.com/new/undated', 'Another new library', 5, NULL, now() - interval '30 days', NULL, '{}'),
      ('github.com/new/oldish', 'An older library', 5, NULL, now() - interval '1 year', NULL, '{}'),
      ('github.com/zio/zio', 'Type-safe, composable asynchronous and concurrent programming for Scala', 200, NULL, now() - interval '9 years', NULL, '{}'),
      ('github.com/zio/zio-kafka', 'A Kafka client for ZIO and ZIO Streams', 400, NULL, now() - interval '6 years',
       'Kafka consumer and producer for ZIO applications, built on ZIO Streams.', '{zio,kafka}'),
      ('github.com/tiny/json', 'json', 3, NULL, now() - interval '2 years', NULL, '{}'),
      ('github.com/playframework/playframework', 'Play Framework', 12600, NULL, now() - interval '15 years', NULL, '{}'),
      ('github.com/tpolecat/skunk', 'A data access library for Scala and Postgres', 1500, NULL, now() - interval '7 years', NULL, '{}'),
      ('github.com/typelevel/skunk', 'A data access library for Scala and Postgres', 1660, NULL, now() - interval '7 years', NULL, '{}'),
      ('github.com/old1/logkit', 'Logging toolkit', 20, NULL, now() - interval '9 years', NULL, '{}'),
      ('github.com/old2/logkit', 'Logging toolkit', 20, NULL, now() - interval '9 years', NULL, '{}')""",
    """INSERT INTO packages (group_id, name, project_id, latest_version, latest_release_at, targets, platforms, scala_versions) VALUES
      ('io.circe', 'circe-core', 'github.com/circe/circe', '0.14.0', now() - interval '5 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('io.circe', 'circe-testing', 'github.com/circe/circe', '0.14.0', now() - interval '1 day', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('com.netflix', 'atlas-core', 'github.com/netflix/atlas', '1.8.0', now() - interval '9 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('com.netflix', 'atlas-jmh', 'github.com/netflix/atlas', '1.8.0', now() - interval '9 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('com.netflix', 'atlas-json', 'github.com/netflix/atlas', '1.8.0', now() - interval '9 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('org.neo4j', 'neo4j-cypher-compiler-2.2', 'github.com/neo4j/neo4j', '2.2.10', now() - interval '10 years', '{jvm:2.11}', '{jvm}', '{2.11}'),
      ('com.acme', 'cachey', 'github.com/acme/cachey', '1.0.0', now() - interval '60 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('com.alpha', 'fastcodec', 'github.com/alpha/fastcodec', '1.0.0', now() - interval '60 days', '{jvm:2.13}', '{jvm}', '{2.13}'),
      ('com.beta', 'fastcodec', 'github.com/beta/fastcodec', '1.0.0', now() - interval '60 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('org.scalameta', 'sbt-scalafmt', 'github.com/scalameta/sbt-scalafmt', '2.5.4', now() - interval '60 days', '{sbt1:2.12}', '{sbt}', '{2.12}'),
      ('org.scala-js', 'sbt-scalajs', 'github.com/scala-js/scala-js', '1.20.1', now() - interval '60 days', '{sbt1:2.12}', '{sbt}', '{2.12}'),
      ('io.fresh', 'fresh-core', 'github.com/new/fresh', '0.1.0', now() - interval '10 days', '{jvm:3}', '{jvm}', '{3}'),
      ('com.github.sbt', 'sbt-testng-interface', 'github.com/sbt/sbt-testng', '3.3.0', now() - interval '20 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('io.undated', 'undated-core', 'github.com/new/undated', '0.1.0', now() - interval '5 days', '{jvm:3}', '{jvm}', '{3}'),
      ('io.undated', 'undated-extra', 'github.com/new/undated', '0.1.0', now() - interval '5 days', '{jvm:3}', '{jvm}', '{3}'),
      ('io.oldish', 'oldish-core', 'github.com/new/oldish', '1.0.0', now() - interval '20 days', '{jvm:3}', '{jvm}', '{3}'),
      ('io.solo', 'solo-lib', NULL, '0.1.0', now() - interval '3 days', '{jvm:3}', '{jvm}', '{3}'),
      ('dev.zio', 'zio', 'github.com/zio/zio', '2.1.0', now() - interval '5 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('dev.zio', 'zio-kafka', 'github.com/zio/zio-kafka', '3.0.0', now() - interval '5 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('io.tiny', 'json', 'github.com/tiny/json', '0.1.0', now() - interval '400 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('com.typesafe.play', 'play', 'github.com/playframework/playframework', '2.9.12', now() - interval '11 days',
       '{jvm:2.11,jvm:2.12,jvm:2.13,jvm:3}', '{jvm}', '{2.11,2.12,2.13,3}'),
      ('org.playframework', 'play', 'github.com/playframework/playframework', '3.0.12', now() - interval '10 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('org.tpolecat', 'skunk-macros', 'github.com/tpolecat/skunk', '0.0.7', now() - interval '5 years', '{jvm:2.13}', '{jvm}', '{2.13}'),
      ('org.tpolecat', 'skunk-core', 'github.com/typelevel/skunk', '1.0.0', now() - interval '5 days', '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'),
      ('io.old1', 'logkit-core', 'github.com/old1/logkit', '1.0.0', now() - interval '5 years', '{jvm:2.13}', '{jvm}', '{2.13}'),
      ('io.old2', 'logkit-api', 'github.com/old2/logkit', '2.0.0', now() - interval '2 years', '{jvm:2.13}', '{jvm}', '{2.13}')""",
    """INSERT INTO artifacts (group_id, artifact_id, name, target, latest_version, project_id, first_release_at) VALUES
      ('io.circe', 'circe-core_3', 'circe-core', 'jvm:3', '0.14.0', 'github.com/circe/circe', now() - interval '5 years'),
      ('io.fresh', 'fresh-core_3', 'fresh-core', 'jvm:3', '0.1.0', 'github.com/new/fresh', now() - interval '10 days'),
      ('com.github.sbt', 'sbt-testng-interface_3', 'sbt-testng-interface', 'jvm:3', '3.3.0', 'github.com/sbt/sbt-testng', now() - interval '20 days'),
      ('io.undated', 'undated-core_3', 'undated-core', 'jvm:3', '0.1.0', 'github.com/new/undated', now() - interval '5 days'),
      ('io.undated', 'undated-extra_3', 'undated-extra', 'jvm:3', '0.1.0', 'github.com/new/undated', NULL),
      ('io.oldish', 'oldish-core_3', 'oldish-core', 'jvm:3', '1.0.0', 'github.com/new/oldish', now() - interval '200 days'),
      ('io.solo', 'solo-lib_3', 'solo-lib', 'jvm:3', '0.1.0', NULL, now() - interval '3 days'),
      ('com.typesafe.play', 'play_2.13', 'play', 'jvm:2.13', '2.9.12', 'github.com/playframework/playframework', now() - interval '12 years'),
      ('org.playframework', 'play_3', 'play', 'jvm:3', '3.0.12', 'github.com/playframework/playframework', now() - interval '2 years'),
      ('org.tpolecat', 'skunk-macros_2.13', 'skunk-macros', 'jvm:2.13', '0.0.7', 'github.com/tpolecat/skunk', now() - interval '5 years')""",
    // thirty zio-* projects with zio all over their text and nearly as many
    // stars, which push zio/zio down the keyword list as the hundreds of them
    // do in the full index
    """INSERT INTO projects (id, description, stars, summary, summary_keywords)
       SELECT 'github.com/zio/zio-mod' || i, 'ZIO integration for ZIO: ZIO layers, ZIO streams and ZIO tests for ZIO apps', 150,
              'Connects ZIO applications to a service with ZIO layers and ZIO streams.', '{zio,zio-streams,zio-layers}'
       FROM generate_series(1, 30) i""",
    """INSERT INTO packages (group_id, name, project_id, latest_version, latest_release_at, targets, platforms, scala_versions)
       SELECT 'dev.zio', 'zio-mod' || i, 'github.com/zio/zio-mod' || i, '1.0.0', now() - interval '30 days',
              '{jvm:3,jvm:2.13}', '{jvm}', '{3,2.13}'
       FROM generate_series(1, 30) i""",
    // circe-core is what other projects depend on, and the old play group
    // more than the new one
    """INSERT INTO package_deps (group_id, name, dep_group_id, dep_name) VALUES
      ('com.acme', 'cachey', 'io.circe', 'circe-core'),
      ('com.beta', 'fastcodec', 'io.circe', 'circe-core'),
      ('com.acme', 'cachey', 'com.typesafe.play', 'play'),
      ('com.beta', 'fastcodec', 'com.typesafe.play', 'play')"""
  )
}
