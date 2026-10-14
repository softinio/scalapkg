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
import cats.effect.Ref
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.softinio.scalapkg.MavenVersions
import com.softinio.scalapkg.db.*
import com.softinio.scalapkg.depsdev.DepsDev
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.maven.MavenRepo
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.concurrent.duration.*
import scala.util.Try

/** Packages visitors report as missing or out of date, since the Maven index
  * comes only every two weeks.
  *
  * `submit` settles what it can at once from deps.dev, the same source Enrich
  * and Freshness use: a missing artifact is added and enriched, a newer version
  * recorded. The visitor's request never reaches Maven Central, which limits
  * our address for the index. It runs at most `slots` checks at once and
  * `budget` an hour across all visitors; past either, and for what deps.dev
  * does not know yet, the report is queued. `run` (the reports job) works
  * through the queue, asking deps.dev again and then Central's repository; the
  * Enrich job fills in what it adds.
  */
final class Reports(
    depsDev: DepsDev,
    central: MavenRepo,
    ingest: IngestRepo,
    store: StatusRepo,
    budget: Budget,
    slots: Semaphore[IO],
    jobInterval: FiniteDuration
)(using log: Logger[IO]):
  import Reports.*

  private val enrich = Enrich(depsDev, ingest)

  def submit(r: PackageReport): IO[ReportResult] =
    store.recent(r, recheckAfter).flatMap {
      case Some(done) => IO.pure(done)
      case None       =>
        indexed(r).flatMap {
          case Some(version) if r.reason == ReportReason.Missing =>
            settle(
              r,
              ReportResult(
                ReportOutcome.NoChange,
                s"${r.coordinate} is already indexed, at $version."
              )
            )
          case _ =>
            // already waiting for the job: count it again, ask nobody
            store
              .isPending(r)
              .ifM(
                queue(r, busy = false),
                slots.tryPermit.use {
                  case false => queue(r, busy = true)
                  case true  =>
                    budget.take.ifM(checkNow(r), queue(r, busy = true))
                }
              )
        }
    }

  private def settle(r: PackageReport, result: ReportResult) =
    store.record(r, result).as(result)

  private def queue(r: PackageReport, busy: Boolean): IO[ReportResult] =
    val why =
      if busy then "Too many reports are being checked right now"
      else "deps.dev does not list it yet"
    val msg = s"$why, so it is queued and will be checked against deps.dev " +
      s"and Maven Central within ${hours(jobInterval)}."
    store.enqueue(r, msg).as(ReportResult(ReportOutcome.Pending, msg))

  /** Its latest version, when the artifact (or some artifact of the package) is
    * stored.
    */
  private def indexed(r: PackageReport): IO[Option[String]] =
    r.artifactId match
      case Some(a) =>
        ingest
          .latestVersions(List(ArtifactKey(r.groupId, a)))
          .map(_.values.headOption.map(_._1))
      case None =>
        ingest
          .artifactsOf(List(PackageRef(r.groupId, r.name)))
          .map(_.toNel.map(MavenVersions.best(_)(_.version, _ => None).version))

  private def checkNow(r: PackageReport): IO[ReportResult] =
    fromDepsDev(r).timeout(liveTimeout).attempt.flatMap {
      case Right(found) if found.nonEmpty =>
        record(r, found, enrichNow = true).flatMap(settle(r, _))
      case Right(_) =>
        notScala(r).ifM(
          settle(r, notScalaResult(r)),
          queue(r, busy = false)
        )
      case Left(e) =>
        log.warn(s"report ${r.coordinate}: ${e.getMessage}") *>
          queue(r, busy = false)
    }

  /** The versions deps.dev lists for each artifact the report could mean. */
  private def fromDepsDev(r: PackageReport): IO[List[Found]] =
    candidates(r).traverseFilter(a =>
      depsDev
        .packageVersions(s"${r.groupId}:$a")
        .map(vs => Option.when(vs.nonEmpty)(Found(a, vs)))
    )

  /** A name only report naming a Java artifact that exists: not Scala. */
  private def notScala(r: PackageReport): IO[Boolean] =
    if r.artifactId.isDefined || ArtifactIds.parse(r.name).isDefined then
      IO.pure(false)
    else depsDev.packageVersions(s"${r.groupId}:${r.name}").map(_.nonEmpty)

  private def notScalaResult(r: PackageReport) =
    ReportResult(
      ReportOutcome.NotScala,
      s"${r.coordinate} is on Maven Central without a Scala version suffix " +
        "(such as _3 or _2.13), so it is not a Scala artifact this site lists."
    )

  /** Records the best version found for each artifact the way `IndexSync` does:
    * new artifacts are added, newer versions recorded with Freshness's
    * compare-and-set, both marked for Enrich. With `enrichNow` they are
    * enriched at once, so the package is searchable when the answer is shown.
    */
  private def record(
      r: PackageReport,
      found: List[Found],
      enrichNow: Boolean
  ): IO[ReportResult] =
    val seen = found.flatMap(f =>
      (ArtifactIds.parse(f.artifactId), f.versions.toNel).mapN { (p, vs) =>
        val b = MavenVersions.best(vs)(_.version, at)
        SeenArtifact(
          r.groupId,
          f.artifactId,
          p.name,
          p.target,
          b.version,
          at(b)
        )
      }
    )
    val firsts = found.flatMap(f =>
      f.versions
        .flatMap(at)
        .minOption
        .map(ArtifactKey(r.groupId, f.artifactId) -> _)
    )
    for
      existing <- ingest.latestVersions(seen.map(_.key))
      added = seen.filterNot(s => existing.contains(s.key))
      newer = seen.flatMap(s =>
        existing
          .get(s.key)
          .filter((cur, curAt) =>
            MavenVersions.isBetter(s.version, s.publishedAt, cur, curAt)
          )
          .map((cur, _) =>
            NewerVersion(s.groupId, s.artifactId, cur, s.version, s.publishedAt)
          )
      )
      _ <- ingest.upsertSeen(added)
      _ <- ingest.advance(newer)
      _ <- ingest.noteFirstReleases(firsts)
      moved = seen.filter(s => newer.exists(_.artifactId == s.artifactId))
      changed = added ++ moved
      // recorded either way: what fails here is left to the Enrich job
      _ <- enrich
        .details(
          changed.map(s =>
            PendingArtifact(s.groupId, s.artifactId, s.name, s.version)
          )
        )
        .void
        .handleErrorWith(e =>
          log.warn(s"report ${r.coordinate}: enrich: ${e.getMessage}")
        )
        .whenA(enrichNow && changed.nonEmpty)
    yield
      def list(xs: List[SeenArtifact]) =
        xs.map(s => s"${s.artifactId} ${s.version}").sorted.mkString(", ")
      if added.nonEmpty then
        ReportResult(ReportOutcome.Added, s"Added ${list(added)}.")
      else if newer.nonEmpty then
        ReportResult(
          ReportOutcome.Updated,
          s"Recorded newer versions: ${list(moved)}."
        )
      else
        ReportResult(
          ReportOutcome.NoChange,
          s"Already indexed and current: ${list(seen)}."
        )

  /** Checks up to `limit` queued reports: deps.dev, then Central. A report
    * whose lookups failed stays queued for the next run.
    */
  def run(limit: Int): IO[String] =
    store.pending(limit).flatMap { reports =>
      reports
        .traverse(s =>
          resolve(s.report).attempt.flatMap {
            case Right(result) => store.resolve(s.id, result).as(Some(result))
            case Left(e)       =>
              log
                .warn(s"report ${s.report.coordinate}: ${e.getMessage}")
                .as(None)
          }
        )
        .map { results =>
          val counts = results.flatten
            .groupMapReduce(_.outcome.param)(_ => 1)(_ + _)
            .toList
            .sorted
            .map((k, n) => s"$n $k")
          s"checked ${reports.size} reports" +
            (if counts.isEmpty then "" else counts.mkString(": ", ", ", ""))
        }
    }

  private def resolve(r: PackageReport): IO[ReportResult] =
    fromDepsDev(r).flatMap {
      case Nil   => fromCentral(r)
      case found => record(r, found, enrichNow = false)
    }

  private def fromCentral(r: PackageReport): IO[ReportResult] =
    val found: IO[List[Found]] = r.artifactId match
      case Some(a) =>
        central.versions(r.groupId, a).map(_.toList.map(Found.plain(a, _)))
      case None =>
        central
          .artifactIds(r.groupId)
          .flatMap(ids =>
            ids.orEmpty
              .filter(ArtifactIds.parse(_).exists(_.name == r.name))
              .take(maxListed)
              .traverseFilter(a =>
                central.versions(r.groupId, a).map(_.map(Found.plain(a, _)))
              )
          )
    found.map(_.filter(_.versions.nonEmpty)).flatMap {
      case Nil =>
        val javaOnly =
          if r.artifactId.isDefined || ArtifactIds.parse(r.name).isDefined then
            IO.pure(false)
          else central.versions(r.groupId, r.name).map(_.exists(_.nonEmpty))
        javaOnly.map(java =>
          if java then notScalaResult(r)
          else
            ReportResult(
              ReportOutcome.NotFound,
              s"Neither deps.dev nor Maven Central has ${r.coordinate} " +
                "with a Scala version suffix."
            )
        )
      case found => record(r, found, enrichNow = false)
    }

object Reports:

  /** A coordinate checked this recently gets the stored answer. */
  val recheckAfter: FiniteDuration = 1.hour

  /** How long a visitor's request waits for deps.dev. */
  val liveTimeout: FiniteDuration = 5.seconds

  /** At most this many artifacts of one package are read from Central. */
  val maxListed = 50

  /** The suffixes a package name is tried with on deps.dev: the JVM Scala
    * versions, Scala.js, Scala Native and sbt 1 plugins.
    */
  val suffixes: List[String] =
    List("_3", "_2.13", "_2.12", "_sjs1_3", "_native0.5_3", "_2.12_1.0")

  /** The artifactIds a report could mean. */
  def candidates(r: PackageReport): List[String] =
    r.artifactId.fold(suffixes.map(r.name + _))(List(_))

  /** Versions of one artifact; dates only from deps.dev. */
  final case class Found(artifactId: String, versions: List[VersionEntry])

  object Found:
    def plain(artifactId: String, versions: List[String]): Found =
      Found(artifactId, versions.map(VersionEntry(_, None)))

  private def at(v: VersionEntry): Option[Instant] =
    v.publishedAt.flatMap(t => Try(Instant.parse(t)).toOption)

  def hours(d: FiniteDuration): String =
    val h = d.toHours
    if h <= 1 then "an hour" else s"$h hours"

/** Allows `limit` uses per fixed `window`, across all callers. */
final class Budget private (
    limit: Int,
    window: FiniteDuration,
    state: Ref[IO, (Long, Int)]
):
  /** Uses one if any are left in this window. */
  def take: IO[Boolean] =
    IO.monotonic.map(_.toMillis).flatMap { now =>
      val start = now - now % window.toMillis
      state.modify { (w, used) =>
        val n = if w == start then used else 0
        if n < limit then ((start, n + 1), true) else ((start, n), false)
      }
    }

object Budget:
  def apply(limit: Int, window: FiniteDuration): IO[Budget] =
    Ref.of[IO, (Long, Int)]((0L, 0)).map(new Budget(limit, window, _))
