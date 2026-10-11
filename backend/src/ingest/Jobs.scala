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

import cats.Order
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import com.softinio.scalapkg.MavenVersions
import com.softinio.scalapkg.db.*
import com.softinio.scalapkg.depsdev.DepsDev
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.embed.Embedder
import com.softinio.scalapkg.github.GitHub
import com.softinio.scalapkg.maven.ChunkGone
import com.softinio.scalapkg.maven.IndexRecord
import com.softinio.scalapkg.maven.MavenIndex
import com.softinio.scalapkg.summary.Anthropic
import com.softinio.scalapkg.summary.BatchState
import com.softinio.scalapkg.summary.Summaries
import fs2.Stream
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.OffsetDateTime
import scala.concurrent.duration.*
import scala.util.Try

/** Keeps `artifacts` in step with the Maven Central nexus index: a full read of
  * the index the first time (or when the cursor has fallen behind the chunks
  * Central still serves), then only the new incremental chunks.
  */
final class IndexSync(
    maven: MavenIndex,
    repo: IngestRepo,
    bootstrapChunks: Option[Int]
)(using log: Logger[IO]):

  private val batchSize = 20000
  private val chunkDelay = 2.seconds

  def run: IO[String] =
    for
      remote <- maven.remoteCursor
      local <- repo.cursor
      msg <- local match
        case Some(c)
            if c.chainId == remote.chainId &&
              c.lastIncremental >= remote.lastIncremental =>
          IO.pure(s"index up to date at chunk ${c.lastIncremental}")
        case Some(c) if c.chainId == remote.chainId =>
          incremental(c.lastIncremental + 1, remote).recoverWith {
            case ChunkGone(n) =>
              log.warn(
                s"chunk $n is no longer served; re-reading the full index"
              ) *>
                fullLoad(remote)
          }
        case Some(_) => fullLoad(remote)
        case None    =>
          bootstrapChunks.fold(fullLoad(remote)) { n =>
            val from = math.max(0, remote.lastIncremental - n + 1)
            log.info(
              s"bootstrapping from chunk $from instead of the full index"
            ) *>
              incremental(from, remote)
          }
    yield msg

  private def incremental(from: Int, remote: IndexCursor): IO[String] =
    (from to remote.lastIncremental).toList
      .traverse { n =>
        IO.sleep(chunkDelay) *>
          ingest(maven.chunk(n), s"chunk $n") <*
          repo.saveCursor(IndexCursor(remote.chainId, n))
      }
      .map(counts =>
        s"read chunks $from..${remote.lastIncremental}: ${counts.sum} artifacts updated"
      )

  private def fullLoad(remote: IndexCursor): IO[String] =
    log.info("reading the full Maven Central index; this takes a while") *>
      ingest(maven.full, "full index")
        .flatTap(_ => repo.saveCursor(remote))
        .map(n => s"full index read: $n artifacts updated")

  /** Records the best version of every Scala artifact in the stream. Returns
    * how many artifacts got a new latest version.
    */
  def ingest(records: Stream[IO, IndexRecord], label: String): IO[Int] =
    records
      .mapFilter(r =>
        ArtifactIds
          .parse(r.artifactId)
          .map(p =>
            SeenArtifact(
              r.groupId,
              r.artifactId,
              p.name,
              p.target,
              r.version,
              r.publishedAt
            )
          )
      )
      .chunkN(batchSize)
      .zipWithIndex
      .evalMap { (chunk, i) =>
        record(chunk.toList).flatTap(n =>
          log
            .info(s"$label: batch ${i + 1}, $n artifacts updated")
            .whenA(i % 25 == 0)
        )
      }
      .compile
      .foldMonoid

  private def record(seen: List[SeenArtifact]): IO[Int] =
    val groups = seen.groupBy(_.key).toList
    val best = groups.flatMap((_, group) =>
      group.toNel.map(MavenVersions.best(_)(_.version, _.publishedAt))
    )
    // the earliest publish time of any version seen, kept even when no
    // version is new: a full read sees every version an artifact ever had
    val firsts = groups.flatMap((key, group) =>
      group.flatMap(_.publishedAt).minOption.map(key -> _)
    )
    repo.latestVersions(best.map(_.key)).flatMap { existing =>
      val changed = best.filter(a =>
        existing
          .get(a.key)
          .forall((cur, curAt) =>
            MavenVersions.isBetter(a.version, a.publishedAt, cur, curAt)
          )
      )
      (repo.upsertSeen(changed) *> repo.noteFirstReleases(firsts))
        .as(changed.size)
    }

/** Dates artifacts the index never gave a publish time for (bootstrapped from
  * recent chunks, or records without an `i` field) from the versions deps.dev
  * lists, so "new projects" only counts projects whose first release is known.
  */
final class FirstReleases(depsDev: DepsDev, repo: IngestRepo)(using
    log: Logger[IO]
):
  private val batchSize = 500

  def run: IO[String] =
    repo.undatedArtifacts(batchSize).flatMap { keys =>
      // a failed lookup is left for the next run
      keys
        .traverseFilter(key =>
          depsDev
            .packageVersions(key.name)
            .map(vs =>
              Option(
                key -> vs
                  .flatMap(_.publishedAt)
                  .flatMap(at => Try(OffsetDateTime.parse(at)).toOption)
                  .minOption
              )
            )
            .handleErrorWith(e =>
              log
                .warn(s"first release of ${key.name}: ${e.getMessage}")
                .as(None)
            )
        )
        .flatTap(rows =>
          repo.noteFirstReleases(rows.collect { case (key, Some(at)) =>
            key -> at.toInstant
          }) *> repo.markFirstReleaseChecked(rows.map(_._1))
        )
        .map(rows =>
          s"dated ${rows.count(_._2.isDefined)} of ${rows.size} undated artifacts"
        )
    }

/** Fills in what the index does not tell us, from deps.dev: release dates,
  * licences, source repository and project stats. Rebuilds the affected
  * packages and their search vectors.
  */
final class Enrich(depsDev: DepsDev, repo: IngestRepo)(using log: Logger[IO]):

  private val batchSize = 5000

  /** Drains every artifact waiting for details. */
  def run: IO[String] =
    Drain(1000)(repo.pending(batchSize))(round).flatMap(n =>
      refreshProjects.map(p => s"enriched $n artifacts, refreshed $p projects")
    )

  private def round(pending: List[PendingArtifact]): IO[Int] =
    for
      found <- depsDev.versions(pending)(p =>
        ArtifactVersion(p.groupId, p.artifactId, p.version)
      )
      missing = pending.filterNot(found.map(_._1).toSet)
      _ <- repo.saveDetails(found)
      _ <- repo.defer(missing.map(p => ArtifactKey(p.groupId, p.artifactId)))
      _ <- refreshProjects
      _ <- rebuild(pending.map(p => PackageRef(p.groupId, p.name)).distinct)
      _ <- log.info(
        s"enrich: ${found.size} artifacts updated, ${missing.size} not on deps.dev yet"
      )
    yield pending.size

  /** Fetches stats for projects that are new or older than a week. Projects
    * deps.dev does not know are stored without stats until the next refresh.
    */
  def refreshProjects: IO[Int] =
    repo.projectsToRefresh(batchSize).flatMap {
      case Nil => IO.pure(0)
      case ids =>
        depsDev.projects(ids).flatMap { found =>
          val known = found.map(_.id).toSet
          val stubs = ids
            .filterNot(known)
            .map(id =>
              ProjectInfo(id, None, None, None, None, None, None, None)
            )
          repo.upsertProjects(found ++ stubs).as(ids.size)
        }
    }

  /** Regroups the stored artifacts of these packages into their `packages`
    * rows, which also refreshes their search documents.
    */
  def rebuild(packages: List[PackageRef]): IO[Unit] =
    repo.artifactsOf(packages).flatMap { artifacts =>
      val rows = artifacts
        .groupByNel(a => (a.groupId, a.name))
        .toList
        .map((_, arts) => Enrich.packageRow(arts))
      repo.upsertPackages(rows)
    }

object Enrich:

  /** Combines the artifacts of one package into its search row. */
  def packageRow(arts: NonEmptyList[ArtifactRow]): PackageRow =
    val version =
      MavenVersions.best(arts)(_.version, _.releasedAt.map(_.toInstant)).version
    // never empty: the best artifact is at this version
    val atLatest = arts.filter(_.version == version)
    val targets = arts.toList.map(_.target).distinct.sorted
    val project = arts.toList
      .flatMap(_.projectId)
      .groupBy(identity)
      .maxByOption(_._2.size)
      .map(_._1)
    PackageRow(
      groupId = arts.head.groupId,
      name = arts.head.name,
      projectId = project,
      version = version,
      releasedAt = atLatest.flatMap(_.releasedAt).maxOption,
      targets = targets,
      licenses = atLatest.flatMap(_.licenses).distinct.sorted,
      deprecated = atLatest.forall(_.deprecated)
    )

/** The nexus index is only published about every two weeks. To show releases
  * sooner, ask deps.dev daily for the versions of recently active packages. One
  * representative artifact per package is checked; its siblings only when it
  * has moved on.
  */
final class Freshness(depsDev: DepsDev, repo: IngestRepo, maxPackages: Int)(
    using log: Logger[IO]
):

  def run: IO[String] =
    repo.activePackages(maxPackages).flatMap { pkgs =>
      pkgs.flatTraverse(updates).flatMap { found =>
        repo.advance(found) *> recheck(maxPackages - pkgs.size).map(r =>
          s"checked ${pkgs.size} active packages, ${found.size} artifacts have newer versions$r"
        )
      }
    }

  /** Asks again for every artifact of up to `limit` packages marked for it,
    * then regroups them, since their latest version may be chosen differently
    * now.
    */
  private def recheck(limit: Int): IO[String] =
    if limit <= 0 then IO.pure("")
    else
      repo.toRecheck(limit).flatMap {
        case Nil  => IO.pure("")
        case pkgs =>
          val refs = pkgs.map(p => PackageRef(p.groupId, p.name))
          for
            found <- pkgs.flatTraverse(p =>
              p.artifacts.toList.traverseFilter(newer(p.groupId, _))
            )
            _ <- repo.advance(found)
            _ <- Enrich(depsDev, repo).rebuild(refs)
            _ <- repo.rechecked(refs)
          yield s"; rechecked ${pkgs.size} packages, ${found.size} artifacts changed"
      }

  /** The representative's newer version, then those of the siblings that moved
    * too; nothing when the representative has not moved.
    */
  private def updates(p: DepsTarget): IO[List[NewerVersion]] =
    val rep = DepsJob.representative(p)
    newer(p.groupId, rep).flatMap {
      case None        => IO.pure(Nil)
      case Some(moved) =>
        p.artifacts
          .filterNot(_.artifactId == rep.artifactId)
          .traverseFilter(newer(p.groupId, _))
          .map(moved :: _)
    }

  /** The artifact's newest version on deps.dev, if that is newer. */
  private def newer(
      groupId: String,
      a: DepsArtifact
  ): IO[Option[NewerVersion]] =
    depsDev
      .packageVersions(s"$groupId:${a.artifactId}")
      .map { versions =>
        def at(v: VersionEntry) =
          v.publishedAt.flatMap(t => Try(Instant.parse(t)).toOption)
        val currentAt = versions.find(_.version == a.version).flatMap(at)
        versions.toNel
          .map(MavenVersions.best(_)(_.version, at))
          .filter(b =>
            MavenVersions.isBetter(b.version, at(b), a.version, currentAt)
          )
          .map(b =>
            NewerVersion(groupId, a.artifactId, a.version, b.version, at(b))
          )
      }
      .handleErrorWith(e =>
        log
          .warn(
            s"freshness check failed for $groupId:${a.artifactId}: ${e.getMessage}"
          )
          .as(None)
      )

/** Adds GitHub topics and README text to projects, refreshed weekly, and
  * rebuilds the search vectors of their packages.
  */
final class GithubSync(github: GitHub, repo: IngestRepo)(using
    log: Logger[IO]
):
  private val batchSize = 200

  def run: IO[String] =
    Drain(200)(repo.githubToRefresh(batchSize)) { ids =>
      // ids GitHub could not be asked about still get a timestamp, so they
      // are not picked up again until next week
      github.fetch(ids).flatMap { fetched =>
        repo.saveGithub(fetched) *>
          log
            .info(
              s"github: ${fetched.count(_.readme.isDefined)}/${ids.size} READMEs"
            )
            .as(ids.size)
      }
    }.map(n => s"refreshed GitHub topics and READMEs of $n projects")

/** Reads which Scala packages each package depends on (the deps.dev
  * requirements of its latest version, test scope included) and recounts the
  * dependents every package and project has. deps.dev serves no dependents for
  * Maven, so they are counted from these edges: a popularity signal for search,
  * and the "used by" figure in the drawer.
  */
final class DepsJob(depsDev: DepsDev, repo: IngestRepo)(using
    log: Logger[IO]
):
  private val batchSize = 100

  def run: IO[String] =
    Drain(1000)(repo.depsToRefresh(batchSize)) { pkgs =>
      pkgs
        .traverse { p =>
          val r = DepsJob.representative(p)
          val a = ArtifactVersion(p.groupId, r.artifactId, r.version)
          depsDev
            .requirements(a)
            .map(reqs => p -> DepsJob.edges(p, reqs))
            .handleErrorWith(e =>
              log
                .warn(s"deps: ${a.name} ${a.version}: ${e.getMessage}")
                .as(p -> Nil)
            )
        }
        .flatMap(repo.saveDeps) *>
        log
          .info(s"deps: read requirements of ${pkgs.size} packages")
          .as(pkgs.size)
    }.flatMap(n =>
      repo.refreshDependents.as(s"read dependencies of $n packages")
    )

object DepsJob:

  /** The artifact asked about for a package (the newest Scala version on the
    * JVM if there is one) and that artifact's own latest version.
    */
  def representative(p: DepsTarget): DepsArtifact =
    p.artifacts.minimumBy(_.target)(using Order.fromOrdering)

  /** Requirement names (`groupId:artifactId`) that are Scala packages, without
    * the package itself.
    */
  def edges(p: DepsTarget, requirements: List[String]): List[PackageRef] =
    requirements
      .flatMap(_.split(':') match
        case Array(g, a) =>
          ArtifactIds.parse(a).map(parsed => PackageRef(g, parsed.name))
        case _ => None)
      .distinct
      .filterNot(_ == PackageRef(p.groupId, p.name))

/** Has Claude write a one-line summary and search keywords for each project,
  * from its description, topics, module names and README. Requests go out as
  * message batches (half price); a later run collects the results. A project is
  * asked about again only when its text changes, at most once a month.
  */
final class SummaryJob(
    anthropic: Anthropic,
    repo: IngestRepo,
    afterGithub: Boolean
)(using log: Logger[IO]):
  private val batchSize = 5000

  def run: IO[String] =
    for
      open <- repo.summaryBatches
      saved <- open.traverse(collect).map(_.sum)
      inputs <- repo.summariesToRequest(afterGithub, batchSize)
      _ <- submit(inputs).whenA(inputs.nonEmpty)
    yield s"saved $saved summaries, requested ${inputs.size}" +
      (if open.nonEmpty then s", ${open.size} batches were open" else "")

  private def submit(inputs: List[SummaryInput]): IO[Unit] =
    anthropic
      .submit(inputs.map(_.text))
      .flatMap(id =>
        repo.saveSummaryBatch(
          SummaryBatch(id, inputs.map(_.projectId), inputs.map(_.hash))
        ) *> log.info(
          s"summaries: batch $id asks about ${inputs.size} projects"
        )
      )

  /** Saves a finished batch's results; requests that expired or hit a server
    * error are left for the next batch.
    */
  private def collect(batch: SummaryBatch): IO[Int] =
    anthropic.state(batch.id).flatMap {
      case BatchState.InProgress => IO.pure(0)
      case BatchState.Gone       => repo.deleteSummaryBatch(batch.id).as(0)
      case BatchState.Ended(resultsUrl) =>
        val requests = batch.projectIds.zip(batch.hashes)
        anthropic.results(resultsUrl, requests).flatMap { results =>
          val rows = results.map { case ((id, hash), s) =>
            (id, hash, Summaries.withoutName(id, s))
          }
          repo.saveSummaries(rows) *> repo.deleteSummaryBatch(batch.id) *>
            log
              .info(
                s"summaries: batch ${batch.id}: ${rows.count(_._3.summary.isDefined)} summaries, ${batch.projectIds.size - rows.size} to retry"
              )
              .as(rows.size)
        }
    }

/** Computes embeddings for packages whose text changed, in batches, until none
  * are left.
  */
final class EmbedJob(embedder: Embedder, repo: IngestRepo)(using
    log: Logger[IO]
):
  private val batchSize = 256

  def run: IO[String] =
    Drain(1000)(repo.staleEmbeddings(batchSize)) { rows =>
      val (changed, same) =
        rows
          .partition(r => !r.embeddedHash.contains(r.hash))
      embedder.embedDocuments(changed.map(_.text)).flatMap { vectors =>
        repo.saveEmbeddings(
          changed.zip(vectors.map(Embedder.toVectorLiteral))
        ) *> repo.markEmbedded(same.map(_.key)) *>
          log
            .info(
              s"embed: ${changed.size} embedded, ${same.size} unchanged"
            )
            .whenA(changed.nonEmpty)
            .as(changed.size)
      }
    }.map(n => s"embedded $n search documents")

/** Fetches a batch with `next` and runs `round` on it, until a fetch finds
  * nothing left to do or `maxRounds` have run, and adds up the counts the
  * rounds report.
  */
object Drain:
  def apply[A](maxRounds: Int)(next: IO[List[A]])(
      round: List[A] => IO[Int]
  ): IO[Int] =
    Stream
      .repeatEval(next)
      .takeWhile(_.nonEmpty)
      .take(maxRounds.toLong)
      .evalMap(round)
      .compile
      .foldMonoid

/** Runs the jobs on fixed delays inside the server process. */
object Scheduler:

  /** Runs `job` after `initialDelay`, then again `interval` after each run
    * ends, forever. The line `job` returns is logged; a failure is logged and
    * the next run still happens.
    */
  def every(
      name: String,
      initialDelay: FiniteDuration,
      interval: FiniteDuration
  )(
      job: IO[String]
  )(using log: Logger[IO]): IO[Nothing] =
    val once =
      log.info(s"$name: starting") *>
        job.timed
          .flatMap((took, msg) => log.info(s"$name: $msg (${took.toSeconds}s)"))
          .handleErrorWith {
            // an expected, self-explanatory condition: no stack trace
            case e: com.softinio.scalapkg.maven.IndexRateLimited =>
              log.warn(s"$name: skipped, ${e.getMessage}")
            case e => log.error(e)(s"$name: failed")
          }
    IO.sleep(initialDelay) *> (once *> IO.sleep(interval)).foreverM
