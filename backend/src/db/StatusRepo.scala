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
import cats.syntax.all.*
import io.circe.Encoder
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.OffsetDateTime
import scala.concurrent.duration.FiniteDuration

/** The last run of one background job (table `job_runs`). */
final case class JobRun(
    job: String,
    startedAt: Option[OffsetDateTime],
    finishedAt: Option[OffsetDateTime],
    okAt: Option[OffsetDateTime],
    summary: Option[String],
    error: Option[String]
) derives Encoder.AsObject

/** How far the Maven index has been read: the last chunk applied and when, and
  * when Central's index was last asked about, its latest chunk then and when
  * that was published.
  */
final case class IndexState(
    lastChunk: Int,
    updatedAt: OffsetDateTime,
    remoteCheckedAt: Option[OffsetDateTime],
    remoteLastChunk: Option[Int],
    remotePublishedAt: Option[OffsetDateTime]
) derives Encoder.AsObject:
  /** Every chunk Central had published when last asked has been read. */
  def caughtUp: Boolean = remoteLastChunk.forall(_ <= lastChunk)

/** Work the jobs still have to do, as `scalapkg-status` counts it. */
final case class Backlog(
    enrichPending: Long,
    depsWaiting: Long,
    unembedded: Long,
    summarised: Long,
    projects: Long
) derives Encoder.AsObject

/** Reports waiting for the reports job, and those that changed the index in the
  * last 30 days.
  */
final case class ReportCounts(pending: Long, resolved: Long)
    derives Encoder.AsObject

/** What `/status` and `/api/v1/status` show. */
final case class SiteStatus(
    index: Option[IndexState],
    jobs: List[JobRun],
    backlog: Backlog,
    reports: ReportCounts
) derives Encoder.AsObject

/** Records job runs for the status page; see [[StatusRepo]]. */
trait JobRuns:
  def started(job: String): IO[Unit]

  /** `Right` the line a successful run logged, `Left` why it failed. */
  def finished(job: String, result: Either[String, String]): IO[Unit]

object JobRuns:
  val noop: JobRuns = new JobRuns:
    def started(job: String): IO[Unit] = IO.unit
    def finished(job: String, result: Either[String, String]): IO[Unit] =
      IO.unit

/** Why a visitor reported a package. */
enum ReportReason derives CanEqual:
  case Missing, Version, Other

  def param: String = this match
    case Missing => "missing"
    case Version => "version"
    case Other   => "other"

object ReportReason:
  def parse(s: String): Option[ReportReason] = values.find(_.param == s)

/** What checking a report found. */
enum ReportOutcome derives CanEqual:
  case Pending, Added, Updated, NoChange, NotFound, NotScala

  def param: String = this match
    case Pending  => "pending"
    case Added    => "added"
    case Updated  => "updated"
    case NoChange => "no_change"
    case NotFound => "not_found"
    case NotScala => "not_scala"

object ReportOutcome:
  def parse(s: String): Option[ReportOutcome] = values.find(_.param == s)

/** A package a visitor reported: a groupId with either an artifactId (whose
  * package is `name`) or only a package name.
  */
final case class PackageReport(
    groupId: String,
    artifactId: Option[String],
    name: String,
    reason: ReportReason,
    note: Option[String]
):
  def coordinate: String = s"$groupId:${artifactId.getOrElse(name)}"

/** A report's outcome and what the check found, in words for the visitor. */
final case class ReportResult(outcome: ReportOutcome, detail: String)

/** A pending report as the reports job reads it. */
final case class StoredReport(
    id: Long,
    report: PackageReport,
    createdAt: OffsetDateTime
)

/** Job runs, the status page and package reports (migration V5). */
final class StatusRepo(sessions: Resource[IO, Session[IO]]) extends JobRuns:
  import StatusRepo.*

  private def run[A](cmd: Command[A], arg: A): IO[Unit] =
    sessions.use(_.execute(cmd)(arg)).void

  def started(job: String): IO[Unit] =
    run(
      sql"""INSERT INTO job_runs (job, started_at) VALUES ($text, now())
            ON CONFLICT (job) DO UPDATE SET started_at = now()""".command,
      job
    )

  def finished(job: String, result: Either[String, String]): IO[Unit] =
    result match
      case Right(summary) =>
        run(
          sql"""INSERT INTO job_runs (job, finished_at, ok_at, summary)
                VALUES ($text, now(), now(), $text)
                ON CONFLICT (job) DO UPDATE
                SET finished_at = now(), ok_at = now(),
                    summary = EXCLUDED.summary, error = NULL""".command,
          (job, summary)
        )
      case Left(error) =>
        run(
          sql"""INSERT INTO job_runs (job, finished_at, error)
                VALUES ($text, now(), $text)
                ON CONFLICT (job) DO UPDATE
                SET finished_at = now(), error = EXCLUDED.error""".command,
          (job, error)
        )

  def status: IO[SiteStatus] =
    sessions.use { s =>
      (
        s.option(
          sql"""SELECT last_incremental, updated_at, remote_checked_at,
                       remote_last_chunk, remote_published_at
                FROM index_cursor"""
            .query(
              int4 *: timestamptz *: timestamptz.opt *: int4.opt *:
                timestamptz.opt
            )
            .to[IndexState]
        ),
        s.execute(
          sql"""SELECT job, started_at, finished_at, ok_at, summary, error
                FROM job_runs ORDER BY job"""
            .query(
              text *: timestamptz.opt *: timestamptz.opt *: timestamptz.opt *:
                text.opt *: text.opt
            )
            .to[JobRun]
        ),
        s.unique(
          sql"""SELECT
                  (SELECT count(*) FROM artifacts WHERE needs_refresh),
                  (SELECT count(*) FROM packages
                   WHERE deps_version IS DISTINCT FROM latest_version),
                  (SELECT count(*) FROM search_docs WHERE embedding IS NULL),
                  (SELECT count(*) FROM projects WHERE summary IS NOT NULL),
                  (SELECT count(*) FROM projects),
                  (SELECT count(*) FROM package_reports
                   WHERE outcome = 'pending'),
                  (SELECT count(*) FROM package_reports
                   WHERE outcome IN ('added', 'updated')
                     AND checked_at > now() - interval '30 days')"""
            .query(int8 *: int8 *: int8 *: int8 *: int8 *: int8 *: int8)
        )
      ).mapN { (index, jobs, counts) =>
        val (
          enrich,
          deps,
          unembedded,
          summarised,
          projects,
          pending,
          resolved
        ) =
          counts
        SiteStatus(
          index,
          jobs,
          Backlog(enrich, deps, unembedded, summarised, projects),
          ReportCounts(pending, resolved)
        )
      }
    }

  /** The newest outcome other than pending recorded for this coordinate within
    * `within`.
    */
  def recent(
      r: PackageReport,
      within: FiniteDuration
  ): IO[Option[ReportResult]] =
    sessions.use(
      _.option(
        sql"""SELECT outcome, coalesce(detail, '') FROM package_reports
              WHERE group_id = $text AND name = $text
                AND coalesce(artifact_id, '') = $text
                AND outcome <> 'pending'
                AND checked_at > now() - make_interval(secs => $float8)
              ORDER BY checked_at DESC LIMIT 1"""
          .query(outcome *: text)
          .to[ReportResult]
      )(
        (
          r.groupId,
          r.name,
          r.artifactId.getOrElse(""),
          within.toSeconds.toDouble
        )
      )
    )

  /** Whether this coordinate is already waiting for the reports job. */
  def isPending(r: PackageReport): IO[Boolean] =
    sessions.use(
      _.unique(
        sql"""SELECT EXISTS (SELECT 1 FROM package_reports
                WHERE group_id = $text AND name = $text
                  AND coalesce(artifact_id, '') = $text
                  AND outcome = 'pending')""".query(bool)
      )((r.groupId, r.name, r.artifactId.getOrElse("")))
    )

  /** Queues the report for the reports job; when it is already waiting, counts
    * it again and keeps the newest note. Returns whether it was new.
    */
  def enqueue(r: PackageReport, detail: String): IO[Boolean] =
    sessions.use(
      _.unique(
        sql"""INSERT INTO package_reports
                (group_id, artifact_id, name, reason, note, detail)
              VALUES ($text, ${text.opt}, $text, $text, ${text.opt}, $text)
              ON CONFLICT (group_id, name, coalesce(artifact_id, ''))
                WHERE outcome = 'pending'
              DO UPDATE SET
                reports = package_reports.reports + 1,
                note = coalesce(EXCLUDED.note, package_reports.note)
              RETURNING reports = 1""".query(bool)
      )((r.groupId, r.artifactId, r.name, r.reason.param, r.note, detail))
    )

  /** Stores a report that was settled when it was submitted. */
  def record(r: PackageReport, result: ReportResult): IO[Unit] =
    run(
      sql"""INSERT INTO package_reports
              (group_id, artifact_id, name, reason, note, checked_at, outcome,
               detail)
            VALUES ($text, ${text.opt}, $text, $text, ${text.opt}, now(),
                    $outcome, $text)""".command,
      (
        r.groupId,
        r.artifactId,
        r.name,
        r.reason.param,
        r.note,
        result.outcome,
        result.detail
      )
    )

  /** Up to `limit` pending reports, the most reported and oldest first. */
  def pending(limit: Int): IO[List[StoredReport]] =
    sessions.use(
      _.execute(
        sql"""SELECT id, group_id, artifact_id, name, reason, note, created_at
              FROM package_reports WHERE outcome = 'pending'
              ORDER BY reports DESC, created_at
              LIMIT $int4"""
          .query(
            int8 *: text *: text.opt *: text *: reason *: text.opt *:
              timestamptz
          )
          .map { case (id, g, a, n, why, note, at) =>
            StoredReport(id, PackageReport(g, a, n, why, note), at)
          }
      )(limit)
    )

  def resolve(id: Long, result: ReportResult): IO[Unit] =
    run(
      sql"""UPDATE package_reports
            SET outcome = $outcome, detail = $text, checked_at = now()
            WHERE id = $int8""".command,
      (result.outcome, result.detail, id)
    )

  /** Deletes reports older than `keep`; the line for the job log. */
  def prune(keep: FiniteDuration): IO[String] =
    sessions
      .use(
        _.execute(
          sql"""DELETE FROM package_reports
                WHERE created_at < now() - make_interval(secs => $float8)""".command
        )(keep.toSeconds.toDouble)
      )
      .map {
        case skunk.data.Completion.Delete(n) =>
          s"deleted $n old package reports"
        case other => s"pruned package reports: $other"
      }

object StatusRepo:
  private val outcome: Codec[ReportOutcome] =
    text.eimap(s => ReportOutcome.parse(s).toRight(s"unknown outcome $s"))(
      _.param
    )
  private val reason: Codec[ReportReason] =
    text.eimap(s => ReportReason.parse(s).toRight(s"unknown reason $s"))(
      _.param
    )
