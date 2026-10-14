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

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import com.softinio.scalapkg.depsdev.VersionInfo
import com.softinio.scalapkg.domain.ArtifactKey
import com.softinio.scalapkg.domain.PackageRef
import com.softinio.scalapkg.domain.ProjectInfo
import com.softinio.scalapkg.domain.Target
import com.softinio.scalapkg.github.RepoText
import com.softinio.scalapkg.summary.GeneratedSummary
import skunk.*
import skunk.codec.all.*
import skunk.data.Arr
import skunk.implicits.*

import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset

/** How far `IndexSync` has read the Maven index: the index chain (a new one
  * means Central rebuilt the index) and the last incremental chunk applied.
  * Read from Central, `publishedAt` is when its latest chunk was published.
  */
final case class IndexCursor(
    chainId: String,
    lastIncremental: Int,
    publishedAt: Option[Instant] = None
)

/** A Scala artifact seen in the Maven index, with the version to record. */
final case class SeenArtifact(
    groupId: String,
    artifactId: String,
    name: String,
    target: Target,
    version: String,
    publishedAt: Option[Instant] = None
):
  def key: ArtifactKey = ArtifactKey(groupId, artifactId)

/** A newer version found for an artifact last seen at `compared`, released at
  * `releasedAt`.
  */
final case class NewerVersion(
    groupId: String,
    artifactId: String,
    compared: String,
    version: String,
    releasedAt: Option[Instant] = None
)

/** An artifact whose latest version `Enrich` still has to look up. */
final case class PendingArtifact(
    groupId: String,
    artifactId: String,
    name: String,
    version: String
)

/** One stored artifact, as `Enrich.packageRow` groups them into a package. */
final case class ArtifactRow(
    groupId: String,
    name: String,
    target: Target,
    version: String,
    releasedAt: Option[OffsetDateTime],
    licenses: List[String],
    projectId: Option[String],
    deprecated: Boolean
)

/** One package as rebuilt from its artifacts. */
final case class PackageRow(
    groupId: String,
    name: String,
    projectId: Option[String],
    version: String,
    releasedAt: Option[OffsetDateTime],
    targets: List[Target],
    licenses: List[String],
    deprecated: Boolean
):
  def platforms: List[String] = targets.map(_.platform.facet).distinct
  def scalaVersions: List[String] = targets.map(_.scalaVersion).distinct

/** Write side used by the ingestion jobs. */
final class IngestRepo(sessions: Resource[IO, Session[IO]]):
  import IngestRepo.*

  private def run[A](cmd: Command[A], arg: A): IO[Unit] =
    sessions.use(_.execute(cmd)(arg)).void

  private def batched[A](rows: List[A])(f: List[A] => IO[Unit]): IO[Unit] =
    rows.grouped(batchSize).toList.traverse_(f)

  private def byKeys[K, A](keys: List[K])(q: Query[List[K], A]): IO[List[A]] =
    keys
      .grouped(batchSize)
      .toList
      .flatTraverse(b => sessions.use(_.execute(q)(b)))

  /** None before `IndexSync` has recorded any position. */
  def cursor: IO[Option[IndexCursor]] =
    sessions.use(
      _.option(
        sql"SELECT chain_id, last_incremental FROM index_cursor"
          .query(text *: int4)
          .map((chain, last) => IndexCursor(chain, last))
      )
    )

  def saveCursor(c: IndexCursor): IO[Unit] =
    run(
      sql"""INSERT INTO index_cursor (chain_id, last_incremental)
            VALUES ($text, $int4)
            ON CONFLICT (id) DO UPDATE
            SET chain_id = EXCLUDED.chain_id,
                last_incremental = EXCLUDED.last_incremental,
                updated_at = now()""".command,
      (c.chainId, c.lastIncremental)
    )

  /** Records that Central's index was just asked about, and when its latest
    * chunk was published (kept when Central did not say). Nothing before the
    * first cursor is saved.
    */
  def noteRemote(remote: IndexCursor): IO[Unit] =
    run(
      sql"""UPDATE index_cursor
            SET remote_checked_at = now(),
                remote_last_chunk = $int4,
                remote_published_at = coalesce(${timestamptz.opt},
                                               remote_published_at)""".command,
      (
        remote.lastIncremental,
        remote.publishedAt.map(_.atOffset(ZoneOffset.UTC))
      )
    )

  /** Each artifact's latest version and when it was released, if known. */
  def latestVersions(
      keys: List[ArtifactKey]
  ): IO[Map[ArtifactKey, (String, Option[Instant])]] =
    byKeys(keys)(
      sql"""SELECT group_id, artifact_id, latest_version, latest_release_at
            FROM artifacts WHERE (group_id, artifact_id) $artifactIn"""
        .query(text *: text *: text *: timestamptz.opt)
    ).map(
      _.map((g, a, v, at) =>
        ArtifactKey(g, a) -> (v, at.map(_.toInstant))
      ).toMap
    )

  /** Adds new artifacts and records new latest versions from the index, marking
    * each for `Enrich` to look up.
    */
  def upsertSeen(artifacts: List[SeenArtifact]): IO[Unit] =
    batched(artifacts) { batch =>
      val enc =
        (text *: text *: text *: target *: text *: timestamptz.opt).values
          .list(batch.size)
      run(
        sql"""INSERT INTO artifacts
                (group_id, artifact_id, name, target, latest_version,
                 latest_release_at)
              VALUES $enc
              ON CONFLICT (group_id, artifact_id) DO UPDATE
              SET latest_version = EXCLUDED.latest_version,
                  latest_release_at = EXCLUDED.latest_release_at,
                  needs_refresh = true,
                  refreshed_at = NULL""".command,
        batch.map(a =>
          (
            a.groupId,
            a.artifactId,
            a.name,
            a.target,
            a.version,
            a.publishedAt.map(_.atOffset(ZoneOffset.UTC))
          )
        )
      )
    }

  /** Records each newer version only where the artifact is still at the version
    * it was compared against, so a newer one recorded meanwhile stays.
    */
  def advance(found: List[NewerVersion]): IO[Unit] =
    batched(found) { batch =>
      run(
        sql"""UPDATE artifacts a
              SET latest_version = v.new,
                  latest_release_at = nullif(v.at, '')::timestamptz,
                  needs_refresh = true,
                  refreshed_at = NULL
              FROM unnest($textList, $textList, $textList, $textList, $textList)
                AS v(g, id, old, new, at)
              WHERE (a.group_id, a.artifact_id) = (v.g, v.id)
                AND a.latest_version = v.old""".command,
        (
          batch.map(_.groupId),
          batch.map(_.artifactId),
          batch.map(_.compared),
          batch.map(_.version),
          batch.map(_.releasedAt.fold("")(_.toString))
        )
      )
    }

  /** Artifacts marked for `Enrich`, never-asked first; one deps.dev did not
    * know is asked again after a day.
    */
  def pending(limit: Int): IO[List[PendingArtifact]] =
    sessions.use(
      _.execute(
        sql"""SELECT group_id, artifact_id, name, latest_version
              FROM artifacts
              WHERE needs_refresh
                -- versions deps.dev did not know yet are retried daily
                AND (refreshed_at IS NULL
                     OR refreshed_at < now() - interval '1 day')
              ORDER BY refreshed_at NULLS FIRST
              LIMIT $int4"""
          .query(text *: text *: text *: text)
          .to[PendingArtifact]
      )(limit)
    )

  /** Stores what deps.dev said about each artifact's latest version, records
    * the dated ones as releases, and adds a row for each new project.
    */
  def saveDetails(found: List[(PendingArtifact, VersionInfo)]): IO[Unit] =
    run(
      // packages link their project, so its row exists before an artifact
      // points at it, even if a later statement here fails
      sql"""INSERT INTO projects (id) SELECT DISTINCT unnest($textList)
            ON CONFLICT DO NOTHING""".command,
      found.flatMap(_._2.sourceRepo)
    ) *> found.traverse_ { (p, i) =>
      run(
        sql"""UPDATE artifacts
              SET latest_release_at = ${timestamptz.opt},
                  licenses = $textList,
                  project_id = ${text.opt},
                  deprecated = $bool,
                  needs_refresh = false,
                  refreshed_at = now()
              WHERE group_id = $text AND artifact_id = $text""".command,
        (
          i.publishedAt,
          i.licenses,
          i.sourceRepo,
          i.deprecated,
          p.groupId,
          p.artifactId
        )
      )
    } *> batched(found.flatMap { (p, i) =>
      i.publishedAt.map(at => (p.groupId, p.name, p.version, at))
    }) { batch =>
      val enc =
        (text *: text *: text *: timestamptz).values.list(batch.size)
      run(
        sql"""INSERT INTO releases (group_id, name, version, released_at)
              VALUES $enc ON CONFLICT DO NOTHING""".command,
        batch
      )
    }

  /** Lowers each artifact's first release to the given time when earlier. */
  def noteFirstReleases(rows: List[(ArtifactKey, Instant)]): IO[Unit] =
    batched(rows) { batch =>
      run(
        sql"""UPDATE artifacts a
              SET first_release_at = LEAST(a.first_release_at, u.at::timestamptz)
              FROM unnest($textList, $textList, $textList) AS u(g, id, at)
              WHERE a.group_id = u.g AND a.artifact_id = u.id""".command,
        (
          batch.map(_._1.groupId),
          batch.map(_._1.artifactId),
          batch.map(_._2.toString)
        )
      )
    }

  /** Artifacts the index never gave a publish time for, not yet looked up. */
  def undatedArtifacts(limit: Int): IO[List[ArtifactKey]] =
    sessions.use(
      _.execute(
        sql"""SELECT group_id, artifact_id FROM artifacts
              WHERE first_release_at IS NULL AND NOT first_release_checked
              LIMIT $int4""".query(text *: text).to[ArtifactKey]
      )(limit)
    )

  /** Marks artifacts' first release as looked up, so an artifact deps.dev has
    * no dates for is not asked again.
    */
  def markFirstReleaseChecked(keys: List[ArtifactKey]): IO[Unit] =
    batched(keys) { batch =>
      run(
        sql"""UPDATE artifacts SET first_release_checked = true
              WHERE (group_id, artifact_id) $artifactIn""".command,
        batch
      )
    }

  /** Leaves these artifacts marked but out of `pending` for a day. */
  def defer(keys: List[ArtifactKey]): IO[Unit] =
    batched(keys) { batch =>
      run(
        sql"""UPDATE artifacts SET refreshed_at = now()
              WHERE (group_id, artifact_id) $artifactIn""".command,
        batch
      )
    }

  /** Projects whose stats are missing or a week old, for `Enrich`. */
  def projectsToRefresh(limit: Int): IO[List[String]] =
    sessions.use(
      _.execute(
        // a project no artifact links any more is not worth a deps.dev call
        sql"""SELECT id FROM projects pr
              WHERE (refreshed_at IS NULL
                     OR refreshed_at < now() - interval '7 days')
                AND EXISTS (SELECT 1 FROM artifacts a WHERE a.project_id = pr.id)
              ORDER BY refreshed_at NULLS FIRST
              LIMIT $int4""".query(text)
      )(limit)
    )

  // package vectors hold names only, so project text changes only the
  // documents
  /** Writes these rows and refreshes their search documents, as
    * `upsertPackages` and `saveGithub` do.
    */
  def upsertProjects(projects: List[ProjectInfo]): IO[Unit] =
    projects.traverse_ { p =>
      run(
        sql"""INSERT INTO projects
                (id, description, stars, forks, open_issues, license,
                 homepage, scorecard, refreshed_at)
              VALUES ($text, ${text.opt}, ${int4.opt}, ${int4.opt},
                      ${int4.opt}, ${text.opt}, ${text.opt},
                      ${float4.opt}, now())
              ON CONFLICT (id) DO UPDATE SET
                description = EXCLUDED.description,
                stars = EXCLUDED.stars,
                forks = EXCLUDED.forks,
                open_issues = EXCLUDED.open_issues,
                license = EXCLUDED.license,
                homepage = EXCLUDED.homepage,
                scorecard = EXCLUDED.scorecard,
                refreshed_at = EXCLUDED.refreshed_at""".command,
        (
          p.id,
          p.description,
          p.stars,
          p.forks,
          p.openIssues,
          p.license,
          p.homepage,
          p.scorecard.map(_.toFloat)
        )
      )
    } *> refreshDocs(projects.map(_.id))

  def artifactsOf(packages: List[PackageRef]): IO[List[ArtifactRow]] =
    byKeys(packages)(
      sql"""SELECT group_id, name, target,
                   latest_version, latest_release_at, licenses,
                   project_id, deprecated
            FROM artifacts WHERE (group_id, name) $packageIn"""
        .query(
          text *: text *: target *: text *: timestamptz.opt *:
            textList *: text.opt *: bool
        )
        .to[ArtifactRow]
    )

  // a package's project, or the project its repository moved to: the
  // artifacts released before a rename or transfer still name the old
  // repository, and would make it a second result. When the new repository
  // is not indexed itself, the old names that moved to it share the first
  // of them (typesafehub/scala-logging and lightbend/scala-logging)
  private val currentProject =
    """coalesce((SELECT coalesce(t.id, (SELECT min(s.id) FROM projects s
                                         WHERE s.moved_to = o.moved_to))
                 FROM projects o LEFT JOIN projects t ON t.id = o.moved_to
                 WHERE o.id = p.project_id), p.project_id)"""

  // the search vector depends only on the name, so it is written once
  /** Writes these packages and refreshes the search documents they belong to.
    */
  def upsertPackages(rows: List[PackageRow]): IO[Unit] =
    rows
      .traverse { r =>
        sessions.use(
          _.unique(
            sql"""INSERT INTO packages
                    (group_id, name, project_id, latest_version,
                     latest_release_at, targets, platforms, scala_versions,
                     licenses, deprecated, search)
                  SELECT p.group_id, p.name, #$currentProject,
                         p.latest_version, p.latest_release_at, p.targets,
                         p.platforms, p.scala_versions, p.licenses,
                         p.deprecated, #$searchExpr
                  FROM (VALUES ($text, $text, ${text.opt}, $text,
                                ${timestamptz.opt}, $textList, $textList,
                                $textList, $textList, $bool))
                    AS p(group_id, name, project_id, latest_version,
                         latest_release_at, targets, platforms,
                         scala_versions, licenses, deprecated)
                  ON CONFLICT (group_id, name) DO UPDATE SET
                    project_id = EXCLUDED.project_id,
                    latest_version = EXCLUDED.latest_version,
                    latest_release_at = EXCLUDED.latest_release_at,
                    targets = EXCLUDED.targets,
                    platforms = EXCLUDED.platforms,
                    scala_versions = EXCLUDED.scala_versions,
                    licenses = EXCLUDED.licenses,
                    deprecated = EXCLUDED.deprecated
                  RETURNING doc_key""".query(text)
          )(
            (
              r.groupId,
              r.name,
              r.projectId,
              r.version,
              r.releasedAt,
              r.targets.map(_.key),
              r.platforms,
              r.scalaVersions,
              r.licenses,
              r.deprecated
            )
          )
        )
      }
      .flatMap(refreshDocs)

  /** GitHub projects whose topics/README are missing or a week old. */
  def githubToRefresh(limit: Int): IO[List[String]] =
    sessions.use(
      _.execute(
        sql"""SELECT id FROM projects
              WHERE id LIKE 'github.com/%'
                AND (github_refreshed_at IS NULL OR github_refreshed_at < now() - interval '7 days')
              ORDER BY github_refreshed_at NULLS FIRST, stars DESC NULLS LAST
              LIMIT $int4""".query(text)
      )(limit)
    )

  /** Saves what GitHub said, and moves the packages of renamed repositories to
    * the new project (see `currentProject`).
    */
  def saveGithub(repos: List[RepoText]): IO[Unit] =
    repos.traverse_ { r =>
      run(
        sql"""UPDATE projects
              SET topics = $textList, readme = ${text.opt},
                  repo_created_at = coalesce(${timestamptz.opt}, repo_created_at),
                  moved_to = ${text.opt},
                  github_refreshed_at = now()
              WHERE id = $text""".command,
        (r.topics, r.readme, r.createdAt, r.movedTo, r.projectId)
      )
    } *> {
      val ids = repos.map(_.projectId)
      sessions
        .use(
          _.execute(
            sql"""UPDATE packages p SET project_id = #$currentProject
                  WHERE p.project_id = ANY($textList)
                    AND p.project_id IS DISTINCT FROM #$currentProject
                  RETURNING p.project_id""".query(text)
          )(ids)
        )
        .flatMap(moved => refreshDocs(ids ++ moved))
    }

  /** Projects with no summary yet, or whose text changed since (at most once a
    * month), and not waiting in a batch. With `afterGithub`, GitHub projects
    * wait for their topics and README first, so they are not summarized twice.
    */
  def summariesToRequest(
      afterGithub: Boolean,
      limit: Int
  ): IO[List[SummaryInput]] =
    sessions.use(
      _.execute(
        sql"""SELECT id, input, hash
              FROM (
                SELECT pr.id, pr.stars, pr.summary_hash, input,
                       md5(regexp_replace(input, '[0-9]+', '', 'g')) AS hash
                FROM projects pr,
                     LATERAL (SELECT concat_ws(E'\n',
                       'Repository: ' || pr.id,
                       'Description: ' || pr.description,
                       CASE WHEN cardinality(pr.topics) > 0
                            THEN 'Topics: ' || array_to_string(pr.topics, ', ') END,
                       'Modules: ' || (
                         SELECT string_agg(m.name, ', ')
                         FROM (SELECT p.name FROM packages p
                               WHERE p.project_id = pr.id
                               ORDER BY length(p.name), p.name LIMIT 15) m),
                       'README (start): ' || left(pr.readme, 4000)) AS input) i
                WHERE (pr.description IS NOT NULL OR pr.readme IS NOT NULL)
                  AND EXISTS (SELECT 1 FROM packages p WHERE p.project_id = pr.id)
                  AND (NOT $bool OR pr.id NOT LIKE 'github.com/%'
                       OR pr.github_refreshed_at IS NOT NULL)
                  AND (pr.summary_at IS NULL
                       OR pr.summary_at < now() - interval '30 days')
                  AND NOT EXISTS (SELECT 1 FROM summary_batches b
                                  WHERE pr.id = ANY(b.project_ids))) s
              WHERE summary_hash IS DISTINCT FROM hash
              ORDER BY stars DESC NULLS LAST, id
              LIMIT $int4"""
          .query(text *: text *: text)
          .to[SummaryInput]
      )((afterGithub, limit))
    )

  def summaryBatches: IO[List[SummaryBatch]] =
    sessions.use(
      _.execute(
        sql"""SELECT id, project_ids, hashes FROM summary_batches
              ORDER BY created_at"""
          .query(text *: textList *: textList)
          .to[SummaryBatch]
      )
    )

  def saveSummaryBatch(b: SummaryBatch): IO[Unit] =
    run(
      sql"""INSERT INTO summary_batches (id, project_ids, hashes)
            VALUES ($text, $textList, $textList)""".command,
      (b.id, b.projectIds, b.hashes)
    )

  def deleteSummaryBatch(id: String): IO[Unit] =
    run(sql"DELETE FROM summary_batches WHERE id = $text".command, id)

  /** Stores generated summaries by project id and input hash, and reindexes
    * those projects. The hash is kept even for `GeneratedSummary.none` (the
    * request failed for good), so the same input is not sent again.
    */
  def saveSummaries(
      rows: List[(String, String, GeneratedSummary)]
  ): IO[Unit] =
    rows.traverse_ { (id, hash, generated) =>
      run(
        sql"""UPDATE projects
              SET summary = ${text.opt}, summary_keywords = $textList,
                  summary_hash = $text, summary_at = now()
              WHERE id = $text""".command,
        (generated.summary, generated.keywords, hash, id)
      )
    } *> refreshDocs(rows.map(_._1))

  // title, description, generated summary and keywords, topics, module
  // names and the opening of the README: the model reads at most 256
  // tokens, so the rest would be cut anyway
  /** Search documents marked `embedding_stale`, with the text `EmbedJob` embeds
    * for them.
    */
  def staleEmbeddings(limit: Int): IO[List[EmbeddingInput]] =
    sessions.use(
      _.execute(
        sql"""SELECT d.key,
                     concat_ws('. ',
                       translate(d.title, '-_', '  '),
                       pr.description,
                       pr.summary,
                       CASE WHEN cardinality(pr.summary_keywords) > 0
                            THEN 'Keywords: ' || array_to_string(pr.summary_keywords, ', ') END,
                       CASE WHEN cardinality(pr.topics) > 0
                            THEN 'Topics: ' || array_to_string(pr.topics, ', ') END,
                       'Modules: ' || (
                         SELECT string_agg(translate(m.name, '-_', '  '), ', ')
                         FROM (SELECT p.name FROM packages p
                               WHERE p.doc_key = d.key
                               ORDER BY length(p.name), p.name LIMIT 12) m),
                       left(pr.readme, 1200)),
                     CASE WHEN d.embedding IS NOT NULL
                          THEN d.embedding_text_hash END
              FROM search_docs d LEFT JOIN projects pr ON pr.id = d.project_id
              WHERE d.embedding_stale
              LIMIT $int4"""
          .query(text *: text *: text.opt)
          .to[EmbeddingInput]
      )(limit)
    )

  def saveEmbeddings(rows: List[(EmbeddingInput, String)]): IO[Unit] =
    rows.traverse_ { (in, vector) =>
      run(
        sql"""UPDATE search_docs
              SET embedding = ${text}::vector, embedding_text_hash = $text,
                  embedding_stale = false
              WHERE key = $text""".command,
        (vector, in.hash, in.key)
      )
    }

  /** Marks documents whose text is unchanged since their last embedding. */
  def markEmbedded(keys: List[String]): IO[Unit] =
    batched(keys) { batch =>
      run(
        sql"""UPDATE search_docs SET embedding_stale = false
              WHERE key = ANY($textList)""".command,
        batch
      )
    }

  // per package: names as typed (prefix search) and stemmed (`parsing`
  // finds `markdown-parser`), all weight A, which is all
  // PackageRepo.nameQuery reads to pick the modules a result shows
  private val searchExpr =
    """setweight(to_tsvector('simple', p.name || ' ' || translate(p.name, '-_.', '   ')), 'A') ||
       setweight(to_tsvector('english', translate(p.name, '-_.', '   ')), 'A')"""

  // per document, by how much a match says about what the project is:
  // its name A; description, topics and the generated summary and
  // keywords B; module names, groups and the
  // repository owner C (a `tapir-json-circe` module does not make tapir
  // a JSON library); README D
  private val docsHead =
    """INSERT INTO search_docs AS d
          (key, project_id, title, platforms, scala_versions,
           latest_release_at, deprecated, search, embedding_stale)
        SELECT a.key, a.project_id, a.title, a.platforms, a.scala_versions,
               a.released, a.deprecated,
               setweight(to_tsvector('simple', translate(a.title, '-_.', '   ')), 'A') ||
               setweight(to_tsvector('english', translate(a.title, '-_.', '   ')), 'A') ||
               setweight(to_tsvector('english', coalesce(pr.description, '')), 'B') ||
               setweight(to_tsvector('english', translate(array_to_string(coalesce(pr.topics, '{}'), ' '), '-', ' ')), 'B') ||
               setweight(to_tsvector('english', coalesce(pr.summary, '') || ' ' || translate(array_to_string(coalesce(pr.summary_keywords, '{}'), ' '), '-', ' ')), 'B') ||
               setweight(to_tsvector('simple', a.name_words), 'C') ||
               setweight(to_tsvector('english', a.name_words), 'C') ||
               setweight(to_tsvector('simple', translate(a.groups, '.-', '  ')), 'C') ||
               setweight(to_tsvector('simple', coalesce(translate(regexp_replace(a.project_id, '^[^/]+/', ''), '/-_.', '    '), '')), 'C') ||
               setweight(to_tsvector('english', coalesce(pr.readme, '')), 'D'),
               true
        FROM (
          SELECT p.doc_key AS key,
                 min(p.project_id) AS project_id,
                 coalesce(regexp_replace(min(p.project_id), '^.*/', ''),
                          CASE WHEN count(*) = 1 THEN min(p.name)
                               ELSE min(p.group_id) END) AS title,
                 -- each word of the project and module names once, so a
                 -- project with twenty `tapir-json-*` modules does not
                 -- outrank a JSON library for `json`
                 (SELECT string_agg(DISTINCT w, ' ')
                  FROM packages q,
                       regexp_split_to_table(
                         coalesce(regexp_replace(q.project_id, '^.*/', ''), '')
                           || ' ' || q.name,
                         '[-_. ]+') w
                  WHERE q.doc_key = p.doc_key AND w <> '') AS name_words,
                 string_agg(DISTINCT p.group_id, ' ') AS groups,
                 max(p.latest_release_at) AS released,
                 bool_and(p.deprecated) AS deprecated,
                 (SELECT coalesce(array_agg(DISTINCT x ORDER BY x), '{}')
                  FROM packages q, unnest(q.platforms) x
                  WHERE q.doc_key = p.doc_key) AS platforms,
                 (SELECT coalesce(array_agg(DISTINCT x ORDER BY x), '{}')
                  FROM packages q, unnest(q.scala_versions) x
                  WHERE q.doc_key = p.doc_key) AS scala_versions
          FROM packages p"""

  private val docsTail =
    """   GROUP BY p.doc_key) a
        LEFT JOIN projects pr ON pr.id = a.project_id
        ON CONFLICT (key) DO UPDATE SET
          project_id = EXCLUDED.project_id,
          title = EXCLUDED.title,
          platforms = EXCLUDED.platforms,
          scala_versions = EXCLUDED.scala_versions,
          latest_release_at = EXCLUDED.latest_release_at,
          deprecated = EXCLUDED.deprecated,
          search = EXCLUDED.search,
          embedding_stale = true"""

  // a package that moved to another project leaves its old document
  // behind; documents without packages are dropped
  private val dropEmptyDocs =
    sql"""DELETE FROM search_docs d
          WHERE NOT EXISTS (SELECT 1 FROM packages p WHERE p.doc_key = d.key)""".command

  private def refreshDocs(keys: List[String]): IO[Unit] =
    batched(keys.distinct) { batch =>
      run(
        sql"#$docsHead WHERE p.doc_key = ANY($textList) #$docsTail".command,
        batch
      )
    } *> sessions.use(_.execute(dropEmptyDocs)).void

  // the seed stores package vectors empty, and older code wrote other terms
  private val rebuildPackageSearch =
    sql"""UPDATE packages p SET search = #$searchExpr
          WHERE p.search IS DISTINCT FROM (#$searchExpr)""".command

  /** Rebuilds every search document and package search vector, so they always
    * match the current code (and a freshly loaded seed). Embeddings are kept:
    * only documents whose text changed get embedded again.
    */
  def rebuildSearchDocs: IO[Unit] =
    sessions.use(_.execute(rebuildPackageSearch)) *>
      sessions.use(_.execute(sql"#$docsHead #$docsTail".command)) *>
      sessions.use(_.execute(dropEmptyDocs)) *> refreshDependents

  /** Packages whose dependencies were not read for their latest version. */
  def depsToRefresh(limit: Int): IO[List[DepsTarget]] =
    withArtifacts(
      sql"""SELECT * FROM packages
            WHERE deps_version IS DISTINCT FROM latest_version
            LIMIT $int4""",
      limit
    )

  /** Packages released within a year, to poll deps.dev for versions the weekly
    * Maven index has not published yet.
    */
  def activePackages(limit: Int): IO[List[DepsTarget]] =
    withArtifacts(
      sql"""SELECT * FROM packages
            WHERE latest_release_at >= now() - interval '365 days'
            LIMIT $int4""",
      limit
    )

  /** Packages whose every artifact's versions are to be asked again, marked by
    * migration V4 after the choice of latest version changed.
    */
  def toRecheck(limit: Int): IO[List[DepsTarget]] =
    withArtifacts(
      sql"SELECT * FROM packages WHERE recheck_versions LIMIT $int4",
      limit
    )

  def rechecked(packages: List[PackageRef]): IO[Unit] =
    batched(packages)(batch =>
      run(
        sql"""UPDATE packages SET recheck_versions = false
              WHERE (group_id, name) $packageIn""".command,
        batch
      )
    )

  /** The packages `choose` selects, each with all its artifacts. */
  private def withArtifacts[A](
      choose: Fragment[A],
      args: A
  ): IO[List[DepsTarget]] =
    sessions.use(
      _.execute(
        sql"""SELECT p.group_id, p.name, p.latest_version,
                     array_agg(a.artifact_id ORDER BY a.artifact_id),
                     array_agg(a.target ORDER BY a.artifact_id),
                     array_agg(a.latest_version ORDER BY a.artifact_id)
              FROM ($choose) p
              JOIN artifacts a ON a.group_id = p.group_id AND a.name = p.name
              GROUP BY p.group_id, p.name, p.latest_version"""
          .query(
            (text *: text *: text *: textList *:
              textList.emap(_.traverse(toTarget)) *: textList)
              .emap { case (g, n, v, ids, targets, versions) =>
                NonEmptyList
                  .fromList(
                    ids
                      .lazyZip(targets)
                      .lazyZip(versions)
                      .map(DepsArtifact.apply)
                  )
                  .toRight(s"package $g:$n without artifacts")
                  .map(DepsTarget(g, n, v, _))
              }
          )
      )(args)
    )

  /** Replaces each package's dependency edges and marks its version read. */
  def saveDeps(rows: List[(DepsTarget, List[PackageRef])]): IO[Unit] =
    rows.traverse_ { (p, edges) =>
      sessions.use(s =>
        s.transaction.surround(
          s.execute(
            sql"""DELETE FROM package_deps
                  WHERE group_id = $text AND name = $text""".command
          )((p.groupId, p.name)) *>
            s.execute(
              sql"""INSERT INTO package_deps (group_id, name, dep_group_id, dep_name)
                    SELECT $text, $text, g, n FROM unnest($textList, $textList) AS e(g, n)
                    ON CONFLICT DO NOTHING""".command
            )(
              (
                p.groupId,
                p.name,
                edges.map(_.groupId),
                edges.map(_.name)
              )
            ) *>
            s.execute(
              sql"""UPDATE packages SET deps_version = $text
                    WHERE group_id = $text AND name = $text""".command
            )((p.latestVersion, p.groupId, p.name))
        )
      )
    }

  // a dependent is another project (or groupId) using the package: a
  // project's own modules depending on each other do not count
  private val dependentsCte =
    """WITH e AS (
         SELECT src.doc_key AS src, dst.doc_key AS dst,
                dst.group_id AS g, dst.name AS n
         FROM package_deps x
         JOIN packages src ON src.group_id = x.group_id AND src.name = x.name
         JOIN packages dst ON dst.group_id = x.dep_group_id AND dst.name = x.dep_name
         WHERE src.doc_key <> dst.doc_key)"""

  /** Recounts dependents (other projects depending on it) of every package and
    * search document.
    */
  def refreshDependents: IO[Unit] =
    sessions.use { s =>
      s.execute(
        sql"""#$dependentsCte, c AS (
                SELECT g, n, count(DISTINCT src)::int AS cnt FROM e GROUP BY g, n)
              UPDATE packages p SET dependents = coalesce(c.cnt, 0)
              FROM packages q LEFT JOIN c ON c.g = q.group_id AND c.n = q.name
              WHERE q.group_id = p.group_id AND q.name = p.name
                AND p.dependents <> coalesce(c.cnt, 0)""".command
      ) *>
        s.execute(
          sql"""#$dependentsCte, c AS (
                  SELECT dst, count(DISTINCT src)::int AS cnt FROM e GROUP BY dst)
                UPDATE search_docs d SET dependents = coalesce(c.cnt, 0)
                FROM search_docs d2 LEFT JOIN c ON c.dst = d2.key
                WHERE d2.key = d.key AND d.dependents <> coalesce(c.cnt, 0)""".command
        )
    }.void

/** A package with its artifacts, for the jobs that ask deps.dev about one of
  * them.
  */
final case class DepsTarget(
    groupId: String,
    name: String,
    latestVersion: String,
    artifacts: NonEmptyList[DepsArtifact]
)

/** One artifact of a package, with the artifact's own latest version. */
final case class DepsArtifact(
    artifactId: String,
    target: Target,
    version: String
)

/** A project's text for the summary model, and the hash it is known by (digits
  * left out, so version numbers in a README do not count as a change).
  */
final case class SummaryInput(projectId: String, text: String, hash: String)

/** A submitted batch: request i is for projectIds(i), made from hashes(i). */
final case class SummaryBatch(
    id: String,
    projectIds: List[String],
    hashes: List[String]
)

/** The text a search document's embedding is computed from, its current hash,
  * and the hash of the text behind its stored embedding, if it has one.
  */
final case class EmbeddingInput(
    key: String,
    text: String,
    embeddedHash: Option[String]
):
  lazy val hash: String =
    java.security.MessageDigest
      .getInstance("SHA-1")
      .digest(text.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString

object IngestRepo:

  private val batchSize = 500

  // every key in the database was written from Target.key
  private def toTarget(key: String): Either[String, Target] =
    Target.parseKey(key).toRight(s"unknown target $key")

  private val target: Codec[Target] = text.eimap(toTarget)(_.key)

  private[db] val textList: Codec[List[String]] =
    _text.imap(_.flattenTo(List))(Arr.fromFoldable(_))

  // two parallel arrays, matched with `(a, b) IN (SELECT * FROM unnest(..))`
  // so the composite primary key index is used
  private def keyIn[K](parts: K => (String, String)): Fragment[List[K]] =
    sql"IN (SELECT * FROM unnest($textList, $textList))"
      .contramap(_.map(parts).unzip)
  private val artifactIn = keyIn[ArtifactKey](k => (k.groupId, k.artifactId))
  private val packageIn = keyIn[PackageRef](r => (r.groupId, r.name))
