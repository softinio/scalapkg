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
import com.softinio.scalapkg.domain.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

/** Read side used by pages, the JSON API and the MCP server. */
trait PackageRepo:

  /** Ranked results for one page of `query`, with facet counts. If embedding
    * the query fails, search continues with keyword matches only.
    */
  def search(query: SearchQuery): IO[SearchResponse]
  def get(ref: PackageRef): IO[Option[PackageSummary]]

  /** The newest version of each of the package's targets: older than its latest
    * version for a target it stopped publishing.
    */
  def targetVersions(ref: PackageRef): IO[Map[Target, String]]

  /** Recently released projects, newest first, each shown by the main module of
    * its latest release; deprecated packages, and those with no source
    * repository (nothing to describe them with), are left out.
    */
  def newest(limit: Int): IO[List[PackageSummary]]

  /** Projects by GitHub stars, one main module each, only those publishing for
    * Scala 2.13 or 3 (or sbt/Mill plugins).
    */
  def mostStarred(limit: Int): IO[List[PackageSummary]]

  /** Projects first released within `newProjectDays`, newest first. */
  def newProjects(limit: Int): IO[List[NewProject]]

  /** Every version released at or after `since`, newest first. */
  def releases(since: OffsetDateTime, limit: Int): IO[List[Release]]

object PackageRepo:

  private val iso = DateTimeFormatter.ISO_INSTANT

  /** How recent a project's first release must be for it to count as new. */
  val newProjectDays = 90

  /** Search text beyond this many characters is dropped. */
  val maxQueryLength = 200

  /** A search result's own columns, before its modules are read. */
  private final case class DocRow(
      key: String,
      title: String,
      released: Option[String],
      dependents: Int
  )

  def isoString(t: OffsetDateTime): String =
    iso.format(t.withOffsetSameInstant(ZoneOffset.UTC))

  /** Turns free text into a prefix tsquery: `cats core` -> `cats:* & core:*`.
    * Only letters and digits survive, so the result is always valid tsquery
    * syntax.
    */
  def tsQuery(q: String): Option[String] =
    val terms = q.toLowerCase
      .split("[^\\p{L}\\p{N}]+")
      .filter(_.nonEmpty)
      .take(8)
    Option.when(terms.nonEmpty)(terms.map(t => s"$t:*").mkString(" & "))

  /** The same terms restricted to what a project says it is: its name (A),
    * description, topics and generated summary (B). Not module names (C), or a
    * time series database with an `atlas-json` module would be a JSON library,
    * and not its README (D).
    */
  def ownTextQuery(tsq: String): String = tsq.replace(":*", ":*AB")

  def isSingleTerm(tsq: String): Boolean = !tsq.contains(" & ")

  /** The same terms as whole words: `testing` but not `testcontainers`. */
  def wholeWords(tsq: String): String = tsq.replace(":*", "")

  /** A name as compared with a query: lower case, separators as single spaces,
    * so `cats effect` is the name `cats-effect`.
    */
  def nameWords(s: String): String =
    s.toLowerCase.split("[-_. ]+").filter(_.nonEmpty).mkString(" ")

  /** The same terms matched against module names only (weight A of a package's
    * own search vector).
    */
  def nameQuery(tsq: String): String = tsq.replace(":*", ":*A")

  /** Ranking knobs, tuned with `SearchEvalSuite` against the full index.
    *
    * @param rrfK
    *   keyword and semantic candidates are fused with reciprocal rank fusion,
    *   score = sum over lists of 1 / (k + rank); a smaller k makes rank count
    *   for more against popularity. Hundreds of projects match a word like
    *   `testing` about equally well, so their order in the keyword list says
    *   little and a large k lets popularity pick among them
    * @param starsWeight
    * @param dependentsWeight
    *   relevance is multiplied by 1 + starsWeight * ln(1 + stars) +
    *   dependentsWeight * ln(1 + dependents): GitHub stars and how many other
    *   indexed projects depend on the project
    * @param titleWeight
    *   weight of the title's trigram similarity to the query in the keyword
    *   score (typos, and names that are the query)
    * @param fieldWeights
    *   ts_rank weights for D (README), C (modules), B (description, topics,
    *   summary) and A (name); a low README weight stops a README that repeats a
    *   word from outranking a project named after it, and B as high as A lets a
    *   JSON library (circe) compete with projects that only have `json` in
    *   their name
    * @param prefixWeight
    *   share of the keyword score that prefix matches add to whole-word ones.
    *   Prefixes find what is still being typed (`cir` finds circe), but the
    *   stemmed `test:*` also matches every `testcontainers-*` project, which
    *   would bury the testing libraries
    * @param exactNameStars
    * @param exactNameDependents
    *   a project whose name is the query (`zio`, `cats-effect`) comes first
    *   when it has this many GitHub stars or dependents; a tiny repo that
    *   happens to be called `json` does not
    * @param scala3Only
    * @param scala213Only
    * @param scalaOlder
    *   2.13 and 3 are the current Scala versions and a project publishing for
    *   both ranks best; relevance is multiplied by these when it publishes for
    *   Scala 3 but not 2.13, for 2.13 but not 3, or only for 2.12 and older.
    *   sbt and Mill plugins are exempt, as their Scala version is set by the
    *   build tool
    */
  final case class Ranking(
      rrfK: Int = 80,
      starsWeight: Double = 0.3,
      dependentsWeight: Double = 0.3,
      titleWeight: Double = 0.3,
      fieldWeights: (Double, Double, Double, Double) = (0.03, 0.2, 1.0, 1.0),
      prefixWeight: Double = 0.5,
      exactNameStars: Int = 100,
      exactNameDependents: Int = 10,
      scala3Only: Double = 0.9,
      scala213Only: Double = 0.7,
      scalaOlder: Double = 0.4
  ):

    def scalaSupport: String =
      s"""(CASE WHEN d.platforms && '{sbt,mill}'::text[]
                  OR d.scala_versions @> '{2.13,3}'::text[] THEN 1
                WHEN '3' = ANY(d.scala_versions) THEN $scala3Only
                WHEN '2.13' = ANY(d.scala_versions) THEN $scala213Only
                ELSE $scalaOlder END)"""

    def weightsArray: String =
      val (d, c, b, a) = fieldWeights
      s"'{$d,$c,$b,$a}'::float4[]"

    def popularity: String =
      s"(1 + $starsWeight * ln(1 + coalesce(pr.stars, 0)) + $dependentsWeight * ln(1 + d.dependents))"

  private val keywordCandidates = 1000
  private val semanticCandidates = 100

  // what `project` decodes
  private val projectColumns =
    """pr.id, pr.description, pr.stars, pr.forks, pr.open_issues, pr.license,
       pr.homepage, pr.scorecard, pr.topics, pr.summary"""

  private val columns: Fragment[Void] =
    sql"""p.group_id, p.name, p.latest_version, p.latest_release_at,
          p.targets, p.licenses, p.deprecated, #$projectColumns, p.dependents"""

  private val from: Fragment[Void] =
    sql"FROM packages p LEFT JOIN projects pr ON pr.id = p.project_id"

  private val isoTime: Decoder[String] = timestamptz.map(isoString)

  // a LEFT JOIN that found no project reads as all NULL, which `opt` decodes
  // as None
  private val project: Decoder[Option[ProjectInfo]] =
    (text *: text.opt *: int4.opt *: int4.opt *: int4.opt *: text.opt *:
      text.opt *: float4.map(_.toDouble).opt *: IngestRepo.textList *:
      text.opt).to[ProjectInfo].opt

  private val summary: Decoder[PackageSummary] =
    (text *: text *: text *: isoTime.opt *: IngestRepo.textList *:
      IngestRepo.textList *: bool *: project *: int4).to[PackageSummary]

  private def where(conds: List[AppliedFragment]): AppliedFragment =
    conds match
      case Nil     => AppliedFragment.empty
      case c :: cs => void" WHERE " |+| c |+| and(cs)

  private def and(conds: List[AppliedFragment]): AppliedFragment =
    conds.foldLeft(AppliedFragment.empty)((acc, c) => acc |+| void" AND " |+| c)

  /** @param embed
    *   query -> pgvector literal (`[0.1,…]`), for the semantic half of search;
    *   None without an embedding model
    * @param maxDistance
    *   cosine distance beyond which a semantic match is too weak to show
    */
  def skunk(
      sessions: Resource[IO, Session[IO]],
      embed: Option[String => IO[String]] = None,
      maxDistance: Double = 0.35,
      ranking: Ranking = Ranking(),
      gate: Option[SearchGate] = None
  ): PackageRepo =
    new PackageRepo:

      private def list[A](af: AppliedFragment, decoder: Decoder[A]) =
        sessions.use(_.execute(af.fragment.query(decoder))(af.argument))

      private def one[A](af: AppliedFragment, decoder: Decoder[A]): IO[A] =
        sessions.use(_.unique(af.fragment.query(decoder))(af.argument))

      /** Facet filters on table alias `t`: `d` for search documents, `p` for
        * the modules listed under a result.
        */
      private def facetConditions(
          query: SearchQuery,
          t: String
      ): List[AppliedFragment] =
        Option
          .when(query.platforms.nonEmpty)(
            sql"#$t.platforms && ${IngestRepo.textList}".apply(query.platforms)
          )
          .toList ++
          Option
            .when(query.scalaVersions.nonEmpty)(
              sql"#$t.scala_versions && ${IngestRepo.textList}"
                .apply(query.scalaVersions)
            )
            .toList

      /** `WITH … hits(key, score, strong)`: search documents matching by
        * keyword (names as typed, everything else stemmed, typo-tolerant
        * titles) fused with the nearest documents by embedding, under the facet
        * conditions it is then given.
        */
      private def hitsCte(
          tsq: String,
          q: String,
          vector: Option[String]
      ): List[AppliedFragment] => AppliedFragment =
        val strongTsq = PackageRepo.ownTextQuery(tsq)
        // the model knows phrases ("parse markdown"), not library names:
        // for a single word it only steps in when nothing matches it
        val semanticOnlyAsFallback = PackageRepo.isSingleTerm(tsq)
        val whole = PackageRepo.wholeWords(tsq)
        val keywordScore =
          sql"""(greatest(ts_rank(#${ranking.weightsArray}, d.search, to_tsquery('simple', $text)),
                          ts_rank(#${ranking.weightsArray}, d.search, to_tsquery('english', $text)))
                 + $float8 * greatest(ts_rank(#${ranking.weightsArray}, d.search, to_tsquery('simple', $text)),
                                      ts_rank(#${ranking.weightsArray}, d.search, to_tsquery('english', $text)))
                 + $float8 * similarity(d.title, $text)
                 -- whole words beat prefixes: `json` prefers zio-json over jsoniter
                 + (CASE WHEN d.search @@ plainto_tsquery('english', $text)
                         THEN 0.5 ELSE 0 END))"""
            .apply(
              (
                whole,
                whole,
                ranking.prefixWeight,
                tsq,
                tsq,
                ranking.titleWeight,
                q,
                q
              )
            )
        val keywordMatch =
          sql"""(d.search @@ to_tsquery('simple', $text)
                 OR d.search @@ to_tsquery('english', $text)
                 OR d.title % $text)""".apply((tsq, tsq, q))
        // the words say what the project is (names, description, topics,
        // summary), not only in a module name or its README, or the title is
        // a close typo, or the query names the owner (`typelevel`)
        val strongMatch =
          sql"""(d.search @@ to_tsquery('simple', $text)
                 OR d.search @@ to_tsquery('english', $text)
                 OR word_similarity($text, d.title) >= 0.5
                 OR lower(split_part(d.key, '/', 2)) = $text)""".apply(
            (strongTsq, strongTsq, q, q)
          )
        // matched only as a close spelling of the title: `scalafmt` is a
        // trigram neighbour of `scala-js`
        val typoOnly =
          sql"""NOT (d.search @@ to_tsquery('simple', $text)
                     OR d.search @@ to_tsquery('english', $text))""".apply(
            (tsq, tsq)
          )
        facets =>
          val keyword =
            void"SELECT d.key, row_number() OVER (ORDER BY " |+|
              keywordScore |+| void" DESC, d.key) AS r, " |+| strongMatch |+|
              void" AS strong, " |+| typoOnly |+|
              void" AS typo FROM search_docs d WHERE " |+| keywordMatch |+|
              and(facets) |+|
              sql" ORDER BY r LIMIT $int4".apply(keywordCandidates)
          val semantic = vector match
            case None =>
              void"SELECT NULL::text, NULL::bigint, NULL::boolean, NULL::boolean WHERE false"
            case Some(v) =>
              val distance = sql"(d.embedding <=> ${text}::vector)".apply(v)
              // semantic neighbours of a short keyword are too loose to count
              // as strong matches on their own
              void"SELECT d.key, row_number() OVER (ORDER BY " |+| distance |+|
                void") AS r, false AS strong, false AS typo FROM search_docs d" |+|
                void" WHERE d.embedding IS NOT NULL AND " |+|
                (if semanticOnlyAsFallback then
                   void"NOT EXISTS (SELECT 1 FROM kw) AND "
                 else AppliedFragment.empty) |+|
                distance |+| sql" < $float8".apply(maxDistance) |+|
                and(facets) |+|
                sql" ORDER BY r LIMIT $int4".apply(semanticCandidates)
          void"WITH kw AS (" |+| keyword |+| void"), sem AS (" |+| semantic |+|
            sql"""), hits AS (
                    SELECT key, sum(1.0 / ($int4 + r)) AS score,
                           bool_or(strong) AS strong, bool_and(typo) AS typo
                    FROM (SELECT * FROM kw UNION ALL SELECT * FROM sem) u
                    GROUP BY key) """.apply(ranking.rrfK)

      private val docDeprecatedPenalty =
        void"(CASE WHEN d.deprecated THEN 0.3 ELSE 1 END)"

      private def textOrder(query: SearchQuery): AppliedFragment =
        query.sort match
          case Sort.Stars =>
            void" ORDER BY pr.stars DESC NULLS LAST, h.score DESC, d.key"
          case Sort.Recent =>
            void" ORDER BY d.latest_release_at DESC NULLS LAST, h.score DESC, d.key"
          case Sort.Relevance =>
            // an established project named exactly what was typed first:
            // among thousands of `zio-*` projects, `zio` means zio/zio
            // (only established ones, or a tiny repo called `json` would
            // beat every JSON library); then relevance, which stars only
            // nudge, so a precise match from a small project still beats a
            // vague one from a famous project. A typo match ranks below
            // real word matches, however popular; when the query itself is
            // misspelled all matches are typos
            sql""" ORDER BY (regexp_replace(lower(d.title), '[-_. ]+', ' ', 'g') = $text
                             AND (coalesce(pr.stars, 0) >= $int4
                                  OR d.dependents >= $int4)) DESC, h.score"""
              .apply(
                (
                  PackageRepo.nameWords(query.q),
                  ranking.exactNameStars,
                  ranking.exactNameDependents
                )
              ) |+|
              sql" * #${ranking.popularity} * #${ranking.scalaSupport} * "
                .apply(Void) |+|
              docDeprecatedPenalty |+|
              void" * (CASE WHEN h.typo THEN 0.3 ELSE 1 END) DESC, d.key"

      private def browseOrder(query: SearchQuery): AppliedFragment =
        query.sort match
          case Sort.Stars =>
            void" ORDER BY pr.stars DESC NULLS LAST, d.key"
          case Sort.Recent =>
            void" ORDER BY d.latest_release_at DESC NULLS LAST, d.key"
          case Sort.Relevance =>
            sql" ORDER BY #${ranking.popularity} * #${ranking.scalaSupport} * "
              .apply(Void) |+|
              docDeprecatedPenalty |+| void" DESC, d.key"

      private val page: SearchQuery => AppliedFragment = query =>
        sql" LIMIT $int4 OFFSET $int4".apply(
          (SearchQuery.pageSize, query.offset)
        )

      /** Platform and Scala version counts over the documents `d`. */
      private def facetCounts(
          prefix: AppliedFragment,
          fromClause: AppliedFragment,
          conds: List[AppliedFragment]
      ): IO[Facets] =
        def counts(column: String): IO[Map[String, Int]] =
          val af = prefix |+| void"SELECT f, count(*)::int4 " |+| fromClause |+|
            sql", unnest(d.#$column) AS f".apply(Void) |+| where(conds) |+|
            void" GROUP BY f"
          list(af, text *: int4).map(_.toMap)
        (counts("platforms"), counts("scala_versions")).parMapN(Facets.apply)

      private val docRow: Decoder[DocRow] =
        (text *: text *: isoTime.opt *: int4).to[DocRow]

      private val shownModules = 6

      /** The modules shown under each result: those whose name matches the
        * query first, then the one named like the project, then in
        * [[mainModuleOrder]]. The first is the one the result's title opens, so
        * `kyo` opens kyo-core, not whichever `kyo-*` module has most targets.
        */
      private def withModules(
          docs: List[DocRow],
          query: SearchQuery,
          nameTsq: Option[String]
      ): IO[List[SearchHit]] =
        if docs.isEmpty then IO.pure(Nil)
        else
          val nameMatch = nameTsq match
            case Some(t) =>
              sql"""(p.search @@ to_tsquery('simple', $text)
                     OR p.search @@ to_tsquery('english', $text))"""
                .apply((t, t))
            case None => void"false"
          val keys = docs.map(_.key)
          val af =
            movedGroups(Some(keys)) |+| void"SELECT " |+| columns(Void) |+|
              void", p.doc_key, p.n FROM (SELECT p.*, row_number() OVER (" |+|
              void"PARTITION BY p.doc_key ORDER BY " |+| nameMatch |+|
              void" DESC, p.deprecated, p.name = d.title DESC, " |+|
              mainModuleOrder |+|
              void""") AS pos,
                   count(*) OVER (PARTITION BY p.doc_key)::int4 AS n
                   FROM packages p JOIN search_docs d ON d.key = p.doc_key
                   WHERE """ |+|
              sql"p.doc_key = ANY(${IngestRepo.textList})".apply(keys) |+|
              and(facetConditions(query, "p")) |+|
              void") p LEFT JOIN projects pr ON pr.id = p.project_id" |+|
              sql" WHERE p.pos <= $int4 ORDER BY p.pos".apply(shownModules)
          list(af, summary *: text *: int4).map { rows =>
            val byDoc = rows.groupBy(_._2)
            docs.flatMap { d =>
              byDoc.get(d.key).flatMap(NonEmptyList.fromList).map { mods =>
                SearchHit(
                  d.key,
                  d.title,
                  d.released,
                  mods.map(_._1),
                  mods.head._3,
                  d.dependents
                )
              }
            }
          }

      def search(query: SearchQuery): IO[SearchResponse] =
        // a longer query adds cost and no relevance
        val q = query.copy(q = query.q.take(maxQueryLength))
        gate.fold(run(q, semantic = true))(_(run(q, _)))

      private def run(query: SearchQuery, semantic: Boolean) =
        val facets = facetConditions(query, "d")
        val docsFrom =
          void"FROM search_docs d LEFT JOIN projects pr ON pr.id = d.project_id"
        PackageRepo.tsQuery(query.q) match
          case None =>
            // browsing: no text, just facets and ordering
            (
              list(
                void"SELECT d.key, d.title, d.latest_release_at, d.dependents " |+|
                  docsFrom |+| where(facets) |+| browseOrder(query) |+|
                  page(query),
                docRow
              ).flatMap(withModules(_, query, None)),
              one(
                void"SELECT count(*)::int4 FROM search_docs d" |+|
                  where(facets),
                int4
              ),
              facetCounts(AppliedFragment.empty, void"FROM search_docs d", Nil)
            ).parMapN((hits, total, counts) =>
              SearchResponse(query, total, hits, counts)
            )

          case Some(tsq) =>
            val q = query.q.trim.toLowerCase
            embed
              .filter(_ => semantic)
              .traverse(_(query.q.trim))
              .handleError(_ => None)
              .flatMap { vector =>
                val hits = hitsCte(tsq, q, vector)
                val withFacets = hits(facets)
                // facet counts ignore the selected facets, so other options
                // keep showing how many they would add
                val withoutFacets = hits(Nil)
                val hitsFrom =
                  void"FROM hits h JOIN search_docs d ON d.key = h.key"
                // sorting by stars or date ignores relevance, so only strong
                // matches take part: a README mention is not enough
                val keep = query.sort match
                  case Sort.Relevance => Nil
                  case _              => List(void"h.strong")
                (
                  list(
                    withFacets |+| void"SELECT d.key, d.title, d.latest_release_at, d.dependents " |+|
                      hitsFrom |+|
                      void" LEFT JOIN projects pr ON pr.id = d.project_id" |+|
                      where(keep) |+| textOrder(query) |+| page(query),
                    docRow
                  ).flatMap(
                    withModules(_, query, Some(PackageRepo.nameQuery(tsq)))
                  ),
                  one(
                    withFacets |+|
                      void"SELECT count(*)::int4, (count(*) FILTER (WHERE h.strong))::int4 FROM hits h" |+|
                      where(keep),
                    int4 *: int4
                  ),
                  facetCounts(withoutFacets, hitsFrom, keep)
                ).parMapN { case (hits, (total, strong), counts) =>
                  SearchResponse(
                    query,
                    total,
                    hits,
                    counts,
                    related = total > 0 && strong == 0
                  )
                }
              }

      def get(ref: PackageRef): IO[Option[PackageSummary]] =
        list(
          void"SELECT " |+| columns(Void) |+| void" " |+| from(Void) |+|
            sql" WHERE p.group_id = $text AND p.name = $text"
              .apply((ref.groupId, ref.name)),
          summary
        ).map(_.headOption)

      def targetVersions(ref: PackageRef): IO[Map[Target, String]] =
        list(
          sql"""SELECT target, latest_version FROM artifacts
                WHERE group_id = $text AND name = $text""".apply(
            (ref.groupId, ref.name)
          ),
          text *: text
        ).map(_.flatMap((t, v) => Target.parseKey(t).map(_ -> v)).toMap)

      /** `WITH moved`: the groupIds of projects that republished their modules
        * under a newer one (`org.playframework` after `com.typesafe.play`), so
        * the old coordinates stop standing for the project: a module whose name
        * the project also publishes under another groupId, first released there
        * later. `docs` limits it to these search documents.
        */
      private def movedGroups(docs: Option[List[String]]): AppliedFragment =
        // grouping, not a self-join: sorting 36k text keys for a merge join
        // took 250 ms, and the home page asks three times
        val inDocs = docs.fold(AppliedFragment.empty)(keys =>
          sql" WHERE doc_key = ANY(${IngestRepo.textList})".apply(keys)
        )
        void"""WITH moved AS MATERIALIZED (
                 SELECT DISTINCT doc_key, group_id FROM (
                   SELECT p.doc_key, p.group_id, f.first_at,
                          max(f.first_at) OVER (PARTITION BY p.doc_key, p.name) AS newest
                   FROM packages p
                   CROSS JOIN LATERAL (SELECT min(a.first_release_at) AS first_at
                                       FROM artifacts a
                                       WHERE a.group_id = p.group_id AND a.name = p.name) f
                   WHERE (p.doc_key, p.name) IN (SELECT doc_key, name FROM packages""" |+|
          inDocs |+| void""" GROUP BY doc_key, name HAVING count(*) > 1)) x
                 WHERE first_at < newest) """

      /** A project's main module: not under a groupId the project moved away
        * from, then the one most depended on, then the one that is not a test,
        * benchmark or internal module, then the one with the most targets, then
        * the shortest name (`cats-core` rather than `cats-testkit`). Needs
        * [[movedGroups]].
        */
      private val mainModuleOrder =
        void"""(p.doc_key, p.group_id) IN (SELECT doc_key, group_id FROM moved),
               p.dependents DESC,
               p.name ~ '(^|-)(tests?|testing|testkit|bench|benchmarks?|jmh|internal|examples?|demo)($$|-)',
               cardinality(p.targets) DESC, length(p.name), p.name"""

      def newest(limit: Int): IO[List[PackageSummary]] =
        // one row per project (a tapir release publishes dozens of modules),
        // showing the main module among those in its latest release
        list(
          movedGroups(None) |+| void"SELECT " |+| columns(Void) |+|
            void""" FROM (SELECT p.*, row_number() OVER (PARTITION BY p.doc_key
                           ORDER BY p.in_latest DESC, """ |+| mainModuleOrder |+|
            void""") AS rn
                   FROM (SELECT p.*, p.latest_release_at >= max(p.latest_release_at)
                                  OVER (PARTITION BY p.doc_key) - interval '1 day' AS in_latest
                         FROM packages p
                         WHERE p.latest_release_at IS NOT NULL AND NOT p.deprecated
                           AND p.project_id IS NOT NULL) p) p
                 LEFT JOIN projects pr ON pr.id = p.project_id
                 WHERE p.rn = 1""" |+|
            sql" ORDER BY p.latest_release_at DESC LIMIT $int4".apply(limit),
          summary
        )

      def mostStarred(limit: Int): IO[List[PackageSummary]] =
        // one package per project, so a big repo does not fill the list
        list(
          movedGroups(None) |+| void"SELECT " |+| columns(Void) |+|
            void""" FROM (SELECT p.*, row_number() OVER (PARTITION BY p.project_id
                           ORDER BY """ |+| mainModuleOrder |+|
            void""") AS rn
                   FROM packages p
                   WHERE p.project_id IS NOT NULL AND NOT p.deprecated) p
                 JOIN projects pr ON pr.id = p.project_id
                 JOIN search_docs d ON d.key = p.project_id
                 -- a famous repo whose only Scala module is a 2.11 leftover
                 -- (neo4j) is not a Scala library worth featuring
                 WHERE p.rn = 1 AND pr.stars IS NOT NULL
                   AND ('3' = ANY(d.scala_versions) OR '2.13' = ANY(d.scala_versions)
                        OR d.platforms && '{sbt,mill}'::text[])
                 ORDER BY pr.stars DESC, p.project_id""" |+|
            sql" LIMIT $int4".apply(limit),
          summary
        )

      def newProjects(limit: Int): IO[List[NewProject]] =
        // a project's first release is the earliest of all its artifacts;
        // one whose dates are not all known yet is left out rather than
        // shown as new, and so is one whose repository is more than a year
        // older than that release (it moved to new Maven coordinates)
        list(
          movedGroups(None) |+| void"SELECT " |+| columns(Void) |+| void""", f.first_at
                 FROM (SELECT p.doc_key, min(a.first_release_at) AS first_at
                       FROM packages p
                       JOIN artifacts a ON a.group_id = p.group_id AND a.name = p.name
                       GROUP BY p.doc_key
                       HAVING bool_and(a.first_release_at IS NOT NULL)) f
                 JOIN (SELECT p.*, row_number() OVER (PARTITION BY p.doc_key
                         ORDER BY p.deprecated, """ |+| mainModuleOrder |+|
            void""") AS rn
                       FROM packages p) p ON p.doc_key = f.doc_key AND p.rn = 1
                 LEFT JOIN projects pr ON pr.id = p.project_id""" |+|
            sql" WHERE f.first_at >= now() - make_interval(days => $int4)"
              .apply(PackageRepo.newProjectDays) |+|
            void""" AND (pr.repo_created_at IS NULL
                          OR pr.repo_created_at >= f.first_at - interval '1 year')""" |+|
            sql" ORDER BY f.first_at DESC, p.doc_key LIMIT $int4".apply(limit),
          (summary *: isoTime).to[NewProject]
        )

      def releases(since: OffsetDateTime, limit: Int): IO[List[Release]] =
        val decoder =
          (text *: text *: text *: isoTime *: IngestRepo.textList *: project)
            .to[Release]
        list(
          sql"""SELECT r.group_id, r.name, r.version, r.released_at,
                       coalesce(p.targets, '{}'), #$projectColumns
                FROM releases r
                JOIN packages p ON p.group_id = r.group_id AND p.name = r.name
                LEFT JOIN projects pr ON pr.id = p.project_id
                WHERE r.released_at >= $timestamptz
                ORDER BY r.released_at DESC
                LIMIT $int4""".apply((since, limit)),
          decoder
        )
