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

package com.softinio.scalapkg.domain

import cats.data.NonEmptyList
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

/** A Maven `groupId` plus the artifact name without its Scala suffix, e.g.
  * `org.typelevel` / `cats-core`. All the `_3`, `_2.13`, `_sjs1_3`, … artifacts
  * of one library share a PackageRef.
  */
final case class PackageRef(groupId: String, name: String)
    derives Codec.AsObject:
  def path: String = s"/p/$groupId/$name"

/** Source repository of a package, with the stats deps.dev reports for it. `id`
  * is the deps.dev project key, e.g. `github.com/typelevel/cats`. `summary` is
  * a one-line description generated from the project's own text.
  *
  * @param scorecard
  *   OpenSSF Scorecard overall score, 0 to 10
  * @param topics
  *   GitHub topics; empty without a GitHub token
  */
final case class ProjectInfo(
    id: String,
    description: Option[String],
    stars: Option[Int],
    forks: Option[Int],
    openIssues: Option[Int],
    license: Option[String],
    homepage: Option[String],
    scorecard: Option[Double],
    topics: List[String] = Nil,
    summary: Option[String] = None
) derives Codec.AsObject:
  def url: String = s"https://$id"

  /** The line a result shows: the maintainers' own description, unless it is
    * missing or too short to say much ("Graphs for Everyone"), then the
    * generated summary.
    */
  def blurb: Option[String] =
    description
      .filter(_.length >= ProjectInfo.minDescription)
      .orElse(summary)
      .orElse(description)

  /** The line a result card shows: the generated summary, written to be scanned
    * (short, same shape for every project), else the description.
    */
  def cardLine: Option[String] = summary.orElse(description)

  /** The maintainers' own description, as a tooltip on a card that shows the
    * generated summary instead.
    */
  def cardTooltip: Option[String] =
    summary.flatMap(s => description.filter(_ != s))

object ProjectInfo:
  val minDescription = 30

/** What search, facets and ranking need about one package. Dates are ISO-8601
  * strings so the same type works in Scala.js without java.time.
  *
  * @param latestReleaseAt
  *   when `latestVersion` was published; None until deps.dev dates it
  * @param targets
  *   `Target.key` strings, e.g. `jvm:3` or `sjs1:2.13`
  */
final case class PackageSummary(
    groupId: String,
    name: String,
    latestVersion: String,
    latestReleaseAt: Option[String],
    targets: List[String],
    licenses: List[String],
    deprecated: Boolean,
    project: Option[ProjectInfo],
    // other indexed projects that depend on this package
    dependents: Int = 0
) derives Codec.AsObject:
  def ref: PackageRef = PackageRef(groupId, name)
  lazy val parsedTargets: List[Target] = targets.flatMap(Target.parseKey)
  def install: List[Snippet] =
    InstallSnippets.render(groupId, name, latestVersion, parsedTargets)

  /** The artifact used to stand in for the package when asking deps.dev about
    * it: the newest Scala version on the JVM if there is one.
    */
  def representative: Option[ArtifactVersion] =
    parsedTargets.sorted.headOption.map(t =>
      ArtifactVersion(groupId, t.artifactId(name), latestVersion)
    )

/** Result counts per facet value (`Platform.facet` values and Scala binary
  * versions), counted without the selected facets so each option shows how many
  * results it would add.
  */
final case class Facets(
    platforms: Map[String, Int],
    scalaVersions: Map[String, Int]
) derives Codec.AsObject

/** Result order. With a query, `Stars` and `Recent` keep only results that
  * match the project's own text; `Relevance` keeps all.
  */
enum Sort derives CanEqual:
  case Relevance, Stars, Recent

  def param: String = toString.toLowerCase

object Sort:
  def parse(s: String): Option[Sort] =
    values.find(_.param == s.trim.toLowerCase)

  given Codec[Sort] = Codec.from(
    Decoder.decodeString.emap(s => parse(s).toRight(s"unknown sort: $s")),
    Encoder.encodeString.contramap(_.param)
  )

/** A search request, shared by the server (query-string parsing, SQL) and the
  * search island (building the same URLs).
  *
  * @param platforms
  *   `Platform.facet` values, e.g. `jvm`, `js`
  * @param scalaVersions
  *   Scala binary versions, e.g. `3`, `2.13`
  * @param page
  *   1-based, at most `SearchQuery.maxPage`
  */
final case class SearchQuery(
    q: String,
    platforms: List[String],
    scalaVersions: List[String],
    sort: Sort,
    page: Int
) derives Codec.AsObject:

  def isEmpty: Boolean =
    q.trim.isEmpty && platforms.isEmpty && scalaVersions.isEmpty

  def offset: Int = (page - 1) * SearchQuery.pageSize

  /** Query-string pairs in a stable order, leaving out defaults. */
  def params: List[(String, String)] =
    Option.when(q.trim.nonEmpty)("q" -> q.trim).toList ++
      platforms.map("platform" -> _) ++
      scalaVersions.map("scala" -> _) ++
      Option.when(sort != Sort.Relevance)("sort" -> sort.param).toList ++
      Option.when(page > 1)("page" -> page.toString).toList

object SearchQuery:
  val pageSize = 20
  val maxPage = 50
  val maxQueryLength = 100

  val empty: SearchQuery = SearchQuery("", Nil, Nil, Sort.Relevance, 1)

  /** Builds a query from (possibly repeated) query-string params. Facet values
    * that are not known platforms or Scala versions are dropped.
    */
  def fromParams(params: Map[String, Seq[String]]): SearchQuery =
    def all(key: String): List[String] =
      params.getOrElse(key, Nil).toList.map(_.trim).filter(_.nonEmpty)
    SearchQuery(
      q = all("q").headOption.getOrElse("").take(maxQueryLength),
      platforms = all("platform").filter(Platform.facets.contains).distinct,
      scalaVersions =
        all("scala").filter(ScalaVersions.supported.contains).distinct,
      sort =
        all("sort").headOption.flatMap(Sort.parse).getOrElse(Sort.Relevance),
      page = all("page").headOption
        .flatMap(_.toIntOption)
        .filter(p => p >= 1 && p <= maxPage)
        .getOrElse(1)
    )

/** One search result: a project, or for packages without a known source
  * repository a Maven groupId, with the modules worth showing (those whose name
  * matches the query first). `key` is the project id, or `maven:<groupId>`;
  * `moduleCount` counts every module that passes the facets, including those
  * not listed; `dependents` counts other indexed projects depending on any of
  * its modules.
  */
final case class SearchHit(
    key: String,
    title: String,
    latestReleaseAt: Option[String],
    packages: NonEmptyList[PackageSummary],
    moduleCount: Int,
    dependents: Int = 0
) derives Codec.AsObject:
  /** The module the result's title opens. */
  def primary: PackageSummary = packages.head

  /** Every module of a result belongs to the same project. */
  def project: Option[ProjectInfo] = primary.project

  def targets: List[Target] =
    packages.toList.flatMap(_.parsedTargets).distinct.sorted

  def licenses: List[String] = packages.toList.flatMap(_.licenses).distinct

/** `total` counts results (projects), not modules.
  *
  * @param related
  *   no result is about the query (its words appear only in READMEs, module
  *   names or nearby embeddings), so the results are related, not matches
  */
final case class SearchResponse(
    query: SearchQuery,
    total: Int,
    results: List[SearchHit],
    facets: Facets,
    related: Boolean = false
) derives Codec.AsObject

/** One published version, for the feed and the releases API. `releasedAt` is
  * ISO-8601 UTC; `targets` are the package's, not only this version's.
  */
final case class Release(
    groupId: String,
    name: String,
    version: String,
    releasedAt: String,
    targets: List[String],
    project: Option[ProjectInfo]
) derives Codec.AsObject:
  def ref: PackageRef = PackageRef(groupId, name)

/** `GET /api/v1/releases`; `since` is the cut-off used, ISO-8601 UTC. */
final case class ReleasesResponse(since: String, releases: List[Release])
    derives Codec.AsObject

/** A project whose first release is recent, shown by its main module.
  * `firstReleaseAt` is the earliest release of any of its artifacts, ISO-8601.
  */
final case class NewProject(
    pkg: PackageSummary,
    firstReleaseAt: String
) derives Codec.AsObject

/** The body of every JSON API and rate-limit error response. */
final case class ApiError(error: String) derives Codec.AsObject

/** `GET /api/v1/packages/{groupId}/{name}`. */
final case class PackageResponse(
    summary: PackageSummary,
    install: List[Snippet]
) derives Codec.AsObject

object PackageResponse:
  def of(p: PackageSummary): PackageResponse = PackageResponse(p, p.install)

/** Date display shared by the server pages and the islands. */
object Dates:
  private val months: Vector[String] =
    Vector.from("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split(' '))

  /** `2024-05-28T18:00:53Z` -> `May 28, 2024`; anything else as it is. */
  def short(iso: String): String =
    iso.take(10).split('-') match
      case Array(y, m, d) =>
        m.toIntOption
          .flatMap(i => months.lift(i - 1))
          .fold(iso)(mon => s"$mon ${d.toIntOption.getOrElse(d)}, $y")
      case _ => iso
