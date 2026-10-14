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

package com.softinio.scalapkg.html

import com.softinio.scalapkg.db.*
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.ingest.Reports
import com.softinio.scalapkg.style.Styles
import io.circe.syntax.*
import scalatags.Text.all.*
import scalatags.Text.tags2.aside
import scalatags.Text.tags2.section

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.concurrent.duration.FiniteDuration

/** Server-rendered HTML for `PublicRoutes`: the home page (landing lists,
  * search results or a `/p/...` deep link), the search results fragment the
  * search island swaps in, and the about and 404 pages.
  */
/** What a visitor typed into the report form on `/status`, kept to show it
  * again when it is refused.
  */
final case class ReportForm(
    group: String,
    artifact: String,
    reason: String,
    note: String
)

object ReportForm:
  val empty: ReportForm = ReportForm("", "", ReportReason.Missing.param, "")

object Pages:

  private def enc(s: String) = URLEncoder.encode(s, StandardCharsets.UTF_8)

  def searchHref(q: SearchQuery): String =
    q.params match
      case Nil    => "/"
      case params =>
        params.map((k, v) => s"$k=${enc(v)}").mkString("/?", "&", "")

  def formatCount(n: Int): String =
    if n >= 1000 then f"${n / 1000.0}%.1fk".replace(".0k", "k") else n.toString

  // ------------------------------------------------------------- home/search

  private def hero(query: SearchQuery): Frag =
    section(
      cls := "bg-gradient-to-br from-ink to-brand text-white",
      div(
        cls := "mx-auto max-w-6xl px-4 py-12 sm:px-6 sm:py-16",
        p(
          cls := "font-mono text-xs font-semibold uppercase tracking-wider text-brand-light",
          "Scala package index"
        ),
        h1(
          cls := "mt-2 font-display text-3xl font-bold tracking-tight sm:text-5xl",
          "Find Scala libraries"
        ),
        p(
          cls := "mt-3 max-w-2xl text-slate-200",
          "Every Scala library on Maven Central for JVM, Scala.js, Scala Native, sbt and Mill, kept fresh from the Maven index and deps.dev."
        ),
        form(
          action := "/",
          method := "get",
          role := "search",
          cls := "relative mt-8 max-w-3xl",
          attr("data-island") := "search",
          span(
            cls := "pointer-events-none absolute left-4 top-1/2 -translate-y-1/2 text-slate-400",
            aria.hidden := "true",
            "⌕"
          ),
          input(
            `type` := "search",
            name := "q",
            id := "search-input",
            value := query.q,
            placeholder := "Search packages, e.g. json, http, cats-effect…",
            attr("autocomplete") := "off",
            aria.label := "Search packages",
            cls := Styles.searchInput
          ),
          query.platforms.map(p =>
            input(`type` := "hidden", name := "platform", value := p)
          ),
          query.scalaVersions.map(s =>
            input(`type` := "hidden", name := "scala", value := s)
          ),
          kbdHint
        )
      )
    )

  private def kbdHint: Frag =
    span(
      cls := "pointer-events-none absolute right-4 top-1/2 hidden -translate-y-1/2 rounded border border-slate-300 px-1.5 font-mono text-xs text-slate-400 sm:inline dark:border-slate-700",
      "/"
    )

  /** The lists shown on the home page when there is no query. */
  final case class Landing(
      newProjects: List[NewProject],
      newest: List[PackageSummary],
      starred: List[PackageSummary]
  )

  /** What the home page shows: the landing lists, with a package's drawer open
    * on a /p/... page, or search results.
    */
  enum Home:
    case Lists(lists: Landing, open: Option[PackageSummary] = None)
    case Results(response: SearchResponse)

  /** The full page for `/` and `/p/{groupId}/{name}`; search result pages are
    * marked `noindex`.
    */
  def home(view: Home, baseUrl: String): String =
    val (query, drawerFor) = view match
      case Home.Lists(_, open) => (SearchQuery.empty, open)
      case Home.Results(r)     => (r.query, None)
    val meta = drawerFor match
      case Some(p) =>
        PageMeta(
          s"${p.groupId}:${p.name}",
          p.project
            .flatMap(_.blurb)
            .getOrElse(s"${p.name} ${p.latestVersion}, a Scala library"),
          p.ref.path
        )
      case None =>
        PageMeta(
          if query.isEmpty then Layout.siteName else s"${query.q} – search",
          "Search every Scala library on Maven Central, with live details from deps.dev.",
          searchHref(query),
          noindex = !query.isEmpty
        )
    Layout.page(meta, baseUrl, drawerFor)(
      hero(query),
      div(
        id := "results",
        cls := "mx-auto max-w-6xl px-4 py-10 sm:px-6",
        aria.live := "polite",
        view match
          case Home.Results(r)      => resultsFrag(r)
          case Home.Lists(lists, _) => landing(lists)
      ),
      // no-JS and crawler fallback for /p/... deep links
      drawerFor.map(p =>
        tag("noscript")(
          div(
            cls := "mx-auto max-w-6xl px-4 pb-10 sm:px-6",
            packageSummaryFrag(p)
          )
        )
      )
    )

  private def landing(lists: Landing): Frag =
    div(
      cls := "grid gap-10 lg:grid-cols-3",
      section(
        div(
          cls := "flex items-baseline justify-between",
          h2(cls := Styles.sectionHeading, "New projects"),
          a(href := "/new", cls := s"text-sm ${Styles.link}", "All new →")
        ),
        newProjectList(lists.newProjects)
      ),
      section(
        div(
          cls := "flex items-baseline justify-between",
          h2(cls := Styles.sectionHeading, "New releases"),
          a(
            href := "/?sort=recent",
            cls := s"text-sm ${Styles.link}",
            "All recent →"
          )
        ),
        compactList(
          lists.newest.map(p =>
            p -> p.latestReleaseAt.map(d =>
              div(cls := "font-mono text-xs text-slate-500", Dates.short(d))
            )
          )
        )
      ),
      section(
        div(
          cls := "flex items-baseline justify-between",
          h2(cls := Styles.sectionHeading, "Most starred"),
          a(
            href := "/?sort=stars",
            cls := s"text-sm ${Styles.link}",
            "All by stars →"
          )
        ),
        compactList(
          lists.starred.map(p =>
            p -> p.project
              .flatMap(_.stars)
              .map(s => div(cls := Styles.stat, s"★ ${formatCount(s)}"))
          )
        )
      )
    )

  private def newProjectList(projects: List[NewProject]): Frag =
    if projects.isEmpty then
      p(
        cls := "mt-4 text-sm text-slate-500",
        s"No Scala project first released in the last ${PackageRepo.newProjectDays} days yet."
      )
    else
      compactList(
        projects.map(n =>
          n.pkg -> Some(
            div(
              cls := "font-mono text-xs text-slate-500",
              title := "First released",
              Dates.short(n.firstReleaseAt)
            )
          )
        )
      )

  /** `/new`: every project first released within `newProjectDays`, a page of
    * `SearchQuery.pageSize` at a time.
    */
  def newProjects(
      projects: List[NewProject],
      page: Int,
      baseUrl: String
  ): String =
    def hrefFor(n: Int) = if n == 1 then "/new" else s"/new?page=$n"
    val pages = pageCount(projects.size)
    val shown = projects
      .drop((page - 1) * SearchQuery.pageSize)
      .take(SearchQuery.pageSize)
    Layout.page(
      PageMeta(
        if page == 1 then "New projects" else s"New projects, page $page",
        s"Scala projects first released on Maven Central in the last ${PackageRepo.newProjectDays} days.",
        hrefFor(page)
      ),
      baseUrl
    )(
      div(
        cls := "mx-auto max-w-3xl px-4 py-12 sm:px-6",
        h1(
          cls := "font-display text-3xl font-bold tracking-tight text-slate-900 dark:text-white",
          "New projects"
        ),
        p_(
          cls := Styles.bodyText,
          s"Scala projects first released on Maven Central in the last ${PackageRepo.newProjectDays} days, newest first. Projects that only moved to new Maven coordinates are left out."
        ),
        newProjectList(shown),
        pageLinks(page, pages, hrefFor, swap = false)
      )
    )

  /** One line per package; its side cell is shown under the version. */
  private def compactList(rows: List[(PackageSummary, Option[Frag])]): Frag =
    if rows.isEmpty then
      p(
        cls := "mt-4 text-sm text-slate-500",
        "The index is still loading. Check back soon."
      )
    else
      ul(
        cls := "mt-4 divide-y divide-slate-200 rounded-xl border border-slate-200 dark:divide-slate-800 dark:border-slate-800",
        rows.map((p, side) =>
          li(
            cls := "flex items-center justify-between gap-4 px-4 py-3",
            div(
              cls := "min-w-0",
              packageLink(
                p,
                "font-semibold text-slate-900 hover:text-brand dark:text-white dark:hover:text-brand-light"
              ),
              p.project.flatMap(pr =>
                pr.cardLine.map(d =>
                  div(
                    cls := "truncate text-sm text-slate-500 dark:text-slate-400",
                    pr.cardTooltip.map(title := _),
                    d
                  )
                )
              )
            ),
            div(
              cls := "shrink-0 text-right",
              div(
                cls := "font-mono text-xs text-slate-600 dark:text-slate-300",
                p.latestVersion
              ),
              side
            )
          )
        )
      )

  /** A link to the package's `/p/...` page that carries the package as JSON in
    * `data-pkg`, which the drawer island reads instead of asking the server.
    */
  def packageLink(
      p: PackageSummary,
      classes: String,
      label: Option[String] = None
  ): Frag =
    a(
      href := p.ref.path,
      attr("data-pkg") := p.asJson.noSpaces,
      cls := classes,
      label.getOrElse(p.name)
    )

  /** The swappable search results region: facets, sort, list, pager.
    * `data-total` and each card's `data-key` are what the search island records
    * about a search (see `SearchEvent`).
    */
  def resultsFrag(r: SearchResponse): Frag =
    val q = r.query
    div(
      cls := "grid gap-8 lg:grid-cols-[14rem_1fr]",
      attr("data-total") := r.total,
      aside(
        cls := "space-y-6",
        facetGroup(
          "Platform",
          Platform.facets,
          r.facets.platforms,
          q.platforms,
          Platform.facetLabel
        )(fs => q.copy(platforms = fs, page = 1)),
        facetGroup(
          "Scala version",
          ScalaVersions.supported,
          r.facets.scalaVersions,
          q.scalaVersions,
          v => s"Scala $v"
        )(vs => q.copy(scalaVersions = vs, page = 1))
      ),
      section(
        div(
          cls := "flex flex-wrap items-center justify-between gap-3",
          p(
            cls := "text-sm text-slate-600 dark:text-slate-400",
            strong(cls := "text-slate-900 dark:text-white", f"${r.total}%,d"),
            if r.total == 1 then " project" else " projects",
            if q.q.trim.nonEmpty then
              frag(
                " for ",
                strong(
                  cls := "text-slate-900 dark:text-white",
                  s"“${q.q.trim}”"
                )
              )
            else frag()
          ),
          div(
            cls := "flex items-center gap-2 text-sm",
            span(cls := "text-slate-500", "Sort"),
            Sort.values.toList.map(s =>
              a(
                href := searchHref(q.copy(sort = s, page = 1)),
                attr("data-swap").empty,
                cls := (if s == q.sort then Styles.facetOn
                        else Styles.facetOff),
                s match
                  case Sort.Relevance => "Best match"
                  case Sort.Stars     => "Stars"
                  case Sort.Recent    => "Recent"
              )
            )
          )
        ),
        if r.related then
          p(
            cls := "mt-4 rounded-lg border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-900 dark:border-amber-900/50 dark:bg-amber-950/40 dark:text-amber-200",
            "No project is described as ",
            strong(s"“${q.q.trim}”"),
            ". These mention it or are related."
          )
        else frag(),
        if r.results.isEmpty then
          div(
            cls := s"mt-6 ${Styles.panel} text-center text-slate-600 dark:text-slate-400",
            if q.sort != Sort.Relevance && q.q.trim.nonEmpty then
              frag(
                "No project is described as ",
                strong(s"“${q.q.trim}”"),
                ". ",
                a(
                  href := searchHref(q.copy(sort = Sort.Relevance, page = 1)),
                  attr("data-swap").empty,
                  cls := "text-brand underline dark:text-brand-light",
                  "Best match"
                ),
                " also shows related projects."
              )
            else "Nothing matches. Try fewer words or remove a filter."
          )
        else ul(cls := "mt-6 space-y-3", r.results.map(resultCard)),
        pager(r)
      )
    )

  private def facetGroup(
      title: String,
      values: List[String],
      counts: Map[String, Int],
      selected: List[String],
      label: String => String
  )(select: List[String] => SearchQuery): Frag =
    div(
      h3(cls := Styles.eyebrow, title),
      div(
        cls := "mt-2 flex flex-wrap gap-2 lg:flex-col lg:items-start",
        values.filter(v => counts.contains(v) || selected.contains(v)).map {
          v =>
            val on = selected.contains(v)
            a(
              href := searchHref(
                select(if on then selected.filterNot(_ == v) else selected :+ v)
              ),
              attr("data-swap").empty,
              cls := (if on then Styles.facetOn else Styles.facetOff),
              aria.pressed := on.toString,
              label(v),
              span(
                cls := "font-mono text-xs opacity-70",
                formatCount(counts.getOrElse(v, 0))
              )
            )
        }
      )
    )

  /** Badge labels: Scala versions newest first, then the non-JVM platforms. */
  def targetSummary(targets: List[Target]): List[String] =
    val scala = targets
      .map(_.scalaVersion)
      .distinct
      .sortBy(ScalaVersions.supported.indexOf)
      .map(v => s"Scala $v")
    val platforms = targets.map(_.platform).distinct.collect {
      case p if p != Platform.Jvm => p.label
    }
    scala ++ platforms

  /** One project: its name opens the main module, the chips below open the
    * modules that matched (or its main ones).
    */
  private def resultCard(hit: SearchHit): Frag =
    val primary = hit.primary
    val source =
      hit.project
        .map(_.id.stripPrefix("github.com/"))
        .getOrElse(primary.groupId)
    val listModules =
      hit.packages.size > 1 || primary.name != hit.title
    li(
      cls := Styles.card,
      attr("data-key") := hit.key,
      div(
        cls := "flex items-start justify-between gap-4",
        div(
          cls := "min-w-0 flex-1",
          div(
            cls := "flex flex-wrap items-baseline gap-x-2",
            packageLink(
              primary,
              "font-display text-lg font-bold text-slate-900 hover:text-brand dark:text-white dark:hover:text-brand-light",
              Some(hit.title)
            ),
            span(cls := "truncate font-mono text-xs text-slate-500", source)
          ),
          hit.project.flatMap(pr =>
            pr.cardLine.map(d =>
              p_(
                cls := "mt-1 line-clamp-1 text-sm text-slate-600 dark:text-slate-400",
                pr.cardTooltip.map(title := _),
                d
              )
            )
          )
        ),
        div(
          cls := "flex shrink-0 items-center gap-3",
          if hit.dependents > 0 then
            span(
              cls := Styles.stat,
              attr(
                "title"
              ) := s"${hit.dependents} other indexed projects depend on it",
              s"used by ${formatCount(hit.dependents)}"
            )
          else frag(),
          hit.project
            .flatMap(_.stars)
            .map(s =>
              span(
                cls := Styles.stat,
                aria.label := s"$s stars",
                s"★ ${formatCount(s)}"
              )
            ),
          span(
            cls := "font-mono text-sm font-semibold text-slate-800 dark:text-slate-200",
            primary.latestVersion
          )
        )
      ),
      if listModules then
        div(
          cls := "mt-3 flex flex-wrap items-center gap-1.5",
          hit.packages.toList.map(m => packageLink(m, Styles.moduleChip)),
          if hit.moduleCount > hit.packages.size then
            span(
              cls := "text-xs text-slate-500",
              s"+${hit.moduleCount - hit.packages.size} more"
            )
          else frag()
        )
      else frag(),
      div(
        cls := "mt-3 flex flex-wrap items-center gap-1.5",
        targetSummary(hit.targets).map(t => span(cls := Styles.chip, t)),
        hit.licenses.take(2).map(l => span(cls := Styles.mutedChip, l)),
        hit.project.toList
          .flatMap(_.topics.take(4))
          .map(t => span(cls := Styles.topicChip, t)),
        if hit.packages.forall(_.deprecated) then
          span(cls := Styles.warnChip, "deprecated")
        else frag(),
        hit.latestReleaseAt.map(d =>
          span(
            cls := "ml-auto font-mono text-xs text-slate-500",
            Dates.short(d)
          )
        )
      )
    )

  // scalatags has no p() shorthand once `p` is shadowed by a parameter
  private val p_ = scalatags.Text.all.p

  private def pager(r: SearchResponse): Frag =
    val q = r.query
    pageLinks(
      q.page,
      math.min(SearchQuery.maxPage, pageCount(r.total)),
      n => searchHref(q.copy(page = n)),
      swap = true
    )

  def pageCount(total: Int): Int =
    (total + SearchQuery.pageSize - 1) / SearchQuery.pageSize

  /** Previous / "Page n of m" / Next; with `swap` the search island swaps the
    * linked page in.
    */
  private def pageLinks(
      page: Int,
      pages: Int,
      hrefFor: Int => String,
      swap: Boolean
  ): Frag =
    def link(n: Int, label: String) =
      a(
        href := hrefFor(n),
        if swap then attr("data-swap").empty else frag(),
        cls := Styles.secondaryButton,
        label
      )
    if pages <= 1 then frag()
    else
      tag("nav")(
        aria.label := "Pagination",
        cls := "mt-8 flex items-center justify-between",
        if page > 1 then link(page - 1, "← Previous") else span(),
        span(
          cls := "font-mono text-sm text-slate-500",
          s"Page $page of $pages"
        ),
        if page < pages then link(page + 1, "Next →") else span()
      )

  /** Stored summary, shown inside <noscript> on /p/... deep links. */
  def packageSummaryFrag(p: PackageSummary): Frag =
    div(
      cls := Styles.panel,
      h2(cls := Styles.sectionHeading, s"${p.groupId}:${p.name}"),
      p.project
        .flatMap(_.blurb)
        .map(d => p_(cls := "mt-2 text-slate-600 dark:text-slate-400", d)),
      p_(cls := "mt-2 font-mono text-sm", s"Latest ${p.latestVersion}"),
      p.install
        .map(s =>
          div(
            cls := "mt-4",
            h3(cls := Styles.eyebrow, s.tool),
            tag("pre")(cls := s"mt-1 ${Styles.code}", s.code)
          )
        )
    )

  // ------------------------------------------------------------------ other

  /** `/about`, which also documents the JSON API and MCP tools: update it with
    * `ApiRoutes` and `Mcp`.
    */
  def about(baseUrl: String): String =
    Layout.page(
      PageMeta(
        "About",
        "How Scala Packages works, its JSON API and MCP server.",
        "/about"
      ),
      baseUrl
    )(
      div(
        cls := "mx-auto max-w-3xl space-y-10 px-4 py-12 sm:px-6",
        section(
          h1(
            cls := "font-display text-3xl font-bold tracking-tight text-slate-900 dark:text-white",
            "About"
          ),
          p_(
            cls := Styles.bodyText,
            "Scala Packages lists every Scala library published to Maven Central. An artifact counts as Scala when its name ends in a Scala version suffix such as ",
            code("_3"),
            ", ",
            code("_2.13"),
            " or ",
            code("_sjs1_3"),
            ". All targets of one library are grouped into one package, and search shows one result per project with its matching packages."
          ),
          p_(
            cls := Styles.bodyText,
            "See also ",
            a(
              href := "https://index.scala-lang.org",
              cls := Styles.link,
              "Scaladex"
            ),
            ", the Scala Center's official index of Scala libraries."
          )
        ),
        section(
          h2(cls := Styles.sectionHeading, "Where the data comes from"),
          ul(
            cls := s"${Styles.bodyText} list-disc space-y-2 pl-6",
            li(
              "The ",
              strong("Maven Central nexus index"),
              " tells us which Scala artifacts exist and their versions. It is read in full once, then through its incremental updates, which Central publishes about every two weeks."
            ),
            li(
              strong("deps.dev"),
              " supplies release dates, licences, the source repository, GitHub stars and OpenSSF Scorecard. Recently active packages are also checked daily for new versions, so releases show up within a day."
            ),
            li(
              "The package panel loads versions, dependencies and security advisories live from the deps.dev API in your browser. “Used by” counts the other indexed projects whose latest release depends on a package (tests included), read from deps.dev’s dependency data; it also helps rank search results."
            ),
            li(
              "Each project also gets a one-line summary and search keywords, written by an AI model (Claude) from the project’s own description, GitHub topics and README. Result cards show the summary (hover it for the project’s own description); the package panel shows both and marks the summary as generated."
            )
          ),
          p_(
            cls := "mt-4 text-sm text-slate-600 dark:text-slate-400",
            "Data generated by deps.dev is available under CC-BY 4.0."
          )
        ),
        section(
          id := "api",
          h2(cls := Styles.sectionHeading, "JSON API"),
          p_(
            cls := Styles.bodyText,
            "Read-only, no key needed, CORS enabled. Please keep it under 60 requests a minute."
          ),
          endpoint(
            "GET /api/v1/search?q=json&platform=js&scala=3&sort=stars&page=1",
            "Search packages. platform: jvm, js, native, sbt, mill. sort: relevance, stars, recent."
          ),
          endpoint(
            "GET /api/v1/packages/{groupId}/{name}",
            "One package: targets, latest version, licences, project stats and install snippets."
          ),
          endpoint(
            "GET /api/v1/releases?since=2026-01-01T00:00:00Z&limit=100",
            "New versions since a date, newest first (default: the last 7 days)."
          ),
          endpoint(
            "GET /api/v1/new-projects?limit=20",
            s"Projects first released in the last ${PackageRepo.newProjectDays} days, newest first, with their first release date."
          ),
          endpoint(
            "GET /api/v1/status",
            "When the index and each background job last ran, and what they still have to do."
          ),
          endpoint("GET /feed.xml", "Atom feed of new releases.")
        ),
        section(
          id := "mcp",
          h2(cls := Styles.sectionHeading, "MCP server"),
          p_(
            cls := Styles.bodyText,
            "AI assistants can search the index through the Model Context Protocol at ",
            code(s"$baseUrl/mcp"),
            " (Streamable HTTP). Tools: search_packages, get_package, recent_releases and install_snippet."
          ),
          tag("pre")(
            cls := s"mt-4 ${Styles.code}",
            s"claude mcp add --transport http scalapkg $baseUrl/mcp"
          )
        ),
        section(
          id := "privacy",
          h2(cls := Styles.sectionHeading, "What we record"),
          p_(
            cls := Styles.bodyText,
            "To improve search results, the site records the searches made on it and through the MCP server, and which results were opened, followed or had an install snippet copied, with their position in the list. Nothing identifies you: no IP address, cookie or browser details are kept, only a random id per browser tab so that rephrased searches can be told apart. Records are deleted after 90 days. Page views are counted with GoatCounter, which does not track visitors either."
          ),
          p_(
            cls := Styles.bodyText,
            "Packages reported on the status page are kept with their note, but nothing about who reported them, and are deleted after 90 days."
          )
        )
      )
    )

  // ----------------------------------------------------------------- status

  private val dayFormat =
    DateTimeFormatter.ofPattern("MMM d, yyyy", Locale.ENGLISH)

  private def day(t: OffsetDateTime): String =
    t.atZoneSameInstant(ZoneOffset.UTC).format(dayFormat)

  /** "5 min ago", "3 h ago", "4 days ago". */
  def ago(t: OffsetDateTime, now: Instant): String =
    val d = java.time.Duration.between(t.toInstant, now)
    val minutes = d.toMinutes
    if minutes < 1 then "just now"
    else if minutes < 60 then s"$minutes min ago"
    else if minutes < 48 * 60 then s"${minutes / 60} h ago"
    else s"${d.toDays} days ago"

  // the jobs the page shows, in order, with what each one does; jobs not
  // set up on this server have no row in job_runs and are left out
  private val jobInfo: List[(String, String, String)] = List(
    (
      "index-sync",
      "Maven Central index",
      "Reads the index updates Central publishes about every two weeks; checked daily."
    ),
    (
      "freshness",
      "New versions",
      "Asks deps.dev daily for newer versions of packages released in the last year."
    ),
    (
      "enrich",
      "Package details",
      "Release dates, licences and source repositories from deps.dev, every 15 minutes."
    ),
    (
      "first-releases",
      "First release dates",
      "Dates artifacts the index gave no publish time for, from deps.dev."
    ),
    ("deps", "Dependencies", "Who depends on what, from deps.dev, hourly."),
    (
      "github",
      "GitHub",
      "Topics, READMEs and creation dates, weekly per repository."
    ),
    (
      "embed",
      "Semantic search",
      "Embeddings for search documents whose text changed."
    ),
    (
      "summaries",
      "Project summaries",
      "One-line summaries and keywords written by Claude."
    ),
    (
      "reports",
      "Package reports",
      "Checks queued reports against deps.dev and Maven Central."
    )
  )

  private def backlog(job: String, s: SiteStatus): Option[String] =
    val b = s.backlog
    job match
      case "enrich" if b.enrichPending > 0 =>
        Some(s"${b.enrichPending} artifacts waiting")
      case "deps" if b.depsWaiting > 0 =>
        Some(s"${b.depsWaiting} packages waiting")
      case "embed" if b.unembedded > 0 =>
        Some(s"${b.unembedded} documents waiting")
      case "summaries" => Some(s"${b.summarised} of ${b.projects} projects")
      case "reports" if s.reports.pending > 0 =>
        Some(s"${s.reports.pending} reports queued")
      case _ => None

  private def jobRow(
      run: JobRun,
      label: String,
      desc: String,
      s: SiteStatus,
      now: Instant
  ): Frag =
    val running =
      run.startedAt.exists(st => run.finishedAt.forall(_.isBefore(st)))
    val failed = run.error.isDefined &&
      run.finishedAt.exists(f => run.okAt.forall(_.isBefore(f)))
    li(
      cls := "flex flex-col gap-1 py-4 sm:flex-row sm:items-start sm:justify-between sm:gap-6",
      div(
        cls := "min-w-0",
        p_(cls := "font-semibold text-slate-900 dark:text-white", label),
        p_(cls := "mt-0.5 text-sm text-slate-600 dark:text-slate-400", desc),
        if failed then
          p_(
            cls := "mt-1 break-words text-sm text-accent",
            "Last run failed: ",
            run.error.getOrElse("")
          )
        else frag()
      ),
      div(
        cls := "flex shrink-0 flex-wrap items-center gap-2 sm:justify-end",
        if running then span(cls := Styles.chip, "running")
        else frag(),
        backlog(run.job, s).map(span(cls := Styles.mutedChip, _)),
        span(
          cls := Styles.stat,
          run.okAt.fold("no successful run yet")(t => s"ok ${ago(t, now)}")
        )
      )
    )

  // Central's directory of index files, each chunk with its date
  private val centralIndex = "https://repo1.maven.org/maven2/.index/"

  private def indexPanel(index: Option[IndexState], now: Instant): Frag =
    div(
      cls := s"mt-6 ${Styles.panel}",
      index match
        case None =>
          p_(
            cls := "text-slate-700 dark:text-slate-300",
            "The Maven Central index has not been read yet."
          )
        case Some(i) =>
          val nextUpdate = i.remotePublishedAt.map(_.plusDays(14))
          frag(
            div(
              cls := "flex flex-wrap items-center gap-2",
              p_(
                cls := "font-semibold text-slate-900 dark:text-white",
                s"Index update ${i.lastChunk} read ${ago(i.updatedAt, now)}"
              ),
              if i.caughtUp then span(cls := Styles.chip, "up to date")
              else span(cls := Styles.warnChip, "newer update waiting")
            ),
            ul(
              cls := "mt-3 space-y-1 text-sm text-slate-600 dark:text-slate-400",
              i.remoteLastChunk.map(n =>
                li(
                  "Central's latest is ",
                  a(
                    href := centralIndex,
                    cls := Styles.link,
                    attr("rel") := "nofollow",
                    s"update $n"
                  ),
                  i.remotePublishedAt.fold(".")(t =>
                    s", published on ${day(t)}."
                  )
                )
              ),
              i.remoteCheckedAt.map(t =>
                li(s"Central was last asked ${ago(t, now)}.")
              ),
              if !i.caughtUp then
                li("The newer update is read at the next daily check.")
              else frag(),
              nextUpdate.map(t =>
                li(
                  if t.toInstant.isAfter(now) then
                    s"The next update is expected around ${day(t)}."
                  else "The next update is due any day now."
                )
              )
            )
          )
    )

  private val inputClass =
    "mt-1 w-full rounded-lg border border-slate-300 bg-white px-3 py-2 text-slate-900 focus:border-brand focus:outline-none focus:ring-2 focus:ring-brand-light/40 dark:border-slate-700 dark:bg-slate-950 dark:text-slate-100"

  private val labelClass =
    "block text-sm font-medium text-slate-700 dark:text-slate-300"

  private def reportForm(form: ReportForm, error: Option[String]): Frag =
    val reasons = List(
      ReportReason.Missing -> "It is missing",
      ReportReason.Version -> "Its latest version is out of date",
      ReportReason.Other -> "Something else is wrong"
    )
    tag("form")(
      attr("method") := "post",
      attr("action") := "/status/report",
      cls := s"mt-6 space-y-4 ${Styles.panel}",
      error.map(e =>
        p_(
          cls := "rounded-lg border border-accent/30 bg-accent/10 px-3 py-2 text-sm text-accent",
          attr("role") := "alert",
          e
        )
      ),
      div(
        cls := "grid gap-4 sm:grid-cols-2",
        div(
          label(cls := labelClass, attr("for") := "report-group", "groupId"),
          input(
            id := "report-group",
            name := "group",
            cls := inputClass,
            attr("required").empty,
            attr("maxlength") := "100",
            attr("autocomplete") := "off",
            attr("spellcheck") := "false",
            placeholder := "dev.zio",
            value := form.group
          )
        ),
        div(
          label(
            cls := labelClass,
            attr("for") := "report-artifact",
            "artifactId or package name"
          ),
          input(
            id := "report-artifact",
            name := "artifact",
            cls := inputClass,
            attr("required").empty,
            attr("maxlength") := "100",
            attr("autocomplete") := "off",
            attr("spellcheck") := "false",
            placeholder := "zio-http_3 or zio-http",
            value := form.artifact
          )
        )
      ),
      div(
        label(
          cls := labelClass,
          attr("for") := "report-reason",
          "What is wrong"
        ),
        select(
          id := "report-reason",
          name := "reason",
          cls := inputClass,
          reasons.map((r, text) =>
            option(
              value := r.param,
              if form.reason == r.param then attr("selected").empty else frag(),
              text
            )
          )
        )
      ),
      div(
        label(
          cls := labelClass,
          attr("for") := "report-note",
          "Note (optional)"
        ),
        textarea(
          id := "report-note",
          name := "note",
          cls := inputClass,
          attr("rows") := "3",
          attr("maxlength") := "500",
          placeholder := "Anything that helps, such as what looks wrong.",
          form.note
        )
      ),
      // left empty by people; bots that fill every field are ignored
      div(
        cls := "hidden",
        attr("aria-hidden") := "true",
        input(
          name := "website",
          attr("tabindex") := "-1",
          attr("autocomplete") := "off"
        )
      ),
      div(
        cls := "flex flex-wrap items-center gap-4",
        button(
          `type` := "submit",
          cls := Styles.primaryButton,
          "Check and report"
        ),
        p_(
          cls := "text-sm text-slate-600 dark:text-slate-400",
          "Nothing identifies you. Reports are deleted after 90 days."
        )
      )
    )

  /** `/status`: how fresh the index is, each job's last run, and the form to
    * report a missing or outdated package (filled in again with `error` when a
    * submission was refused).
    */
  def status(
      s: SiteStatus,
      now: Instant,
      form: ReportForm,
      error: Option[String],
      reportsInterval: FiniteDuration,
      baseUrl: String
  ): String =
    val runs = s.jobs.map(r => r.job -> r).toMap
    Layout.page(
      PageMeta(
        "Status",
        "How up to date the Scala Packages index is, and how to report a missing package.",
        "/status"
      ),
      baseUrl
    )(
      div(
        cls := "mx-auto max-w-3xl space-y-10 px-4 py-12 sm:px-6",
        section(
          h1(
            cls := "font-display text-3xl font-bold tracking-tight text-slate-900 dark:text-white",
            "Status"
          ),
          p_(
            cls := Styles.bodyText,
            "Which Scala libraries exist comes from the Maven Central index, which Central updates only about every two weeks. New versions of libraries already listed show up within a day through deps.dev, but a brand-new library waits for the next index update, unless you ",
            a(href := "#report", cls := Styles.link, "report it below"),
            "."
          ),
          indexPanel(s.index, now)
        ),
        section(
          h2(cls := Styles.sectionHeading, "Background jobs"),
          ul(
            cls := "mt-4 divide-y divide-slate-200 dark:divide-slate-800",
            jobInfo.flatMap((job, label, desc) =>
              runs.get(job).map(jobRow(_, label, desc, s, now))
            )
          )
        ),
        section(
          id := "report",
          h2(cls := Styles.sectionHeading, "Report a package"),
          p_(
            cls := Styles.bodyText,
            "Missing a library, or showing an old version? Enter its coordinates. They are checked against deps.dev straight away, and the package is added or updated if it is there. If deps.dev does not have it yet, it is checked against deps.dev and Maven Central again within ",
            Reports.hours(reportsInterval),
            "."
          ),
          reportForm(form, error),
          if s.reports.resolved > 0 then
            p_(
              cls := "mt-3 text-sm text-slate-600 dark:text-slate-400",
              s"${s.reports.resolved} reported packages were added or updated in the last 30 days."
            )
          else frag()
        )
      )
    )

  /** The answer to a submitted report. */
  def reportResult(
      r: PackageReport,
      result: ReportResult,
      baseUrl: String
  ): String =
    val (title, linked) = result.outcome match
      case ReportOutcome.Added    => ("Added to the index", true)
      case ReportOutcome.Updated  => ("Updated", true)
      case ReportOutcome.NoChange => ("Already up to date", true)
      case ReportOutcome.Pending  => ("Queued", false)
      case ReportOutcome.NotFound => ("Not found", false)
      case ReportOutcome.NotScala => ("Not a Scala artifact", false)
    Layout.page(
      PageMeta(
        title,
        "The outcome of a package report.",
        "/status",
        noindex = true
      ),
      baseUrl
    )(
      div(
        cls := "mx-auto max-w-3xl px-4 py-16 sm:px-6",
        p_(cls := Styles.eyebrow, r.coordinate),
        h1(
          cls := "mt-2 font-display text-3xl font-bold tracking-tight text-slate-900 dark:text-white",
          title
        ),
        p_(cls := Styles.bodyText, result.detail),
        if result.outcome == ReportOutcome.NoChange && r.note.isDefined then
          p_(
            cls := Styles.bodyText,
            "Your note has been kept for the maintainer to look at."
          )
        else frag(),
        div(
          cls := "mt-8 flex flex-wrap gap-3",
          if linked then
            a(
              href := PackageRef(r.groupId, r.name).path,
              cls := Styles.primaryButton,
              s"Open ${r.groupId}:${r.name}"
            )
          else frag(),
          a(
            href := "/status#report",
            cls := Styles.secondaryButton,
            "Report another"
          )
        )
      )
    )

  private def endpoint(sig: String, desc: String): Frag =
    div(
      cls := "mt-4",
      code(cls := "font-mono text-sm text-brand dark:text-brand-light", sig),
      p_(cls := "mt-1 text-sm text-slate-600 dark:text-slate-400", desc)
    )

  def notFound(baseUrl: String): String =
    Layout.page(
      PageMeta("Not found", "Page not found", "/404", noindex = true),
      baseUrl
    )(
      div(
        cls := "mx-auto max-w-3xl px-4 py-20 text-center sm:px-6",
        h1(
          cls := "font-display text-3xl font-bold text-slate-900 dark:text-white",
          "Page not found"
        ),
        p_(
          cls := "mt-3 text-slate-600 dark:text-slate-400",
          "That page or package isn't in the index."
        ),
        a(
          href := "/",
          cls := s"mt-6 ${Styles.primaryButton}",
          "Search packages"
        )
      )
    )
