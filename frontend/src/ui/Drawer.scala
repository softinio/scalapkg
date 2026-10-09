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

package com.softinio.scalapkg.ui

import calico.html.io.{*, given}
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.style.Styles
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import org.scalajs.dom

/** The package panel: a right-hand sheet (full screen on phones) showing the
  * stored summary at once and live details from deps.dev as they arrive.
  */
object Drawer:

  private val muted = "text-sm text-slate-500 dark:text-slate-400"
  private val listCls =
    "mt-2 divide-y divide-slate-200 rounded-lg border border-slate-200 text-sm dark:divide-slate-800 dark:border-slate-800"
  private val rowCls = "flex items-center justify-between gap-3 px-3 py-2"

  /** Renders the package in `current`, fetching its live details whenever it
    * changes; a fetch still running for the previous package is cancelled.
    */
  def apply(
      current: Signal[IO, Option[PackageSummary]],
      close: IO[Unit]
  ): Resource[IO, HtmlElement[IO]] =
    for
      live <- current.discrete
        .switchMap {
          case None    => fs2.Stream.empty
          case Some(p) =>
            fs2.Stream.emit(None) ++
              fs2.Stream
                .eval(
                  p.representative.fold(IO.pure(LiveInfo(None, None, None)))(
                    DepsDevApi.live
                  )
                )
                .map(Some(_))
        }
        .holdResource[IO, Option[LiveInfo]](None)
      el <- div(
        cls := "flex h-full flex-col",
        current.map {
          case None    => div(cls := "hidden")
          case Some(p) => panel(p, live, close)
        }
      )
    yield el

  private def panel(
      p: PackageSummary,
      // None while deps.dev is being asked
      live: Signal[IO, Option[LiveInfo]],
      close: IO[Unit]
  ): Resource[IO, HtmlElement[IO]] =
    val rep = p.representative
    div(
      cls := "flex h-full flex-col",
      // header
      div(
        cls := "flex items-start justify-between gap-4 border-b border-slate-200 bg-gradient-to-br from-ink to-brand px-5 py-4 text-white dark:border-slate-800",
        div(
          cls := "min-w-0",
          div(cls := "truncate font-mono text-xs text-brand-light", p.groupId),
          h2(cls := "truncate font-display text-2xl font-bold", p.name),
          div(
            cls := "mt-1 flex flex-wrap items-center gap-2 font-mono text-sm",
            span(s"v${p.latestVersion}"),
            p.latestReleaseAt.map(d =>
              span(cls := "text-slate-300", s"· ${Dates.short(d)}")
            ),
            Option.when(p.deprecated)(
              span(cls := Styles.warnChip, "deprecated")
            )
          )
        ),
        button(
          `type` := "button",
          cls := "inline-flex h-9 w-9 shrink-0 items-center justify-center rounded-lg border border-white/30 text-white hover:bg-white/10",
          aria.label := "Close",
          onClick --> (_.foreach(_ => close)),
          "✕"
        )
      ),
      // body
      div(
        cls := "flex-1 space-y-6 overflow-y-auto px-5 py-5",
        p.project
          .flatMap(_.description)
          .map(d => p_(cls := "text-slate-700 dark:text-slate-300", d)),
        p.project
          .flatMap(_.summary)
          .filterNot(s => p.project.flatMap(_.description).contains(s))
          .map(s =>
            p_(
              cls := "text-sm text-slate-600 dark:text-slate-400",
              s,
              span(
                cls := "ml-1.5 text-xs text-slate-400 dark:text-slate-500",
                title := "Written by an AI model from the project's description and README",
                "· generated summary"
              )
            )
          ),
        div(
          cls := "flex flex-wrap gap-1.5",
          p.parsedTargets.map(t => span(cls := Styles.chip, t.label)),
          p.licenses.map(l => span(cls := Styles.mutedChip, l)),
          p.project.toList
            .flatMap(_.topics)
            .map(t => span(cls := Styles.topicChip, t))
        ),
        p.project.map(stats),
        linksRow(p, rep),
        install(p),
        live.map(_.fold(loading)(liveSections(p, _))),
        p_(
          cls := "border-t border-slate-200 pt-4 text-xs text-slate-500 dark:border-slate-800",
          "Live details from ",
          a(href := "https://deps.dev", cls := Styles.link, "deps.dev"),
          " (CC-BY 4.0), loaded for ",
          code(rep.fold(p.name)(_.artifactId)),
          "."
        )
      )
    )

  // scalatags-style `p` clashes with the PackageSummary parameter name
  private def p_ = calico.html.io.p

  private def stats(pr: ProjectInfo): Resource[IO, HtmlElement[IO]] =
    def stat(label: String, value: Option[String]) =
      value.map(v =>
        div(
          cls := "rounded-lg border border-slate-200 px-3 py-2 dark:border-slate-800",
          div(cls := Styles.eyebrow, label),
          div(
            cls := "font-mono text-lg font-semibold text-slate-900 dark:text-white",
            v
          )
        )
      )
    div(
      cls := "grid grid-cols-2 gap-2 sm:grid-cols-4",
      stat("Stars", pr.stars.map(_.toString)),
      stat("Forks", pr.forks.map(_.toString)),
      stat("Open issues", pr.openIssues.map(_.toString)),
      stat("Scorecard", pr.scorecard.map(s => f"$s%.1f / 10"))
    )

  private def linksRow(
      p: PackageSummary,
      rep: Option[ArtifactVersion]
  ): Resource[IO, HtmlElement[IO]] =
    def ext(url: String, label: String) =
      a(
        href := url,
        target := "_blank",
        rel := List("noopener"),
        cls := Styles.secondaryButton,
        label
      )
    div(
      cls := "flex flex-wrap gap-2",
      p.project.map(pr => ext(pr.url, "Source ↗")),
      p.project.flatMap(_.homepage).map(h => ext(h, "Homepage ↗")),
      rep.map(a => ext(a.depsDevUrl, "deps.dev ↗")),
      rep.map(a => ext(a.mavenCentralUrl, "Maven Central ↗"))
    )

  private def install(p: PackageSummary): Resource[IO, HtmlElement[IO]] =
    div(
      h3(cls := Styles.eyebrow, "Install"),
      div(
        cls := "mt-2 space-y-3",
        p.install.map { s =>
          div(
            div(
              cls := "mb-1 flex items-center justify-between",
              span(
                cls := "text-sm font-semibold text-slate-700 dark:text-slate-300",
                s.tool
              ),
              copyButton(s.code)
            ),
            pre(cls := Styles.code, s.code)
          )
        }
      )
    )

  private def copyButton(text: String): Resource[IO, HtmlElement[IO]] =
    SignallingRef[IO].of(false).toResource.flatMap { copied =>
      button(
        `type` := "button",
        cls := "rounded-md px-2 py-0.5 text-xs font-medium text-brand hover:bg-brand/10 dark:text-brand-light",
        onClick --> (_.foreach(_ =>
          IO.fromPromise(IO(dom.window.navigator.clipboard.writeText(text)))
            .attempt *>
            copied.set(true) *> IO
              .sleep(scala.concurrent.duration.DurationInt(1500).millis) *>
            copied.set(false)
        )),
        copied.map(if _ then "Copied ✓" else "Copy")
      )
    }

  private def loading: Resource[IO, HtmlElement[IO]] =
    div(
      cls := "space-y-3",
      aria.busy := true,
      div(
        cls := "h-4 w-1/3 animate-pulse rounded bg-slate-200 dark:bg-slate-800"
      ),
      div(
        cls := "h-24 animate-pulse rounded-lg bg-slate-100 dark:bg-slate-900"
      ),
      div(cls := "h-24 animate-pulse rounded-lg bg-slate-100 dark:bg-slate-900")
    )

  private def unavailable(what: String) =
    p_(cls := muted, s"$what not available from deps.dev for this version.")

  private def liveSections(
      p: PackageSummary,
      info: LiveInfo
  ): Resource[IO, HtmlElement[IO]] =
    div(
      cls := "space-y-6",
      // security
      div(
        h3(cls := Styles.eyebrow, "Security advisories"),
        info.advisories match
          case None      => unavailable("Advisories")
          case Some(Nil) =>
            p_(
              cls := "mt-2 text-sm text-emerald-700 dark:text-emerald-400",
              s"No known advisories for ${p.latestVersion}."
            )
          case Some(ids) =>
            ul(
              cls := listCls,
              ids.map(id =>
                li(
                  cls := rowCls,
                  a(
                    href := Links.advisory(id),
                    target := "_blank",
                    rel := List("noopener"),
                    cls := "font-mono text-accent hover:underline",
                    id
                  )
                )
              )
            )
      ),
      // dependents, counted from the index (deps.dev has none for Maven)
      div(
        h3(cls := Styles.eyebrow, "Used by"),
        if p.dependents == 0 then
          p_(cls := muted, "No other indexed project depends on it.")
        else
          p_(
            cls := "mt-2 text-sm text-slate-700 dark:text-slate-300",
            strong(p.dependents.toString),
            if p.dependents == 1 then " other indexed project depends on it"
            else " other indexed projects depend on it",
            " (tests included)."
          )
      ),
      // dependencies
      div(
        h3(cls := Styles.eyebrow, "Dependencies"),
        info.dependencies match
          case None                       => unavailable("Dependencies")
          case Some(Dependencies(Nil, _)) =>
            p_(
              cls := "mt-2 text-sm text-slate-700 dark:text-slate-300",
              "No dependencies."
            )
          case Some(Dependencies(deps, indirect)) =>
            div(
              ul(
                cls := listCls,
                deps
                  .take(30)
                  .map(d =>
                    li(
                      cls := rowCls,
                      span(cls := "truncate font-mono text-xs", d.name),
                      span(
                        cls := "shrink-0 font-mono text-xs text-slate-500",
                        d.version
                      )
                    )
                  )
              ),
              p_(
                cls := s"mt-2 $muted",
                s"${deps.size} direct",
                s", $indirect transitive",
                Option.when(deps.size > 30)(" (first 30 shown)").getOrElse("")
              )
            )
      ),
      // versions
      div(
        h3(cls := Styles.eyebrow, "Versions"),
        info.versions match
          case None     => unavailable("Versions")
          case Some(vs) =>
            div(
              ul(
                cls := listCls,
                vs.take(15)
                  .map(v =>
                    li(
                      cls := rowCls,
                      span(
                        cls := (if v.version == p.latestVersion then
                                  "font-mono text-xs font-semibold text-brand dark:text-brand-light"
                                else "font-mono text-xs"),
                        v.version
                      ),
                      span(
                        cls := "font-mono text-xs text-slate-500",
                        v.publishedAt.map(Dates.short).getOrElse("")
                      )
                    )
                  )
              ),
              p_(cls := s"mt-2 $muted", s"${vs.size} versions published")
            )
      )
    )
