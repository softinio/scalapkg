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

import com.softinio.scalapkg.domain.PackageSummary
import com.softinio.scalapkg.style.Styles
import io.circe.syntax.*
import scalatags.Text.all.*
import scalatags.Text.tags2.nav

/** A page's title, description and canonical `path` (appended to the base URL);
  * `noindex` keeps it out of search engines.
  */
final case class PageMeta(
    title: String,
    description: String,
    path: String,
    noindex: Boolean = false
)

/** Cache-busting tag for /assets URLs; changes on every deploy (restart). */
object AssetVersion:
  val tag: String = java.lang.Long.toString(System.currentTimeMillis(), 36)

/** Page shell ported from bayareatechtalks' Layout: sticky blurred header,
  * JS-free mobile menu, footer. Adds the dark-mode toggle and the package
  * drawer every page can open.
  */
object Layout:

  val siteName = "Scala Packages"

  private val navLinks = List(
    "/" -> "Search",
    "/about" -> "About",
    "/about#api" -> "API",
    "https://www.scalanews.net" -> "ScalaNews ↗"
  )

  // runs before first paint so a stored or OS dark preference never flashes
  private val themeScript = raw(
    """(function(){try{var t=localStorage.getItem('theme');var d=t?t==='dark':matchMedia('(prefers-color-scheme: dark)').matches;document.documentElement.classList.toggle('dark',d);}catch(e){}})();"""
  )

  private val goatcounter: Frag = frag(
    script(
      raw(
        "window.goatcounter = { path: function (p) { return location.host + p; } };"
      )
    ),
    script(
      attr("data-goatcounter") := "https://wisdom.softinio.com/count",
      src := "https://wisdom.softinio.com/count.js",
      attr("async").empty
    )
  )

  /** A complete HTML document. With `drawerFor` the drawer opens on that
    * package as soon as the frontend loads (the `/p/...` deep links).
    */
  def page(
      meta: PageMeta,
      baseUrl: String,
      drawerFor: Option[PackageSummary] = None
  )(
      content: Frag*
  ): String =
    val canonical = s"$baseUrl${meta.path}"
    val fullTitle =
      if meta.title == siteName then siteName else s"${meta.title} · $siteName"
    "<!DOCTYPE html>" + html(
      lang := "en",
      cls := "h-full",
      head(
        scalatags.Text.tags2.title(fullTitle),
        tag("meta")(attr("charset") := "utf-8"),
        tag("meta")(
          name := "viewport",
          attr("content") := "width=device-width, initial-scale=1"
        ),
        tag("meta")(name := "description", attr("content") := meta.description),
        if meta.noindex then
          tag("meta")(name := "robots", attr("content") := "noindex")
        else frag(),
        tag("meta")(
          attr("property") := "og:title",
          attr("content") := fullTitle
        ),
        tag("meta")(
          attr("property") := "og:description",
          attr("content") := meta.description
        ),
        tag("meta")(
          attr("property") := "og:type",
          attr("content") := "website"
        ),
        tag("meta")(attr("property") := "og:url", attr("content") := canonical),
        tag("meta")(
          attr("property") := "og:site_name",
          attr("content") := siteName
        ),
        tag("meta")(name := "twitter:card", attr("content") := "summary"),
        link(rel := "canonical", href := canonical),
        link(
          rel := "alternate",
          attr("type") := "application/atom+xml",
          attr("title") := "New Scala releases",
          href := "/feed.xml"
        ),
        script(themeScript),
        link(
          rel := "stylesheet",
          href := s"/assets/app.css?v=${AssetVersion.tag}"
        ),
        script(
          src := s"/assets/main.js?v=${AssetVersion.tag}",
          attr("defer").empty
        ),
        goatcounter
      ),
      body(
        cls := "flex min-h-full flex-col bg-white font-sans text-slate-800 antialiased dark:bg-slate-950 dark:text-slate-200",
        a(
          href := "#main",
          cls := "sr-only focus:not-sr-only focus:absolute focus:left-4 focus:top-4 focus:z-50 focus:rounded-lg focus:bg-brand focus:px-4 focus:py-2 focus:text-white",
          "Skip to content"
        ),
        headerFrag,
        tag("main")(id := "main", cls := "flex-1", content),
        footerFrag,
        // filled by the package-drawer island
        tag("dialog")(
          id := "package-drawer",
          cls := "drawer bg-white p-0 text-slate-800 shadow-2xl dark:bg-slate-950 dark:text-slate-200",
          aria.label := "Package details",
          div(
            attr("data-island") := "package-drawer",
            drawerFor.map(p => attr("data-props") := p.asJson.noSpaces),
            cls := "h-full"
          )
        )
      )
    ).render

  private def headerFrag: Frag =
    header(
      cls := "sticky top-0 z-40 border-b border-slate-200 bg-white/90 backdrop-blur dark:border-slate-800 dark:bg-slate-950/90",
      div(
        cls := "mx-auto flex max-w-6xl items-center justify-between gap-4 px-4 py-3 sm:px-6",
        a(
          href := "/",
          cls := "font-display text-lg font-bold tracking-tight text-slate-900 dark:text-white",
          span(cls := "text-scala", "Scala"),
          " Packages"
        ),
        div(
          cls := "flex items-center gap-3",
          nav(
            aria.label := "Main",
            cls := "hidden items-center gap-x-5 text-sm font-medium sm:flex",
            navLinks.map((path, label) =>
              a(
                href := path,
                cls := "inline-flex items-center py-2 text-slate-600 transition hover:text-brand dark:text-slate-300 dark:hover:text-brand-light",
                label
              )
            )
          ),
          // phone: a JS-free disclosure keeps the sticky header one row tall
          tag("details")(
            cls := "relative sm:hidden",
            tag("summary")(
              cls := "nav-toggle inline-flex cursor-pointer items-center rounded-lg border border-slate-300 px-4 py-2 text-sm font-medium text-slate-600 dark:border-slate-700 dark:text-slate-300",
              "Menu"
            ),
            nav(
              aria.label := "Main",
              cls := "absolute right-0 top-full z-50 mt-2 flex w-52 flex-col rounded-xl border border-slate-200 bg-white py-1 text-sm font-medium shadow-lg dark:border-slate-800 dark:bg-slate-900",
              navLinks.map((path, label) =>
                a(
                  href := path,
                  cls := "px-4 py-3 text-slate-600 transition hover:bg-slate-100 hover:text-brand dark:text-slate-300 dark:hover:bg-slate-800 dark:hover:text-brand-light",
                  label
                )
              )
            )
          ),
          button(
            `type` := "button",
            id := "theme-toggle",
            cls := Styles.iconButton,
            aria.label := "Toggle dark mode",
            attr("title") := "Toggle dark mode",
            span(cls := "dark:hidden", "☾"),
            span(cls := "hidden dark:inline", "☀")
          )
        )
      )
    )

  private val footerHeading = cls := Styles.eyebrow

  private def footerLink(url: String, label: String): Frag =
    a(
      href := url,
      cls := "inline-block py-1.5 text-slate-600 transition hover:text-brand dark:text-slate-300 dark:hover:text-brand-light",
      label
    )

  private def footerFrag: Frag =
    footer(
      cls := "border-t border-slate-200 bg-slate-50 dark:border-slate-800 dark:bg-slate-900",
      div(
        cls := "mx-auto grid max-w-6xl gap-8 px-4 py-10 sm:grid-cols-3 sm:px-6",
        div(
          h2(
            cls := "font-display text-base font-bold text-slate-900 dark:text-white",
            span(cls := "text-scala", "Scala"),
            " Packages"
          ),
          p(
            cls := "mt-2 text-sm text-slate-600 dark:text-slate-400",
            "A search index of Scala libraries on Maven Central, part of ",
            a(
              href := "https://www.scalanews.net",
              cls := Styles.link,
              "ScalaNews"
            ),
            "."
          )
        ),
        div(
          h2(footerHeading, "Use it"),
          ul(
            cls := "mt-1 space-y-0.5 text-sm",
            li(footerLink("/about#api", "JSON API")),
            li(footerLink("/about#mcp", "MCP server")),
            li(footerLink("/feed.xml", "New releases feed"))
          )
        ),
        div(
          h2(footerHeading, "Data"),
          p(
            cls := "mt-2 text-sm text-slate-600 dark:text-slate-400",
            "Package metadata from ",
            a(href := "https://deps.dev", cls := Styles.link, "deps.dev"),
            " (CC-BY 4.0) and the ",
            a(
              href := "https://central.sonatype.com",
              cls := Styles.link,
              "Maven Central"
            ),
            " index."
          )
        )
      ),
      div(
        cls := "space-y-1 border-t border-slate-200 px-4 py-4 text-center text-xs text-slate-500 dark:border-slate-800",
        p(
          "Created by ",
          a(
            href := "https://www.softinio.com",
            cls := Styles.link,
            "Salar Rahmanian"
          ),
          ". Open source under the Apache 2.0 licence."
        ),
        p(
          "Not affiliated with EPFL or the Scala Center; Scala is a trademark of EPFL."
        )
      )
    )
