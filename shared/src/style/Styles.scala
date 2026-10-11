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

package com.softinio.scalapkg.style

/** Tailwind class strings shared by the server-rendered pages and the Calico
  * islands, ported from bayareatechtalks' `Ui` with the ScalaNews palette.
  * Every class must appear here as a whole string literal so the Tailwind
  * scanner finds it; never build one by interpolation.
  */
object Styles:

  val primaryButton =
    "inline-flex items-center justify-center rounded-lg bg-brand px-5 py-2.5 font-semibold text-white shadow-sm transition hover:bg-brand-dark focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand"

  val secondaryButton =
    "inline-flex items-center justify-center rounded-lg border border-slate-300 px-4 py-2 text-sm font-semibold text-slate-700 transition hover:bg-slate-100 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand dark:border-slate-700 dark:text-slate-200 dark:hover:bg-slate-800"

  val iconButton =
    "inline-flex h-9 w-9 items-center justify-center rounded-lg border border-slate-300 text-slate-600 transition hover:bg-slate-100 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand dark:border-slate-700 dark:text-slate-300 dark:hover:bg-slate-800"

  val card =
    "rounded-xl border border-slate-200 bg-white p-5 shadow-sm transition hover:shadow-md dark:border-slate-800 dark:bg-slate-900"

  val panel =
    "rounded-xl border border-slate-200 bg-white p-5 dark:border-slate-800 dark:bg-slate-900"

  val searchInput =
    "w-full rounded-xl border border-white/20 bg-white px-4 py-3.5 pl-11 text-lg text-slate-900 shadow-lg placeholder-slate-400 focus:border-brand focus:outline-none focus:ring-4 focus:ring-brand-light/40 dark:bg-slate-900 dark:text-slate-100"

  val sectionHeading =
    "font-display text-2xl font-bold tracking-tight text-slate-900 dark:text-white"

  val bodyText = "mt-4 text-slate-700 dark:text-slate-300"

  val eyebrow =
    "font-mono text-xs font-semibold uppercase tracking-wider text-slate-500 dark:text-slate-400"

  val link =
    "text-brand hover:underline dark:text-brand-light"

  val chip =
    "inline-flex items-center rounded-full border border-brand/30 bg-brand/10 px-2.5 py-0.5 font-mono text-xs text-brand dark:text-brand-light"

  val mutedChip =
    "inline-flex items-center rounded-full border border-slate-300 px-2.5 py-0.5 font-mono text-xs text-slate-600 dark:border-slate-700 dark:text-slate-400"

  val topicChip =
    "inline-flex items-center rounded-md bg-slate-100 px-2 py-0.5 text-xs text-slate-600 dark:bg-slate-800 dark:text-slate-300"

  // a module listed under a search result; opens the package drawer
  val moduleChip =
    "inline-flex items-center rounded-md border border-slate-200 bg-white px-2 py-0.5 font-mono text-xs text-slate-800 transition hover:border-brand hover:text-brand dark:border-slate-700 dark:bg-slate-950 dark:text-slate-200 dark:hover:border-brand-light dark:hover:text-brand-light"

  val warnChip =
    "inline-flex items-center rounded-full border border-accent/30 bg-accent/10 px-2.5 py-0.5 font-mono text-xs text-accent"

  val facetOn =
    "inline-flex items-center gap-1.5 rounded-full border border-brand bg-brand px-3 py-1 text-sm font-medium text-white"

  val facetOff =
    "inline-flex items-center gap-1.5 rounded-full border border-slate-300 px-3 py-1 text-sm font-medium text-slate-700 transition hover:border-brand hover:text-brand dark:border-slate-700 dark:text-slate-300"

  val code =
    "block overflow-x-auto whitespace-pre rounded-lg bg-ink px-4 py-3 font-mono text-sm text-slate-100"

  val stat = "font-mono text-sm text-slate-600 dark:text-slate-400"
