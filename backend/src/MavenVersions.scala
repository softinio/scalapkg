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

package com.softinio.scalapkg

import cats.data.NonEmptyList
import coursier.version.Version

import java.time.Duration
import java.time.Instant

/** Picks the version to show as "latest" for an artifact. Stable releases win
  * over pre-releases (RC, M, alpha, snapshots, builds named after a commit);
  * among versions of the same kind, the highest by Maven/coursier ordering
  * wins, unless a lower one came out more than [[supersededAfter]] later: then
  * the higher one was a stray publish (kyo-core 3.0.7, two years before 0.19.0)
  * or a line the project left behind.
  */
object MavenVersions:

  val supersededAfter: Duration = Duration.ofDays(365)

  private val preReleaseMarker =
    """(?i).*(?:[-.+](?:m\d|rc|alpha|beta|snap|milestone|cr|ea|preview|pre)|\+).*""".r

  // a commit hash as one part of the version (dynver's 1.0-234-d1a2b53, or
  // Mill's 2780582bcf…-0-278058); at least one letter, so dates do not count
  private val commit =
    """(?i)(?:.*[-.+])?(?=\d*[a-f])[0-9a-f]{6,}(?:[-.+].*)?""".r

  def isPreRelease(version: String): Boolean =
    preReleaseMarker.matches(version) || commit.matches(version)

  def compare(a: String, b: String): Int = Version(a).compare(Version(b))

  /** True when `candidate`, released at `candidateAt`, should replace `current`
    * as the latest version.
    */
  def isBetter(
      candidate: String,
      candidateAt: Option[Instant],
      current: String,
      currentAt: Option[Instant]
  ): Boolean =
    best(
      NonEmptyList.of(
        (false, current, currentAt),
        (true, candidate, candidateAt)
      )
    )(_._2, _._3)._1

  /** The element with the best version; the first one when several share it.
    * Undated elements are never ruled out by date.
    */
  def best[A](xs: NonEmptyList[A])(
      version: A => String,
      at: A => Option[Instant]
  ): A =
    val pool = NonEmptyList
      .fromList(xs.filterNot(x => isPreRelease(version(x))))
      .getOrElse(xs)
    val recent = pool.toList.flatMap(at).maxOption.fold(pool) { newest =>
      val cutoff = newest.minus(supersededAfter)
      // never empty: the newest is kept
      NonEmptyList.fromListUnsafe(
        pool.filter(x => at(x).forall(!_.isBefore(cutoff)))
      )
    }
    recent.reduceLeft((cur, x) =>
      if compare(version(x), version(cur)) > 0 then x else cur
    )
