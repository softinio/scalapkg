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

/** Picks the version to show as "latest" for an artifact. Stable releases win
  * over pre-releases (RC, M, alpha, snapshots, dynver builds); among versions
  * of the same kind, the highest by Maven/coursier ordering wins.
  */
object MavenVersions:

  private val preReleaseMarker =
    """(?i).*(?:[-.+](?:m\d|rc|alpha|beta|snapshot|milestone|cr|ea|preview|pre)|\+).*""".r

  def isPreRelease(version: String): Boolean =
    preReleaseMarker.matches(version)

  def compare(a: String, b: String): Int = Version(a).compare(Version(b))

  /** True when `candidate` should replace `current` as the latest version. */
  def isBetter(candidate: String, current: String): Boolean =
    (isPreRelease(candidate), isPreRelease(current)) match
      case (false, true) => true
      case (true, false) => false
      case _             => compare(candidate, current) > 0

  /** The element with the best version; the first one when several share it.
    */
  def best[A](xs: NonEmptyList[A])(version: A => String): A =
    xs.reduceLeft((cur, x) =>
      if isBetter(version(x), version(cur)) then x else cur
    )
