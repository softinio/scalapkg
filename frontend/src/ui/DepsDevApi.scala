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

import cats.effect.IO
import cats.syntax.all.*
import com.softinio.scalapkg.domain.*
import io.circe.parser.parse

/** Calls api.deps.dev directly; it allows any origin (CORS `*`). */
object DepsDevApi:

  // a 404 fails like any other status, so the drawer shows the part as
  // unavailable
  private val v3 = DepsDevV3[IO](
    "https://api.deps.dev",
    url => Dom.fetchText(url).flatMap(t => IO.fromEither(parse(t))).map(Some(_))
  )

  /** Versions (newest first), advisories and dependencies, fetched in parallel;
    * a part whose call fails is None.
    */
  def live(a: ArtifactVersion): IO[LiveInfo] =
    (
      v3.versions(a.name).attempt,
      v3.advisories(a).attempt,
      v3.dependencies(a).attempt
    ).parMapN { (vs, adv, deps) =>
      LiveInfo(
        vs.toOption.flatten.map(_.sortBy(_.publishedAt.getOrElse("")).reverse),
        adv.toOption.flatten,
        deps.toOption.flatten
      )
    }
