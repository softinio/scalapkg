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

import cats.effect.IO
import cats.effect.std.Semaphore

/** Caps how many searches run at once, so a flood cannot pin the CPU. A search
  * beyond the cap fails at once with [[SearchGate.Busy]] (the routes answer 503
  * with `Retry-After`) instead of queueing. Once more than half the slots are
  * taken, searches skip the semantic step, which costs an embedding each.
  */
final class SearchGate private (slots: Semaphore[IO], size: Long):

  /** Runs `search` in a slot, telling it whether semantic search is allowed. */
  def apply[A](search: Boolean => IO[A]): IO[A] =
    slots.tryAcquire.flatMap {
      case false => IO.raiseError(SearchGate.Busy)
      case true  =>
        slots.available
          .flatMap(free => search(size - free <= size / 2))
          .guarantee(slots.release)
    }

object SearchGate:

  /** Every slot is taken; the caller should try again shortly. */
  case object Busy
      extends RuntimeException("too many searches at once", null, false, false)

  def apply(size: Int): IO[SearchGate] =
    Semaphore[IO](size.toLong).map(new SearchGate(_, size.toLong))
