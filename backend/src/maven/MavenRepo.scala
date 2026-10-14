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

package com.softinio.scalapkg.maven

import cats.effect.IO
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.UnexpectedStatus

/** Reads Maven Central's plain repository, not the rate-limited index: the
  * versions in an artifact's `maven-metadata.xml` and the artifactIds in a
  * groupId's directory listing. Only the reports job uses it, for packages
  * deps.dev does not know yet; visitors' requests never reach it.
  */
final class MavenRepo(client: Client[IO], base: Uri):

  // a groupId's listing is a few hundred KB at most
  private val maxBody = 4L * 1024 * 1024

  private def groupPath(groupId: String): Uri =
    groupId.split('.').foldLeft(base)(_ / _)

  /** Every version Central lists for the artifact; None when it has none. */
  def versions(groupId: String, artifactId: String): IO[Option[List[String]]] =
    get(groupPath(groupId) / artifactId / "maven-metadata.xml")
      .map(_.map(MavenRepo.versions))

  /** The artifactIds under the groupId; None when Central has no such group. */
  def artifactIds(groupId: String): IO[Option[List[String]]] =
    val dir = groupPath(groupId)
    get(dir.withPath(dir.path.addEndsWithSlash)).map(_.map(MavenRepo.listing))

  private def get(uri: Uri): IO[Option[String]] =
    val req = Request[IO](uri = uri)
    client.run(req).use { resp =>
      if resp.status == Status.NotFound then IO.pure(None)
      else if resp.status.isSuccess then
        resp.body
          .take(maxBody)
          .through(fs2.text.utf8.decode)
          .compile
          .string
          .map(Some(_))
      else IO.raiseError(UnexpectedStatus(resp.status, req.method, req.uri))
    }

object MavenRepo:

  val central: Uri = Uri.unsafeFromString("https://repo1.maven.org/maven2")

  private val version = "<version>\\s*([^<\\s]+)\\s*</version>".r
  private val versionsBlock = "(?s)<versions>(.*?)</versions>".r
  // `<a href="cats-core_3/" title="cats-core_3/">`; parents and files skipped
  private val dirLink = "href=\"([A-Za-z0-9][A-Za-z0-9_.-]*)/\"".r

  /** The versions listed in a `maven-metadata.xml`. */
  def versions(xml: String): List[String] =
    versionsBlock
      .findFirstMatchIn(xml)
      .toList
      .flatMap(m => version.findAllMatchIn(m.group(1)).map(_.group(1)))
      .distinct

  /** The subdirectories named in a repository directory listing. */
  def listing(html: String): List[String] =
    dirLink.findAllMatchIn(html).map(_.group(1)).toList.distinct
