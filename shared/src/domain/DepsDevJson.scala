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

import cats.Functor
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.Json

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** One published version; `publishedAt` is ISO-8601, None if deps.dev has no
  * date.
  */
final case class VersionEntry(version: String, publishedAt: Option[String])
    derives Encoder.AsObject

/** A direct dependency: `name` is `groupId:artifactId`. */
final case class Dependency(name: String, version: String)
    derives Encoder.AsObject

/** A version's direct dependencies, by name, and how many indirect ones it has.
  */
final case class Dependencies(direct: List[Dependency], indirect: Int)
    derives Encoder.AsObject

/** Live facts about a package's latest version from deps.dev. Each part is
  * optional so one failing call does not hide the rest.
  */
final case class LiveInfo(
    versions: Option[List[VersionEntry]],
    advisories: Option[List[String]],
    dependencies: Option[Dependencies]
) derives Encoder.AsObject

/** deps.dev v3 single lookups over any HTTP GET that reads a 404 as None, for
  * the server and the drawer alike.
  */
final class DepsDevV3[F[_]: Functor](
    base: String,
    get: String => F[Option[Json]]
):

  def packageUrl(api: String, name: String): String =
    s"$base/$api/systems/maven/packages/${encode(name)}"

  def versionUrl(api: String, a: ArtifactVersion): String =
    s"${packageUrl(api, a.name)}/versions/${encode(a.version)}"

  /** All published versions of one Maven artifact (`groupId:artifactId`). */
  def versions(name: String): F[Option[List[VersionEntry]]] =
    get(packageUrl("v3", name)).map(_.map(DepsDevJson.versions))

  def advisories(a: ArtifactVersion): F[Option[List[String]]] =
    get(versionUrl("v3", a)).map(_.map(DepsDevJson.advisories))

  def dependencies(a: ArtifactVersion): F[Option[Dependencies]] =
    get(s"${versionUrl("v3", a)}:dependencies")
      .map(_.map(DepsDevJson.dependencies))

  private def encode(s: String): String =
    URLEncoder.encode(s, StandardCharsets.UTF_8)

/** Reads deps.dev v3 responses, for the server and the drawer alike. */
object DepsDevJson:

  /** A package's versions, with the publish time as deps.dev writes it. */
  def versions(json: Json): List[VersionEntry] =
    json.hcursor
      .get[List[Json]]("versions")
      .getOrElse(Nil)
      .flatMap { v =>
        val c = v.hcursor
        c.downField("versionKey")
          .get[String]("version")
          .toOption
          .map(VersionEntry(_, c.get[String]("publishedAt").toOption))
      }

  /** The advisory ids of one version. */
  def advisories(json: Json): List[String] =
    json.hcursor
      .get[List[Json]]("advisoryKeys")
      .getOrElse(Nil)
      .flatMap(_.hcursor.get[String]("id").toOption)

  /** A version's dependencies. */
  def dependencies(json: Json): Dependencies =
    val nodes =
      json.hcursor.get[List[Json]]("nodes").getOrElse(Nil).map(_.hcursor)
    val direct =
      nodes.filter(_.get[String]("relation").contains("DIRECT")).flatMap { c =>
        val key = c.downField("versionKey")
        (key.get[String]("name").toOption, key.get[String]("version").toOption)
          .mapN(Dependency.apply)
      }
    val indirect = nodes.count(_.get[String]("relation").contains("INDIRECT"))
    Dependencies(direct.sortBy(_.name), indirect)
