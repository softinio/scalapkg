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

package com.softinio.scalapkg.depsdev

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import com.softinio.scalapkg.domain.ArtifactVersion
import com.softinio.scalapkg.domain.DepsDevV3
import com.softinio.scalapkg.domain.LiveInfo
import com.softinio.scalapkg.domain.ProjectInfo
import com.softinio.scalapkg.domain.VersionEntry
import com.softinio.scalapkg.ingest.Throttle
import io.circe.Decoder
import io.circe.Json
import io.circe.syntax.*
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client

import java.time.OffsetDateTime
import scala.concurrent.duration.FiniteDuration

/** The parts of a deps.dev version we keep. */
final case class VersionInfo(
    publishedAt: Option[OffsetDateTime],
    licenses: List[String],
    deprecated: Boolean,
    sourceRepo: Option[String]
)

/** Client for the deps.dev API (https://docs.deps.dev/api/v3alpha/). Stable v3
  * is used for single lookups; the batch endpoints only exist in v3alpha. All
  * calls share one throttle.
  */
final class DepsDev(client: Client[IO], base: Uri, throttle: Throttle):

  private val maxBatch = 5000

  private given Decoder[OffsetDateTime] =
    Decoder.decodeString.emapTry(s => scala.util.Try(OffsetDateTime.parse(s)))

  private def post(path: Uri, body: Json): IO[Json] =
    throttle(
      client.expect[Json](
        Request[IO](Method.POST, path).withEntity(body)
      )
    )

  /** Posts `requests` to a v3alpha batch endpoint in batches of `maxBatch`,
    * following page tokens, and reads `field` of every response.
    */
  private def batch[A](endpoint: String, field: String, requests: List[Json])(
      read: Json => Option[A]
  ): IO[List[A]] =
    requests.grouped(maxBatch).toList.flatTraverse { reqs =>
      def page(token: Option[String]): IO[List[A]] =
        val body = Json.obj(
          "requests" -> reqs.asJson,
          "pageToken" -> token.getOrElse("").asJson
        )
        post(base / "v3alpha" / endpoint, body).flatMap { json =>
          val c = json.hcursor
          val items = c
            .get[List[Json]]("responses")
            .getOrElse(Nil)
            .flatMap(_.hcursor.downField(field).focus.flatMap(read))
          c.get[String]("nextPageToken").toOption.filter(_.nonEmpty) match
            case Some(next) => page(Some(next)).map(items ++ _)
            case None       => IO.pure(items)
        }
      page(None)
    }

  private def versionInfo(v: Json): Option[((String, String), VersionInfo)] =
    val c = v.hcursor
    for
      key <- c.downField("versionKey").focus
      name <- key.hcursor.get[String]("name").toOption
      version <- key.hcursor.get[String]("version").toOption
    yield
      val related = c
        .get[List[Json]]("relatedProjects")
        .getOrElse(Nil)
        .flatMap { p =>
          val pc = p.hcursor
          Option
            .when(pc.get[String]("relationType").contains("SOURCE_REPO"))(
              pc.downField("projectKey").get[String]("id").toOption
            )
            .flatten
        }
      (name, version) -> VersionInfo(
        c.get[OffsetDateTime]("publishedAt").toOption,
        c.get[List[String]]("licenses").getOrElse(Nil).filter(_.nonEmpty),
        c.get[Boolean]("isDeprecated").getOrElse(false),
        related.headOption.map(_.toLowerCase)
      )

  /** Looks up Maven versions in bulk, `artifact` giving each key's version.
    * Keys whose version deps.dev has not ingested yet are missing from the
    * result.
    */
  def versions[K](keys: List[K])(
      artifact: K => ArtifactVersion
  ): IO[List[(K, VersionInfo)]] =
    val wanted = keys.map(k => artifact(k) -> k)
    val requests = wanted.map { (a, _) =>
      Json.obj(
        "versionKey" -> Json.obj(
          "system" -> "MAVEN".asJson,
          "name" -> a.name.asJson,
          "version" -> a.version.asJson
        )
      )
    }
    val byKey = wanted.map((a, k) => (a.name, a.version) -> k).toMap
    batch("versionbatch", "version", requests)(
      versionInfo(_).flatMap((key, info) => byKey.get(key).map(_ -> info))
    )

  /** Project stats for `github.com/org/repo` style ids, in bulk. */
  def projects(ids: List[String]): IO[List[ProjectInfo]] =
    batch(
      "projectbatch",
      "project",
      ids.map(id => Json.obj("projectKey" -> Json.obj("id" -> id.asJson)))
    )(DepsDev.project)

  private def getJson(uri: Uri): IO[Option[Json]] =
    throttle(client.run(Request[IO](uri = uri)).use { resp =>
      if resp.status == Status.NotFound then IO.pure(None)
      else if resp.status.isSuccess then resp.as[Json].map(Some(_))
      else IO.raiseError(new RuntimeException(s"$uri returned ${resp.status}"))
    })

  private val v3 =
    DepsDevV3[IO](base.renderString, url => getJson(Uri.unsafeFromString(url)))

  /** The dependencies one Maven version declares, as `groupId:artifactId`
    * names, test scope included. Empty if deps.dev does not know the version.
    */
  def requirements(a: ArtifactVersion): IO[List[String]] =
    getJson(
      Uri.unsafeFromString(s"${v3.versionUrl("v3alpha", a)}:requirements")
    ).map(
      _.fold(Nil)(
        _.hcursor
          .downField("maven")
          .get[List[Json]]("dependencies")
          .getOrElse(Nil)
          .flatMap { d =>
            val c = d.hcursor
            c.get[String]("resolvedName")
              .toOption
              .filter(_.nonEmpty)
              .orElse(c.get[String]("name").toOption)
          }
          .distinct
      )
    )

  /** Live facts about one version for the MCP `get_package` tool: advisories
    * and dependencies. Missing parts are left out. (deps.dev serves no
    * dependents for Maven; those are counted from the index instead.)
    */
  def live(a: ArtifactVersion): IO[LiveInfo] = lookup(a).map(_._1)

  /** `live` with each answer remembered for `ttl` (MCP `get_package` asks
    * deps.dev on every call otherwise). Only an answer whose lookups all
    * succeeded is kept; a failed part is asked again next time.
    */
  def cachedLive(ttl: FiniteDuration): IO[ArtifactVersion => IO[LiveInfo]] =
    Ref
      .of[IO, Map[ArtifactVersion, (FiniteDuration, LiveInfo)]](Map.empty)
      .map { cache => a =>
        IO.monotonic.flatMap { now =>
          cache.get.map(_.get(a).filter((at, _) => now - at < ttl)).flatMap {
            case Some((_, info)) => IO.pure(info)
            case None            =>
              lookup(a).flatMap { (info, complete) =>
                cache
                  .update(m =>
                    (if m.size >= 5000 then Map.empty else m) + (a -> (
                      now,
                      info
                    ))
                  )
                  .whenA(complete)
                  .as(info)
              }
          }
        }
      }

  private def lookup(a: ArtifactVersion): IO[(LiveInfo, Boolean)] =
    (v3.advisories(a).attempt, v3.dependencies(a).attempt).mapN((adv, deps) =>
      (
        LiveInfo(None, adv.toOption.flatten, deps.toOption.flatten),
        adv.isRight && deps.isRight
      )
    )

  /** All published versions of one Maven artifact (`groupId:artifactId`). */
  def packageVersions(name: String): IO[List[VersionEntry]] =
    v3.versions(name).map(_.orEmpty)

object DepsDev:

  val api: Uri = Uri.unsafeFromString("https://api.deps.dev")

  private[depsdev] def project(json: Json): Option[ProjectInfo] =
    val c = json.hcursor
    c.downField("projectKey").get[String]("id").toOption.map { id =>
      ProjectInfo(
        id = id.toLowerCase,
        description = c.get[String]("description").toOption.filter(_.nonEmpty),
        stars = c.get[Int]("starsCount").toOption,
        forks = c.get[Int]("forksCount").toOption,
        openIssues = c.get[Int]("openIssuesCount").toOption,
        license = c.get[String]("license").toOption.filter(_.nonEmpty),
        homepage = c.get[String]("homepage").toOption.filter(_.nonEmpty),
        scorecard =
          c.downField("scorecard").get[Double]("overallScore").toOption
      )
    }
