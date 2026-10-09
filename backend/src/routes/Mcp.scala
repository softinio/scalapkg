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

package com.softinio.scalapkg.routes

import cats.effect.IO
import cats.syntax.all.*
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.domain.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*

import java.time.OffsetDateTime
import java.time.ZoneOffset

/** A stateless Model Context Protocol server over Streamable HTTP: JSON-RPC 2.0
  * requests POSTed to /mcp, answered with plain JSON. It only offers tools, so
  * no sessions or server-sent events are needed.
  */
final class Mcp(
    repo: PackageRepo,
    live: ArtifactVersion => IO[LiveInfo],
    version: String
):

  private val latestVersion = "2025-06-18"
  private val supportedVersions =
    Set(latestVersion, "2025-03-26", "2024-11-05")

  private def schema(props: (String, Json)*)(required: String*): Json =
    Json.obj(
      "type" -> "object".asJson,
      "properties" -> Json.obj(props*),
      "required" -> required.toList.asJson
    )

  private def str(desc: String): Json =
    Json.obj("type" -> "string".asJson, "description" -> desc.asJson)

  private def enumOf(desc: String, values: List[String]): Json =
    Json.obj(
      "type" -> "string".asJson,
      "description" -> desc.asJson,
      "enum" -> values.asJson
    )

  /** A compact search hit: one project and its modules. */
  private def hitJson(h: SearchHit): Json =
    Json.obj(
      "project" -> h.title.asJson,
      "repository" -> h.project.map(_.url).asJson,
      "description" -> h.project.flatMap(_.description).asJson,
      // generated from the project's own text
      "summary" -> h.project.flatMap(_.summary).asJson,
      "stars" -> h.project.flatMap(_.stars).asJson,
      "used_by_projects" -> h.dependents.asJson,
      "latest_release" -> h.latestReleaseAt.asJson,
      "modules" -> h.packages
        .map(p =>
          Json.obj(
            "group_id" -> p.groupId.asJson,
            "name" -> p.name.asJson,
            "version" -> p.latestVersion.asJson,
            "targets" -> p.targets.asJson
          )
        )
        .asJson,
      "more_modules" -> (h.moduleCount - h.packages.size).asJson
    )

  /** One MCP tool: what `tools/list` advertises and what `tools/call` runs. */
  private final case class Tool(
      name: String,
      description: String,
      inputSchema: Json,
      run: io.circe.JsonObject => IO[Json]
  ):
    def descriptor: Json = Json.obj(
      "name" -> name.asJson,
      "description" -> description.asJson,
      "inputSchema" -> inputSchema
    )

  private def result(id: Json, value: Json): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> value)

  private def error(id: Json, code: Int, message: String): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> id,
      "error" -> Json.obj("code" -> code.asJson, "message" -> message.asJson)
    )

  private def toolText(value: Json, isError: Boolean = false): Json =
    Json.obj(
      "content" -> List(
        Json.obj("type" -> "text".asJson, "text" -> value.spaces2.asJson)
      ).asJson,
      "isError" -> isError.asJson
    )

  private def arg(args: io.circe.JsonObject, key: String): Option[String] =
    args(key).flatMap(_.asString).map(_.trim).filter(_.nonEmpty)

  /** Runs `f` on the package the `group_id` and `name` arguments name. */
  private def withPackage(args: io.circe.JsonObject)(
      f: PackageSummary => IO[Json]
  ): IO[Json] =
    (arg(args, "group_id"), arg(args, "name")) match
      case (Some(g), Some(n)) =>
        repo.get(PackageRef(g, n)).flatMap {
          case None =>
            IO.pure(
              toolText(s"No package $g:$n in the index".asJson, isError = true)
            )
          case Some(p) => f(p)
        }
      case _ =>
        IO.pure(
          toolText("group_id and name are required".asJson, isError = true)
        )

  private val tools: List[Tool] = List(
    Tool(
      "search_packages",
      "Search Scala libraries on Maven Central by keyword or description of what you need, optionally filtered by platform and Scala version. Results are projects ranked by relevance, each with its matching modules (groupId + name, usable with get_package and install_snippet).",
      schema(
        "query" -> str("Keywords, e.g. 'json', 'http client', 'cats-effect'"),
        "platform" -> enumOf(
          "Only packages built for this platform",
          Platform.facets
        ),
        "scala_version" -> enumOf(
          "Only packages built for this Scala version",
          ScalaVersions.supported
        ),
        "sort" -> enumOf("Result order", Sort.values.toList.map(_.param))
      )("query"),
      args =>
        val q = SearchQuery.fromParams(
          Map(
            "q" -> arg(args, "query").toList,
            "platform" -> arg(args, "platform").toList,
            "scala" -> arg(args, "scala_version").toList,
            "sort" -> arg(args, "sort").toList
          )
        )
        repo
          .search(q)
          .map(r =>
            toolText(
              Json.obj(
                "total" -> r.total.asJson,
                "results" -> r.results.map(hitJson).asJson
              )
            )
          )
    ),
    Tool(
      "get_package",
      "Details of one Scala package: latest version, targets, licences, repository stats, install snippets, how many indexed projects depend on it, plus live security advisories and dependencies from deps.dev.",
      schema(
        "group_id" -> str("Maven groupId, e.g. org.typelevel"),
        "name" -> str("Library name without the Scala suffix, e.g. cats-core")
      )("group_id", "name"),
      args =>
        withPackage(args) { p =>
          val liveInfo = p.representative.traverse(a =>
            live(a).map(info =>
              Json
                .obj("artifact" -> a.name.asJson, "version" -> a.version.asJson)
                .deepMerge(info.asJson.dropNullValues)
            )
          )
          liveInfo.map(l =>
            toolText(
              PackageResponse
                .of(p)
                .asJson
                .deepMerge(Json.obj("live" -> l.asJson))
            )
          )
        }
    ),
    Tool(
      "recent_releases",
      "New Scala library versions published since a date, newest first.",
      schema(
        "since" -> str("ISO-8601 date or timestamp; defaults to 7 days ago"),
        "limit" -> Json.obj(
          "type" -> "integer".asJson,
          "minimum" -> 1.asJson,
          "maximum" -> 200.asJson
        )
      )(),
      args =>
        val since = arg(args, "since")
          .flatMap(ApiRoutes.parseSince)
          .getOrElse(OffsetDateTime.now(ZoneOffset.UTC).minusDays(7))
        val limit = args("limit")
          .flatMap(_.asNumber)
          .flatMap(_.toInt)
          .getOrElse(50)
          .max(1)
          .min(200)
        repo.releases(since, limit).map(rs => toolText(rs.asJson))
    ),
    Tool(
      "install_snippet",
      "Dependency line(s) to add a Scala package to an sbt, Mill or scala-cli build.",
      schema(
        "group_id" -> str("Maven groupId"),
        "name" -> str("Library name without the Scala suffix"),
        "build_tool" -> enumOf("Build tool", List("sbt", "mill", "scala-cli"))
      )("group_id", "name"),
      args =>
        withPackage(args) { p =>
          val tool = arg(args, "build_tool").map(_.toLowerCase)
          val snippets = p.install
            .filter(s => tool.forall(t => s.tool.toLowerCase == t))
          IO.pure(toolText(snippets.asJson))
        }
    )
  )

  private val toolsByName: Map[String, Tool] = tools.map(t => t.name -> t).toMap

  /** Handles one JSON-RPC message; None for notifications. */
  def handle(msg: Json): IO[Option[Json]] =
    val c = msg.hcursor
    val method = c.get[String]("method").getOrElse("")
    val params = c
      .downField("params")
      .focus
      .flatMap(_.asObject)
      .getOrElse(io.circe.JsonObject.empty)
    // a notification (no id, e.g. notifications/initialized) gets no answer
    c.downField("id").focus.traverse(answer(_, method, params))

  private def answer(
      id: Json,
      method: String,
      params: io.circe.JsonObject
  ): IO[Json] =
    method match
      case "initialize" =>
        val requested = params("protocolVersion").flatMap(_.asString)
        val chosen = requested
          .filter(supportedVersions)
          .getOrElse(latestVersion)
        IO.pure(
          result(
            id,
            Json.obj(
              "protocolVersion" -> chosen.asJson,
              "capabilities" -> Json.obj("tools" -> Json.obj()),
              "serverInfo" -> Json.obj(
                "name" -> "scalapkg".asJson,
                "title" -> "Scala Packages".asJson,
                "version" -> version.asJson
              ),
              "instructions" -> "Search and inspect Scala libraries published to Maven Central.".asJson
            )
          )
        )
      case "ping"       => IO.pure(result(id, Json.obj()))
      case "tools/list" =>
        IO.pure(result(id, Json.obj("tools" -> tools.map(_.descriptor).asJson)))
      case "tools/call" =>
        val name = params("name").flatMap(_.asString).getOrElse("")
        val args = params("arguments")
          .flatMap(_.asObject)
          .getOrElse(io.circe.JsonObject.empty)
        toolsByName.get(name) match
          case None       => IO.pure(error(id, -32602, s"Unknown tool: $name"))
          case Some(tool) =>
            tool
              .run(args)
              .handleError(e =>
                toolText(s"Tool failed: ${e.getMessage}".asJson, isError = true)
              )
              .map(result(id, _))
      case other => IO.pure(error(id, -32601, s"Method not found: $other"))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root =>
      req.as[Json].attempt.flatMap {
        case Left(_)     => BadRequest(error(Json.Null, -32700, "Parse error"))
        case Right(body) =>
          body.asArray match
            case Some(batch) =>
              batch.toList.traverseFilter(handle).flatMap {
                case Nil       => Accepted()
                case responses => Ok(responses.asJson)
              }
            case None =>
              handle(body).flatMap {
                case Some(r) => Ok(r)
                case None    => Accepted()
              }
      }
    // no server-initiated stream: this server only answers requests
    case GET -> Root    => MethodNotAllowed(headers.Allow(Method.POST))
    case DELETE -> Root => MethodNotAllowed(headers.Allow(Method.POST))
  }
