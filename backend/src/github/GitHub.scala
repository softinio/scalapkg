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

package com.softinio.scalapkg.github

import cats.effect.IO
import cats.syntax.all.*
import com.softinio.scalapkg.ingest.Throttle
import io.circe.Json
import io.circe.syntax.*
import org.http4s.AuthScheme
import org.http4s.Credentials
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.headers.Authorization

import java.time.OffsetDateTime

/** Topics and README text of one GitHub repository. */
final case class RepoText(
    projectId: String,
    topics: List[String],
    readme: Option[String],
    createdAt: Option[OffsetDateTime] = None
)

/** Reads topics and READMEs through the GitHub GraphQL API, many repositories
  * per request (one aliased `repository` field each). Repositories that no
  * longer exist simply come back empty.
  */
final class GitHub(client: Client[IO], token: String, throttle: Throttle):

  private val endpoint = Uri.unsafeFromString("https://api.github.com/graphql")

  // README.md is by far the most common; the rest cover other spellings
  private val readmeFiles = List(
    "md" -> "README.md",
    "lmd" -> "readme.md",
    "cmd" -> "Readme.md",
    "mkd" -> "README.markdown",
    "rst" -> "README.rst",
    "adoc" -> "README.adoc",
    "txt" -> "README"
  )

  private val validName = "[A-Za-z0-9_.-]+".r

  /** `github.com/owner/repo` -> (owner, repo), if it is a GitHub id GraphQL can
    * be asked about safely.
    */
  def ownerRepo(projectId: String): Option[(String, String)] =
    projectId.split('/') match
      case Array("github.com", owner, repo)
          if validName.matches(owner) && validName.matches(repo) =>
        Some((owner, repo))
      case _ => None

  /** One RepoText per id; ids GitHub cannot be asked about come back empty.
    */
  def fetch(projectIds: List[String]): IO[List[RepoText]] =
    val (invalid, valid) =
      projectIds.partitionMap(id => ownerRepo(id).map(id -> _).toRight(id))
    valid
      .grouped(25)
      .toList
      .flatTraverse(batch)
      .map(_ ++ invalid.map(RepoText(_, Nil, None)))

  private def batch(
      repos: List[(String, (String, String))]
  ): IO[List[RepoText]] =
    val fields = repos.zipWithIndex.map { case ((_, (owner, name)), i) =>
      val readmes = readmeFiles
        .map((alias, file) =>
          s"""$alias: object(expression: "HEAD:$file") { ... on Blob { text } }"""
        )
        .mkString("\n")
      s"""r$i: repository(owner: "$owner", name: "$name") {
         |  createdAt
         |  repositoryTopics(first: 20) { nodes { topic { name } } }
         |  $readmes
         |}""".stripMargin
    }
    val query = s"query {\n${fields.mkString("\n")}\n}"
    val req = Request[IO](Method.POST, endpoint)
      .withEntity(Json.obj("query" -> query.asJson))
      .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
    throttle(client.expect[Json](req)).map { json =>
      val data = json.hcursor.downField("data")
      repos.zipWithIndex.map { case ((id, _), i) =>
        val repo = data.downField(s"r$i")
        val topics = repo
          .downField("repositoryTopics")
          .downField("nodes")
          .as[List[Json]]
          .getOrElse(Nil)
          .flatMap(_.hcursor.downField("topic").get[String]("name").toOption)
        val readme = readmeFiles.view
          .flatMap((alias, _) =>
            repo.downField(alias).get[String]("text").toOption
          )
          .headOption
          .map(Readme.clean)
          .filter(_.nonEmpty)
        RepoText(
          id,
          topics,
          readme,
          repo.get[OffsetDateTime]("createdAt").toOption
        )
      }
    }

/** Turns a README into plain text worth indexing: no code blocks, HTML, images,
  * badges or URLs, and at most `maxChars` characters.
  */
object Readme:

  val maxChars = 20000

  private val fencedCode = """(?s)```.*?```|~~~.*?~~~""".r
  private val htmlComment = """(?s)<!--.*?-->""".r
  private val htmlTag = """<[^>]+>""".r
  private val image = """!\[[^\]]*\]\([^)]*\)""".r
  private val linkTarget = """\]\([^)]*\)""".r
  private val refLink = """(?m)^\s*\[[^\]]+\]:\s*\S+.*$""".r
  private val url = """https?://\S+""".r
  private val markup = """[#*_>`|=~\[\]]+""".r
  private val spaces = """\s+""".r

  def clean(text: String): String =
    val steps = List(fencedCode, htmlComment, image, refLink)
      .foldLeft(text)((t, r) => r.replaceAllIn(t, " "))
    val noLinks = linkTarget.replaceAllIn(htmlTag.replaceAllIn(steps, " "), "]")
    val plain = markup.replaceAllIn(url.replaceAllIn(noLinks, " "), " ")
    spaces.replaceAllIn(plain, " ").trim.take(maxChars)
