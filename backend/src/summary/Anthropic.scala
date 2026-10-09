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

package com.softinio.scalapkg.summary

import cats.effect.IO
import com.softinio.scalapkg.ingest.Http
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.typelevel.ci.CIString
import org.typelevel.log4cats.LoggerFactory

/** What the model wrote about one project: a line for result cards (None when
  * the input did not say clearly what the project does) and search keywords.
  */
final case class GeneratedSummary(
    summary: Option[String],
    keywords: List[String]
)

object GeneratedSummary:
  /** A request that failed for good: only its input hash is stored. */
  val none: GeneratedSummary = GeneratedSummary(None, Nil)

/** Where a submitted batch is. */
enum BatchState:
  case InProgress
  case Ended(resultsUrl: Uri)
  // deleted, never existed for this key, or ended without results
  case Gone

/** Asks Claude for project summaries through the Message Batches API: half the
  * price of single calls, and summaries are not urgent. Results usually arrive
  * within the hour.
  */
final class Anthropic(plain: Client[IO], apiKey: String, model: String)(using
    LoggerFactory[IO]
):

  // a submit that failed after Anthropic accepted it would create a second
  // paid batch if repeated, so only reads are retried
  private val client = Http.retrying(plain)

  private val batches = Uri.unsafeFromString("https://api.anthropic.com") /
    "v1" / "messages" / "batches"

  private val auth = Headers(
    Header.Raw(CIString("x-api-key"), apiKey),
    Header.Raw(CIString("anthropic-version"), "2023-06-01")
  )

  /** Submits one request per project text; returns the batch id. */
  def submit(texts: List[String]): IO[String] =
    val body = Json.obj(
      "requests" -> texts.zipWithIndex.map { (input, i) =>
        Json.obj(
          "custom_id" -> customId(i).asJson,
          "params" -> Summaries.params(model, input)
        )
      }.asJson
    )
    plain
      .expect[Json](
        Request[IO](Method.POST, batches).withHeaders(auth).withEntity(body)
      )
      .flatMap(json =>
        IO.fromEither(
          json.hcursor
            .get[String]("id")
            .left
            .map(_ => new RuntimeException(s"no batch id in $json"))
        )
      )

  /** Polls one batch (a safe read, so retried); fails on any status but 200 and
    * 404.
    */
  def state(id: String): IO[BatchState] =
    client.run(Request[IO](uri = batches / id).withHeaders(auth)).use { resp =>
      resp.status match
        case Status.NotFound => IO.pure(BatchState.Gone)
        case Status.Ok       =>
          resp.as[Json].map { json =>
            val c = json.hcursor
            if c.get[String]("processing_status").contains("ended") then
              c.get[Uri]("results_url")
                .fold(_ => BatchState.Gone, BatchState.Ended(_))
            else BatchState.InProgress
          }
        case status =>
          IO.raiseError(new RuntimeException(s"batch $id: $status"))
    }

  /** Reads a batch's final results (JSON lines), each paired with its entry of
    * `requests` (what the batch was submitted for, in order); a request left
    * out is to be asked again.
    */
  def results[A](
      url: Uri,
      requests: List[A]
  ): IO[List[(A, GeneratedSummary)]] =
    client
      .stream(
        Request[IO](uri = url).withHeaders(auth)
      )
      .flatMap { resp =>
        if resp.status.isSuccess then resp.body
        else
          fs2.Stream.raiseError[IO](
            new RuntimeException(s"batch results: ${resp.status}")
          )
      }
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .map(Summaries.parseResult)
      .unNone
      .compile
      .toList
      .map { rows =>
        val byId = rows.toMap
        requests.zipWithIndex
          .flatMap((a, i) => byId.get(customId(i)).map(a -> _))
      }

  private def customId(i: Int): String = s"p$i"

/** The prompt, and turning the model's answer into what is stored. */
object Summaries:

  private val tool = "save_index_entry"

  val system: String =
    """You write index entries for Scala libraries on a package search site.
      |From the project information given (repository, description, topics,
      |module names and the start of its README), call save_index_entry with:
      |
      |summary: one short clause of at most 100 characters saying what the
      |project does, as a reader scanning search results needs it, e.g.
      |"JSON encoding and decoding with automatic codec derivation." State
      |only what the input says. Do not name the project, do not start with
      |"A library" or "This library", do not add "for Scala" or list
      |platforms, no marketing words (fast, best, powerful, simple, modern,
      |blazing), no version numbers. If the input does not make clear what
      |the project does, use an empty string.
      |
      |keywords: up to 8 lowercase terms a developer would search for to find
      |this project: what it does and its domain, e.g. "json", "http client",
      |"property-based testing", "postgres", plus common synonyms. Name only
      |what the project is for, not the libraries, formats or tools it
      |integrates with or has optional modules for: a JSON library with a
      |YAML module and http4s support gets neither "yaml" nor "http4s", and a
      |logging library with a Kafka appender does not get "kafka". Leave out
      |the project's own name, "scala", "library", platforms (jvm, scala.js,
      |scala native) and generic words (functional, type-safe, typelevel)
      |unless they are what the project is about. Only terms the input
      |supports.""".stripMargin

  /** One request's Messages API params: the system prompt, and the project text
    * as the user turn, answered by a forced `save_index_entry` tool call.
    */
  def params(model: String, input: String): Json =
    Json.obj(
      "model" -> model.asJson,
      "max_tokens" -> 300.asJson,
      "system" -> system.asJson,
      "tools" -> Json.arr(
        Json.obj(
          "name" -> tool.asJson,
          "description" -> "Save the index entry for the project.".asJson,
          "input_schema" -> Json.obj(
            "type" -> "object".asJson,
            "properties" -> Json.obj(
              "summary" -> Json.obj("type" -> "string".asJson),
              "keywords" -> Json.obj(
                "type" -> "array".asJson,
                "items" -> Json.obj("type" -> "string".asJson)
              )
            ),
            "required" -> List("summary", "keywords").asJson
          )
        )
      ),
      "tool_choice" -> Json.obj(
        "type" -> "tool".asJson,
        "name" -> tool.asJson
      ),
      "messages" -> Json.arr(
        Json.obj("role" -> "user".asJson, "content" -> input.asJson)
      )
    )

  val maxSummary = 140
  val maxKeywords = 10

  private val spaces = "\\s+".r

  /** Trims what the model wrote; a summary that ignored the length limit is
    * dropped rather than cut mid-sentence.
    */
  def clean(summary: String, keywords: List[String]): GeneratedSummary =
    val line = spaces
      .replaceAllIn(summary, " ")
      .trim
      .stripPrefix("\"")
      .stripSuffix("\"")
      .trim
    GeneratedSummary(
      Option.when(line.nonEmpty && line.length <= maxSummary)(line),
      keywords
        .map(k => spaces.replaceAllIn(k.toLowerCase, " ").trim)
        .filter(k => k.nonEmpty && k.length <= 40)
        .distinct
        .take(maxKeywords)
    )

  private val nonWord = "[^a-z0-9]".r

  /** Drops keywords that only repeat the project's name (`munit` for
    * github.com/scalameta/munit), which the name already matches in search.
    */
  def withoutName(projectId: String, s: GeneratedSummary): GeneratedSummary =
    val last = projectId.split('/').lastOption.getOrElse("")
    val name = nonWord.replaceAllIn(last.toLowerCase, "")
    if name.length < 3 then s
    else
      s.copy(keywords =
        s.keywords.filterNot(k => nonWord.replaceAllIn(k, "").contains(name))
      )

  /** One line of a batch results file, when its outcome is final. */
  def parseResult(line: String): Option[(String, GeneratedSummary)] =
    parse(line).toOption.flatMap { json =>
      val c = json.hcursor
      val result = c.downField("result")
      val outcome = result.get[String]("type").toOption match
        case Some("succeeded") =>
          result
            .downField("message")
            .get[List[Json]]("content")
            .getOrElse(Nil)
            .map(_.hcursor)
            .find(_.get[String]("type").contains("tool_use"))
            .map(_.downField("input"))
            .map(in =>
              clean(
                in.get[String]("summary").getOrElse(""),
                in.get[List[String]]("keywords").getOrElse(Nil)
              )
            )
            .orElse(Some(GeneratedSummary.none))
        // the request itself was bad: asking again would fail the same way
        case Some("errored")
            if result
              .downField("error")
              .downField("error")
              .get[String]("type")
              .contains("invalid_request_error") =>
          Some(GeneratedSummary.none)
        // expired, canceled or a server error: ask again later
        case _ => None
      c.get[String]("custom_id").toOption.zip(outcome)
    }
