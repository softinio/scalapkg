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

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import com.softinio.scalapkg.summary.Anthropic
import com.softinio.scalapkg.summary.GeneratedSummary
import com.softinio.scalapkg.summary.Summaries
import munit.CatsEffectSuite
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

class SummariesSuite extends CatsEffectSuite {

  private given LoggerFactory[IO] = NoOpFactory[IO]

  test("a failed batch submit is sent once: a repeat could bill twice") {
    for
      calls <- Ref[IO].of(0)
      failing = Client[IO](_ =>
        Resource.eval(calls.update(_ + 1).as(Response[IO](Status.BadGateway)))
      )
      _ <- Anthropic(failing, "key", "model").submit(List("text")).attempt
      n <- calls.get
    yield assertEquals(n, 1)
  }

  // shaped like a line of a Message Batches results file
  private def succeeded(id: String, input: String) =
    s"""{"custom_id":"$id","result":{"type":"succeeded","message":{"id":"msg_1","type":"message","role":"assistant","model":"claude-haiku-5-5","content":[{"type":"tool_use","id":"toolu_1","name":"save_index_entry","input":$input}],"stop_reason":"tool_use"}}}"""

  test("a succeeded result gives the summary and cleaned keywords") {
    val line = succeeded(
      "p0",
      """{"summary":"  JSON encoding and decoding with automatic codec derivation. ","keywords":["JSON","json"," codecs ","",  "serialization"]}"""
    )
    assertEquals(
      Summaries.parseResult(line),
      Some(
        "p0" -> GeneratedSummary(
          Some("JSON encoding and decoding with automatic codec derivation."),
          List("json", "codecs", "serialization")
        )
      )
    )
  }

  test("an empty or overlong summary is left out, keywords are kept") {
    val empty =
      Summaries.parseResult(succeeded("p1", """{"summary":"","keywords":["http"]}"""))
    assertEquals(
      empty,
      Some("p1" -> GeneratedSummary(None, List("http")))
    )
    val long = "word " * 60
    assertEquals(Summaries.clean(long, Nil).summary, None)
  }

  test("bad requests are not retried, expired and server errors are") {
    val invalid =
      """{"custom_id":"p2","result":{"type":"errored","error":{"type":"error","error":{"type":"invalid_request_error","message":"bad"}}}}"""
    val overloaded =
      """{"custom_id":"p3","result":{"type":"errored","error":{"type":"error","error":{"type":"overloaded_error","message":"busy"}}}}"""
    val expired = """{"custom_id":"p4","result":{"type":"expired"}}"""
    assertEquals(Summaries.parseResult(invalid), Some("p2" -> GeneratedSummary.none))
    assertEquals(Summaries.parseResult(overloaded), None)
    assertEquals(Summaries.parseResult(expired), None)
    assertEquals(Summaries.parseResult("not json"), None)
  }

  test("requests force the tool, so every answer has the same shape") {
    val params = Summaries.params("claude-haiku-5-5", "Repository: github.com/a/b")
    val c = params.hcursor
    assertEquals(c.downField("tool_choice").get[String]("name"), Right("save_index_entry"))
    assertEquals(
      c.downField("messages").downArray.get[String]("content"),
      Right("Repository: github.com/a/b")
    )
  }

  test("keywords that only repeat the project name are dropped") {
    val s = GeneratedSummary(
      Some("Scala testing framework."),
      List("munit", "munit tests", "testing", "assertions")
    )
    assertEquals(
      Summaries.withoutName("github.com/scalameta/munit", s).keywords,
      List("testing", "assertions")
    )
    val effect = GeneratedSummary(None, List("cats effect", "effect system"))
    assertEquals(
      Summaries.withoutName("github.com/typelevel/cats-effect", effect).keywords,
      List("effect system")
    )
    assertEquals(Summaries.withoutName("/", effect), effect)
  }
}
