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
import com.softinio.scalapkg.github.GitHub
import com.softinio.scalapkg.ingest.Throttle
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client

import java.time.OffsetDateTime
import scala.concurrent.duration.*

class GitHubSuite extends CatsEffectSuite {

  // shaped like a GraphQL answer for one aliased repository
  private val answer =
    """{"data":{"r0":{
      |  "createdAt":"2013-10-20T09:15:00Z",
      |  "repositoryTopics":{"nodes":[{"topic":{"name":"sbt"}},{"topic":{"name":"testng"}}]},
      |  "md":{"text":"# sbt-testng\n\nRun TestNG tests from sbt."}
      |}}}""".stripMargin

  test("repository topics, README and creation date are read from GraphQL") {
    val app = HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok).withEntity(answer)))
    for
      throttle <- Throttle(0.millis)
      repos <- GitHub(Client.fromHttpApp(app), "token", throttle)
        .fetch(List("github.com/sbt/sbt-testng"))
    yield
      val repo = repos.head
      assertEquals(repo.topics, List("sbt", "testng"))
      assert(repo.readme.exists(_.contains("Run TestNG tests")), repo.readme)
      assertEquals(
        repo.createdAt,
        Some(OffsetDateTime.parse("2013-10-20T09:15:00Z"))
      )
  }
}
