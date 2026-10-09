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

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*

class DomainSuite extends munit.FunSuite {

  private def parsed(id: String): Option[(String, String)] =
    ArtifactIds.parse(id).map(p => p.name -> p.target.key)

  test("JVM artifacts for each supported Scala version") {
    assertEquals(parsed("cats-core_3"), Some("cats-core" -> "jvm:3"))
    assertEquals(parsed("cats-core_2.13"), Some("cats-core" -> "jvm:2.13"))
    assertEquals(parsed("scala-xml_2.10"), Some("scala-xml" -> "jvm:2.10"))
  }

  test("names containing underscores keep them") {
    assertEquals(parsed("foo_bar_2.13"), Some("foo_bar" -> "jvm:2.13"))
  }

  test("Scala.js and Scala Native artifacts") {
    assertEquals(parsed("fs2-io_sjs1_2.13"), Some("fs2-io" -> "sjs1:2.13"))
    assertEquals(parsed("cats-core_sjs0.6_2.12"), Some("cats-core" -> "sjs0.6:2.12"))
    assertEquals(parsed("cats-core_native0.5_3"), Some("cats-core" -> "native0.5:3"))
    assertEquals(parsed("cats-core_native0.4_2.13"), Some("cats-core" -> "native0.4:2.13"))
  }

  test("sbt plugins, classic and sbt 2 style") {
    assertEquals(parsed("sbt-scalafmt_2.12_1.0"), Some("sbt-scalafmt" -> "sbt1:2.12"))
    assertEquals(parsed("sbt-assembly_2.10_0.13"), Some("sbt-assembly" -> "sbt0.13:2.10"))
    assertEquals(parsed("sbt-ci-release_sbt2_3"), Some("sbt-ci-release" -> "sbt2:3"))
    assertEquals(parsed("sbt-foo_sbt2.0.0-RC2_3"), Some("sbt-foo" -> "sbt2.0.0-RC2:3"))
    // sbt 1 and 0.13 plugins only use the classic `_2.12_1.0` form
    assertEquals(parsed("sbt-foo_sbt1_2.12"), None)
    assertEquals(parsed("sbt-foo_sbt0.13_2.10"), None)
  }

  test("Mill plugins") {
    assertEquals(parsed("mill-scalafix_mill0.11_2.13"), Some("mill-scalafix" -> "mill0.11:2.13"))
    assertEquals(parsed("mill-scalafix_mill1_3"), Some("mill-scalafix" -> "mill1:3"))
  }

  test("non-Scala and unsupported artifactIds are rejected") {
    List(
      "guava",
      "commons-lang3",
      "foo_2.9",
      "foo_2.14",
      "foo_sjs2_3",
      "foo_native0.2_2.13",
      "foo_2.13_0.13",
      "_3",
      "foo bar_3",
      "foo__3",
      "foo_native0.55_3",
      "foo_mill1.2.3_3"
    ).foreach(id => assertEquals(ArtifactIds.parse(id), None, id))
  }

  test("target keys round-trip") {
    List("jvm:3", "sjs1:2.13", "native0.5:3", "sbt1:2.12", "mill1:3").foreach {
      key => assertEquals(Target.parseKey(key).map(_.key), Some(key))
    }
    assertEquals(Target.parseKey("sjs:3"), None)
    assertEquals(Target.parseKey("jvm:2.9"), None)
  }

  test("query params are parsed and unknown facets dropped") {
    val q = SearchQuery.fromParams(
      Map(
        "q" -> Seq(" json "),
        "platform" -> Seq("js", "bogus", "js"),
        "scala" -> Seq("3", "4"),
        "sort" -> Seq("stars"),
        "page" -> Seq("99")
      )
    )
    assertEquals(q, SearchQuery("json", List("js"), List("3"), Sort.Stars, 1))
    assertEquals(
      q.params,
      List("q" -> "json", "platform" -> "js", "scala" -> "3", "sort" -> "stars")
    )
  }

  test("library install snippets") {
    val targets = List("jvm:3", "jvm:2.13", "sjs1:3").flatMap(Target.parseKey)
    val snippets =
      InstallSnippets.render("org.typelevel", "cats-core", "2.13.0", targets)
    assertEquals(
      snippets.map(_.tool),
      List("sbt", "Mill", "scala-cli")
    )
    assertEquals(
      snippets.head.code,
      """libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
        |libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0"""".stripMargin
    )
    assertEquals(
      snippets(2).code,
      """//> using dep org.typelevel::cats-core:2.13.0
        |//> using dep org.typelevel::cats-core::2.13.0""".stripMargin
    )
  }

  test("sbt plugin install snippet") {
    val snippets = InstallSnippets.render(
      "org.scalameta",
      "sbt-scalafmt",
      "2.5.4",
      Target.parseKey("sbt1:2.12").toList
    )
    assertEquals(
      snippets,
      List(Snippet("sbt", """addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.4")"""))
    )
  }

  test("codecs round-trip a search response") {
    val summary = PackageSummary(
      "org.typelevel",
      "cats-core",
      "2.13.0",
      Some("2025-01-01T00:00:00Z"),
      List("jvm:3"),
      List("MIT"),
      deprecated = false,
      Some(
        ProjectInfo(
          "github.com/typelevel/cats",
          Some("FP"),
          Some(5000),
          None,
          None,
          None,
          None,
          Some(7.5)
        )
      )
    )
    val response =
      SearchResponse(
        SearchQuery.empty,
        1,
        List(
          SearchHit(
            "github.com/typelevel/cats",
            "cats",
            summary.latestReleaseAt,
            NonEmptyList.one(summary),
            3
          )
        ),
        Facets(Map.empty, Map.empty)
      )
    assertEquals(decode[SearchResponse](response.asJson.noSpaces), Right(response))
  }

  test("a result shows the description unless it is too thin to say much") {
    def proj(desc: Option[String], summary: Option[String]) =
      ProjectInfo("github.com/a/b", desc, None, None, None, None, None, None)
        .copy(summary = summary)
    val generated = Some("JSON encoding and decoding with derived codecs.")
    val clear = Some("Compositional, streaming I/O library for Scala")
    assertEquals(proj(clear, generated).blurb, clear)
    assertEquals(proj(Some("JSON library"), generated).blurb, generated)
    assertEquals(proj(None, generated).blurb, generated)
    assertEquals(proj(Some("JSON library"), None).blurb, Some("JSON library"))
    assertEquals(proj(None, None).blurb, None)
  }

  test("a card shows the summary, with the description as its tooltip") {
    def proj(desc: Option[String], summary: Option[String]) =
      ProjectInfo("github.com/a/b", desc, None, None, None, None, None, None)
        .copy(summary = summary)
    val generated = Some("JSON encoding and decoding with derived codecs.")
    val clear = Some("Compositional, streaming I/O library for Scala")
    assertEquals(proj(clear, generated).cardLine, generated)
    assertEquals(proj(clear, generated).cardTooltip, clear)
    assertEquals(proj(None, generated).cardTooltip, None)
    assertEquals(proj(clear, None).cardLine, clear)
    assertEquals(proj(clear, None).cardTooltip, None)
    assertEquals(proj(None, None).cardLine, None)
  }
}
