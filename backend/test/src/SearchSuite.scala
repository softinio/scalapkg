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

import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.db.SearchGate

import cats.effect.IO
import com.softinio.scalapkg.embed.Embedder
import com.softinio.scalapkg.embed.WordPiece
import com.softinio.scalapkg.github.GitHub
import com.softinio.scalapkg.github.Readme
import munit.CatsEffectSuite

import java.nio.file.Files
import java.nio.file.Path
import scala.concurrent.duration.*

class SearchSuite extends CatsEffectSuite {

  test("the search gate refuses past its size and drops semantic past half") {
    for
      gate <- SearchGate(2)
      latch <- cats.effect.Deferred[IO, Unit]
      // the first search holds its slot until the others have been tried
      first <- gate(semantic => latch.get.as(semantic)).start
      _ <- IO.sleep(50.millis)
      second <- gate(semantic => IO.pure(semantic))
      _ <- latch.complete(())
      firstSemantic <- first.joinWithNever
      full <- SearchGate(1).flatMap(g =>
        cats.effect.Deferred[IO, Unit].flatMap { hold =>
          g(_ => hold.get).start.flatMap(f =>
            IO.sleep(50.millis) *> g(_ => IO.unit).attempt <* hold.complete(()) <* f.joinWithNever
          )
        }
      )
    yield
      assert(firstSemantic, "one of two slots taken: semantic allowed")
      assert(!second, "two of two slots taken: keyword only")
      assertEquals(full, Left(SearchGate.Busy))
  }

  private val tiny = WordPiece(
    List("[PAD]", "[UNK]", "[CLS]", "[SEP]", "un", "##aff", "##able", "markdown",
      "pars", "##ing", ",", "!", "cafe")
      .zipWithIndex
      .toMap
  )

  test("own-text query keeps the terms but skips README matches") {
    val tsq = PackageRepo.tsQuery("Parse Markdown!").get
    assertEquals(tsq, "parse:* & markdown:*")
    assertEquals(PackageRepo.ownTextQuery(tsq), "parse:*AB & markdown:*AB")
    assert(!PackageRepo.isSingleTerm(tsq))
    assert(PackageRepo.isSingleTerm(PackageRepo.tsQuery("duckdb").get))
  }

  test("basic tokens lowercase, strip accents and split punctuation") {
    assertEquals(
      tiny.basicTokens("Café, MARKDOWN!"),
      Vector("cafe", ",", "markdown", "!")
    )
  }

  test("word pieces split greedily and fall back to [UNK]") {
    assertEquals(tiny.wordPieces("unaffable"), Vector(4, 5, 6))
    assertEquals(tiny.wordPieces("parsing"), Vector(8, 9))
    assertEquals(tiny.wordPieces("zzz"), Vector(1))
  }

  test("encode adds [CLS]/[SEP] and truncates") {
    assertEquals(tiny.encode("markdown parsing", 16), Vector(2, 7, 8, 9, 3))
    assertEquals(tiny.encode("markdown parsing", 3), Vector(2, 7, 3))
  }

  test("README cleaning keeps prose and drops code, HTML, badges and URLs") {
    val md =
      """# Laika
        |
        |[![Build](https://img.shields.io/x.svg)](https://ci) <img src="logo.png"/>
        |
        |Laika is a **text markup** transformer for [Markdown](https://commonmark.org).
        |
        |```scala
        |val x = Transformer.from(Markdown)
        |```
        |
        |<!-- hidden -->
        |See https://typelevel.org/Laika for docs.
        |[ref]: https://example.com
        |""".stripMargin
    assertEquals(
      Readme.clean(md),
      "Laika Laika is a text markup transformer for Markdown . See for docs."
    )
  }

  test("only well-formed github.com ids are sent to GraphQL") {
    val gh = GitHub(org.http4s.client.Client[IO](_ => cats.effect.Resource.never), "t", null)
    assertEquals(gh.ownerRepo("github.com/typelevel/cats"), Some(("typelevel", "cats")))
    assertEquals(gh.ownerRepo("gitlab.com/a/b"), None)
    assertEquals(gh.ownerRepo("github.com/a/b\") { evil }"), None)
  }

  // runs only after `fetchModel` (the model is not in the repo)
  private val modelDir =
    Path.of(sys.props.getOrElse("scalapkg.modelDir", ".models/bge-small-en-v1.5"))

  test("embeddings put related texts closer than unrelated ones") {
    assume(Files.exists(modelDir.resolve("model.onnx")), "embedding model not downloaded")
    Embedder.load(modelDir).use { e =>
      for
        q <- e.embedQuery("parse markdown")
        docs <- e.embedDocuments(
          List(
            "laika. Site and e-book generator and customizable text markup transformer",
            "http4s-ember-client. A minimal, idiomatic Scala interface for HTTP"
          )
        )
      yield
        def cos(a: Array[Float], b: Array[Float]) = a.zip(b).map(_ * _).sum
        assertEquals(q.length, Embedder.dimensions)
        assert(math.abs(cos(q, q) - 1f) < 1e-3, "vectors are normalised")
        assert(cos(q, docs(0)) > cos(q, docs(1)), s"${cos(q, docs(0))} vs ${cos(q, docs(1))}")
    }
  }
}
