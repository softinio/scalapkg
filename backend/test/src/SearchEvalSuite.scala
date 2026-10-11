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
import cats.effect.Resource
import cats.syntax.all.*
import com.softinio.scalapkg.db.Database
import com.softinio.scalapkg.db.IngestRepo
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.domain.*
import com.softinio.scalapkg.embed.Embedder
import munit.CatsEffectSuite

import java.nio.file.Files
import java.nio.file.Path
import scala.concurrent.duration.*

/** Search quality against real data: standard queries and the projects a
  * person would expect near the top. Needs a database loaded with the dev seed
  * (`loadSeed`; `SCALAPKG_EVAL_DB_NAME` names another database than
  * `scalapkg`) and runs only when `SCALAPKG_EVAL_DB_PORT` is set. CI runs it
  * against the committed seed:
  *
  * {{{
  * SCALAPKG_EVAL_DB_PORT=5432 ./mill backend.test.testOnly com.softinio.scalapkg.SearchEvalSuite
  * }}}
  *
  * Reports recall@5 (expected projects among the first five results) and MRR
  * (1 / rank of the first expected project), and fails if either drops below
  * the floor below, so ranking changes cannot quietly make search worse.
  */
class SearchEvalSuite extends CatsEffectSuite {

  // a grid run over the full index takes longer
  override val munitIOTimeout =
    if sys.env.contains("SCALAPKG_EVAL_GRID") then 2.hours else 5.minutes

  private val port = sys.env.get("SCALAPKG_EVAL_DB_PORT").flatMap(_.toIntOption)

  private def gh(repo: String) = s"github.com/$repo"

  /** query -> projects expected in the top five, most important first */
  val golden: List[(String, List[String])] = List(
    "json" -> List("circe/circe", "plokhotnyuk/jsoniter-scala", "playframework/play-json", "spray/spray-json", "json4s/json4s", "zio/zio-json", "com-lihaoyi/upickle"),
    "xml" -> List("scala/scala-xml", "com-lihaoyi/scalatags"),
    "yaml" -> List("circe/circe-yaml", "virtuslab/scala-yaml", "hnaderi/yaml4s", "armanbilge/circe-scala-yaml", "jodersky/yamlesque"),
    "csv" -> List("nrinaudo/kantan.csv", "christopherdavenport/cormorant", "sovt/spata", "gekomad/itto-csv", "zamblauskas/scala-csv-parser"),
    "http client" -> List("softwaremill/sttp", "com-lihaoyi/requests-scala", "http4s/http4s", "timt/naive-http", "spinoco/fs2-http"),
    "http server" -> List("http4s/http4s", "akka/akka-http", "zio/zio-http", "apache/pekko-http", "finagle/finch"),
    "logging" -> List("lightbend/scala-logging", "outr/scribe", "typelevel/log4cats", "zio/zio-logging", "log4s/log4s", "lego/woof"),
    "command line arguments" -> List("scopt/scopt", "com-lihaoyi/mainargs", "alexarchambault/case-app", "bkirwi/decline", "jfkelley/argyle"),
    "parse markdown" -> List("noelwelsh/mads", "onewebpro/scala-md-tag", "lightbend/paradox", "scalameta/mdoc"),
    "parser combinators" -> List("scala/scala-parser-combinators", "com-lihaoyi/fastparse", "j-mie6/parsley", "tpolecat/atto", "djspiewak/gll-combinators"),
    "property based testing" -> List("typelevel/scalacheck", "hedgehogqa/scala-hedgehog", "propensive/probably"),
    "testing" -> List("scalatest/scalatest", "scalameta/munit", "com-lihaoyi/utest", "etorreborre/specs2", "typelevel/weaver-test", "typelevel/cats-effect-testing"),
    "database access" -> List("typelevel/doobie", "slick/slick", "typelevel/skunk", "augustnagro/magnum", "playframework/anorm"),
    "postgres" -> List("typelevel/skunk", "typelevel/doobie"),
    "duckdb" -> List("outr/lightdb"),
    "configuration" -> List("pureconfig/pureconfig", "vlovgr/ciris", "zio/zio-config", "ekrich/sconfig", "h8io/borscht"),
    "functional programming" -> List("typelevel/cats", "zio/zio", "typelevel/cats-effect"),
    "streaming" -> List("typelevel/fs2", "akka/akka", "apache/pekko", "zio/zio", "monix/monix"),
    "htp4s" -> List("http4s/http4s"),
    "cirse" -> List("circe/circe"),
    "cats" -> List("typelevel/cats", "typelevel/cats-effect"),
    "zio" -> List("zio/zio"),
    "akka" -> List("akka/akka"),
    "fs2" -> List("typelevel/fs2"),
    "http4s" -> List("http4s/http4s"),
    "http4s-circe" -> List("http4s/http4s"),
    "optics" -> List("optics-dev/monocle"),
    "reactive ui scala.js" -> List("armanbilge/calico"),
    "pretty print" -> List("com-lihaoyi/pprint"),
    "release to sonatype" -> List("sbt/sbt-ci-release"),
    "mutation testing" -> List("stryker-mutator/stryker4s"),
    "database migrations" -> List("flyway/flyway", "sbt/flyway-sbt", "rolang/dumbo", "geirolz/fly4s"),
    "html templates" -> List("com-lihaoyi/scalatags")
  ).map((q, e) => q -> e.map(gh))

  /** Current names of renamed repositories, for databases whose packages
    * still carry the old ones (the seed predates merging them).
    */
  val renamed: Map[String, String] = Map(
    "tpolecat/doobie" -> "typelevel/doobie",
    "typesafehub/scala-logging" -> "lightbend/scala-logging",
    "tpolecat/skunk" -> "typelevel/skunk"
  ).map((from, to) => gh(from) -> gh(to))

  /** The project each result belongs to, in result order, without repeats. */
  def projectsOf(r: SearchResponse): List[String] =
    r.results.map(h => renamed.getOrElse(h.key, h.key)).distinct

  private val modelDir =
    sys.props
      .get("scalapkg.modelDir")
      .map(Path.of(_))
      .filter(d => Files.exists(d.resolve("model.onnx")))

  /** Builds a repo per ranking over one pool and one embedder (queries are
    * embedded once).
    */
  private def repos: Resource[IO, PackageRepo.Ranking => PackageRepo] =
    val db = DbConfig(
      "localhost",
      port.getOrElse(5432),
      sys.env.getOrElse("SCALAPKG_EVAL_DB_USER", "scalapkg"),
      sys.env.get("SCALAPKG_EVAL_DB_PASSWORD"),
      sys.env.getOrElse("SCALAPKG_EVAL_DB_NAME", "scalapkg"),
      None
    )
    for
      sessions <- Database.pool(db)
      // the seed ships without search vectors (they hold README words); build
      // them from the current code, as the server does when it starts
      _ <- Resource.eval(IngestRepo(sessions).rebuildSearchDocs)
      embedder <- modelDir.traverse(Embedder.load(_))
      embed <- Resource.eval(embedder.traverse(_.queryVectors))
    yield
      ranking => PackageRepo.skunk(sessions, embed = embed, ranking = ranking)

  private final case class Score(
      rows: List[(String, Double, Double, Int, List[String])]
  ):
    def recall: Double = rows.map(_._2).sum / rows.size
    def mrr: Double = rows.map(_._3).sum / rows.size

  private def evaluate(r: PackageRepo): IO[Score] =
    golden
      .traverse { (q, expected) =>
        r.search(SearchQuery(q, Nil, Nil, Sort.Relevance, 1)).map { res =>
          val got = projectsOf(res)
          val top5 = got.take(5)
          val wanted = expected.take(5)
          val recall = wanted.count(top5.contains).toDouble / wanted.size
          val rank = got.take(10).indexWhere(expected.contains)
          val rr = if rank < 0 then 0.0 else 1.0 / (rank + 1)
          (q, recall, rr, res.total, got.take(5))
        }
      }
      .map(Score(_))

  test("standard queries find the expected projects") {
    assume(port.isDefined, "set SCALAPKG_EVAL_DB_PORT to run the search evaluation")
    repos.use { make =>
      evaluate(make(PackageRepo.Ranking())).map { score =>
        score.rows.foreach { (q, recall, rr, total, top) =>
          println(f"$q%-24s recall@5 $recall%.2f  rr $rr%.2f  total $total%4d  ${top.map(_.stripPrefix("github.com/")).mkString(", ")}")
        }
        println(f"MEAN recall@5 ${score.recall}%.3f  MRR ${score.mrr}%.3f  over ${score.rows.size} queries")
        assert(score.recall >= SearchEvalSuite.recallFloor, f"recall@5 ${score.recall}%.3f")
        assert(score.mrr >= SearchEvalSuite.mrrFloor, f"MRR ${score.mrr}%.3f")
      }
    }
  }

  // SCALAPKG_EVAL_GRID=1 tries ranking settings and prints the best ones
  test("ranking grid") {
    assume(
      port.isDefined && sys.env.contains("SCALAPKG_EVAL_GRID"),
      "set SCALAPKG_EVAL_DB_PORT and SCALAPKG_EVAL_GRID to tune ranking"
    )
    val grid =
      for
        k <- List(40, 80, 120)
        stars <- List(0.15, 0.3)
        deps <- List(0.15, 0.3)
        prefix <- List(0.2, 0.5, 1.0)
      yield PackageRepo.Ranking(
        rrfK = k,
        starsWeight = stars,
        dependentsWeight = deps,
        prefixWeight = prefix
      )
    repos.use { make =>
      def line(r: PackageRepo.Ranking, s: Score) =
        f"k ${r.rrfK}%2d  stars ${r.starsWeight}%.2f  dependents ${r.dependentsWeight}%.2f  prefix ${r.prefixWeight}%.1f  title ${r.titleWeight}%.1f  ->  recall@5 ${s.recall}%.3f  MRR ${s.mrr}%.3f"
      grid
        .traverse(r => evaluate(make(r)).flatTap(s => IO.println(s"tried ${line(r, s)}")).map(r -> _))
        .map { results =>
          results
            .sortBy((_, s) => -(s.recall + s.mrr))
            .take(12)
            .foreach((r, s) => println(line(r, s)))
        }
    }
  }
}

object SearchEvalSuite:
  // raise these as search improves; they guard against regressions. The
  // ranking is tuned on the full index, which the seed is too small to stand
  // for (`zio` was 27th there among hundreds of zio-* projects); see "Tuning
  // on the full index" in CLAUDE.md. 2026-10-11, 33 queries:
  //   full index (8,450 projects): recall@5 0.786, MRR 0.871
  //   committed seed (no READMEs): recall@5 0.842, MRR 0.889
  // The floors apply to the seed, which CI runs; the margin allows for small
  // differences in embeddings across CPUs.
  val recallFloor = 0.8
  val mrrFloor = 0.85
