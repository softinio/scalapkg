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

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.softinio.scalapkg.db.ArtifactRow
import com.softinio.scalapkg.db.PackageRepo
import com.softinio.scalapkg.domain.PackageRef
import com.softinio.scalapkg.domain.Target
import com.softinio.scalapkg.ingest.Enrich
import com.softinio.scalapkg.maven.IndexRecord
import com.softinio.scalapkg.maven.MavenIndex
import com.softinio.scalapkg.maven.MavenRepo
import com.softinio.scalapkg.db.PackageReport
import com.softinio.scalapkg.db.ReportReason
import com.softinio.scalapkg.domain.ArtifactIds
import com.softinio.scalapkg.ingest.Reports

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.time.Instant
import java.time.OffsetDateTime
import java.util.zip.GZIPOutputStream

class IngestSuite extends munit.FunSuite {

  /** Writes a gzipped index in the nexus chunk format. */
  private def chunk(docs: List[List[(String, String)]]): Array[Byte] =
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(new GZIPOutputStream(bytes))
    out.writeByte(1)
    out.writeLong(System.currentTimeMillis())
    docs.foreach { fields =>
      out.writeInt(fields.size)
      fields.foreach { (name, value) =>
        out.writeByte(0)
        out.writeUTF(name)
        val v = value.getBytes("UTF-8")
        out.writeInt(v.length)
        out.write(v)
      }
    }
    out.close()
    bytes.toByteArray

  private def decode(gz: Array[Byte]) =
    fs2.Stream
      .chunk(fs2.Chunk.array(gz))
      .covary[IO]
      .through(MavenIndex.records)
      .compile
      .toList

  test("index parser fails on a record cut short at a clean gzip end") {
    // a complete gzip stream whose last document stops mid-field
    val whole = chunk(List(List("u" -> "a|b_3|1|NA|jar")))
    val raw = new java.util.zip.GZIPInputStream(
      new java.io.ByteArrayInputStream(whole)
    ).readAllBytes()
    val bytes = new ByteArrayOutputStream()
    val out = new GZIPOutputStream(bytes)
    out.write(raw.take(raw.length - 3))
    out.close()
    assert(decode(bytes.toByteArray).attempt.unsafeRunSync().isLeft)
  }

  test("index parser reads added records and skips the rest") {
    val gz = chunk(
      List(
        // lastModified 2025-01-20T10:00:00Z
        List("u" -> "org.typelevel|cats-core_3|2.13.0|NA|jar", "i" -> "jar|1737367200000|2|0|0|0|jar"),
        List("DESCRIPTOR" -> "NexusIndex", "IDXINFO" -> "1.0|index"),
        List("del" -> "org.old|gone_2.13|1.0|NA|jar"),
        List("u" -> "malformed")
      )
    )
    val records = decode(gz).unsafeRunSync()
    assertEquals(
      records,
      List(
        IndexRecord(
          "org.typelevel",
          "cats-core_3",
          "2.13.0",
          Some(java.time.Instant.parse("2025-01-20T10:00:00Z"))
        )
      )
    )
  }

  test("index parser fails on a truncated stream instead of ending early") {
    val gz = chunk(List(List("u" -> "a|b_3|1|NA|jar")))
    // cut the gzip trailer and part of the record
    val truncated = gz.take(gz.length - 12)
    assert(decode(truncated).attempt.unsafeRunSync().isLeft)
  }

  test("dependency edges keep Scala packages only, without the package itself") {
    val target = com.softinio.scalapkg.db.DepsTarget(
      "io.circe",
      "circe-yaml",
      "0.15.1",
      NonEmptyList.of(
        ("circe-yaml_2.13", "jvm:2.13", "0.15.1"),
        ("circe-yaml_3", "jvm:3", "0.15.1"),
        ("circe-yaml_sjs1_3", "sjs1:3", "0.15.0")
      ).map((id, key, version) =>
        com.softinio.scalapkg.db.DepsArtifact(id, Target.parseKey(key).getOrElse(fail(s"bad target $key")), version)
      )
    )
    val rep = com.softinio.scalapkg.ingest.DepsJob.representative(target)
    assertEquals((rep.artifactId, rep.version), ("circe-yaml_3", "0.15.1"))
    assertEquals(
      com.softinio.scalapkg.ingest.DepsJob.edges(
        target,
        List(
          "org.scala-lang:scala-library",
          "io.circe:circe-yaml-common_3",
          "org.yaml:snakeyaml",
          "org.scalatest:scalatest_3",
          "io.circe:circe-yaml_3"
        )
      ),
      List(PackageRef("io.circe", "circe-yaml-common"), PackageRef("org.scalatest", "scalatest"))
    )
  }

  test("cursor properties are parsed") {
    val c = MavenIndex.parseCursor(
      "nexus.index.chain-id=1318453614498\nnexus.index.last-incremental=939\n"
    )
    assertEquals(c.map(c => (c.chainId, c.lastIncremental)), Right(("1318453614498", 939)))
  }

  test("stable versions win over pre-releases") {
    def better(a: String, b: String) = MavenVersions.isBetter(a, None, b, None)
    def best(vs: String*) = MavenVersions.best(NonEmptyList.fromListUnsafe(vs.toList))(identity, _ => None)
    assert(better("2.13.0", "2.12.0"))
    assert(better("2.10.0", "2.9.0"))
    assert(!better("3.0.0-RC1", "2.13.0"))
    assert(better("2.13.0", "3.0.0-RC1"))
    assert(better("3.0.0-RC2", "3.0.0-RC1"))
    assert(MavenVersions.isPreRelease("1.0.0-M3"))
    assert(MavenVersions.isPreRelease("0.4.0+12-abcdef12-SNAPSHOT"))
    assert(!MavenVersions.isPreRelease("1.0.0"))
    assertEquals(best("1.0.0", "1.1.0-RC1", "1.0.1", "0.9.0"), "1.0.1")
    assertEquals(best("1.0.0-M1", "1.0.0-M2"), "1.0.0-M2")
  }

  test("builds named after a commit and SNAP builds are pre-releases") {
    List(
      "1.0-234-d1a2b53", // dynver (http4s)
      "0.10.3-16-2cae72",
      "2780582bcfd73c8888a7bfa198351e413b684459-0-278058", // Mill
      "3.3.0.0-SNAP3" // scalatestplus
    ).foreach(v => assert(MavenVersions.isPreRelease(v), v))
    // all digits is a date, not a commit
    List("20240101", "1.0.0", "0.23.33", "4.1.0-9.6.0", "1.0.0-scala3")
      .foreach(v => assert(!MavenVersions.isPreRelease(v), v))
  }

  test("a lower version released over a year after a higher one replaces it") {
    def at(d: String) = Some(Instant.parse(s"${d}T00:00:00Z"))
    // kyo-core_3: a stray 3.0.7, two years before 0.19.0
    assert(MavenVersions.isBetter("0.19.0", at("2025-05-12"), "3.0.7", at("2023-05-02")))
    assert(!MavenVersions.isBetter("3.0.7", at("2023-05-02"), "0.19.0", at("2025-05-12")))
    // a maintenance release of an older line within the year does not
    assert(!MavenVersions.isBetter("10.1.15", at("2025-03-01"), "10.2.4", at("2024-06-01")))
    // nor does one whose date is unknown
    assert(!MavenVersions.isBetter("0.19.0", None, "3.0.7", at("2023-05-02")))
    val versions = NonEmptyList.of(
      "3.0.7" -> at("2023-05-02"),
      "0.18.0" -> at("2025-01-10"),
      "0.19.0" -> at("2025-05-12"),
      "1.0.0-RC7" -> at("2026-09-29")
    )
    assertEquals(MavenVersions.best(versions)(_._1, _._2)._1, "0.19.0")
  }

  test("artifacts are grouped into one package row") {
    val at = OffsetDateTime.parse("2025-01-20T10:00:00Z")
    def row(target: String, version: String, project: Option[String]) =
      ArtifactRow("org.typelevel", "cats-core", Target.parseKey(target).getOrElse(fail(s"bad target $target")), version, Some(at), List("MIT"), project, false)
    val pkg = Enrich
      .packageRow(
        NonEmptyList.of(
          row("jvm:2.13", "2.13.0", Some("github.com/typelevel/cats")),
          row("jvm:3", "2.13.0", Some("github.com/typelevel/cats")),
          row("sjs1:3", "2.12.0", None),
          row("jvm:2.11", "2.0.0", Some("github.com/old/cats"))
        )
      )
    assertEquals(pkg.version, "2.13.0")
    assertEquals(pkg.projectId, Some("github.com/typelevel/cats"))
    assertEquals(pkg.targets.map(_.key), List("jvm:3", "jvm:2.13", "jvm:2.11", "sjs1:3"))
    assertEquals(pkg.platforms, List("jvm", "js"))
    assertEquals(pkg.scalaVersions, List("3", "2.13", "2.11"))
    assertEquals(pkg.licenses, List("MIT"))
    assertEquals(pkg.releasedAt, Some(at))
  }

  test("search text becomes a safe prefix tsquery") {
    assertEquals(PackageRepo.tsQuery("cats core"), Some("cats:* & core:*"))
    assertEquals(PackageRepo.tsQuery("cats-effect"), Some("cats:* & effect:*"))
    assertEquals(PackageRepo.tsQuery("a'b & c | !d"), Some("a:* & b:* & c:* & d:*"))
    assertEquals(PackageRepo.tsQuery("  !!  "), None)
  }

  test("cursor properties carry the latest chunk's publish time") {
    val c = MavenIndex.parseCursor(
      "nexus.index.chain-id=1318453614498\nnexus.index.last-incremental=939\nnexus.index.timestamp=20261002043023.811 +0000\n"
    )
    assertEquals(c.map(_.publishedAt), Right(Some(Instant.parse("2026-10-02T04:30:23.811Z"))))
  }

  test("maven-metadata.xml versions are read") {
    val xml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<metadata>
        |  <groupId>dev.zio</groupId>
        |  <artifactId>zio-http_3</artifactId>
        |  <versioning>
        |    <latest>3.1.0</latest>
        |    <release>3.1.0</release>
        |    <versions>
        |      <version>3.0.0</version>
        |      <version> 3.1.0 </version>
        |    </versions>
        |    <lastUpdated>20260930120000</lastUpdated>
        |  </versioning>
        |</metadata>""".stripMargin
    assertEquals(MavenRepo.versions(xml), List("3.0.0", "3.1.0"))
    assertEquals(MavenRepo.versions("<metadata/>"), Nil)
  }

  test("a repository directory listing gives its subdirectories") {
    val html =
      """<a href="../">../</a>
        |<a href="zio-http_3/" title="zio-http_3/">zio-http_3/</a>    2026-09-30 12:00 -
        |<a href="zio-http_sjs1_3/" title="zio-http_sjs1_3/">zio-http_sjs1_3/</a>
        |<a href="maven-metadata.xml" title="maven-metadata.xml">maven-metadata.xml</a>""".stripMargin
    assertEquals(MavenRepo.listing(html), List("zio-http_3", "zio-http_sjs1_3"))
  }

  test("a package name is tried with the common Scala suffixes, an artifactId as it is") {
    val byName = PackageReport("dev.zio", None, "zio-http", ReportReason.Missing, None)
    val byId = byName.copy(artifactId = Some("zio-http_sjs1_2.13"))
    assert(Reports.candidates(byName).contains("zio-http_3"))
    assert(Reports.candidates(byName).forall(a => ArtifactIds.parse(a).exists(_.name == "zio-http")), Reports.candidates(byName))
    assertEquals(Reports.candidates(byId), List("zio-http_sjs1_2.13"))
  }
}
