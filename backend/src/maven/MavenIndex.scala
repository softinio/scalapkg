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

package com.softinio.scalapkg.maven

import cats.effect.IO
import cats.syntax.all.*
import com.softinio.scalapkg.db.IndexCursor
import fs2.Pipe
import fs2.Stream
import fs2.compression.Compression
import fs2.interop.scodec.StreamDecoder
import fs2.io.file.CopyFlag
import fs2.io.file.CopyFlags
import fs2.io.file.Files
import fs2.io.file.Path
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.UnexpectedStatus
import scodec.Codec
import scodec.Decoder
import scodec.Err
import scodec.bits.ByteVector
import scodec.codecs.*

import java.io.StringReader
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Properties

/** A `u` (added) entry of the Maven Central nexus index. `publishedAt` is the
  * file's last-modified time from the `i` field, which is when the version was
  * published.
  */
final case class IndexRecord(
    groupId: String,
    artifactId: String,
    version: String,
    publishedAt: Option[Instant] = None
)

/** The incremental chunk asked for is no longer served by Central. */
final case class ChunkGone(n: Int)
    extends RuntimeException(s"Maven index chunk $n is no longer served")

/** Central's per-IP quota for /maven2/.index/ is used up. */
final case class IndexRateLimited(retryAfter: Option[String])
    extends RuntimeException(
      "Maven Central's per-IP limit for the index directory is used up" +
        retryAfter.fold("")(s => s"; retry after ${s}s") +
        " (https://central.sonatype.org/faq/429-maven-index/)"
    )

/** Reads the Maven Central nexus index: the properties file (chain id and
  * latest incremental chunk), the small incremental chunks (published about
  * every two weeks), and the full index (~3.3 GB) for the initial load.
  *
  * Central rate-limits downloads from /maven2/.index/ per IP and only serves
  * recent incremental chunks, so: read each file once, and keep the cursor in
  * the database. With `cacheDir` set, downloaded files are also kept on disk
  * and reused (development, where the database gets reset). Without it
  * everything is streamed and nothing is written.
  */
final class MavenIndex(client: Client[IO], cacheDir: Option[Path]):

  private val prefix = "nexus-maven-repository-index"

  /** Central's current chain id and latest chunk, from the properties file.
    * This and the other downloads fail with `IndexRateLimited` on a 429.
    */
  def remoteCursor: IO[IndexCursor] =
    download(s"$prefix.properties")
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .flatMap(body =>
        IO.fromEither(
          MavenIndex.parseCursor(body).left.map(IllegalArgumentException(_))
        )
      )

  /** Incremental chunk `n`; fails with `ChunkGone` once Central no longer
    * serves it.
    */
  def chunk(n: Int): Stream[IO, IndexRecord] =
    records(s"$prefix.$n.gz").adaptError {
      case UnexpectedStatus(Status.NotFound, _, _) => ChunkGone(n)
    }

  def full: Stream[IO, IndexRecord] =
    records(s"$prefix.gz")

  private def records(name: String): Stream[IO, IndexRecord] =
    cached(name).through(MavenIndex.records)

  private def cached(name: String): Stream[IO, Byte] =
    cacheDir match
      case None      => download(name)
      case Some(dir) =>
        val file = dir / name
        val part = dir / s"$name.part"
        Stream.eval(Files[IO].exists(file)).flatMap {
          case true  => Files[IO].readAll(file)
          case false =>
            Stream.exec(
              Files[IO].createDirectories(dir) *>
                download(name)
                  .through(Files[IO].writeAll(part))
                  .compile
                  .drain *>
                Files[IO].move(part, file, CopyFlags(CopyFlag.ReplaceExisting))
            ) ++ Files[IO].readAll(file)
        }

  private def download(name: String): Stream[IO, Byte] =
    val req = Request[IO](uri = MavenIndex.central / name)
    Stream.resource(client.run(req)).flatMap { resp =>
      resp.status match
        case Status.Ok              => resp.body
        case Status.TooManyRequests =>
          Stream.raiseError[IO](
            IndexRateLimited(
              resp.headers
                .get(org.typelevel.ci.CIString("Retry-After"))
                .map(_.head.value)
            )
          )
        case status =>
          Stream.raiseError[IO](UnexpectedStatus(status, req.method, req.uri))
    }

object MavenIndex:

  val central: Uri =
    Uri.unsafeFromString("https://repo1.maven.org/maven2/.index")

  def parseCursor(body: String): Either[String, IndexCursor] =
    val props = new Properties()
    props.load(new StringReader(body))
    def prop[A](key: String)(read: String => Option[A]): Either[String, A] =
      Option(props.getProperty(s"nexus.index.$key"))
        .flatMap(read)
        .toRight(s"missing nexus.index.$key")
    // when the latest chunk was published, e.g. `20261002043023.811 +0000`
    val published = prop("timestamp")(s =>
      scala.util.Try(ZonedDateTime.parse(s.trim, timestampFormat)).toOption
    ).toOption.map(_.toInstant)
    for
      chainId <- prop("chain-id")(Some(_).filter(_.nonEmpty))
      last <- prop("last-incremental")(_.toIntOption)
    yield IndexCursor(chainId, last, published)

  private val timestampFormat =
    DateTimeFormatter.ofPattern("yyyyMMddHHmmss.SSS Z")

  /** Decodes a gzipped index (incremental chunk or full index) as it streams
    * in. Only `u` (added) records come out; `del` (removed), descriptor and
    * other documents are skipped.
    */
  val records: Pipe[IO, Byte, IndexRecord] =
    _.through(Compression[IO].gunzip(64 * 1024))
      .flatMap(_.content)
      .through(IndexFormat.records.toPipeByte)

/** The nexus index binary format (Apache Maven Indexer, `IndexDataWriter`):
  * after gunzip, a header of `byte version` and `long timestamp`, then
  * documents until the end of the stream. A document is an `int` field count
  * and that many fields of `byte flags`, a name (`unsigned short` length +
  * UTF-8) and a value (`int` length + UTF-8). A document ends only on a field
  * boundary, so a truncated download fails instead of ending early.
  */
private object IndexFormat:

  private val header: Codec[Unit] = (uint8 :: int64).unit((0, 0L))

  // values are kept as raw bytes: only `u` and `i` are ever decoded
  private val field: Codec[(String, ByteVector)] =
    ignore(8) ~> (variableSizeBytes(uint16, utf8) :: variableSizeBytes(
      int32,
      bytes
    ))

  private val document: Codec[List[(String, ByteVector)]] =
    listOfN(int32, field)

  private def record(fields: List[(String, ByteVector)]): Option[IndexRecord] =
    fields
      .collectFirst { case ("u", v) => v }
      .flatMap(_.decodeUtf8.toOption.map(_.split('|')).collect {
        case Array(g, a, v, _*) => IndexRecord(g, a, v, publishedAt(fields))
      })

  /** `i` is `packaging|lastModified|size|...`, lastModified in epoch millis. */
  private def publishedAt(fields: List[(String, ByteVector)]): Option[Instant] =
    fields
      .collectFirst { case ("i", v) => v }
      .flatMap(_.decodeUtf8.toOption)
      .flatMap(_.split('|').lift(1))
      .flatMap(_.toLongOption)
      .filter(_ > 0)
      .map(Instant.ofEpochMilli)

  val records: StreamDecoder[IndexRecord] =
    StreamDecoder
      .once(header)
      .flatMap(_ =>
        StreamDecoder
          .many(document)
          .flatMap(doc => StreamDecoder.emits(record(doc).toList))
      ) ++ StreamDecoder.once(truncated)

  // `many` stops at a partial document and hands back the leftover bytes;
  // the stream only ends cleanly when there are none
  private val truncated: Decoder[Nothing] =
    fail(Err("index ends in the middle of a document (truncated download?)"))
