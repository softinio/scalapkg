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
import cats.syntax.all.*
import ciris.*
import com.comcast.ip4s.*

/** Where the server listens; `127.0.0.1` in production, behind nginx. */
final case class HttpConfig(host: Host, port: Port)

/** Postgres connection settings for `Database.pool`. */
final case class DbConfig(
    host: String,
    port: Int,
    user: String,
    password: Option[String],
    database: String,
    // when set (e.g. /run/postgresql on NixOS) connect via unix socket
    unixSocketDir: Option[String]
)

/** Background ingestion. `enabled = false` serves whatever is already in the
  * database (useful for UI work and tests).
  */
final case class IngestConfig(
    enabled: Boolean,
    // development: keep deps.dev enrichment but leave the Maven index alone
    indexSyncEnabled: Boolean,
    freshnessMaxPackages: Int,
    // development: on an empty database read only the last N incremental
    // chunks instead of the 3.3 GB full index
    bootstrapChunks: Option[Int],
    // development: keep downloaded Maven index files here and reuse them;
    // Central limits index downloads per IP
    indexCacheDir: Option[String]
)

/** Topic search: GitHub topics/READMEs (needs a token) and semantic search with
  * a local embedding model (needs the model directory). Each is off when its
  * setting is missing.
  */
final case class SearchConfig(
    githubToken: Option[ciris.Secret[String]],
    modelDir: Option[String],
    semanticMaxDistance: Double,
    summaries: Option[SummaryConfig]
)

/** Generated project summaries (Claude, through the Message Batches API). Off
  * without an API key; the dev seed already carries summaries.
  */
final case class SummaryConfig(apiKey: ciris.Secret[String], model: String)

/** Every setting, read from `SCALAPKG_*` environment variables by `load`, all
  * optional. `baseUrl` is the public origin used in canonical URLs, the feed
  * and the MCP docs.
  */
final case class AppConfig(
    http: HttpConfig,
    db: DbConfig,
    baseUrl: String,
    devAssetsDir: Option[String],
    ingest: IngestConfig,
    search: SearchConfig
)

object AppConfig:

  private given ConfigDecoder[String, Host] =
    ConfigDecoder[String].mapOption("Host")(Host.fromString)

  private val portDecoder: ConfigDecoder[String, Port] =
    ConfigDecoder[String, Int].mapOption("Port")(Port.fromInt)

  val load: IO[AppConfig] =
    val http = (
      env("SCALAPKG_HTTP_HOST")
        .as[Host]
        .default(host"0.0.0.0"),
      env("SCALAPKG_HTTP_PORT")
        .as[Port](using portDecoder)
        .default(port"8080")
    ).parMapN(HttpConfig.apply)

    val db = (
      env("SCALAPKG_DB_HOST").default("localhost"),
      env("SCALAPKG_DB_PORT").as[Int].default(5432),
      env("SCALAPKG_DB_USER").default("scalapkg"),
      env("SCALAPKG_DB_PASSWORD").option,
      env("SCALAPKG_DB_NAME").default("scalapkg"),
      env("SCALAPKG_DB_SOCKET_DIR").option
    ).parMapN(DbConfig.apply)

    val ingest = (
      env("SCALAPKG_INGEST_ENABLED").as[Boolean].default(true),
      env("SCALAPKG_INDEX_SYNC_ENABLED").as[Boolean].default(true),
      env("SCALAPKG_FRESHNESS_MAX_PACKAGES").as[Int].default(20000),
      env("SCALAPKG_INDEX_BOOTSTRAP_CHUNKS").as[Int].option,
      env("SCALAPKG_INDEX_CACHE_DIR").option
    ).parMapN(IngestConfig.apply)

    val search = (
      env("SCALAPKG_GITHUB_TOKEN").secret.option
        .map(_.filter(_.value.trim.nonEmpty)),
      env("SCALAPKG_EMBEDDINGS_MODEL_DIR").option,
      env("SCALAPKG_SEMANTIC_MAX_DISTANCE").as[Double].default(0.35),
      (
        env("SCALAPKG_ANTHROPIC_API_KEY").secret.option
          .map(_.filter(_.value.trim.nonEmpty)),
        env("SCALAPKG_SUMMARY_MODEL").default("claude-haiku-5-5")
      ).parMapN((key, model) => key.map(SummaryConfig(_, model)))
    ).parMapN(SearchConfig.apply)

    (
      http,
      db,
      env("SCALAPKG_BASE_URL").default("http://localhost:8080"),
      env("SCALAPKG_DEV_ASSETS").option,
      ingest,
      search
    ).parMapN(AppConfig.apply).load[IO]
