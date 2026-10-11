# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Scala Packages (pkg.scalanews.net): a search index of Scala libraries on Maven
Central. README.md is deliberately brief; this file is the detailed developer
guide, for agents and the contributors they help.

## Helping a contributor

The project takes contributions as pull requests only (GitHub Issues are
disabled), so a bug report arrives as a PR with a fix or a failing test. When
someone asks how to get started, walk them through:

1. `nix develop` (needs Nix with flakes; `nix develop path:.` if the flake files
   are untracked in their checkout). Everything below runs inside it.
2. Run the site on the committed dev seed, with no network calls:
   `SCALAPKG_INGEST_ENABLED=false runDev` (Postgres in `/tmp`, migrates, serves
   http://localhost:8080), then `loadSeed` in another shell and restart
   `runDev`, since search documents are rebuilt on start. Without
   `SCALAPKG_INGEST_ENABLED=false` an empty database starts reading the full
   Maven index (~3.3 GB).
3. Optional: `fetchModel` (~130 MB) and
   `SCALAPKG_EMBEDDINGS_MODEL_DIR=$PWD/.models/bge-small-en-v1.5` for semantic
   search; `SCALAPKG_GITHUB_TOKEN` (no scopes) for GitHub topics and READMEs.
4. Find the code for their change from the Architecture section below.

Before they open a PR, make sure:

- `checkCI` passes (format, scalafix, compile with fatal warnings, tests), and
  so do the database suites below, which CI runs too.
- A bug fix comes with a test that fails without it. Ranking SQL and home page
  lists are tested in `PackageRepoSuite` against its fixture.
- Changes to search ranking keep `SearchEvalSuite` at or above its recorded
  baseline (see Search below).
- New files carry the license header, and the conventions below hold.

## Commands

Mill is the pinned `./mill` launcher (there is no `mill` on the PATH).

```bash
./mill __.compile
./mill __.test                       # all tests (JVM + JS)
./mill backend.test.testOnly com.softinio.scalapkg.RoutesSuite
./mill shared.jvm.test
./mill __.reformat                   # scalafmt; CI checks __.checkFormat
./mill __.fix                        # scalafix; CI checks __.fix --check
./mill backend.assembly              # fat jar (also builds Tailwind CSS and the Scala.js bundle)
checkCI                              # what CI runs, with CI=true (fatal warnings)
runDev                               # Postgres in /tmp + server on :8080
```

Devshell commands (`menu` lists them): `runDev`, `fetchModel`, `saveSeed` /
`loadSeed`, `runTests`, `runLint`, `checkCI`, `buildJar`, `watchFrontend`
(relink Scala.js on change; pair with
`SCALAPKG_DEV_ASSETS=out/frontend/fastLinkJS.dest`), `startPostgres` /
`stopPostgres` / `postgresStatus` / `resetDB`.

Database-backed suites are skipped unless their port variable is set:

```bash
# ranking/home-page SQL against a fixture; drops and recreates `scalapkg_test` each run
SCALAPKG_TEST_DB_PORT=5432 ./mill backend.test.testOnly com.softinio.scalapkg.PackageRepoSuite
# search quality on the dev seed (loadSeed first); fails below the floors in SearchEvalSuite
SCALAPKG_EVAL_DB_PORT=5432 ./mill backend.test.testOnly com.softinio.scalapkg.SearchEvalSuite
SCALAPKG_EVAL_GRID=1 SCALAPKG_EVAL_DB_PORT=5432 ...   # ranking weight grid
```

The eval uses semantic search only if the embedding model is present. CI runs
both suites against a pgvector service with `seed/seed.sql.gz` loaded.

A running `./mill backend.run` holds the lock on `out/`; build or test in
parallel with `MILL_OUTPUT_DIR=<other dir>`.

## Layout

Mill modules: `shared` (JVM + JS: domain types, circe codecs, artifactId
parsing in `Targets.scala`, install snippets, the deps.dev JSON decoding and
v3 client in `DepsDevJson.scala` used by both the server and the drawer,
Tailwind class tokens), `backend` (http4s server), `frontend` (Calico
islands). Sources live directly in `<module>/src`, tests in
`<module>/test/src`. `tailwind/app.css` holds the palette, fonts and drawer
styles.

## Architecture

**Ingestion** is in-process background jobs (`ingest/Jobs.scala`, scheduled in
`Main.jobs` and `Main.run`):

- index-sync (daily): Maven nexus index → `artifacts`. An empty database
  streams the full index once (~3.3 GB, never written to disk); after that only
  new incremental chunks, which Central publishes about every two weeks. Central
  no longer serves old chunks and rate-limits `/maven2/.index/` per IP, so the
  cursor lives in the database, a cursor too far behind triggers a full read,
  and a 429 skips the run. Only artifactIds with a Scala suffix are kept, each
  with the earliest publish time the index shows for any of its versions.
- enrich (15 min): deps.dev batch API → release dates, licences, source
  repository, deprecation; groups artifacts into `packages`; project stats
  weekly (only for projects some artifact still links).
- freshness (daily): asks deps.dev for newer versions of packages released in
  the last year, since the index is published only every couple of weeks.
- deps (hourly): deps.dev requirements → dependency edges → dependents counts
  (deps.dev serves no dependents for Maven).
- first-releases (5 min): deps.dev fallback for artifacts the index never
  dated.
- github (optional, hourly, weekly per repo): GraphQL, 25 repositories per
  request → topics, README text, repository creation date.
- embed (optional, 10 min): local ONNX bge-small → `search_docs.embedding`,
  only for documents whose text changed.
- summaries (optional, 30 min): Anthropic Message Batches API →
  `projects.summary` + `summary_keywords`. A project is asked again only when
  its input text changes (digits ignored), at most monthly; with a GitHub token
  GitHub projects wait for their README first. About $10 per ten thousand
  projects at Haiku batch prices.

Optional jobs switch off when their key, token or model is missing.
`SCALAPKG_INGEST_ENABLED=false` turns off the Maven/deps.dev jobs;
`SCALAPKG_INDEX_SYNC_ENABLED=false` only the index sync. The jobs run
concurrently (`parSequence_` in `Main.jobs`), so a write that can race another
job must not blindly overwrite: Freshness records versions with a
compare-and-set (`IngestRepo.advance`), and `saveDetails` inserts project rows
before any artifact points at them. `IngestRepo` writes refresh the search
documents they affect themselves; there is no separate reindex step.

**Data model** (`backend/resources/db/migration/`): `artifacts` (one row per Maven
artifactId) → `packages` (artifacts grouped by group + name across
platform/Scala suffixes) → `search_docs` (one document per project, or per
groupId when there is no repository; `packages.doc_key` is the link).
`IngestRepo.rebuildSearchDocs` rebuilds every search document from packages and
projects and runs on every server start. Production has applied the
migrations, so never edit one: a schema change is a new `V<n>__name.sql`,
also listed in `Database.migrate`. Dumbo refuses to start when an applied
file's checksum changed.

**Search events** (`db/SearchLog.scala`, table `search_events`, migration V2)
record searches and what was done with their results, to check ranking
against real use. The search island (`ui/SearchEvents.scala`) records a
search once its results have stayed on screen for 1.5 s with no typing (so
search as you type does not record half-typed queries), reading the total and
result keys from `data-total` / `data-key` in the results HTML. It also
records a result opened from it with its position, and the links followed and
snippets copied in that drawer. Events go by `sendBeacon` to `POST /events`
(`EventRoutes`, which checks them; its own 60/min limit). They are recorded in
the browser because Cloudflare caches `/fragments/search`, so the server never
sees most searches. MCP's `search_packages`, `get_package` and
`install_snippet` are recorded on the server. Recording never waits: a
bounded queue is written in batches and drops events when full. Nothing
identifies a visitor (a random id per tab, no address); rows are deleted
after 90 days, and the About page says so. `scripts/search-report.sql` turns
them into the queries to add to `SearchEvalSuite`. The seed never includes
this table.

**Search** (`db/PackageRepo.scala`): a keyword list (Postgres FTS with field
weights A name, B description/topics/summary/keywords, C module names/groups,
D README; whole and stemmed words, prefixes at a lower weight, plus trigram
title similarity for typos)
fused by reciprocal rank with a semantic list (pgvector, within
`SCALAPKG_SEMANTIC_MAX_DISTANCE`; for one-word queries only when nothing matches
by keyword), then multiplied by popularity (stars, dependents), a Scala-version
factor (2.13 and 3 best; 2.13-only, 3-only and older lower; sbt/Mill plugins
exempt) and penalties for deprecated and typo-only matches. An established
project (`Ranking.exactNameStars` / `exactNameDependents`) whose name is the
query comes first, so `zio` finds zio/zio among hundreds of zio-* projects
while a tiny repo called `json` gains nothing. A hit is "strong"
when it matches the project's own text (weights A/B, a close title typo, or the
owner name); the Stars and Recent sorts keep only strong hits, and Best match
sets `related` when none are strong. `PackageRepo.Ranking` holds the
constants, tuned on the full index (below). Change them only with the eval or
grid run on it, and update the baseline comment in `SearchEvalSuite`.

The seed is small, so a ranking that scores well on it can still fail on the
full index. To tune on real data, restore a production dump into its own
database and point the eval at it (leave `search_events` behind; it stays on
the server):

```bash
# on the server
pg_dump -Fc --exclude-table-data=search_events scalapkg > scalapkg.dump
# locally
createdb -h localhost -U scalapkg scalapkg_full
pg_restore -h localhost -U scalapkg -d scalapkg_full --no-owner scalapkg.dump
SCALAPKG_EVAL_DB_NAME=scalapkg_full SCALAPKG_EVAL_DB_PORT=5432 ./mill backend.test.testOnly com.softinio.scalapkg.SearchEvalSuite
```

The floors are set for the seed, so the eval may fail on the full index; the
per-query lines are what to read there.

**Home page lists** (`newest`, `mostStarred`, `newProjects` in `PackageRepo`)
show one main module per project (not under a groupId the project moved away
from, then most dependents, not test/bench/internal, most targets, shortest
name). A project that republished its modules under a new groupId
(`com.typesafe.play` → `org.playframework`) is shown by the new one
(`movedGroups`), on the home page and in search results. New projects are those whose earliest artifact
release is within `newProjectDays` and whose GitHub repo is not much older than
that release, so projects that only moved Maven coordinates are left out. Most
starred only lists projects publishing for 2.13 or 3 (or plugins), and new
releases only those with a source repository (the feed and releases API keep
them all).

**Renamed repositories**: artifacts released before a GitHub rename or
transfer still name the old repository. The GitHub job records the new one in
`projects.moved_to` (migration V3), and packages join that project, or when
it is not indexed the first of the old names that moved to it
(`IngestRepo.currentProject`), so tpolecat/skunk and typelevel/skunk are one
result. About one GitHub project in eleven has moved. Without a GitHub token nothing is merged.

**Web**: scalatags server rendering (`html/Pages.scala`, `Layout.scala`), JSON
API `/api/v1/*` and MCP JSON-RPC at `/mcp` (`routes/`). The frontend mounts
Calico islands on `data-island` elements. The search island swaps in
`/fragments/search` HTML and the drawer reads package data from `data-pkg`
attributes and calls api.deps.dev from the browser. Every page works without JS
(SSR fallback, `/p/{group}/{name}` deep links). `RateLimit` (60 per client per
minute) wraps only `/api`, `/mcp` and `/events` and keys on `X-Real-IP`, which
only nginx may set; the app must not be reachable except through nginx.

## Configuration

Environment variables, all optional (`backend/src/Config.scala`):

| Variable | Default | |
| --- | --- | --- |
| `SCALAPKG_HTTP_HOST` / `_HTTP_PORT` | `0.0.0.0` / `8080` | `127.0.0.1` in production, behind nginx |
| `SCALAPKG_BASE_URL` | `http://localhost:8080` | canonical URLs, feed, MCP docs |
| `SCALAPKG_DB_HOST` / `_PORT` / `_USER` / `_PASSWORD` / `_NAME` | `localhost` / `5432` / `scalapkg` / – / `scalapkg` | |
| `SCALAPKG_DB_SOCKET_DIR` | – | connect over a unix socket (production) |
| `SCALAPKG_DEV_ASSETS` | – | serve JS from a directory during development |
| `SCALAPKG_INGEST_ENABLED` | `true` | `false` serves what is already stored |
| `SCALAPKG_FRESHNESS_MAX_PACKAGES` | `20000` | packages checked per daily freshness run |
| `SCALAPKG_INDEX_BOOTSTRAP_CHUNKS` | – | dev: start from the last N chunks instead of the full index |
| `SCALAPKG_INDEX_CACHE_DIR` | – | dev: keep downloaded index files here and reuse them |
| `SCALAPKG_INDEX_SYNC_ENABLED` | `true` | dev: `false` keeps deps.dev enrichment but never touches the Maven index |
| `SCALAPKG_GITHUB_TOKEN` | – | GitHub token (no scopes needed) for topics, READMEs and repository creation dates (which keep projects that only moved Maven coordinates out of "New projects") |
| `SCALAPKG_EMBEDDINGS_MODEL_DIR` | – | directory with `model.onnx` + `vocab.txt` for semantic search |
| `SCALAPKG_SEMANTIC_MAX_DISTANCE` | `0.35` | cosine distance cut-off for semantic matches |
| `SCALAPKG_ANTHROPIC_API_KEY` | – | Anthropic API key for generated project summaries |
| `SCALAPKG_SUMMARY_MODEL` | `claude-haiku-5-5` | model that writes the summaries |

## Releasing and deploying

Tag `vX.Y.Z`; `.github/workflows/release.yml` builds the assembly jar and
attaches it to a GitHub release (CI also uploads the jar built on `main`).
How the jar is deployed is kept outside this repository.

The app expects to run behind a reverse proxy and a caching CDN, and protects
itself because CPU (each search is a Postgres query plus an embedding) is what
a scraper could exhaust:

- `RateLimit` (60/min per client on `/api` and `/mcp`) keys on `X-Real-IP`,
  so the proxy must overwrite that header and the app must listen only where
  the proxy can reach it.
- `SearchGate` allows 2 searches per core; beyond that the request gets 503
  with `Retry-After`, and past half full search is keyword-only.
- The web pool has a 2 s statement timeout (also a 503); ingestion has its own
  pool without one. Queries are cut at 200 characters.
- Responses carry the `Cache-Control` a CDN should follow (`Html.cacheControl`:
  search 60 s, home/`/p/`/feed 10 min, API per endpoint, assets a year); 404s
  and 503s are not cacheable.
- MCP's deps.dev lookups are cached for an hour, and the jar ships no source
  map.

## Conventions and pitfalls

- Every source file starts with the Apache-2.0 header (Scala/Mill `/* */`,
  `#` or `--` comments elsewhere; in `build.mill` below the `//|` directives).
- Frontend handlers that must stop the browser's default action use
  `Dom.intercept`, which decides synchronously. `Dom.listen` runs the handler
  asynchronously, so `preventDefault` inside it comes too late.
- In skunk `sql"..."` interpolations a literal `$` must be written `$$`
  (e.g. regex anchors).
- Keys are typed: `ArtifactKey` (groupId + artifactId) and `PackageRef`
  (groupId + package name). IngestRepo's `artifactIn` / `packageIn` fragments
  only accept the matching type; don't go back to bare `(String, String)`.
- `Http.retrying` retries only safe-to-repeat requests. Never retry a request
  with side effects, such as the Anthropic batch submit (a repeat creates and
  bills a second batch).
- Maven Central rate-limits `/maven2/.index/` per IP to a few downloads a day.
  For dev use `SCALAPKG_INDEX_BOOTSTRAP_CHUNKS` / `SCALAPKG_INDEX_CACHE_DIR`,
  or just the seed.
- `seed/seed.sql.gz` (via `scripts/seed.sh`, `saveSeed`/`loadSeed`) is a small
  local index (~140 projects, ~800 packages, with summaries and embeddings). It
  excludes READMEs and search vectors (others' text); keep it that way. Re-save
  only when the data meaningfully changes, since each version adds ~1 MB to
  history. Its deps.dev data is CC-BY 4.0 (see `NOTICE`).
- The summaries prompt and cleaning live in `summary/Anthropic.scala`
  (`Summaries`).
