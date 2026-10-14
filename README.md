# Scala Packages

[![CI](https://github.com/softinio/scalapkg/actions/workflows/ci.yml/badge.svg)](https://github.com/softinio/scalapkg/actions/workflows/ci.yml)
[![Release](https://github.com/softinio/scalapkg/actions/workflows/release.yml/badge.svg)](https://github.com/softinio/scalapkg/actions/workflows/release.yml)

A search index of every Scala library on Maven Central, at
<https://pkg.scalanews.net>, part of [ScalaNews](https://www.scalanews.net).

Search finds libraries by what they do, not just by name: "parse markdown"
finds markdown parsers even when they never use those words. Results are
projects with their modules, filtered by platform (JVM, Scala.js, Scala Native,
sbt and Mill plugins) and Scala version, and ranked by relevance, popularity,
real usage and current Scala support. Clicking a result opens install
snippets for sbt, Mill and scala-cli, with versions, dependencies and security
advisories from [deps.dev](https://deps.dev).

There is also a JSON API (`/api/v1`), an Atom feed of new releases
(`/feed.xml`) and an MCP server (`/mcp`) for AI assistants.

See also [Scaladex](https://index.scala-lang.org), the Scala Center's official
index of Scala libraries.

## How it works

The package list comes from the Maven Central index, and deps.dev is polled
daily for new versions in between its updates. Details come from deps.dev,
project topics from GitHub, and a one-line summary of each project from an AI
model. Search combines Postgres full-text search
with semantic search over sentence embeddings computed locally (pgvector).

Built with Scala 3: an http4s server rendering pages with scalatags, Calico
(Scala.js) islands for search-as-you-type and the package panel, skunk and
PostgreSQL, Tailwind and Mill.

## Development

Everything runs inside the Nix devshell (`nix develop`; type `menu` for its
commands). The repository includes a small dev seed, so you can run the site
without fetching anything:

```bash
nix develop
SCALAPKG_INGEST_ENABLED=false runDev   # PostgreSQL + server on http://localhost:8080
loadSeed                               # in another shell, then restart runDev
```

```bash
./mill __.test       # all tests
./mill __.reformat   # scalafmt
checkCI              # exactly what CI runs; make it pass before opening a PR
```

The database-backed tests and the search quality evaluation, which CI also
runs, are enabled with environment variables. [CLAUDE.md](CLAUDE.md) holds the
detailed developer notes: architecture, configuration, conventions and how to
test changes to search ranking. AI coding agents read it automatically.

## Contributing

**GitHub Issues** are disabled to encourage direct community contribution. When you encounter bugs or documentation issues, please contribute fixes through Pull Requests instead.

**How to contribute:** [Open a PR](https://github.com/softinio/scalapkg/pulls) with your solution, draft changes, or a test reproducing the issue. We'll collaborate from there to refine and merge improvements.

This approach creates faster fixes while building a stronger, community-driven project where everyone benefits from shared contributions.

Everyone taking part is expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md).

## License

Apache-2.0. See [LICENSE](LICENSE) and [NOTICE](NOTICE). Package metadata from
deps.dev is available under CC-BY 4.0.
