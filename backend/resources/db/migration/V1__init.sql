-- Copyright 2026 Salar Rahmanian
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

CREATE EXTENSION IF NOT EXISTS pg_trgm;
-- pgvector must be installed; production creates the extension as the
-- postgres superuser before the app starts
CREATE EXTENSION IF NOT EXISTS vector;

-- Position in the Maven Central nexus incremental index. One row.
CREATE TABLE index_cursor (
  id               boolean PRIMARY KEY DEFAULT true CHECK (id),
  chain_id         text        NOT NULL,
  last_incremental int         NOT NULL,
  updated_at       timestamptz NOT NULL DEFAULT now()
);

-- Source repositories, keyed by deps.dev project id (github.com/org/repo),
-- with GitHub topics and plain-text README for search. `summary` (one line
-- for result cards) and `summary_keywords` (search only) are written by an
-- LLM from that text (SummaryJob); `summary_hash` is the hash of the input
-- they were made from.
CREATE TABLE projects (
  id                  text PRIMARY KEY,
  description         text,
  stars               int,
  forks               int,
  open_issues         int,
  license             text,
  homepage            text,
  scorecard           real,
  refreshed_at        timestamptz,
  topics              text[]      NOT NULL DEFAULT '{}',
  readme              text,
  github_refreshed_at timestamptz,
  -- when the GitHub repository was created: a project that only moved to new
  -- Maven coordinates is not a new project
  repo_created_at     timestamptz,
  summary             text,
  summary_keywords    text[]      NOT NULL DEFAULT '{}',
  summary_hash        text,
  summary_at          timestamptz
);

-- Anthropic message batches asking for project summaries, until their
-- results are collected; request i of a batch is for project_ids[i], made
-- from input with hash hashes[i].
CREATE TABLE summary_batches (
  id          text PRIMARY KEY,
  project_ids text[]      NOT NULL,
  hashes      text[]      NOT NULL,
  created_at  timestamptz NOT NULL DEFAULT now()
);

-- One row per Scala Maven artifact (cats-core_3, cats-core_sjs1_3, ...),
-- tracking only its latest version.
CREATE TABLE artifacts (
  group_id          text        NOT NULL,
  artifact_id       text        NOT NULL,
  name              text        NOT NULL,
  target            text        NOT NULL,
  latest_version    text        NOT NULL,
  latest_release_at timestamptz,
  licenses          text[]      NOT NULL DEFAULT '{}',
  project_id        text,
  deprecated        boolean     NOT NULL DEFAULT false,
  needs_refresh     boolean     NOT NULL DEFAULT true,
  refreshed_at      timestamptz,
  -- earliest publish time seen in the index (all of it on a full read);
  -- filled from deps.dev when the index never showed one
  first_release_at  timestamptz,
  first_release_checked boolean NOT NULL DEFAULT false,
  PRIMARY KEY (group_id, artifact_id)
);
CREATE INDEX artifacts_pending ON artifacts (refreshed_at NULLS FIRST) WHERE needs_refresh;
CREATE INDEX artifacts_undated ON artifacts (group_id, artifact_id)
  WHERE first_release_at IS NULL AND NOT first_release_checked;
CREATE INDEX artifacts_package ON artifacts (group_id, name);
CREATE INDEX artifacts_project ON artifacts (project_id);

-- All targets of one library, grouped by groupId + name. `search` matches
-- the modules listed under a search result; `doc_key` is the search document
-- (project, or groupId without a known repository) it belongs to.
-- `dependents` counts other projects depending on it (package_deps);
-- `deps_version` is the version whose requirements are in package_deps.
CREATE TABLE packages (
  group_id          text    NOT NULL,
  name              text    NOT NULL,
  project_id        text    REFERENCES projects (id) ON DELETE SET NULL,
  latest_version    text    NOT NULL,
  latest_release_at timestamptz,
  targets           text[]  NOT NULL DEFAULT '{}',
  platforms         text[]  NOT NULL DEFAULT '{}',
  scala_versions    text[]  NOT NULL DEFAULT '{}',
  licenses          text[]  NOT NULL DEFAULT '{}',
  deprecated        boolean NOT NULL DEFAULT false,
  search            tsvector NOT NULL DEFAULT ''::tsvector,
  dependents        int     NOT NULL DEFAULT 0,
  deps_version      text,
  doc_key           text GENERATED ALWAYS AS (coalesce(project_id, 'maven:' || group_id)) STORED,
  PRIMARY KEY (group_id, name)
);
CREATE INDEX packages_search ON packages USING gin (search);
CREATE INDEX packages_name_trgm ON packages USING gin (name gin_trgm_ops);
CREATE INDEX packages_platforms ON packages USING gin (platforms);
CREATE INDEX packages_scala_versions ON packages USING gin (scala_versions);
CREATE INDEX packages_released ON packages (latest_release_at DESC NULLS LAST);
CREATE INDEX packages_project ON packages (project_id);
CREATE INDEX packages_doc ON packages (doc_key);
CREATE INDEX packages_deps_stale ON packages (group_id, name)
  WHERE deps_version IS DISTINCT FROM latest_version;

-- Direct dependencies between Scala packages, any scope (tests count: a
-- testing library is popular because projects test with it), read from the
-- deps.dev requirements of each package's latest version. deps.dev serves no
-- dependents for Maven, so they are counted from these edges.
CREATE TABLE package_deps (
  group_id     text NOT NULL,
  name         text NOT NULL,
  dep_group_id text NOT NULL,
  dep_name     text NOT NULL,
  PRIMARY KEY (group_id, name, dep_group_id, dep_name)
);
CREATE INDEX package_deps_dep ON package_deps (dep_group_id, dep_name);

-- What search ranks: one document per project (all its modules), or per
-- groupId for packages without a known source repository. Rebuilt from
-- packages and projects (IngestRepo.rebuildSearchDocs). `embedding` is the
-- bge-small-en-v1.5 sentence embedding of its text; `embedding_stale` marks
-- documents whose text may have changed.
CREATE TABLE search_docs (
  key                 text PRIMARY KEY,
  project_id          text,
  title               text        NOT NULL,
  platforms           text[]      NOT NULL DEFAULT '{}',
  scala_versions      text[]      NOT NULL DEFAULT '{}',
  latest_release_at   timestamptz,
  -- every module of the project is deprecated
  deprecated          boolean     NOT NULL DEFAULT false,
  -- other projects depending on any of its modules
  dependents          int         NOT NULL DEFAULT 0,
  search              tsvector    NOT NULL DEFAULT ''::tsvector,
  embedding           vector(384),
  embedding_text_hash text,
  embedding_stale     boolean     NOT NULL DEFAULT true
);
CREATE INDEX search_docs_search ON search_docs USING gin (search);
CREATE INDEX search_docs_title_trgm ON search_docs USING gin (title gin_trgm_ops);
CREATE INDEX search_docs_platforms ON search_docs USING gin (platforms);
CREATE INDEX search_docs_scala_versions ON search_docs USING gin (scala_versions);
CREATE INDEX search_docs_released ON search_docs (latest_release_at DESC NULLS LAST);
CREATE INDEX search_docs_embedding ON search_docs USING hnsw (embedding vector_cosine_ops);
CREATE INDEX search_docs_embedding_stale ON search_docs (embedding_stale) WHERE embedding_stale;

-- New versions as they are seen, for /api/v1/releases and the Atom feed.
CREATE TABLE releases (
  group_id    text        NOT NULL,
  name        text        NOT NULL,
  version     text        NOT NULL,
  released_at timestamptz NOT NULL,
  PRIMARY KEY (group_id, name, version)
);
CREATE INDEX releases_released ON releases (released_at DESC);
