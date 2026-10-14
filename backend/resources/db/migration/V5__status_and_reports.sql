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

-- The last run of each background job, for the /status page (written by
-- Scheduler.every). `summary` is the line the last successful run logged;
-- `error` is why the last run failed, cleared by the next success.
CREATE TABLE job_runs (
  job         text PRIMARY KEY,
  started_at  timestamptz,
  finished_at timestamptz,
  ok_at       timestamptz,
  summary     text,
  error       text
);

-- Backfilled from what earlier runs left behind, so the page is right from
-- the first deploy. The other jobs leave no timestamp and run within
-- minutes of a start.
INSERT INTO job_runs (job, finished_at, ok_at)
SELECT 'index-sync', updated_at, updated_at FROM index_cursor
UNION ALL
SELECT 'enrich', max(refreshed_at), max(refreshed_at) FROM artifacts
HAVING max(refreshed_at) IS NOT NULL
UNION ALL
SELECT 'github', max(github_refreshed_at), max(github_refreshed_at) FROM projects
HAVING max(github_refreshed_at) IS NOT NULL
UNION ALL
SELECT 'summaries', max(summary_at), max(summary_at) FROM projects
HAVING max(summary_at) IS NOT NULL;

-- When IndexSync last read Central's index properties, and the latest chunk
-- they named with its publish time.
ALTER TABLE index_cursor
  ADD COLUMN remote_checked_at   timestamptz,
  ADD COLUMN remote_last_chunk   int,
  ADD COLUMN remote_published_at timestamptz;

-- The last chunk applied was Central's latest when it was read, so the cursor
-- names both until the first sync after deploy. Central's publish time is
-- only in its properties, so it waits for that sync.
UPDATE index_cursor
SET remote_checked_at = updated_at, remote_last_chunk = last_incremental;

-- Packages visitors reported as missing or out of date (/status). A report
-- names a groupId and either an artifactId (`artifact_id`, with `name` its
-- package) or only a package name. It is checked against deps.dev when
-- submitted; what deps.dev does not know yet stays 'pending' for the
-- reports job, which asks deps.dev again and then Maven Central.
--
-- reason:  'missing', 'version' or 'other'
-- reports: how many times it was reported while pending
-- outcome: 'pending', 'added' (now indexed), 'updated' (newer version
--          recorded), 'no_change' (already indexed and current),
--          'not_found' or 'not_scala'
-- detail:  what the check found, shown to the visitor
--
-- Nothing identifies a visitor. Rows older than 90 days are deleted daily.
CREATE TABLE package_reports (
  id          bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  group_id    text        NOT NULL,
  artifact_id text,
  name        text        NOT NULL,
  reason      text        NOT NULL,
  note        text,
  reports     int         NOT NULL DEFAULT 1,
  created_at  timestamptz NOT NULL DEFAULT now(),
  checked_at  timestamptz,
  outcome     text        NOT NULL DEFAULT 'pending',
  detail      text
);
-- one pending report per coordinate; reporting it again bumps `reports`
CREATE UNIQUE INDEX package_reports_pending
  ON package_reports (group_id, name, coalesce(artifact_id, ''))
  WHERE outcome = 'pending';
CREATE INDEX package_reports_recent
  ON package_reports (group_id, name, checked_at DESC);
CREATE INDEX package_reports_created ON package_reports (created_at);
