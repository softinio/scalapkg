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

-- Search quality report from search_events (the last 30 days; change
-- :'days' with -v days=N). Run against the production database:
--
--   psql -d scalapkg -f scripts/search-report.sql
--   psql -d scalapkg -v days=7 -f scripts/search-report.sql
--
-- Sessions with implausibly many searches (scripts, floods) are left out.
-- Queries that do badly here are candidates for SearchEvalSuite: add them
-- with the projects people actually used (copied snippets count most), then
-- tune PackageRepo.Ranking with the grid run.

\if :{?days}
\else
  \set days 30
\endif

CREATE TEMP VIEW recent AS
  SELECT * FROM search_events
  WHERE at >= now() - make_interval(days => :days)
    AND (session IS NULL OR session NOT IN (
      SELECT session FROM search_events
      WHERE kind = 'search' AND session IS NOT NULL
        AND at >= now() - make_interval(days => :days)
      GROUP BY session HAVING count(*) > 200));

CREATE TEMP VIEW searches AS
  SELECT s.source, s.search_id, lower(s.query) AS query, s.filters, s.total,
         s.results,
         min(o.position) FILTER (WHERE o.kind = 'open') AS first_open,
         coalesce(bool_or(o.kind = 'copy'), false) AS copied,
         coalesce(bool_or(o.kind IN ('open', 'link', 'copy')), false) AS acted
  FROM recent s
  LEFT JOIN recent o ON o.search_id = s.search_id AND o.kind <> 'search'
  WHERE s.kind = 'search'
  GROUP BY 1, 2, 3, 4, 5, 6;

\echo '== Overview (web searches act on results; MCP outcomes cannot be linked)'
SELECT source,
       count(*) AS searches,
       round(100.0 * avg((total = 0)::int), 1) AS zero_result_pct,
       round(100.0 * avg(acted::int), 1) AS acted_pct,
       round(100.0 * avg(copied::int), 1) AS copied_pct,
       -- mean reciprocal rank of the first result opened (0 when none)
       round(avg(coalesce(1.0 / first_open, 0)), 3) AS click_mrr
FROM searches GROUP BY source ORDER BY source;

\echo '== Most frequent queries'
SELECT query, count(*) AS n,
       round(100.0 * avg(acted::int)) AS acted_pct,
       round(avg(first_open), 1) AS avg_first_open
FROM searches WHERE query IS NOT NULL
GROUP BY query ORDER BY n DESC LIMIT 30;

\echo '== Queries with no results'
SELECT query, filters, count(*) AS n
FROM searches WHERE total = 0 AND query IS NOT NULL
GROUP BY query, filters ORDER BY n DESC LIMIT 30;

\echo '== Frequent web queries where people rarely act, or act far down the list'
SELECT query, count(*) AS n,
       round(100.0 * avg(acted::int)) AS acted_pct,
       round(avg(first_open), 1) AS avg_first_open
FROM searches
WHERE source = 'web' AND query IS NOT NULL AND total > 0
GROUP BY query
HAVING count(*) >= 3 AND (avg(acted::int) < 0.3 OR avg(first_open) > 4)
ORDER BY n DESC LIMIT 30;

\echo '== What people used for each query (strongest action, top 3 per query)'
SELECT query, result, uses, copies FROM (
  SELECT lower(s.query) AS query, o.result,
         count(*) AS uses,
         count(*) FILTER (WHERE o.kind = 'copy') AS copies,
         row_number() OVER (PARTITION BY lower(s.query)
                            ORDER BY count(*) FILTER (WHERE o.kind = 'copy') DESC,
                                     count(*) DESC) AS rank
  FROM recent s JOIN recent o ON o.search_id = s.search_id AND o.kind <> 'search'
  WHERE s.kind = 'search' AND s.query IS NOT NULL
  GROUP BY 1, 2
) t WHERE rank <= 3 AND query IN (
  SELECT lower(query) FROM recent WHERE kind = 'search' AND query IS NOT NULL
  GROUP BY 1 ORDER BY count(*) DESC LIMIT 30)
ORDER BY query, rank;

\echo '== Rephrasings: a search followed within 30 s by another in the same tab'
SELECT a.query AS first, b.query AS then, count(*) AS n
FROM recent a
JOIN LATERAL (
  SELECT query FROM recent b
  WHERE b.session = a.session AND b.kind = 'search'
    AND b.at > a.at AND b.at <= a.at + interval '30 seconds'
    AND b.query IS DISTINCT FROM a.query
  ORDER BY b.at LIMIT 1) b ON true
WHERE a.kind = 'search' AND a.session IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM recent o
                  WHERE o.search_id = a.search_id AND o.kind <> 'search')
GROUP BY 1, 2 ORDER BY n DESC LIMIT 30;

\echo '== MCP: most looked-up packages'
SELECT result, count(*) FILTER (WHERE kind = 'open') AS lookups,
       count(*) FILTER (WHERE kind = 'copy') AS snippets
FROM recent WHERE source = 'mcp' AND kind <> 'search'
GROUP BY result ORDER BY count(*) DESC LIMIT 20;
