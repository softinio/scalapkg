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

-- Searches and what was done with their results, to check ranking against
-- real use (scripts/search-report.sql). Nothing identifies a visitor:
-- `session` is a random id per browser tab, and no address or user agent is
-- kept. Rows older than 90 days are deleted daily (SearchLog.prune).
--
-- source:    'web' (the search and drawer islands) or 'mcp' (tool calls)
-- kind:      'search', then 'open' (drawer or get_package), 'link' (a link
--            followed from the drawer) or 'copy' (an install snippet copied,
--            or install_snippet)
-- search_id: ties an outcome to its search; NULL for MCP outcomes, which
--            MCP's stateless calls cannot link
-- filters:   the search's other parameters as `key=value` (platform, scala,
--            sort, page)
-- results:   the first result keys shown, in order (search only)
-- result:    the result acted on: a search-result key on the web,
--            `groupId:name` for MCP
-- position:  1-based rank of `result` across pages (web only)
-- detail:    what was opened, followed or copied (package, link, build tool)
CREATE TABLE search_events (
  at         timestamptz NOT NULL DEFAULT now(),
  source     text        NOT NULL,
  kind       text        NOT NULL,
  search_id  text,
  session    text,
  query      text,
  filters    text[]      NOT NULL DEFAULT '{}',
  total      int,
  results    text[]      NOT NULL DEFAULT '{}',
  result     text,
  position   int,
  detail     text
);

-- rows arrive in time order, so a BRIN index stays tiny
CREATE INDEX search_events_at ON search_events USING brin (at);
