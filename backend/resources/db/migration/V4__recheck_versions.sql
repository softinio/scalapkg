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

-- The latest version used to be the highest number of its kind, so a stray
-- publish (kyo-core 3.0.7, two years before 0.19.0) and builds named after a
-- commit (http4s 1.0-234-d1a2b53) stood for their packages. The choice now
-- leaves those out, and the freshness job asks deps.dev again for every
-- version of each package marked here, then clears the mark.
ALTER TABLE packages ADD COLUMN recheck_versions boolean NOT NULL DEFAULT false;
UPDATE packages SET recheck_versions = true;
CREATE INDEX packages_recheck_versions ON packages (group_id, name) WHERE recheck_versions;
