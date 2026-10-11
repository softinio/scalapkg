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

-- A GitHub repository that was renamed or transferred keeps its old id on
-- the artifacts released before the move (tpolecat/skunk, now
-- typelevel/skunk). The GitHub job records where it went, and its packages
-- join the new project, or when that is not indexed the other old names
-- that moved to it, so it is one result.
ALTER TABLE projects ADD COLUMN moved_to text;
CREATE INDEX projects_moved_to ON projects (moved_to) WHERE moved_to IS NOT NULL;
