#!/usr/bin/env bash
# Copyright 2026 Salar Rahmanian
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Dev seed: a snapshot of the local database's data, so a fresh clone (and CI)
# can run the site and the search evaluation without Maven, deps.dev, GitHub
# or Anthropic calls.
#
#   scripts/seed.sh save   # local database -> seed/seed.sql.gz
#   scripts/seed.sh load   # seed/seed.sql.gz -> local database (schema must exist)
#
# READMEs are other authors' text, so the seed leaves them out, along with the
# search vectors built partly from them; the server rebuilds search vectors
# from the remaining text when it starts (as does SearchEvalSuite).
#
# Connection settings come from the usual PG* variables (PGHOST, PGPORT,
# PGUSER, PGDATABASE), defaulting to the devshell's local database.
set -euo pipefail

export PGHOST="${PGHOST:-localhost}" PGUSER="${PGUSER:-scalapkg}" PGDATABASE="${PGDATABASE:-scalapkg}"
seed="$(dirname "$0")/../seed/seed.sql.gz"
# parents before children (packages references projects)
tables=(index_cursor projects artifacts packages package_deps search_docs releases)

# a column's value in the seed: left out for READMEs and search vectors
column_expr() {
  case "$1.$2" in
    projects.readme) echo "NULL" ;;
    search_docs.search | packages.search) echo "''::tsvector" ;;
    *) echo "\"$2\"" ;;
  esac
}

save() {
  mkdir -p "$(dirname "$seed")"
  {
    echo "BEGIN;"
    echo "TRUNCATE $(IFS=,; echo "${tables[*]}") CASCADE;"
    for t in "${tables[@]}"; do
      cols=$(psql -XtA -c "SELECT column_name FROM information_schema.columns
                           WHERE table_schema = 'public' AND table_name = '$t'
                             AND is_generated = 'NEVER'
                           ORDER BY ordinal_position")
      names=$(echo "$cols" | sed 's/.*/"&"/' | paste -sd, -)
      exprs=$(for c in $cols; do column_expr "$t" "$c"; done | paste -sd, -)
      echo "COPY $t ($names) FROM stdin;"
      psql -XqA -c "COPY (SELECT $exprs FROM $t ORDER BY 1) TO STDOUT"
      echo '\.'
    done
    echo "COMMIT;"
  } | gzip -9n > "$seed.tmp"
  mv "$seed.tmp" "$seed"
  echo "saved $seed ($(du -h "$seed" | cut -f1))"
}

load() {
  gunzip -c "$seed" | psql -Xq -v ON_ERROR_STOP=1
  psql -XtAc "SELECT count(*) || ' packages loaded' FROM packages"
}

case "${1:-}" in
  save) save ;;
  load) load ;;
  *) echo "usage: $0 save|load" >&2; exit 2 ;;
esac
