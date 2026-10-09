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

{
  description = "Scala Packages — Typelevel + Calico full-stack web application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Dev shell only. Releases are assembly jars built by
  # .github/workflows/release.yml on v* tags and deployed from a separate
  # infrastructure repository.
  outputs = { self, nixpkgs, devshell, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ devshell.overlays.default ];
          };
        in
        {
          # commands and most packages live in devshell.toml; JAVA_HOME
          # needs a nix store path so it is set here. JDK 25, as in CI and
          # production.
          devShells.default = pkgs.devshell.mkShell {
            imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
            # PostgreSQL with pgvector for semantic search
            packages = [
              pkgs.jdk25
              (pkgs.postgresql.withPackages (p: [ p.pgvector ]))
            ];
            env = [
              {
                name = "JAVA_HOME";
                value = "${pkgs.jdk25}";
              }
            ];
          };
        }
      );
}
