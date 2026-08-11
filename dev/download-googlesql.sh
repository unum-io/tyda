#!/usr/bin/env bash
set -euo pipefail

readonly release='2026.7.2'
readonly sha256='d362b2469f7077db5c8029f594dd4c435efb162fce0f50c51c9b85695fe98041'
readonly executable="${TYDA_GOOGLESQL_EXECUTABLE_PATH:-$PWD/target/execute_query_linux}"
readonly download_url="https://github.com/google/googlesql/releases/download/$release/execute_query_linux"

if ! command -v curl >/dev/null; then
  echo 'curl is required to download the GoogleSQL executable.' >&2
  exit 1
fi

if [[ -x "$executable" ]] && echo "$sha256  $executable" | sha256sum --check --status; then
  printf '%s\n' "$executable"
  exit 0
fi

mkdir -p "$(dirname "$executable")"
temporary_executable="$(mktemp "${executable}.download.XXXXXX")"
readonly temporary_executable
trap 'rm -f "$temporary_executable"' EXIT

curl --fail --location --retry 3 --output "$temporary_executable" "$download_url"
echo "$sha256  $temporary_executable" | sha256sum --check --status
chmod +x "$temporary_executable"
mv "$temporary_executable" "$executable"

printf '%s\n' "$executable"
