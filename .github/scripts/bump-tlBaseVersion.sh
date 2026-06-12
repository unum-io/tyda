#!/usr/bin/env bash

set -euo pipefail

release_tag="${1:-${GITHUB_RELEASE_TAG:-}}"

if [[ ! "$release_tag" =~ ^v([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
  echo "Skipping invalid tag: ${release_tag:-<empty>}"
  exit 0
fi

major="${BASH_REMATCH[1]}"
minor="${BASH_REMATCH[2]}"

version="${major}.$((minor + 1))"
branch="bump-base-version-${version}"

echo "Release tag: ${release_tag}"
echo "Next tlBaseVersion: ${version}"
echo "Branch: ${branch}"

git fetch origin main
git checkout -B "$branch" origin/main

if ! grep -qE '^ThisBuild / tlBaseVersion := "[^"]+"' build.sbt; then
  echo "Could not find tlBaseVersion setting in build.sbt" >&2
  exit 1
fi

VERSION="$version" perl -0pi -e \
  's{(^ThisBuild / tlBaseVersion := ")[^"]*(")}{$1 . $ENV{VERSION} . $2}me' \
  build.sbt

if git diff --quiet -- build.sbt; then
  echo "No changes. tlBaseVersion is already ${version}."
  exit 0
fi

existing_pr_url="$(
  gh pr list \
    --state all \
    --head "$branch" \
    --json url \
    --jq '.[0].url // ""'
)"

if [[ -n "$existing_pr_url" ]]; then
  echo "PR already exists for ${branch}: ${existing_pr_url}"
  exit 0
fi

if ! git config user.name >/dev/null; then
  git config user.name "github-actions[bot]"
fi

if ! git config user.email >/dev/null; then
  git config user.email "github-actions[bot]@users.noreply.github.com"
fi

git add build.sbt
git commit -m "Update tlBaseVersion to ${version}"

git push origin "$branch"

if ! create_output="$(
  gh pr create \
    --title "Update tlBaseVersion to ${version}" \
    --body "Derived from release tag ${release_tag}. Patch ignored. Next base version: ${version}." \
    --base main \
    --head "$branch" 2>&1
)"; then
  existing_pr_url="$(
    gh pr list \
      --state all \
      --head "$branch" \
      --json url \
      --jq '.[0].url // ""'
  )"

  if [[ -n "$existing_pr_url" ]]; then
    echo "PR already exists for ${branch}: ${existing_pr_url}"
    exit 0
  fi

  echo "$create_output" >&2
  exit 1
fi

echo "$create_output"
