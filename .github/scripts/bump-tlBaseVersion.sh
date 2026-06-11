#!/usr/bin/env bash

set -euo pipefail

require_env() {
  local name="$1"

  if [[ -z "${!name:-}" ]]; then
    echo "Missing required environment variable: $name" >&2
    exit 1
  fi
}

append_github_env() {
  require_env GITHUB_ENV
  echo "$1" >> "$GITHUB_ENV"
}

append_github_output() {
  require_env GITHUB_OUTPUT
  echo "$1" >> "$GITHUB_OUTPUT"
}

command="${1:-}"

case "$command" in
  compute-version)
    TAG="${GITHUB_RELEASE_TAG:-}"

    if [[ ! "$TAG" =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
      echo "Skipping invalid tag"
      append_github_output "run=false"
      exit 0
    fi

    TAG="${TAG#v}"
    IFS='.' read -r X Y Z <<< "$TAG"

    VERSION="${X}.$((Y + 1))"

    append_github_env "VERSION=$VERSION"
    append_github_env "TAG=$TAG"
    append_github_output "run=true"
    ;;

  check-changes)
    require_env VERSION

    sed -i -E 's|(^ThisBuild / tlBaseVersion := ")[^"]+(")|\1'"$VERSION"'\2|' build.sbt

    if git diff --quiet; then
      echo "No changes"
      append_github_output "changed=false"
    else
      append_github_output "changed=true"
    fi
    ;;

  check-pr)
    require_env VERSION

    BRANCH="bump-base-version-${VERSION}"
    append_github_env "BRANCH=$BRANCH"

    PR_EXISTS=$(gh pr list --head "$BRANCH" --json number --jq 'length > 0')

    append_github_output "PR_EXISTS=$PR_EXISTS"
    echo "PR already exists: $PR_EXISTS"
    ;;

  push-branch)
    require_env BRANCH
    require_env VERSION

    git fetch origin main
    git checkout -B "$BRANCH" origin/main

    git add build.sbt
    git commit -m "Update tlBaseVersion to $VERSION"

    git push origin "$BRANCH"
    ;;

  create-pr)
    require_env BRANCH
    require_env TAG
    require_env VERSION

    gh pr create \
      --title "Update tlBaseVersion to $VERSION" \
      --body "Derived from release tag v${TAG}. Patch ignored. Next base version: ${VERSION}." \
      --base main \
      --head "$BRANCH"
    ;;

  *)
    echo "Unknown command: $command" >&2
    exit 1
    ;;
esac
