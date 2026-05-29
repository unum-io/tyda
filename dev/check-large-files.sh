#!/usr/bin/env bash
set -euo pipefail

TARGET_BRANCH="${1:?Usage: $0 <target-branch> <max-size-mb>}"
MAX_FILE_SIZE_MB="${2:?Usage: $0 <target-branch> <max-size-mb>}"

MAX_SIZE=$(( MAX_FILE_SIZE_MB * 1024 * 1024 ))

LARGE_FILES=$(
git diff --name-only -z --diff-filter=AC \
  "origin/${TARGET_BRANCH}...HEAD" \
| xargs -0 --no-run-if-empty stat -c '%s %n' 2>/dev/null \
| awk -v max="$MAX_SIZE" '$1 > max'
)

if [ -n "$LARGE_FILES" ]; then
echo "Files exceeding ${MAX_FILE_SIZE_MB}MB:"
echo "$LARGE_FILES"
exit 1
fi

echo "All files are under ${MAX_FILE_SIZE_MB}MB."
