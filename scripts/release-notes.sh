#!/usr/bin/env bash
# Prints the release notes for a release of HEAD: one bullet per change on
# main since the previous release tag, then the API break overrides added in
# that range. release-tag.yml passes it to the shared release workflow as
# notes_command, which sets LAST_TAG (empty for a first release).
#
# Usage: LAST_TAG=v1.2.3 scripts/release-notes.sh
set -euo pipefail

last_tag="${LAST_TAG:-}"
notes_range=HEAD
if [[ -n "$last_tag" ]]; then
  notes_range="$last_tag..HEAD"
fi

echo "## Changes"
echo
# One bullet per change on main: the PR title (and number) for a merge
# commit, the subject otherwise.
git log --first-parent --format='%H' -200 "$notes_range" | while read -r commit; do
  subject=$(git log -1 --format=%s "$commit")
  parents=$(git rev-list --parents -n 1 "$commit" | wc -w)
  if (( parents > 2 )) && [[ "$subject" =~ ^Merge\ pull\ request\ (#[0-9]+)\  ]]; then
    pr="${BASH_REMATCH[1]}"
    title=$(git log -1 --format=%b "$commit" | awk 'NF { print; exit }')
    echo "- ${title:-$subject} ($pr)"
  else
    echo "- $subject"
  fi
done

# The waived API breaks (scripts/api-breaks.txt) added since the last
# release tag. Every one must be listed; never omit one.
breaks=$(bash scripts/api-breaks-since.sh "${last_tag:-none}")
if [[ "$breaks" != "(none)" ]]; then
  echo
  echo "## Breaking changes"
  echo
  echo "API break overrides added since ${last_tag:-the first release} (surface | symbol | expires | issue | reason):"
  echo
  while IFS= read -r line; do
    echo "- $line"
  done <<< "$breaks"
fi
