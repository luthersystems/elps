#!/usr/bin/env bash
#
# Print the API break overrides (scripts/api-breaks.txt, #761) added since a
# tag: the waived breaking changes a release must list in its notes.  Used by
# `make release-notes` and the release workflow (release-tag.yml).
#
#   bash scripts/api-breaks-since.sh <tag>      (`none` = no tag yet: all entries)

set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FILE=scripts/api-breaks.txt
tag="${1:?usage: $0 <tag|none>}"
cd "$REPO_ROOT"

entries() { grep -Ev '^[[:space:]]*(#|$)' || true; }

if [ "$tag" = "none" ] || ! git cat-file -e "${tag}:${FILE}" 2>/dev/null; then
	# No tag, or the file did not exist at it: every entry is new.
	out="$(entries <"$FILE")"
else
	# Entries present now that were not at the tag (order-insensitive).
	out="$(comm -13 <(git show "${tag}:${FILE}" | entries | sort) <(entries <"$FILE" | sort))"
fi
if [ -z "$out" ]; then
	echo "(none)"
else
	printf '%s\n' "$out"
fi
