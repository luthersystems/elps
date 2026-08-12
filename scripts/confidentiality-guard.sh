#!/usr/bin/env bash
#
# Confidentiality guard: fails CI when a forbidden (confidential customer)
# term appears anywhere in the tracked tree.
#
# Two deliberate design points:
#
#  1. The forbidden term is NEVER written literally in this script (or in
#     the workflow that invokes it).  It is assembled below from octal
#     character codes.  That way the guard's own source can never trip the
#     guard, and this file never needs to be excluded from the scan -- an
#     exclusion would be a hole a future violation could hide in.
#
#  2. The scan uses a case-insensitive WORD-BOUNDARY pattern (\b...\b), so
#     ordinary English words that merely contain the term as a substring
#     (e.g. "massacre", "acreage", "wiseacre") never false-positive.  The
#     self-test below proves both directions on every run: those substring
#     words must NOT match, and runtime-constructed bounded occurrences
#     (bare, hyphenated identifier, package-qualified) MUST match.  Note
#     the negative fixtures are safe to write literally here precisely
#     because they do not match the bounded pattern.
#
# On a hit the script prints file:line locations only -- not the matched
# text -- so the term does not end up echoed into public CI logs.
#
# Usage: scripts/confidentiality-guard.sh   (run from anywhere in the repo)

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# Forbidden term, assembled from octal codes so it never appears literally.
TERM_="$(printf '\141\143\162\145')"
PATTERN="\\b${TERM_}\\b"

# --- self-test: boundary handling -------------------------------------------

# Substring-containing words must NOT match (word boundaries do the work).
for fixture in "massacre" "acreage" "wiseacre" "LambdaCreatesGlobal"; do
	if printf '%s\n' "$fixture" | grep -qiE "$PATTERN"; then
		echo "guard self-test failed: false positive on substring word '$fixture'" >&2
		exit 2
	fi
done

# Bounded occurrences (constructed at runtime, never stored literally)
# MUST match: bare word, mixed case, hyphenated identifier, pkg-qualified.
UP="$(printf '%s' "$TERM_" | tr '[:lower:]' '[:upper:]')"
for fixture in "$TERM_" "$UP" "def-${TERM_}-route" "${TERM_}:helper" "(${TERM_})"; do
	if ! printf '%s\n' "$fixture" | grep -qiE "$PATTERN"; then
		echo "guard self-test failed: pattern missed a bounded occurrence" >&2
		exit 2
	fi
done

# --- scan the tracked tree ---------------------------------------------------

# git grep exits 0 when it finds a match, 1 when clean.
if matches="$(git grep -inE "$PATTERN" -- . 2>/dev/null)"; then
	echo "confidentiality guard: forbidden term found at the following locations:" >&2
	# file:line only; deliberately do not echo the matched text.
	printf '%s\n' "$matches" | cut -d: -f1,2 >&2
	echo "confidentiality guard: remove the term before merging (ask a maintainer if unsure which term this is)." >&2
	exit 1
fi

echo "confidentiality guard: clean"
