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

# Exit codes:
#   0  scanned the tree, term not present
#   1  the term was FOUND
#   2  the guard could not run (see "a guard that cannot scan" below)
#
# Exit 2 matches the convention scripts/fuzz.sh and scripts/fuzz-budget-check.sh
# already use: a gate that cannot read its inputs must not report a pass.

# The repository root, checked (issue #486).
#
# This was `cd "$(git rev-parse --show-toplevel)"`. A command substitution's
# failure does NOT trip `set -e` when it is only an ARGUMENT, and `cd ""` is a
# silent no-op that succeeds -- so when git could not identify a repository the
# script simply carried on in whatever directory it happened to be started
# from, and scanned that instead.
REPO_TOP=""
rev_rc=0
REPO_TOP="$(git rev-parse --show-toplevel 2>&1)" || rev_rc=$?
if [ "$rev_rc" -ne 0 ] || [ -z "$REPO_TOP" ]; then
	echo "confidentiality guard: cannot locate the repository root (git rev-parse exit ${rev_rc})" >&2
	[ -n "$REPO_TOP" ] && printf '%s\n' "$REPO_TOP" | sed 's/^/  | /' >&2
	echo "confidentiality guard: refusing to report clean -- nothing was scanned." >&2
	exit 2
fi
cd "$REPO_TOP"

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

# --- a guard that cannot scan must not report clean (issue #486) -------------
#
# The self-test above proves the PATTERN works. It says nothing about whether
# the scan below actually reaches any content -- and silence from `git grep`
# was being read as proof of absence in two different ways:
#
#   1. `git grep` exits >=2 on ERROR (not a repository, unreadable object, bad
#      pathspec, I/O error). The old code was `if matches="$(git grep ...)"`,
#      so any error made the `if` false, execution fell through, and the script
#      printed "clean" and exited 0. `2>/dev/null` hid the reason as well.
#
#   2. `git grep` exits 1 both when the tree is genuinely clean AND when there
#      is nothing to look at. Exit status alone cannot tell those apart, so an
#      empty or unpopulated tree reads as a pass.
#
# All four were confirmed by running the guard, unmodified, and watching it
# print "confidentiality guard: clean" and exit 0:
#
#   * outside a repository            -> git grep exit 128   (case 1)
#   * corrupt .git/index              -> git grep exit 128   (case 1)
#   * .git gitdir pointer dangling    -> git grep exit 128   (case 1)
#   * tracked files absent from the
#     working tree (git grep reads
#     the working tree, and skips
#     missing files in silence)       -> git grep exit 1     (case 2)
#
# So the scan is bounded on both sides: prove it can SEE the tree, then handle
# all three of its exit classes explicitly rather than by truthiness.

# How many files the scan can actually read. Same mechanism as the real scan --
# `git grep` over the same pathspec -- so this measures the scan itself rather
# than something adjacent to it that might succeed while the scan does not.
# `-I` skips binaries; '.' matches any file with at least one character.
scan_err="$(mktemp)"
trap 'rm -f "$scan_err"' EXIT
seen_rc=0
seen="$(git grep -IlE '.' -- . 2>"$scan_err")" || seen_rc=$?
if [ "$seen_rc" -gt 1 ]; then
	echo "confidentiality guard: the tree could not be read (git grep exit ${seen_rc})" >&2
	sed 's/^/  | /' "$scan_err" >&2
	echo "confidentiality guard: refusing to report clean -- nothing was scanned." >&2
	exit 2
fi
n_seen="$(printf '%s' "$seen" | grep -c . || true)"
if [ "$n_seen" -eq 0 ]; then
	echo "confidentiality guard: the scan matched ZERO readable files in $(pwd)." >&2
	echo "confidentiality guard: an empty scan cannot demonstrate the term is absent," >&2
	echo "confidentiality guard: it only demonstrates that nothing was looked at." >&2
	echo "confidentiality guard: refusing to report clean." >&2
	exit 2
fi

# --- scan the tracked tree ---------------------------------------------------

# git grep exits 0 on a match, 1 when clean, and >=2 on error. All three are
# handled explicitly: relying on truthiness put the error case in the "clean"
# branch, which is the whole of #486.
grep_rc=0
matches="$(git grep -inE "$PATTERN" -- . 2>"$scan_err")" || grep_rc=$?
case "$grep_rc" in
0)
	echo "confidentiality guard: forbidden term found at the following locations:" >&2
	# file:line only; deliberately do not echo the matched text.
	printf '%s\n' "$matches" | cut -d: -f1,2 >&2
	echo "confidentiality guard: remove the term before merging (ask a maintainer if unsure which term this is)." >&2
	exit 1
	;;
1)
	# Genuinely clean -- and, thanks to the coverage check above, clean over a
	# tree we know was actually read.
	;;
*)
	echo "confidentiality guard: git grep failed (exit ${grep_rc})" >&2
	sed 's/^/  | /' "$scan_err" >&2
	echo "confidentiality guard: refusing to report clean -- the scan did not complete." >&2
	exit 2
	;;
esac

echo "confidentiality guard: clean (${n_seen} files scanned)"
