#!/usr/bin/env bash
#
# Work marker gate: fails CI when a tracked file contains a work marker that
# scripts/work-markers.txt does not allow.
#
# A work marker is one of five upper-case words (deferred-work, broken-code and
# kludge notes). The words are listed in the header of scripts/work-markers.txt.
# They are never written literally in this script: the script assembles them
# below, so its own source cannot trip the gate. The allowlist file is the only
# path the scan excludes, because every non-comment line in it is a reviewed
# entry that quotes a marker.
#
# Matching is case-sensitive and bounded: a marker counts only when the
# characters on both sides are not letters, digits or `_`. So a lower-case
# marker, a marker with a letter suffix and a marker inside an identifier do
# not match, while a marker followed by `:` or `(` does. The self-test below
# proves both directions on every run.
#
# Exit codes:
#   0  every marker in the tree is covered by an allowlist entry, and every
#      entry covers at least one marker
#   1  a marker is not covered, or an entry is stale (covers nothing)
#   2  the gate could not run: no repository, an unreadable tree, an empty
#      scan, a missing or malformed allowlist, or a failed self-test
#
# Environment:
#   WORK_MARKER_TODAY  YYYY-MM-DD to use as today (for scripts/ci-gates-test.sh).
#                      Defaults to the current UTC date.
#
# Usage: scripts/work-marker-gate.sh   (run from anywhere in the repo)

set -euo pipefail

ALLOWLIST="scripts/work-markers.txt"

die() {
	echo "work marker gate: $*" >&2
	echo "work marker gate: refusing to report clean." >&2
	exit 2
}

REPO_TOP=""
rev_rc=0
REPO_TOP="$(git rev-parse --show-toplevel 2>&1)" || rev_rc=$?
if [ "$rev_rc" -ne 0 ] || [ -z "$REPO_TOP" ]; then
	[ -n "$REPO_TOP" ] && printf '%s\n' "$REPO_TOP" | sed 's/^/  | /' >&2
	die "cannot locate the repository root (git rev-parse exit ${rev_rc})"
fi
cd "$REPO_TOP"

TODAY="${WORK_MARKER_TODAY:-$(date -u +%Y-%m-%d)}"
[[ "$TODAY" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "today ${TODAY} is not a YYYY-MM-DD date"

# The markers, assembled so that no marker appears literally in this file.
MARKERS="$(printf '%s|' "TO""DO" "FIX""ME" "X""XX" "HA""CK" "B""UG")"
MARKERS="${MARKERS%|}"
PATTERN="(^|[^[:alnum:]_])(${MARKERS})([^[:alnum:]_]|$)"

has_marker() { [[ "$1" =~ $PATTERN ]]; }

# --- self-test: boundary handling -------------------------------------------

m="TO""DO"
for fixture in "todo" "${m}S" "_${m}" "${m}_" "1${m}" "${m}1" "X""X" "X""XXL" "DEB""UG"; do
	if has_marker "$fixture"; then
		die "self-test failed: false positive on ${fixture}"
	fi
done
for fixture in "$m" "// ${m}: x" "${m}(#1)" "(B""UG)" "; FIX""ME" "x-HA""CK-y" "pkg:X""XX"; do
	if ! has_marker "$fixture"; then
		die "self-test failed: missed ${fixture}"
	fi
done

# --- read the allowlist ------------------------------------------------------

[ -f "$ALLOWLIST" ] || die "${ALLOWLIST} is missing; an absent allowlist is not an empty one"

# valid_date <YYYY-MM-DD>: a real calendar date. Portable (no GNU `date -d`).
valid_date() {
	[[ "$1" =~ ^([0-9]{4})-([0-9]{2})-([0-9]{2})$ ]] || return 1
	local y=$((10#${BASH_REMATCH[1]})) mo=$((10#${BASH_REMATCH[2]})) d=$((10#${BASH_REMATCH[3]}))
	local -a days=(0 31 28 31 30 31 30 31 31 30 31 30 31)
	if ((y % 4 == 0 && (y % 100 != 0 || y % 400 == 0))); then days[2]=29; fi
	((mo >= 1 && mo <= 12 && d >= 1 && d <= days[mo]))
}

# valid_issues <field>: one or more tracking references, every token valid.
# Same rule as scripts/api-breaks.txt (cmd/apibreak issuesOK).
valid_issues() {
	local good=0 t
	for t in ${1//,/ }; do
		if [[ "$t" =~ ^[A-Za-z0-9._/-]*#[0-9]+$ ]] ||
			[[ "$t" =~ ^https://github\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/(issues|pull)/[0-9]+$ ]]; then
			good=$((good + 1))
		else
			return 1
		fi
	done
	[ "$good" -gt 0 ]
}

trim() {
	local s="$1"
	s="${s#"${s%%[![:space:]]*}"}"
	s="${s%"${s##*[![:space:]]}"}"
	printf '%s' "$s"
}

e_path=()
e_text=()
e_expires=()
e_line=()
e_used=()
e_expired=()
malformed=0
declare -A seen=()
lineno=0
bad() {
	echo "  ALLOW-BAD  ${ALLOWLIST}:${lineno}  $1" >&2
	malformed=1
}
while IFS= read -r raw || [ -n "$raw" ]; do
	lineno=$((lineno + 1))
	raw="${raw%$'\r'}"
	[[ "$raw" =~ ^[[:space:]]*(#|$) ]] && continue
	IFS='|' read -r -a f <<<"$raw"
	if [ "${#f[@]}" -ne 5 ] || [[ "$raw" == *"|" ]]; then
		bad "expected 5 |-separated fields (path | text | expires | issue | reason)"
		continue
	fi
	path="$(trim "${f[0]}")"
	text="$(trim "${f[1]}")"
	expires="$(trim "${f[2]}")"
	issue="$(trim "${f[3]}")"
	reason="$(trim "${f[4]}")"
	ok=1
	if [ -z "$path" ] || [[ "$path" == *[[:space:]*?]* ]]; then
		bad "path must be one exact repository-relative path (no spaces or globs)"
		ok=0
	fi
	if ! has_marker "$text"; then
		bad "text must contain a work marker, so the entry cannot cover lines without one"
		ok=0
	fi
	if [ "$expires" = "never" ]; then
		if [ "$issue" != "-" ] && ! valid_issues "$issue"; then
			bad "issue ${issue} is not - or a tracking reference (#412, owner/repo#412 or a github.com issue/PR URL)"
			ok=0
		fi
	elif ! valid_date "$expires"; then
		bad "expires ${expires} is not a real YYYY-MM-DD date or never"
		ok=0
	elif ! valid_issues "$issue"; then
		bad "issue ${issue} is not a tracking reference; deferred work needs an issue (#412, owner/repo#412 or a github.com issue/PR URL)"
		ok=0
	fi
	if [ "${#reason}" -lt 10 ]; then
		bad "reason is missing or shorter than 10 characters"
		ok=0
	fi
	[ "$ok" -eq 1 ] || continue
	key="${path}|${text}"
	if [ -n "${seen[$key]:-}" ]; then
		bad "duplicate entry for ${key} (first at line ${seen[$key]})"
		continue
	fi
	seen[$key]="$lineno"
	e_path+=("$path")
	e_text+=("$text")
	e_expires+=("$expires")
	e_line+=("$lineno")
	e_used+=(0)
	if [ "$expires" != "never" ] && [[ "$TODAY" > "$expires" ]]; then
		e_expired+=(1)
	else
		e_expired+=(0)
	fi
done <"$ALLOWLIST"

[ "$malformed" -eq 0 ] || die "${ALLOWLIST} is malformed (see ALLOW-BAD lines above)"

# --- prove the scan can see the tree ----------------------------------------
#
# git grep exits 1 both on a clean tree and when it read nothing (for example,
# tracked files missing from the working tree). Count readable files with the
# same command and pathspec first, so an empty scan cannot read as clean.

PATHSPEC=(-- . ":(exclude)${ALLOWLIST}")
scan_err="$(mktemp)"
trap 'rm -f "$scan_err"' EXIT
seen_rc=0
n_seen="$(git grep -IlE '.' "${PATHSPEC[@]}" 2>"$scan_err" | grep -c .)" || seen_rc=$?
if [ "$seen_rc" -ne 0 ] && [ "$seen_rc" -ne 1 ]; then
	sed 's/^/  | /' "$scan_err" >&2
	die "the tree could not be read (exit ${seen_rc})"
fi
if [ -s "$scan_err" ]; then
	sed 's/^/  | /' "$scan_err" >&2
	die "the tree could not be read"
fi
[ "${n_seen:-0}" -gt 0 ] || die "the scan matched ZERO readable files in $(pwd)"

# --- scan the tracked tree ---------------------------------------------------

hits="$(mktemp)"
trap 'rm -f "$scan_err" "$hits"' EXIT
grep_rc=0
git grep -IznE "$PATTERN" "${PATHSPEC[@]}" >"$hits" 2>"$scan_err" || grep_rc=$?
if [ "$grep_rc" -gt 1 ]; then
	sed 's/^/  | /' "$scan_err" >&2
	die "git grep failed (exit ${grep_rc})"
fi

uncovered=0
while IFS= read -r -d '' path && IFS= read -r -d '' ln && IFS= read -r line; do
	rest="$line"
	for i in "${!e_path[@]}"; do
		[ "${e_path[$i]}" = "$path" ] || continue
		[ "${e_expired[$i]}" -eq 0 ] || continue
		text="${e_text[$i]}"
		if [[ "$rest" == *"$text"* ]]; then
			rest="${rest//"$text"/}"
			e_used[i]=1
		fi
	done
	if has_marker "$rest"; then
		if [ "$uncovered" -eq 0 ]; then
			echo "work marker gate: work markers not covered by ${ALLOWLIST}:" >&2
		fi
		uncovered=$((uncovered + 1))
		echo "  ${path}:${ln}: $(trim "$line")" >&2
	fi
done <"$hits"

stale=0
for i in "${!e_path[@]}"; do
	[ "${e_used[$i]}" -eq 1 ] && continue
	stale=$((stale + 1))
	if [ "${e_expired[$i]}" -eq 1 ]; then
		echo "  ALLOW-EXPIRED  ${ALLOWLIST}:${e_line[$i]}  ${e_path[$i]} | ${e_text[$i]} expired ${e_expires[$i]}; fix the marker, or re-decide and set a new date" >&2
	else
		echo "  ALLOW-STALE  ${ALLOWLIST}:${e_line[$i]}  ${e_path[$i]} | ${e_text[$i]} covers nothing; delete the entry" >&2
	fi
done

if [ "$uncovered" -gt 0 ] || [ "$stale" -gt 0 ]; then
	if [ "$uncovered" -gt 0 ]; then
		echo "work marker gate: ${uncovered} uncovered line(s). Do the work, or open an issue and add an entry:" >&2
		echo "  path | text from the line | YYYY-MM-DD | #NNN | reason (see ${ALLOWLIST})" >&2
	fi
	[ "$stale" -gt 0 ] && echo "work marker gate: ${stale} entry(ies) cover nothing." >&2
	exit 1
fi

echo "work marker gate: clean (${n_seen} files scanned, ${#e_path[@]} allowlist entries)"
