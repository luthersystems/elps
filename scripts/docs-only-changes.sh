#!/usr/bin/env bash
#
# Decide whether a pull request changes ONLY documentation, so the expensive
# CI jobs may be skipped. Runs as the `changes` job in every workflow that
# produces a `Required: *` check (elps.yml, fuzz.yml, govulncheck.yml,
# tree-sitter.yml, benchmark.yml); scripts/ci-gates-test.sh fixture-tests the
# classification below.
#
# WHY NOT `paths-ignore:` ON THE WORKFLOW. A path-filtered workflow does not run
# at all on a PR that misses the filter, so its `Required: *` check never
# reports, and a required check that never reports blocks the PR forever (see
# the note on tree-sitter.yml's trigger). So every workflow still runs on every
# PR; only the heavy jobs inside it are conditioned on this script's output,
# and the aggregate accepts their `skipped` result ONLY when this job said
# docs_only=true (scripts/require-jobs-succeeded.sh, DOCS_ONLY).
#
# WHEN IN DOUBT, RUN EVERYTHING. Every path below that is not a positive,
# recognised "docs-only" answer prints docs_only=false:
#   * any event other than pull_request (push to main, schedule, dispatch);
#   * a diff that cannot be computed, or that lists no files at all;
#   * any single changed path that is not on the docs-only list.
#
# WHAT COUNTS AS DOCS-ONLY (a path must match one of these and none of the
# exclusions):
#   * `*.md` anywhere, EXCEPT
#       - docs/lang.md, docs/debugging-guide.md, docs/lsp-guide.md: docs/embed.go
#         compiles them into the binary with //go:embed (`elps doc --guide`
#         and friends), so changing one changes the build output -- that is a
#         code change and gets the full suite;
#       - anything under a `testdata/` directory: tests read those files
#         (parser/rdparser/bench_fixture_test.go reads testdata/bench/sicp/
#         README.md);
#       - anything under .github/, scripts/ or tree-sitter-elps/, which are CI
#         configuration, gate code and a separately tested module.
#   * LICENSE, AUTHORS, CONTRIBUTORS at the repository root.
#   * .claude/ gets no blanket exemption: its *.md files (skills, agent
#     definitions) are docs by the rule above, but anything else there -- a
#     hook script, settings, and above all a .go file -- is not.
#     internal/fuzzwatch/callsite_guard_test.go walks the whole repository,
#     .claude/ included, and checks every .go file it finds.
# Everything else -- *.go, *.lisp, go.mod/go.sum, Makefile, editors/ code
# (editors/**/*.md is docs), config files, images -- is not docs-only.
#
# Modes:
#   docs-only-changes.sh              compute from git and write
#                                     `docs_only=<bool>` to $GITHUB_OUTPUT
#                                     (and stdout). Needs EVENT_NAME, and a
#                                     checkout of the PR merge commit with at
#                                     least fetch-depth 2.
#   docs-only-changes.sh --classify   read newline-separated paths on stdin,
#                                     print true/false. Exit 0 either way.
#
# The diff is the PR merge commit against its FIRST parent (the base branch
# tip GitHub merged into), i.e. exactly what merging the PR would change on the
# base -- not the PR's own history, which may contain reverted code edits.
set -euo pipefail

# is_docs_path <path> -> exit 0 if this one path is documentation only.
is_docs_path() {
	local p="$1"
	case "$p" in
	# Exclusions first: these win over every docs pattern below.
	docs/lang.md | docs/debugging-guide.md | docs/lsp-guide.md) return 1 ;;
	.github/* | scripts/* | tree-sitter-elps/*) return 1 ;;
	testdata/* | */testdata/*) return 1 ;;
	# Docs.
	LICENSE | AUTHORS | CONTRIBUTORS) return 0 ;;
	*.md) return 0 ;;
	esac
	return 1
}

# classify: stdin is a newline-separated path list; prints true or false.
classify() {
	local p n=0
	while IFS= read -r p || [ -n "$p" ]; do
		[ -n "$p" ] || continue
		n=$((n + 1))
		if ! is_docs_path "$p"; then
			echo "not docs-only: ${p}" >&2
			echo false
			return 0
		fi
	done
	# An empty change list proves nothing about the PR; run everything.
	if [ "$n" -eq 0 ]; then
		echo "no changed paths were listed -- running everything" >&2
		echo false
		return 0
	fi
	echo true
}

emit() {
	echo "docs_only=$1"
	if [ -n "${GITHUB_OUTPUT-}" ]; then
		echo "docs_only=$1" >>"$GITHUB_OUTPUT"
	fi
}

if [ "${1-}" = "--classify" ]; then
	classify
	exit 0
fi

EVENT_NAME="${EVENT_NAME-}"
if [ "$EVENT_NAME" != "pull_request" ]; then
	echo "event '${EVENT_NAME}' is not pull_request -- every job runs"
	emit false
	exit 0
fi

# HEAD is the PR merge commit actions/checkout fetched; HEAD^1 is the base tip.
# A HEAD that is not a merge (no HEAD^2), or any git failure, means the diff is
# not the one this script reasons about: run everything.
if ! git rev-parse --verify --quiet 'HEAD^2' >/dev/null ||
	! files="$(git diff --no-renames --name-only 'HEAD^1' HEAD)"; then
	echo "::warning::Could not compute the PR's changed files from the merge commit -- running every job."
	emit false
	exit 0
fi

echo "changed files:"
printf '%s\n' "$files" | sed 's/^/  /'
emit "$(printf '%s\n' "$files" | classify)"
