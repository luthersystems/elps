#!/usr/bin/env bash
#
# API break gate (issue #761): fail when this tree removes or incompatibly
# changes elps's public API relative to BASE, unless each break has a
# reviewed entry in scripts/api-breaks.txt.  See that file for the format and
# cmd/apibreak for the rules.
#
#   bash scripts/api-break-gate.sh <base-ref>     (make api-break-gate BASE=origin/main)
#
# Three surfaces, computed from a checkout of BASE (a temporary git
# worktree) and from this tree:
#   go    apidiff -m, pinned below; internal packages are ignored by apidiff.
#   lisp  `elps doc --json -l` from an elps binary built from each tree.
#   golden  frozen canonical/typed bytes and canonize errors in testdata/*.txt;
#           changed/removed entries require overrides; additions are compatible.
#
# Exit 0 = no unwaived break, 1 = unwaived break, 2 = could not run/judge.

set -euo pipefail

# golang.org/x/exp has no tags; bump deliberately (the release date is in the
# pseudo-version).  apidiff rather than gorelease: gorelease answers "what
# version should this be", checks go.mod/retractions and needs a published
# module version to compare against; apidiff compares two trees directly and
# prints one line per incompatible change, which is the unit an override
# names.
APIDIFF_VERSION="${APIDIFF_VERSION:-v0.0.0-20260908205506-85c1c2202aba}"
MODULE=github.com/luthersystems/elps
GOLDEN_DIR=lisp/lisplib/libjson/typedgolden/testdata

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OVERRIDES="${APIBREAK_OVERRIDES:-${REPO_ROOT}/scripts/api-breaks.txt}"
BASE="${1:-}"
if [ -z "$BASE" ]; then
	echo "usage: $0 <base-ref>" >&2
	exit 2
fi

cd "$REPO_ROOT"
# Compare against the merge base, so commits that landed on BASE after this
# branch forked do not read as removals.  In CI the checkout is the PR's merge
# commit and BASE is its first parent, which is its own merge base.
base_sha="$(git merge-base "$BASE" HEAD)" || {
	echo "api-break-gate: cannot find a merge base with ${BASE} (fetch it, with history)" >&2
	exit 2
}

tmp="$(mktemp -d)"
# shellcheck disable=SC2329 # invoked by the EXIT trap
cleanup() {
	git worktree remove --force "${tmp}/base" >/dev/null 2>&1 || true
	rm -rf "$tmp"
}
trap cleanup EXIT
trap 'exit 2' ERR

echo "api-break-gate: base ${BASE} (${base_sha}), head $(git rev-parse HEAD)"
GOBIN="${tmp}/bin" go install "golang.org/x/exp/cmd/apidiff@${APIDIFF_VERSION}"
go build -buildvcs=false -o "${tmp}/bin/apibreak" ./cmd/apibreak

git worktree add --quiet --detach "${tmp}/base" "$base_sha"
(
	cd "${tmp}/base"
	go build -buildvcs=false -o "${tmp}/elps-base" .
	"${tmp}/bin/apidiff" -m -w "${tmp}/base.api" "$MODULE" 2> >(grep -v "^Ignoring internal package " >&2)
)
"${tmp}/elps-base" doc --json -l >"${tmp}/lisp-base.json"

go build -buildvcs=false -o "${tmp}/elps-head" .
"${tmp}/elps-head" doc --json -l >"${tmp}/lisp-head.json"
"${tmp}/bin/apidiff" -m -incompatible "${tmp}/base.api" "$MODULE" >"${tmp}/go-report.txt" \
	2> >(grep -v "^Ignoring internal package " >&2)

trap - ERR
set +e
"${tmp}/bin/apibreak" -overrides "${OVERRIDES#"${REPO_ROOT}"/}" \
	-go-report "${tmp}/go-report.txt" \
	-lisp-base "${tmp}/lisp-base.json" -lisp-head "${tmp}/lisp-head.json" \
	-golden-base "${tmp}/base/${GOLDEN_DIR}" -golden-head "${REPO_ROOT}/${GOLDEN_DIR}"
rc=$?
exit "$rc"
