#!/usr/bin/env bash
# Copyright © 2026 The ELPS authors
#
# Materialize a runnable dfuzz workdir from two refs of this repository.
#
#   ./setup.sh <left-ref> <right-ref> <workdir>
#
# The workdir is scratch and is never committed: it holds two full checkouts
# plus a throwaway Go module.  Everything the harness needs beyond that lives
# in this directory, under version control.
#
# WHAT THIS SCRIPT IS WORKING AROUND
#
# The harness runs BOTH interpreters in ONE process, which requires importing
# two copies of package `lisp` at once.  Go will not do that directly: both
# checkouts declare `module github.com/luthersystems/elps`, and a build may
# resolve one module path to exactly one directory.
#
# The fix is to rename the module in the left-hand checkout -- go.mod and every
# import of it -- to `github.com/luthersystems/elpsstock`, so the two trees are
# genuinely different modules as far as the toolchain is concerned, and then
# point one `replace` at each.  The rename is a mechanical sed over a scratch
# checkout, it is never committed anywhere, and it is verified by the build:
# if it were incomplete the harness would not compile.
#
# The alternative -- two subprocess evaluators diffing stdout over a shared
# corpus -- also works and needs no trickery, but it pays a process spawn per
# program (measured ~40x the per-program cost here) and it cannot see a Go
# panic as anything richer than an exit status.  In-process is worth the sed.

set -euo pipefail

if [ $# -lt 3 ]; then
	sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
	exit 2
fi

LEFT_REF=$1
RIGHT_REF=$2
WORKDIR=$3

HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(git -C "$HERE" rev-parse --show-toplevel)

mkdir -p "$WORKDIR"
WORKDIR=$(cd "$WORKDIR" && pwd)

LEFT=$WORKDIR/left
RIGHT=$WORKDIR/right
HARNESS=$WORKDIR/harness

# Two detached worktrees, one per ref.  Worktrees rather than clones so the
# object store is shared and setup is a second rather than a minute.
for d in "$LEFT" "$RIGHT"; do
	if [ -d "$d" ]; then
		git -C "$REPO" worktree remove --force "$d" 2>/dev/null || rm -rf "$d"
	fi
done
git -C "$REPO" worktree add --detach "$LEFT" "$LEFT_REF" >/dev/null
git -C "$REPO" worktree add --detach "$RIGHT" "$RIGHT_REF" >/dev/null

# The module rename, left side only.
grep -rl 'github.com/luthersystems/elps' \
	--include='*.go' --include='go.mod' "$LEFT" |
	xargs sed -i 's|github.com/luthersystems/elps|github.com/luthersystems/elpsstock|g'

rm -rf "$HARNESS"
mkdir -p "$HARNESS"
cp "$HERE"/*.go "$HARNESS/"
cat >"$HARNESS/go.mod" <<EOF
module github.com/luthersystems/elps-dfuzz

go 1.25.0

require (
	github.com/luthersystems/elps v0.0.0-00010101000000-000000000000
	github.com/luthersystems/elpsstock v0.0.0-00010101000000-000000000000
)

replace github.com/luthersystems/elps => $RIGHT

replace github.com/luthersystems/elpsstock => $LEFT
EOF

cd "$HARNESS"
GOFLAGS=-mod=mod go mod tidy >/dev/null
go build -o "$WORKDIR/dfuzz" .

echo "left  (stock)  $LEFT_REF  -> $LEFT"
echo "right (sealed) $RIGHT_REF -> $RIGHT"
echo "binary         $WORKDIR/dfuzz"
