#!/usr/bin/env bash
# Copyright © 2026 The ELPS authors
#
# Supervise a dfuzz run in seed blocks, surviving a process-fatal crash.
#
#   ./run.sh <workdir> <blocks> <block-size> [extra dfuzz args...]
#
# WHY A SUPERVISOR IS NEEDED
#
# Not every crash is recoverable in-process.  The elpspath write-back defect
# (fixed by 0442c52) lets a lisp program build a value that contains ITSELF:
# the reworked cell slice is written into one of the list's own elements.
# Printing that value recurses until the goroutine stack is exhausted, and a
# Go stack overflow is a `fatal error`, not a panic -- recover() cannot see it
# and the process dies where it stands.
#
# That is the single most valuable outcome a differential harness can produce,
# so losing the rest of the run to it would be perverse.  This script runs the
# harness in seed blocks, and when a block dies with the runtime's fatal-error
# status it re-runs that block single-threaded with -trace to name the exact
# seed, reports it, and carries on with the next block.
#
# Exit statuses from dfuzz: 0 clean, 1 findings, 2 process-fatal (the Go
# runtime's own), 3 usage.

set -uo pipefail

if [ $# -lt 3 ]; then
	sed -n '2,26p' "$0" | sed 's/^# \{0,1\}//'
	exit 3
fi

WORKDIR=$1
BLOCKS=$2
SIZE=$3
shift 3

BIN=$WORKDIR/dfuzz
rc_overall=0
start=${DFUZZ_START:-1}

for ((b = 0; b < BLOCKS; b++)); do
	echo "=== block $b: seeds $start..$((start + SIZE - 1))"
	"$BIN" -start "$start" -n "$SIZE" "$@"
	rc=$?
	case $rc in
	0) ;;
	1) rc_overall=1 ;;
	2)
		rc_overall=2
		echo "!!! PROCESS-FATAL CRASH in block $b (seeds $start..$((start + SIZE - 1)))"
		echo "!!! isolating the seed single-threaded"
		"$BIN" -start "$start" -n "$SIZE" -workers 1 -trace "$@" 2>"$WORKDIR/trace-$b.err" >/dev/null
		echo "!!! last seed attempted: $(grep -c . <"$WORKDIR/trace-$b.err" >/dev/null && tail -n 20 "$WORKDIR/trace-$b.err" | grep '^TRY seed=' | tail -1)"
		;;
	*)
		echo "!!! dfuzz exited $rc"
		rc_overall=$rc
		;;
	esac
	start=$((start + SIZE))
done

exit $rc_overall
