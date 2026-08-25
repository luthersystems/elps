#!/usr/bin/env bash
#
# Pre-flight check for a two-arm benchmark comparison: confirm the two raw
# `go test -bench` outputs are actually COMPARABLE before benchstat is asked to
# compare them.
#
# Why this script exists
# ----------------------
# benchstat pairs rows by the FULL benchmark name, and it keys its notion of a
# "configuration" off the `goos:` / `goarch:` / `cpu:` headers that `go test`
# emits.  When either of those disagrees between the arms, benchstat does not
# error in a way anyone can read -- it emits a table with no comparison rows, or
# one row per arm with nothing paired.  Downstream, cmd/benchgate correctly
# refuses to report "no regression" and exits 2, so the build does go red; but
# the log says only "could not be interpreted", which is a true statement about
# the symptom and says nothing about the cause.  Both of the causes below have
# actually happened in this repo:
#
#   * cpu / goarch mismatch.  The old design restored the baseline from
#     actions/cache -- a different machine, on a different day.  Consecutive
#     runs of IDENTICAL code drew an EPYC 7763 and an EPYC 9V74 and paired
#     nothing at all.  Fixed structurally by measuring both arms in one job;
#     this check is the backstop that names it if it ever recurs.
#
#   * GOMAXPROCS suffix mismatch.  `go test` appends `-N` to every benchmark
#     name, where N is GOMAXPROCS.  Two arms measured at different core counts
#     produce `EnvGet-4` and `EnvGet-2`, which are simply different names, so
#     the intersection is EMPTY and benchstat pairs nothing -- from a table that
#     otherwise looks completely normal.  This is a footgun with no warning
#     attached: it is triggered by a change to `runs-on`, in a different file,
#     by someone who has no reason to think they touched the benchmark gate.
#     The workflow now pins GOMAXPROCS so a runner swap cannot cause it; this
#     check is what catches it if the pin is ever removed or overridden.
#
# So: prevention lives in the workflow (one job, one runner, pinned GOMAXPROCS),
# and DETECTION lives here, phrased as the diagnosis rather than the symptom.
#
# Usage:  scripts/bench-arms-check.sh <base-output> <pr-output>
#
# Exit codes:
#   0  the two arms are comparable
#   2  they are not, or the input is unusable.  2 (not 1) on purpose: it matches
#      benchgate's "cannot interpret" code, and the workflow maps both
#      onto the same hard failure.  There is no exit 1 -- this script never
#      adjudicates performance, only comparability.

set -euo pipefail

if [ "$#" -ne 2 ]; then
	echo "usage: $0 <base-output> <pr-output>" >&2
	exit 2
fi

base="$1"
pr="$2"

problems=0

note() { echo "  $*"; }
problem() {
	problems=$((problems + 1))
	echo "  MISMATCH  $*" >&2
}

for f in "$base" "$pr"; do
	if [ ! -f "$f" ]; then
		echo "bench-arms-check: no such file: $f" >&2
		exit 2
	fi
	if [ ! -s "$f" ]; then
		echo "bench-arms-check: $f is empty -- that arm produced no output at all." >&2
		exit 2
	fi
done

# Unique values of a `key: value` context header across the whole file. `go
# test ./...` re-emits the header block per package, so these are sets, not
# scalars; a set of size > 1 within ONE arm means that arm itself was measured
# inconsistently, which is just as fatal as a mismatch between arms.
headers() {
	awk -v k="$1" '
		substr($0, 1, length(k) + 1) == k ":" {
			v = substr($0, length(k) + 2)
			gsub(/^[ \t]+|[ \t]+$/, "", v)
			if (v != "") print v
		}
	' "$2" | sort -u
}

# Benchmark result lines look like:
#   BenchmarkEnvGet-2   	  138022	      8104 ns/op	    1808 B/op	      27 allocs/op
# The iteration count in field 2 is what distinguishes a RESULT from the
# "--- FAIL" / "ok  " / "PASS" lines and from a `-bench` listing.
bench_names() {
	awk '$1 ~ /^Benchmark/ && $2 ~ /^[0-9]+$/ { print $1 }' "$1"
}

for key in goos goarch; do
	b="$(headers "$key" "$base" | paste -sd, -)"
	p="$(headers "$key" "$pr" | paste -sd, -)"
	if [ -z "$b" ] || [ -z "$p" ]; then
		note "no ${key}: header in one or both arms (base=${b:-<none>} pr=${p:-<none>}) -- skipping that comparison"
		continue
	fi
	if [ "$b" != "$p" ]; then
		problem "${key}: base=[${b}] pr=[${p}] -- the arms were measured on different platforms."
	else
		note "${key}: ${b} (both arms)"
	fi
done

# The CPU model is the one that bit this repo. benchstat treats it as part of
# the configuration key, so two arms drawn from a heterogeneous pool pair
# nothing even though goos/goarch agree.
#
# NOTE: `go test` emits no `cpu:` header on linux/arm64 -- confirmed on the
# runner this gate uses, where every measured run logs "header absent ...
# skipping". So on ARM this particular check is INERT, and it is the one-job
# design (both arms on one runner) that actually prevents the failure; this is
# a backstop for amd64 and for anyone who reintroduces a cross-machine
# baseline. It reports "skipping" rather than "match" on purpose: a check that
# cannot run must not be mistaken for one that passed.
bcpu="$(headers cpu "$base" | paste -sd' | ' -)"
pcpu="$(headers cpu "$pr" | paste -sd' | ' -)"
if [ -n "$bcpu" ] && [ -n "$pcpu" ]; then
	if [ "$bcpu" != "$pcpu" ]; then
		problem "cpu: base=[${bcpu}] pr=[${pcpu}] -- benchstat keys its configuration off this header, so the arms will not pair. Measure both arms in ONE job on ONE runner."
	else
		note "cpu: ${bcpu} (both arms)"
	fi
else
	note "cpu: header absent from one or both arms -- skipping"
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

bench_names "$base" | sort -u >"$tmp/base.names"
bench_names "$pr" | sort -u >"$tmp/pr.names"

nbase="$(wc -l <"$tmp/base.names" | tr -d ' ')"
npr="$(wc -l <"$tmp/pr.names" | tr -d ' ')"

if [ "$nbase" -eq 0 ] || [ "$npr" -eq 0 ]; then
	echo "bench-arms-check: no benchmark RESULT lines found in $([ "$nbase" -eq 0 ] && echo base) $([ "$npr" -eq 0 ] && echo pr) -- the benchmarks did not run, or failed to build." >&2
	exit 2
fi

comm -12 "$tmp/base.names" "$tmp/pr.names" >"$tmp/paired"
comm -23 "$tmp/base.names" "$tmp/pr.names" >"$tmp/base.only"
comm -13 "$tmp/base.names" "$tmp/pr.names" >"$tmp/pr.only"

npaired="$(wc -l <"$tmp/paired" | tr -d ' ')"

if [ "$npaired" -eq 0 ]; then
	# Zero pairs. Before reporting the generic "nothing in common", test the
	# specific hypothesis that only the GOMAXPROCS suffix differs -- that
	# turns an inscrutable empty table into a one-line diagnosis with an
	# obvious fix.
	sed 's/-[0-9][0-9]*$//' "$tmp/base.names" | sort -u >"$tmp/base.stripped"
	sed 's/-[0-9][0-9]*$//' "$tmp/pr.names" | sort -u >"$tmp/pr.stripped"
	if [ -n "$(comm -12 "$tmp/base.stripped" "$tmp/pr.stripped")" ]; then
		bsuf="$(sed -n 's/.*-\([0-9][0-9]*\)$/\1/p' "$tmp/base.names" | sort -u | paste -sd, -)"
		psuf="$(sed -n 's/.*-\([0-9][0-9]*\)$/\1/p' "$tmp/pr.names" | sort -u | paste -sd, -)"
		problem "GOMAXPROCS suffix: base benchmarks end in -[${bsuf:-none}], PR benchmarks in -[${psuf:-none}]. \`go test\` appends GOMAXPROCS to every benchmark name, so these are DIFFERENT names and benchstat pairs none of them -- from a table that looks normal. The arms ran with different GOMAXPROCS (usually: different runner sizes). Pin GOMAXPROCS in the workflow so the suffix cannot move."
	else
		problem "benchmark names: the arms share NO benchmark names at all (base has ${nbase}, PR has ${npr}) and the difference is not just the GOMAXPROCS suffix. Nothing can be compared."
	fi
else
	note "paired benchmarks: ${npaired} (base ${nbase}, pr ${npr})"
fi

# An unpaired benchmark is legitimate -- a PR may add or delete one -- so this
# is reported, never failed on. It is worth printing because a benchmark that
# silently DISAPPEARS from the comparison set is how a regression in it stops
# being gated, which has happened here before (a benchmarked package was
# deleted and the gate went quiet rather than red).
if [ -s "$tmp/base.only" ] || [ -s "$tmp/pr.only" ]; then
	note "unpaired (not compared; expected when a PR adds or removes a benchmark):"
	sed 's/^/    only in base: /' "$tmp/base.only"
	sed 's/^/    only in pr:   /' "$tmp/pr.only"
fi

# Sample counts. benchstat tolerates unequal n, but a large imbalance means one
# arm's loop died partway through, which is worth surfacing next to the numbers
# rather than leaving to be inferred from an "n=" column nobody reads.
if [ "$npaired" -gt 0 ]; then
	bmin="$(bench_names "$base" | sort | uniq -c | awk 'NR==1||$1<m{m=$1} END{print m+0}')"
	pmin="$(bench_names "$pr" | sort | uniq -c | awk 'NR==1||$1<m{m=$1} END{print m+0}')"
	note "min samples per benchmark: base=${bmin} pr=${pmin}"
	if [ "$bmin" -lt 6 ] || [ "$pmin" -lt 6 ]; then
		note "WARNING: benchstat cannot compute a confidence interval below 6 samples; rows will read 'need >= 6 samples'."
	fi
fi

if [ "$problems" -gt 0 ]; then
	echo "bench-arms-check: ${problems} mismatch(es) -- the arms are NOT comparable; refusing to let a meaningless comparison be adjudicated." >&2
	exit 2
fi

echo "bench-arms-check: arms are comparable."
exit 0
