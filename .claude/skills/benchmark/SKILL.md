# /benchmark — Performance Benchmarking Skill

Runs before/after benchmark comparisons using `benchstat` to measure the performance impact of changes.

## Trigger

Use when asked to benchmark changes, check for performance regressions, or optimize code.

## Workflow

### 1. Find Benchmarks

```bash
go test -list 'Benchmark' ./... 2>/dev/null | grep -E '^Benchmark'
```

This lists all available benchmark functions across the codebase.

For an interpreter change, start with the whole-program benchmarks:
`BenchmarkWorkload` (`lisp/lisplib/workload_bench_test.go`: json, sortedmap,
string, recursion, template) and `BenchmarkCorpus` (`elpstest/`). A gain
claimed for a change should show there; micro-benchmarks explain it.

### 2. Measure base and head interleaved

Build one test binary per arm and alternate them, so machine noise lands on
both arms alike:

```bash
git worktree add /tmp/elps-base origin/main
(cd /tmp/elps-base && go test -c -o /tmp/base.test ./lisp/lisplib/)
go test -c -o /tmp/head.test ./lisp/lisplib/
cd lisp/lisplib
for i in $(seq 10); do
  /tmp/base.test -test.run='^$' -test.bench='^BenchmarkWorkload' -test.benchmem >> /tmp/bench-before.txt
  /tmp/head.test -test.run='^$' -test.bench='^BenchmarkWorkload' -test.benchmem >> /tmp/bench-after.txt
done
```

Swap the package and `-test.bench` pattern for a package-specific benchmark.
On a shared or loaded machine, `allocs/op` and `B/op` are deterministic and
are the primary evidence; treat `sec/op` as indicative, and run
`make bench-burnin` first.

### 3. Compare with benchstat

```bash
benchstat base=/tmp/bench-before.txt pr=/tmp/bench-after.txt
```

### 4. Interpret Results

Report the results with focus on:
- **Time**: `sec/op` — lower is better
- **Memory**: `B/op` — lower is better
- **Allocations**: `allocs/op` — lower is better
- **Statistical significance**: benchstat shows `~` for no significant change, `+`/`-` for changes with confidence intervals

**Regression threshold**: CI's gate fails a statistically significant bad-direction move of 15% or more on timing and 5% or more on allocations (see "CI Integration" below). Report any significant regression, and investigate those at or above the gate before pushing.

### 5. Report

Format results as a clear summary:

```
## Benchmark Results

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| BenchmarkEval | 1.23 µs/op | 1.15 µs/op | -6.5% |
| BenchmarkParse | 456 ns/op | 460 ns/op | ~ (no change) |

No regressions detected. Memory allocations reduced by 12%.
```

## CI Integration

The repo has a benchmark CI workflow (`.github/workflows/benchmark.yml`) that automatically runs benchstat comparisons on PRs. It posts results as a PR comment.

The comparison is adjudicated by `cmd/benchgate` (a Go tool built on
`golang.org/x/perf/benchfmt` + `benchmath`; substrate runs the same binary),
which fails the PR on a significant bad-direction move at or above the threshold
for that metric class (15% for timing, 5% for allocations — set in
`benchmark.yml`).

### When the gate fires on a regression you mean to accept

Do **not** raise either threshold, and do not skip the gate. Those are
per-metric-class noise floors; moving one to accept a single benchmark blinds
every other benchmark in the repo to the same magnitude of move. Add a **waiver**
to `scripts/benchstat-waivers.txt` instead:

```
pkg | benchmark | metric | ceiling | expires | issue | reason
```

It covers exactly one package, one benchmark and one metric column; it records a
ceiling, so the row fails again if the regression grows; it must name a tracking
issue and it expires. A waived row is still measured, still printed in the job
log, and still shown in the PR comment marked `WAIVED` — the waiver changes the
verdict, never the visibility. The format is documented at the top of that file,
and `scripts/ci-gates-test.sh` covers the mechanism.

Run the gate locally against a saved comparison before pushing:

```bash
benchstat base=/tmp/bench-before.txt pr=/tmp/bench-after.txt > /tmp/benchstat.txt
make bench-gate BENCHSTAT_OUT=/tmp/benchstat.txt              # as CI will judge it
# or drive the binary directly (this is what `make bench-gate` runs):
go run ./cmd/benchgate -waivers-default scripts/benchstat-waivers.txt /tmp/benchstat.txt
BENCH_WAIVERS= go run ./cmd/benchgate /tmp/benchstat.txt      # with waivers off
```

benchgate can also adjudicate the two raw `go test -bench` arms directly, with no
`benchstat` binary in the loop (`make bench-gate-arms BENCH_BASE=… BENCH_HEAD=…`).

### CI measures at `GOMAXPROCS=1`

`benchmark.yml` pins `GOMAXPROCS: "1"` (issue #767). At 2 Ps the concurrent
GC's mark workers ran on the second P, and allocation-heavy rows measured how
the shared runner scheduled that thread: ±7-20% on identical code, enough to
cross the 15% gate on PRs that never touched them. One P keeps GC cost in
sec/op and makes it deterministic. So CI rows carry no `-N` suffix, and to
reproduce a CI verdict locally measure the same way:

```bash
GOMAXPROCS=1 go test -run='^$' -bench=. -benchmem -benchtime=100ms -count=10 ./lisp/... | tee /tmp/bench.txt
```

### Before you measure: is the machine fit? (`make bench-burnin`)

```bash
make bench-burnin      # ~half a second; exit 0 fit, exit 3 re-measure elsewhere
```

A fixed, code-independent loop run seven times, requiring the samples to agree
to within ±10%. A machine that cannot reproduce a fixed loop cannot resolve a
10% gate on anything else either, and half an hour of benchmarking on one
produces numbers that read like findings. Run it first — on a laptop with a
browser open as much as on CI.

### `UNMEASURABLE` and exit 3

The gate has a fourth verdict (issue #542). A **timing** row whose own
confidence interval is at or above the fitness ceiling (`-variance-ceiling`,
default ±30%) is `UNMEASURABLE`: its delta is printed and adjudicated in
neither direction. If such a row moved at or above the gate, the run exits **3
(RUNNER-UNFIT)** — it found no regression *and* certified nothing, so the answer
is to re-run, not to read the diff. An unmeasurable row *below* its gate is a
warning only and changes no exit code.

Exit 1 still wins over exit 3: a regression measured on a row that could be
measured is a finding regardless of what else in the table was unmeasurable. And
the ceiling can only ever withhold a finding — it never turns a passing run into
a failing one.

The shape it exists for: an arm measuring itself at ±71% against the other arm's
±3%, on code that did not change, adjudicated as `+83% REGRESSION`. Preserved as
`cmd/benchgate/testdata/elps/benchstat-runner-unfit-542.txt`.

## Checklist

- [ ] Base and head measured interleaved, with the same flags
- [ ] Whole-program benchmarks (`BenchmarkWorkload`) checked for an interpreter change
- [ ] `benchstat` comparison run
- [ ] Results reported with clear formatting
- [ ] Significant regressions reported; any at or above the CI gate (15% timing / 5% allocs) investigated or waived
- [ ] Temp files cleaned up when done
