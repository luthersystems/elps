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

### 2. Run Baseline (Before Changes)

Stash or switch to the base branch to measure the baseline:

```bash
git stash  # or: git checkout main
go test -bench=. -benchmem -count=5 -timeout=300s ./... | tee /tmp/bench-before.txt
git stash pop  # or: git checkout <feature-branch>
```

If targeting specific packages:
```bash
go test -bench=. -benchmem -count=5 ./lisp/... | tee /tmp/bench-before.txt
```

### 3. Make Changes

Implement the optimization or code change.

### 4. Run After

```bash
go test -bench=. -benchmem -count=5 -timeout=300s ./... | tee /tmp/bench-after.txt
```

Use the same flags and packages as the baseline run for a fair comparison.

### 5. Compare with benchstat

```bash
benchstat base=/tmp/bench-before.txt pr=/tmp/bench-after.txt
```

### 6. Interpret Results

Report the results with focus on:
- **Time**: `sec/op` — lower is better
- **Memory**: `B/op` — lower is better
- **Allocations**: `allocs/op` — lower is better
- **Statistical significance**: benchstat shows `~` for no significant change, `+`/`-` for changes with confidence intervals

**Regression threshold**: Flag any benchmark that regresses by more than 5% with statistical significance.

### 7. Report

Format results as a clear summary:

```
## Benchmark Results

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| BenchmarkEval | 1.23 µs/op | 1.15 µs/op | -6.5% |
| BenchmarkParse | 456 ns/op | 460 ns/op | ~ (no change) |

No regressions detected. Memory allocations reduced by 12%.
```

## Package-Specific Benchmarking

For changes scoped to a single package:
```bash
go test -bench=. -benchmem -count=5 ./parser/rdparser/... | tee /tmp/bench-before.txt
# ... make changes ...
go test -bench=. -benchmem -count=5 ./parser/rdparser/... | tee /tmp/bench-after.txt
benchstat base=/tmp/bench-before.txt pr=/tmp/bench-after.txt
```

## CI Integration

The repo has a benchmark CI workflow (`.github/workflows/benchmark.yml`) that automatically runs benchstat comparisons on PRs. It posts results as a PR comment.

The comparison is adjudicated by `cmd/benchgate` (a Go tool built on
`golang.org/x/perf/benchfmt` + `benchmath`; it replaced the old
`scripts/benchstat-gate.sh` in issue #538, and substrate runs the same binary),
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

- [ ] Baseline benchmarks captured before changes
- [ ] Same benchmark flags used for before/after
- [ ] `benchstat` comparison run
- [ ] Results reported with clear formatting
- [ ] Regressions >5% flagged and investigated
- [ ] Temp files cleaned up when done
