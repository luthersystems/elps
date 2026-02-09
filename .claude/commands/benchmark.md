# Run Benchmarks

Run before/after benchmark comparisons using `benchstat` to measure performance impact.

## Arguments

Optional: package path to benchmark (default: all packages). E.g., `./lisp/...`, `./parser/rdparser/...`

## Steps

### 1. Find Benchmarks

```bash
go test -list 'Benchmark' ./... 2>/dev/null | grep -E '^Benchmark'
```

### 2. Capture Baseline (Before Changes)

Stash or switch to the base branch:

```bash
git stash   # or: git checkout main
go test -bench=. -benchmem -count=5 -timeout=300s ./... | tee /tmp/bench-before.txt
git stash pop   # or: git checkout <feature-branch>
```

For specific packages:
```bash
go test -bench=. -benchmem -count=5 ./lisp/... | tee /tmp/bench-before.txt
```

### 3. Make Changes

Implement the optimization or code change.

### 4. Capture After

```bash
go test -bench=. -benchmem -count=5 -timeout=300s ./... | tee /tmp/bench-after.txt
```

Use the same flags and packages as the baseline.

### 5. Compare

```bash
benchstat base=/tmp/bench-before.txt pr=/tmp/bench-after.txt
```

If `benchstat` is not installed:
```bash
go install golang.org/x/perf/cmd/benchstat@latest
```

### 6. Report Results

| Metric | Meaning |
|--------|---------|
| `sec/op` | Time per operation (lower is better) |
| `B/op` | Memory per operation (lower is better) |
| `allocs/op` | Allocations per operation (lower is better) |
| `~` | No statistically significant change |
| `+`/`-` | Significant change with confidence interval |

Flag any benchmark that regresses by more than 5% with statistical significance.

Format results as a summary table:

```
| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| BenchmarkEval | 1.23 us/op | 1.15 us/op | -6.5% |
```

## CI Integration

The repo has `.github/workflows/benchmark.yml` that automatically runs benchstat on PRs and posts results as a PR comment. Results are informational -- they don't gate merges.

## Cleanup

Remove temp files when done:
```bash
rm -f /tmp/bench-before.txt /tmp/bench-after.txt
```
