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

## Checklist

- [ ] Baseline benchmarks captured before changes
- [ ] Same benchmark flags used for before/after
- [ ] `benchstat` comparison run
- [ ] Results reported with clear formatting
- [ ] Regressions >5% flagged and investigated
- [ ] Temp files cleaned up when done
