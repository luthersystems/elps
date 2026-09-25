# /verify — CI Gate Skill

Runs the same checks as the `Lint & Test` job in `.github/workflows/elps.yml`,
in the same order. Use it before committing or creating a PR. If CI's steps
change, update this file to match — the workflow is the authority.

## Trigger

Use when asked to verify changes, check CI readiness, or as a pre-commit/pre-PR
gate.

## Pipeline

Run in order. Stop and report on the first failure.

| # | Step | Command | CI step name |
|---|------|---------|--------------|
| 1 | Confidentiality guard | `bash scripts/confidentiality-guard.sh` | Confidentiality guard |
| 2 | Go static checks | `make static-checks` | golangci-lint, golangci-lint config schema |
| 3 | Tests | `make test` | Tests |
| 4 | Race detector | `make race` | Race detection (full) |
| 5 | Tagged tests | `make test-elpscheck` | Singleton integrity (elpscheck) |
| 6 | elps's own Go analyzers | `make elpsvet` | Seal contract (elpsvet) |
| 7 | Sealed-AST benchmark oracle | `make bench-elpscheck-smoke` | Sealed-AST oracle over the benchmarks |
| 8 | Format | `go build -o elps . && ./elps fmt -l ./...` | Check formatting |
| 9 | Lisp lint | `./elps lint --workspace=. --exclude 'grammar' --include '_examples' ./...` | Lint ELPS source |
| 10 | Docstrings | `./elps doc -m` | Check missing documentation |

Notes:

- **Step 2**: `make static-checks` runs `golangci-lint config verify`, then
  `golangci-lint run ./...`, then a second pass under `--build-tags elpscheck` scoped to `./lisp/...`
  (tagged files are invisible to the first, and today they only live there). It warns when your golangci-lint
  differs from CI's pin; on a mismatch, trust CI — see "golangci-lint version
  skew" in `AGENTS.md` before deleting any `//nolint`.
- **Step 6**: two passes (untagged and `GOFLAGS=-tags=elpscheck`). For
  interpreting a finding, use the `/elpsvet` skill.
- **Step 8**: `-l` lists files that would change; fix with `./elps fmt ./...`
  and re-run.
- **Step 9**: fix the diagnostic, or add `; nolint:<check-name>` only for a
  genuine false positive.
- **Step 10**: add the missing docstring to the builtin/op/macro/export.
- CI also builds and vets on Windows (`go build ./... && go vet ./...`); run
  `go vet ./...` if you touched OS-specific code.
- Touched `scripts/`, `cmd/benchgate`, `benchmark.yml` or `fuzz.yml`? Also run
  `make ci-gates-test` (and `make fuzz-classify-test` for fuzz changes).

## On Failure

1. Report which step failed and the error output.
2. Fix the issue.
3. Re-run the pipeline from the beginning (fixes can introduce new issues).
4. When a fix to an analyzer (lisp `lint/` or `cmd/elpsvet`) stops it skipping
   nodes, expect it to surface new true positives in the repo's own files —
   fix those in the same PR rather than suppressing them.

## Quick Mode

For iterative development, run the relevant subset, then the full pipeline
before pushing:

- Changed Go code: steps 2, 3, 6
- Changed concurrency-sensitive code: add step 4
- Changed `.lisp` files: steps 3, 8, 9
- Added builtins/ops/macros: steps 2, 3, 10
