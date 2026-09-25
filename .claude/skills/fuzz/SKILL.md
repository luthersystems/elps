# /fuzz — Native Go Fuzzing Skill

Runs the repository's `go test -fuzz` targets, adds new ones, and triages
crashers. CI runs `.github/workflows/fuzz.yml`: 30s per target on PRs, 10m per
target nightly, sharded.

## Trigger

Use when asked to fuzz, add or change a fuzz target, investigate a fuzz CI
failure, or triage a file under some package's `testdata/fuzz/<Target>/`.

## Run

```bash
make fuzz-list                                  # authoritative list of targets (discovered with go test -list)
make fuzz                                       # every target, 30s each (the PR budget)
make fuzz FUZZTIME=10m                          # the nightly budget
make fuzz FUZZ_PKGS=./parser/rdparser/...       # one package
go test -run='^$' -fuzz='^FuzzEval$' -fuzztime=60s ./lisp/   # one target by hand
```

`scripts/fuzz.sh` refuses to run without a parsable `FUZZTIME` and caps every
invocation with a hard `-timeout`; never write a `-fuzz` invocation without
`-fuzztime` (CI checks for that). `FUZZ_TAGS=elpscheck` fuzzes the tagged
build, whose targets are otherwise invisible.

Plain `go test ./...` (and so `make test`) replays every committed seed and
crasher in `testdata/fuzz/` as a regression case.

## Triage a crasher

1. Find it: `go test -fuzz` writes `testdata/fuzz/<Target>/<hash>` in the
   target's package; CI uploads the same file as an artifact.
2. Reproduce without fuzzing: `go test -run='^<Target>$/^<hash>$' ./<pkg>/`.
3. Classify: a real defect in the code under test, a harness defect (budget
   too small for a terminating program, a watchdog on wall clock), or a
   toolchain race. `make fuzz-classify-test` covers how `scripts/fuzz.sh`
   reports each; read its output before assuming the product is at fault.
4. Fix the root cause with a focused regression test, and **commit the
   crasher file** so `go test` keeps replaying it.

## Add a target

1. Put it in the package's `fuzz_test.go` (or `<area>_fuzz_test.go`), named
   `FuzzXxx`. Discovery is automatic — there is no registration table.
2. Seed from `internal/fuzzseed` (`All()`, `LispSources()`, `Adversarial()`,
   `Pathological()`) plus target-specific adversarial input.
3. Assert a property with teeth (round-trip equality, no internal panic,
   termination), not merely "did not crash".
4. Any termination watchdog must use `internal/fuzzwatch` (scheduled-time
   budget, with a floor enforced by its call-site guard test), not a bare
   `time.After`.
5. Run `make fuzz-budget-check`: each new target adds a `FUZZTIME` to the
   nightly sweep, and the check derives whether `fuzz.yml`'s
   `timeout-minutes` still holds. Follow its printed options rather than
   raising the timeout by hand.
6. After touching `scripts/fuzz.sh` or `fuzz.yml`, run `make ci-gates-test`
   and `make fuzz-classify-test`.

## Hazards that are load-bearing

Read the header comment of the target before changing it; these are the
condensed reasons.

- **Evaluator (`lisp.FuzzEval`, `lisp/eval_fuzz_test.go`)** — evaluation is
  Turing-complete and `go test -fuzz` has no per-input deadline, so every
  evaluation runs under explicit budgets (`WithMaxSteps`,
  `WithMaxTailIterations`, `WithMaximumPhysicalStackHeight`, `WithMaxAlloc`, a
  context deadline) plus a watchdog. `env.eval` recovers Go panics into an
  ordinary-looking `*LVal`, so the real assertion is
  **`lisp.IsInternalPanic(result) == false`**. Eval seeds are executed, so
  `internal/fuzzseed/evalseed.go` is hand-written and does **not** seed from
  `LispSources()`. Seeds are split: `EvalRunaway` must error (a budget
  stopping an infinite loop is correct) and `EvalTerminating` must complete
  without error (a bounded program tripping a budget is a defect).
- **Debugger (`lisp/x/debugger.FuzzDebugEval`)** — attaching a debugger
  disables tail-call optimisation and stamps `MacroExpansionInfo` onto
  macro-expanded nodes, so it is a different eval path from `FuzzEval`'s.
- **Language server (`lsp.FuzzLSPSession`, `lsp/lsp_fuzz_test.go`)** — a
  fuzzer-chosen sequence of requests against fuzzer-chosen documents and
  cursor positions; the only target that reaches `analysis/` and `lint/`. The
  server has four blanket `recover()`s, so "no crash" means nothing until they
  are accounted for; the header documents which are neutralised, which is
  detected, and which is not covered.

## Checklist

- [ ] Target asserts a real property, with budgets and a `fuzzwatch` watchdog if it evaluates
- [ ] Seeds come from `internal/fuzzseed`
- [ ] `make fuzz-budget-check` passes after adding a target
- [ ] Crasher fixed at the root cause and its `testdata/fuzz/` file committed
- [ ] `make test` passes (replays the corpus)
