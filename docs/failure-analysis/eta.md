# ETA -- Event Tree Analysis

## Date and Scope

**Date:** 2026-02-13
**Scope:** Full system analysis of the ELPS embedded Lisp interpreter (module `github.com/luthersystems/elps`), covering the parser, evaluator, runtime resource controls, error handling, type system, standard library, static analysis tooling, and CI pipeline.

**Method:** Forward-looking barrier analysis tracing initiating events through sequential safety controls to end states. Barrier effectiveness probabilities are derived from code-level analysis of implementation quality and coverage.

---

## Initiating Events

Six initiating events are identified, spanning the threat landscape for an embedded interpreter library.

| ID  | Initiating Event | Description | Estimated Frequency |
|-----|-----------------|-------------|---------------------|
| IE1 | Unbounded recursion / stack exhaustion | User or adversarial code triggers infinite or deeply nested recursion | High (common programming error) |
| IE2 | Excessive memory allocation | Code requests allocation of very large strings, sequences, or data structures | Medium-High (adversarial input or logic error) |
| IE3 | Malformed or adversarial source input | Invalid syntax, mismatched brackets, unterminated strings, or crafted inputs to the parser | High (any untrusted input scenario) |
| IE4 | Type confusion / incorrect LVal usage | Calling a function with wrong argument types, accessing fields on wrong LVal types, or violating singleton invariants | Medium (common in dynamic languages) |
| IE5 | Unhandled error propagation failure | Error LVals not checked by callers, causing downstream panics or silent corruption | Medium (code quality issue) |
| IE6 | Semantic/logic errors in user code | Undefined symbols, arity mismatches, misuse of set vs set!, incorrect macro expansion | Very High (most common ELPS usage error) |

---

## Barrier Inventory

### Preventive Barriers (stop the initiating event from causing harm)

| ID | Barrier | Type | Stage | Target IE | Effectiveness | Justification |
|----|---------|------|-------|-----------|---------------|---------------|
| B1 | Physical stack height limit | Preventive | Runtime | IE1 | 0.95 | `CallStack.checkHeightPhysical()` in `stack.go:179-187` enforces `MaxHeightPhysical=25000`. Checked on every `PushFID` call. Prevents Go goroutine stack overflow. Gap: the Go stack itself has no configurable limit -- a sufficiently deep native-call chain bypassing PushFID could still overflow. |
| B2 | Logical stack height limit | Preventive | Runtime | IE1 | 0.95 | `CallStack.CheckHeight()` in `stack.go:189-200` enforces `MaxHeightLogical=50000`. Also checked after TRO frame elision (`env.go:999-1001,1069-1072`). Covers the case where TRO collapses physical frames but logical depth grows. |
| B3 | Tail recursion optimization (TRO) | Preventive | Runtime | IE1 | 0.80 | `TerminalFID()` in `stack.go:113-145` detects terminal recursion chains. `funCall()` in `env.go:1042-1053` returns `markTailRec` markers that unwind the stack. TROBlock correctly prevents collapse across macro/handler-bind boundaries (`stack.go:123-136`). Gap: only works for self-tail-recursion; mutual recursion is not optimized. |
| B4 | MaxAlloc single-allocation limit | Preventive | Runtime | IE2 | 0.90 | `DefaultMaxAlloc = 10*1024*1024` (10M elements/bytes) in `runtime.go:68`. Enforced in `make-sequence` (`builtins.go:1714-1718`) and `string:repeat` (`libstring.go:135`). Gap: not all allocation paths are covered -- `Array()` in `lisp.go:437-470` validates dimensions but does not check against MaxAlloc for multi-dimensional arrays. `concat` and `map` builtins create output proportional to input without MaxAlloc checks. |
| B5 | Type checking in builtins | Preventive | Runtime | IE4 | 0.92 | Every builtin function checks argument types at entry (e.g., `builtinCAR` checks `v.Type != LSExpr` at `builtins.go:787`). Pattern is consistent across all ~80 builtins. Returns `env.Errorf()` with type information. Gap: some builtins use `panic()` for internal invariant violations (e.g., `FunData()` at `lisp.go:708-712`, `UserData()` at `lisp.go:753-758`) which would crash the host application rather than returning a graceful error. |
| B6 | Constant protection (true/false) | Preventive | Runtime | IE4 | 0.98 | `Put()` and `Update()` in both `env.go:461-462,475-476` and `package.go:126-127,139-140` explicitly check `k.Str == TrueSymbol || k.Str == FalseSymbol` and return errors. Singleton values (`lisp.go:289-293`) are pre-allocated and documented as immutable. |
| B7 | Parser bracket matching | Preventive | Parse-time | IE3 | 0.95 | `rdparser` detects mismatched brackets (e.g., `(]`) and reports both the opening and closing locations. Unmatched brackets produce `CondUnmatchedSyntax` or `CondMismatchedSyntax` errors (`conditions.go:10-11`). Format-preserving mode (`NewFormatting`) retains comments and metadata while maintaining full validation. |
| B8 | Go RE2 regex engine | Preventive | Runtime | IE2 | 0.99 | `libregexp.go:67` uses `regexp.Compile()` which guarantees linear-time matching (RE2 semantics). No backtracking. Eliminates ReDoS entirely. |
| B9 | Formal argument binding validation | Preventive | Runtime | IE4 | 0.90 | `bind()` in `env.go:1226-1282` validates argument counts against formals. Handles `&rest`, `&optional`, `&key` with proper error messages. `bindFormalNext()` detects invalid control symbol placement. Gap: arity errors only report count, not expected signature. |

### Detective Barriers (detect the fault after it occurs)

| ID | Barrier | Type | Stage | Target IE | Effectiveness | Justification |
|----|---------|------|-------|-----------|---------------|---------------|
| D1 | Stack trace on errors | Detective | Runtime | IE5 | 0.92 | `env.Errorf()` / `env.ErrorCondition()` in `env.go:782-798` always capture `env.Runtime.Stack.Copy()`. `ErrorAssociate()` in `env.go:803-818` fills in missing stack/source data. `ErrorVal.WriteTrace()` in `error.go:73-95` renders full traces. Gap: errors created via package-level `Errorf()` (not method) have no stack (`lisp.go:583-601`). |
| D2 | Static linter (16 analyzers) | Detective | Pre-runtime | IE6 | 0.85 | `DefaultAnalyzers()` in `lint.go:635-654` returns 16 checks including `set-usage`, `if-arity`, `let-bindings`, `defun-structure`, `cond-structure`, `builtin-arity`, `undefined-symbol`, `unused-variable`, `user-arity`, `rethrow-context`, etc. Runs in CI (`elps.yml:44-45`). Gap: semantic analyzers require `--workspace` flag and are no-ops without it. Not all user errors are detectable statically in a dynamic language. |
| D3 | CI pipeline (4 checks) | Detective | Build-time | IE6 | 0.90 | `elps.yml` runs: golangci-lint, `make test`, `elps fmt -l ./...`, `elps lint --workspace=. ./...`, and `elps doc -m`. Catches formatting drift, missing docs, Go-level issues, and ELPS lint failures. Gap: benchmark CI is informational only (exit 0). |
| D4 | Go test suite | Detective | Build-time | IE4,IE5 | 0.88 | Both Go unit tests (`_test.go`) and lisp test files via `elpstest.Runner`. `TestRepoFileRoundTrip` verifies formatter AST preservation. `TestASTPreservation` tests comprehensive patterns. `TestAllocLimits` verifies resource limit behavior. Gap: coverage of edge cases in macro expansion and deeply nested TRO is limited. |
| D5 | Missing documentation check | Detective | Build-time | IE6 | 0.95 | `elps doc -m` in CI (`elps.yml:48-49`) enforces that all builtins, special ops, macros, and library exports have docstrings. Implementation in `cmd/doc.go`. Comprehensive -- every public API point is checked. |
| D6 | Nolint suppression tracking | Detective | Lint-time | IE6 | 0.88 | `filterSuppressed()` in `lint.go:534-566` tracks usage of `; nolint:` directives. Unused nolints are reported as `unused-nolint` warnings (`lint.go:463-469`). Unknown analyzer names are detected (`lint.go:449-462`). Self-suppression via `; nolint:unused-nolint` is supported. |

### Mitigative Barriers (reduce consequences after fault manifests)

| ID | Barrier | Type | Stage | Target IE | Effectiveness | Justification |
|----|---------|------|-------|-----------|---------------|---------------|
| M1 | handler-bind / condition system | Mitigative | Runtime | IE5 | 0.85 | `opHandlerBind()` in `op.go:641-698` implements condition-based error handling. Condition matching by error type string (`val.Str`). `rethrow` preserves original stack trace (`builtins.go:777-783`). Condition stack managed via `PushCondition/PopCondition` with `defer` cleanup (`runtime.go:40-52`). Gap: handler evaluation errors silently replace original error (`op.go:678-680`, comment "Well, we're boned"). |
| M2 | ignore-errors | Mitigative | Runtime | IE5 | 0.90 | `opIgnoreErrors()` in `op.go:700-716` returns `Nil()` on any error, suppressing propagation. TROBlock is correctly set (`op.go:708`) to prevent stack unwinding. Simple and reliable. Gap: silently swallows all errors including potentially critical ones. |
| M3 | Error-as-value propagation | Mitigative | Runtime | IE5 | 0.88 | All functions return `*LVal` -- errors are values with `Type == LError`. Consistent pattern: `if v.Type == LError { return v }` throughout evaluator (`env.go:868-871, 891-893, 1061-1063, 1118-1119, 1148-1149`). Gap: requires discipline -- every call site must check. Some internal `panic()` calls bypass this mechanism. |
| M4 | Diagnostic rendering | Mitigative | User-facing | IE3,IE6 | 0.90 | Rust-style annotated source snippets via `diagnostic/` package. `^^^` underlines, `-->` location, `= note:` lines. Wired into `cmd/run.go` and `cmd/lint.go`. Color mode detection (`Auto/Always/Never`). Every lint diagnostic includes `; nolint:` suppression hint in notes. |
| M5 | Package isolation | Mitigative | Runtime | IE6 | 0.80 | Package system in `package.go` provides namespace isolation. Qualified symbols (`pkg:name`) prevent accidental shadowing. `UsePackage` only imports exported symbols (`env.go:195-203`). Gap: all packages share a single `Runtime` (no sandboxing), so resource limits and condition stack are shared. `in-package` auto-imports the lang package (`builtins.go:485-491`) which could mask errors. |

---

## Event Trees

### Event Tree 1: Unbounded Recursion (IE1)

```
IE1: Deep/infinite recursion triggered
 |
 +-- B3: TRO detects self-tail-recursion?
 |    |
 |    +-- YES (p=0.80): Stack frames collapsed
 |    |    |
 |    |    +-- B2: Logical height still within limit?
 |    |         |
 |    |         +-- YES (p=0.95): [END STATE A: Normal completion]
 |    |         |
 |    |         +-- NO  (p=0.05): LogicalStackOverflowError returned
 |    |              |
 |    |              +-- M3: Error propagated as LVal?
 |    |                   |
 |    |                   +-- YES (p=0.95): [END STATE B: Graceful error with stack trace]
 |    |                   +-- NO  (p=0.05): [END STATE C: Error lost or host panic]
 |    |
 |    +-- NO  (p=0.20): Mutual recursion or non-tail position
 |         |
 |         +-- B1: Physical stack limit exceeded?
 |              |
 |              +-- YES before Go crash (p=0.95): PhysicalStackOverflowError
 |              |    |
 |              |    +-- M3: Error propagated?
 |              |         |
 |              |         +-- YES (p=0.95): [END STATE B: Graceful error]
 |              |         +-- NO  (p=0.05): [END STATE C: Error lost]
 |              |
 |              +-- NO -- Go goroutine stack overflow (p=0.05):
 |                   [END STATE D: Host process crash (panic)]
```

### Event Tree 2: Excessive Memory Allocation (IE2)

```
IE2: Large allocation requested
 |
 +-- Is it a guarded allocation path? (make-sequence, string:repeat)
 |    |
 |    +-- YES (p=0.30): B4 MaxAlloc check
 |    |    |
 |    |    +-- BLOCKS (p=0.99): [END STATE B: Graceful error]
 |    |    +-- PASSES (p=0.01): [END STATE E: Memory exhaustion]
 |    |
 |    +-- NO  (p=0.70): Unguarded path (concat, map, Array, json:load-bytes)
 |         |
 |         +-- B8: Is this a regex (RE2 linear time)?
 |         |    |
 |         |    +-- YES (p=0.10): [END STATE A: Normal, bounded execution]
 |         |    +-- NO  (p=0.90): Allocation proceeds unchecked
 |         |         |
 |         |         +-- Go runtime OOM?
 |         |              |
 |         |              +-- YES (p=0.15): [END STATE E: Memory exhaustion / Go OOM]
 |         |              +-- NO  (p=0.85): [END STATE A: Normal completion]
```

### Event Tree 3: Malformed Source Input (IE3)

```
IE3: Invalid source code presented to parser
 |
 +-- B7: Parser detects syntax error?
 |    |
 |    +-- YES (p=0.95): Parse error with location info
 |    |    |
 |    |    +-- M4: Diagnostic rendering provides context?
 |    |         |
 |    |         +-- YES (p=0.90): [END STATE F: Clear error message with source annotation]
 |    |         +-- NO  (p=0.10): [END STATE G: Raw error message (still safe)]
 |    |
 |    +-- NO  (p=0.05): Accepts malformed input as valid AST
 |         |
 |         +-- B5: Type checking catches semantic nonsense at eval time?
 |              |
 |              +-- YES (p=0.85): [END STATE B: Runtime error with stack trace]
 |              +-- NO  (p=0.15): [END STATE H: Silent incorrect behavior]
```

### Event Tree 4: Type Confusion (IE4)

```
IE4: Wrong type passed to function/operation
 |
 +-- B5: Builtin type check catches it?
 |    |
 |    +-- YES (p=0.92): Error returned with type info
 |    |    |
 |    |    +-- M1: handler-bind catches error?
 |    |    |    |
 |    |    |    +-- YES (p=0.30): [END STATE I: Handled recovery]
 |    |    |    +-- NO  (p=0.70):
 |    |    |         +-- M3: Error propagates to top level?
 |    |    |              |
 |    |    |              +-- YES (p=0.95): [END STATE B: Graceful error]
 |    |    |              +-- NO  (p=0.05): [END STATE C: Error lost]
 |    |
 |    +-- NO  (p=0.08): Reaches a panic() path
 |         |
 |         +-- B9: Formal binding catches arity mismatch first?
 |              |
 |              +-- YES (p=0.70): [END STATE B: Graceful error]
 |              +-- NO  (p=0.30): panic() called
 |                   [END STATE D: Host process crash]
```

### Event Tree 5: Unhandled Error Propagation Failure (IE5)

```
IE5: Error LVal not checked at some call site
 |
 +-- D1: Stack trace attached to error?
 |    |
 |    +-- YES (p=0.92): Error has source + call context
 |    |    |
 |    |    +-- M3: Error-as-value pattern followed?
 |    |         |
 |    |         +-- YES (p=0.88): Error bubbles up correctly
 |    |         |    [END STATE B: Graceful error with trace]
 |    |         +-- NO  (p=0.12): Error used as non-error value
 |    |              [END STATE H: Silent incorrect behavior]
 |    |
 |    +-- NO  (p=0.08): Package-level Errorf (no stack)
 |         |
 |         +-- M3: Still propagated?
 |              +-- YES (p=0.88): [END STATE G: Error without full trace]
 |              +-- NO  (p=0.12): [END STATE H: Silent incorrect behavior]
```

### Event Tree 6: Semantic/Logic Errors in User Code (IE6)

```
IE6: User writes semantically incorrect ELPS code
 |
 +-- D2: Linter detects the issue?
 |    |
 |    +-- YES (p=0.50): Lint diagnostic raised
 |    |    |
 |    |    +-- D3: CI catches it before deployment?
 |    |         |
 |    |         +-- YES (p=0.90): [END STATE J: Issue caught pre-runtime]
 |    |         +-- NO  (p=0.10): [END STATE K: Known but unresolved issue]
 |    |
 |    +-- NO  (p=0.50): Issue not detectable statically
 |         |
 |         +-- B5/B9: Runtime type/arity check catches it?
 |              |
 |              +-- YES (p=0.75): [END STATE B: Runtime error with trace]
 |              +-- NO  (p=0.25):
 |                   |
 |                   +-- D4: Test suite catches it?
 |                        |
 |                        +-- YES (p=0.60): [END STATE J: Caught in testing]
 |                        +-- NO  (p=0.40): [END STATE H: Silent incorrect behavior]
```

---

## End-State Probability Calculations

### End-State Definitions

| End State | Consequence | Severity |
|-----------|-------------|----------|
| A | Normal completion | None (0) |
| B | Graceful error with stack trace | Low (1) |
| C | Error lost / silently dropped | Medium (3) |
| D | Host process crash (Go panic) | Critical (5) |
| E | Memory exhaustion / Go OOM | Critical (5) |
| F | Clear diagnostic with source annotation | None (0) |
| G | Raw error message (no trace/annotation) | Low (1) |
| H | Silent incorrect behavior | High (4) |
| I | Handled recovery via handler-bind | None (0) |
| J | Issue caught pre-runtime (lint/test) | None (0) |
| K | Known but unresolved issue | Low (1) |

### IE1: Unbounded Recursion -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| TRO success + within limit | 0.80 x 0.95 = 0.760 | A |
| TRO success + logical overflow + propagated | 0.80 x 0.05 x 0.95 = 0.038 | B |
| TRO success + logical overflow + lost | 0.80 x 0.05 x 0.05 = 0.002 | C |
| No TRO + physical limit catches + propagated | 0.20 x 0.95 x 0.95 = 0.181 | B |
| No TRO + physical limit catches + lost | 0.20 x 0.95 x 0.05 = 0.010 | C |
| No TRO + Go stack overflow | 0.20 x 0.05 = 0.010 | D |

**Risk summary for IE1:** P(D)=0.01, P(C)=0.012, P(B)=0.219, P(A)=0.760.

### IE2: Excessive Allocation -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| Guarded path + blocked | 0.30 x 0.99 = 0.297 | B |
| Guarded path + bypass | 0.30 x 0.01 = 0.003 | E |
| Unguarded + regex (safe) | 0.70 x 0.10 = 0.070 | A |
| Unguarded + non-regex + no OOM | 0.70 x 0.90 x 0.85 = 0.536 | A |
| Unguarded + non-regex + OOM | 0.70 x 0.90 x 0.15 = 0.095 | E |

**Risk summary for IE2:** P(E)=0.098, P(B)=0.297, P(A)=0.606. The 9.8% OOM probability on unguarded paths is the highest-risk finding.

### IE3: Malformed Source -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| Parser catches + good diagnostic | 0.95 x 0.90 = 0.855 | F |
| Parser catches + raw error | 0.95 x 0.10 = 0.095 | G |
| Parser misses + eval catches | 0.05 x 0.85 = 0.043 | B |
| Parser misses + eval misses | 0.05 x 0.15 = 0.008 | H |

**Risk summary for IE3:** P(H)=0.008, P(F)=0.855. Parser is highly effective.

### IE4: Type Confusion -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| Builtin catches + handled | 0.92 x 0.30 = 0.276 | I |
| Builtin catches + propagated | 0.92 x 0.70 x 0.95 = 0.612 | B |
| Builtin catches + lost | 0.92 x 0.70 x 0.05 = 0.032 | C |
| Builtin misses + arity catches | 0.08 x 0.70 = 0.056 | B |
| Builtin misses + panic | 0.08 x 0.30 = 0.024 | D |

**Risk summary for IE4:** P(D)=0.024, P(C)=0.032, P(B)=0.668, P(I)=0.276.

### IE5: Error Propagation Failure -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| Has stack + propagated | 0.92 x 0.88 = 0.810 | B |
| Has stack + used-as-value | 0.92 x 0.12 = 0.110 | H |
| No stack + propagated | 0.08 x 0.88 = 0.070 | G |
| No stack + used-as-value | 0.08 x 0.12 = 0.010 | H |

**Risk summary for IE5:** P(H)=0.120, P(B)=0.810. The 12% silent-incorrect-behavior rate is significant.

### IE6: Semantic Errors -- End-State Probabilities

| Path | Probability | End State |
|------|-------------|-----------|
| Lint catches + CI blocks | 0.50 x 0.90 = 0.450 | J |
| Lint catches + not blocked | 0.50 x 0.10 = 0.050 | K |
| Lint misses + runtime catches | 0.50 x 0.75 = 0.375 | B |
| Lint misses + runtime misses + test catches | 0.50 x 0.25 x 0.60 = 0.075 | J |
| Lint misses + runtime misses + test misses | 0.50 x 0.25 x 0.40 = 0.050 | H |

**Risk summary for IE6:** P(H)=0.050, P(J)=0.525, P(B)=0.375.

---

## Critical End States Ranked by Risk

Risk = P(end state across all IEs) x Consequence severity.

| Rank | End State | Primary IE | Probability | Severity | Risk Score | Description |
|------|-----------|-----------|-------------|----------|------------|-------------|
| 1 | E (OOM) | IE2 | 0.098 | 5 (Critical) | 0.49 | Memory exhaustion from unguarded allocation paths. Most allocation builtins (concat, map, json:load-bytes, Array) do not check MaxAlloc. |
| 2 | H (Silent wrong) | IE5 | 0.120 | 4 (High) | 0.48 | Error LVals not checked at all call sites, causing downstream computation on error values as if they were normal data. |
| 3 | H (Silent wrong) | IE6 | 0.050 | 4 (High) | 0.20 | Semantic logic errors that escape both linting and runtime checks. Unavoidable floor in dynamic languages but improvable. |
| 4 | D (Crash) | IE4 | 0.024 | 5 (Critical) | 0.12 | Internal `panic()` calls in type-assertion paths (FunData, UserData, Bytes, Map) crash the host when invariants are violated. |
| 5 | D (Crash) | IE1 | 0.010 | 5 (Critical) | 0.05 | Go goroutine stack overflow before ELPS physical limit triggers. Rare but unrecoverable. |
| 6 | C (Error lost) | IE4 | 0.032 | 3 (Medium) | 0.10 | Errors dropped in propagation chain, leading to confusing nil results. |

---

## Barrier Redundancy Analysis

### Redundant/Parallel Barriers

**IE1 (Recursion):** Three barriers operate in series: TRO (B3) -> Logical limit (B2) -> Physical limit (B1). This is excellent defense-in-depth. Even if TRO fails (mutual recursion), the physical limit catches it. The only gap is Go goroutine stack overflow before B1 triggers, which is a residual risk.

**IE4 (Type confusion):** Two barriers in series: Formal argument binding (B9) -> Builtin type checking (B5). B9 catches arity errors before B5 runs. However, the failure mode when both miss is `panic()` (not a graceful error), which means the consequence jumps from Low to Critical with no further mitigation.

**IE6 (Semantic errors):** Four barriers in series: Linter (D2) -> CI (D3) -> Runtime checks (B5/B9) -> Test suite (D4). This is strong layered defense, though each layer operates on different error subsets rather than the same errors, so they are more complementary than redundant.

### Single Points of Failure

1. **MaxAlloc enforcement (B4):** Only applied to 2 of ~15 allocation-capable builtins. No redundant barrier exists for unguarded paths. This is the system's largest single-point-of-failure for resource exhaustion.

2. **Error-as-value discipline (M3):** No compile-time enforcement that error LVals are checked. Go's type system cannot distinguish `*LVal` with `Type==LError` from any other `*LVal`. This is an inherent weakness of the error-as-value pattern without sum types.

3. **panic() in type assertions (B5 gap):** Methods like `FunData()`, `Bytes()`, `Map()`, `UserData()` use `panic()` rather than returning errors. There is no `recover()` wrapper in the evaluator to catch these. A single missed type check can crash the entire host process.

---

## Barrier Degradation Model

### Dynamic Barrier Effectiveness Over Time

| Barrier | Fresh Effectiveness | 1yr Degradation | 3yr Degradation | Degradation Driver |
|---------|--------------------|-----------------|-----------------|--------------------|
| B1/B2 Stack limits | 0.95 | 0.95 | 0.94 | Stable -- constants in code, no external dependencies |
| B3 TRO | 0.80 | 0.78 | 0.75 | New language features (e.g., async, continuations) could introduce TRO-incompatible frames |
| B4 MaxAlloc | 0.90 | 0.85 | 0.75 | New builtins/stdlib functions are likely to be added without MaxAlloc checks |
| B5 Type checking | 0.92 | 0.90 | 0.85 | Each new builtin must manually add type checks; no automated enforcement |
| B7 Parser | 0.95 | 0.95 | 0.93 | Parser is mature; new syntax additions are rare |
| B8 RE2 | 0.99 | 0.99 | 0.99 | Provided by Go stdlib, no degradation expected |
| D2 Linter | 0.85 | 0.87 | 0.90 | Improving -- new analyzers are actively added (16 currently) |
| D3 CI | 0.90 | 0.88 | 0.85 | Config drift, Go version changes, action version bumps |
| M1 handler-bind | 0.85 | 0.85 | 0.83 | Stable mechanism, but subtle handler-evaluation bugs could accumulate |
| M3 Error-as-value | 0.88 | 0.86 | 0.82 | Each new code path is another potential check omission |

**Key degradation insight:** B4 (MaxAlloc) and B5 (type checking) are the barriers most at risk of degradation because they require manual application in every new function. B4 especially degrades as the stdlib grows, since new allocation-capable builtins are likely to omit the check (the pattern is only established in 2 of ~15 paths).

---

## Top 5 Recommendations

### 1. Extend MaxAlloc Checks to All Allocation-Capable Builtins (Risk: 0.49)

**Target:** IE2, End State E (OOM)

Create a centralized `checkAlloc(env *LEnv, n int) *LVal` helper that validates against `env.Runtime.MaxAllocBytes()` and use it in `concat`, `map`, `builtinAppend`, `Array()`, `json:load-bytes`, and any function that creates output proportional to input size. Currently only `make-sequence` and `string:repeat` are protected. This is the single highest-impact change.

**Implementation:** Add a method to `Runtime`:
```go
func (r *Runtime) CheckAlloc(n int) error {
    if n > r.MaxAllocBytes() {
        return fmt.Errorf("allocation of %d exceeds limit (%d)", n, r.MaxAllocBytes())
    }
    return nil
}
```
Then call it at the top of every allocating builtin before creating the output slice.

### 2. Replace Internal panic() Calls with Error Returns (Risk: 0.12)

**Target:** IE4, End State D (host crash)

The methods `FunData()`, `Bytes()`, `Map()`, `UserData()`, `seqCells()`, and `makeByteSeq()` in `lisp.go` all use `panic()` when type invariants are violated. These should return `(*LFunData, error)` or use a checked-variant pattern. Alternatively, add a `recover()` wrapper in the top-level `Eval()` path that converts panics to `LError` values with diagnostic information. This prevents a single type confusion bug from crashing the entire host application.

### 3. Add Go Lint Rule or Code Generator for Error-Check Discipline (Risk: 0.48)

**Target:** IE5, End State H (silent incorrect behavior)

The 12% probability of error LVals being used as normal values is the second-highest risk. Consider:
- A custom `go vet` analyzer that flags `*LVal` return values not checked for `.Type == LError`
- Or use `errcheck`-style analysis adapted for the LVal error pattern
- At minimum, add a comment convention and audit tool that ensures every `env.Eval()` and `env.FunCall()` return is followed by an error type check

### 4. Enable Semantic Analyzers by Default in CI (Risk: 0.20)

**Target:** IE6, End State H (silent incorrect behavior)

The CI already runs `elps lint --workspace=. ./...` which enables semantic analysis. However, the `undefined-symbol`, `unused-variable`, `unused-function`, and `shadowing` analyzers are only active with workspace scanning. Ensure the workspace path is always provided in CI and consider making semantic analysis the default mode. Additionally, add more targeted analyzers for common error patterns (e.g., unchecked `load-file` return values, `set` on already-bound symbols in function bodies).

### 5. Add recover() Safety Net in Evaluator Entry Points (Risk: 0.05 + 0.12)

**Target:** IE1/IE4, End State D (host crash)

Wrap `env.Eval()`, `env.FunCall()`, and `env.MacroCall()` with `defer recover()` blocks that convert Go panics into `LError` values. This provides a last-resort barrier against both unexpected Go stack overflows and internal `panic()` calls. The recovered error should include the panic value and a synthetic stack trace. This is specifically important for embedded use cases where crashing the host process is unacceptable.

```go
func (env *LEnv) SafeEval(v *LVal) (result *LVal) {
    defer func() {
        if r := recover(); r != nil {
            result = env.Errorf("internal error (recovered panic): %v", r)
        }
    }()
    return env.Eval(v)
}
```
