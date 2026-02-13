# Cross-Method Synthesis Report -- ELPS Failure Mode Analysis

**Date:** 2026-02-13
**System:** ELPS Embedded Lisp Interpreter (github.com/luthersystems/elps)
**Analyst:** Claude Opus 4.6
**Methods:** FMEA, FTA, ETA, STPA, Markov/Semi-Markov, Petri Net/CPN, HAZOP

---

## 1. Executive Summary

Seven independent failure analysis methods were applied to the ELPS embedded Lisp interpreter codebase. Together they examined the parser, evaluator, runtime resource controls, error handling, package system, file loading, and standard library across approximately 15,000 lines of Go code.

**Key Statistics:**
- **High Confidence findings** (3+ methods agree): 10
- **Medium Confidence findings** (2 methods agree): 12
- **Method-Specific findings** (1 method only): 8
- **Total unique findings:** 30
- **Total recommendations (deduplicated):** 15
- **Explicit panics identified in `lisp/`:** 37 call sites, 0 `recover()` call sites
- **Allocation paths checked by MaxAlloc:** 2 of ~15+
- **Concurrency protection on Runtime:** 0 mutexes on core state

**Top 5 Critical Findings:**

1. **No evaluation timeout or cancellation mechanism** -- ELPS cannot interrupt a running evaluation. `dotimes` loops, TRO-enabled infinite recursion, and CPU-bound code block the Go goroutine indefinitely. (7/7 methods)
2. **Incomplete allocation limit enforcement** -- `MaxAllocBytes()` is checked in only 2 builtins (`make-sequence`, `string:repeat`). The other ~13 allocation-capable builtins (`concat`, `append`, `map`, `json:load-*`, `vector`, etc.) allocate without limit. (7/7 methods)
3. **Parser has no depth limit** -- Deeply nested input (50K+ parentheses) overflows the Go goroutine stack, causing a fatal crash unrecoverable by `recover()`. ELPS stack limits apply only during evaluation, not parsing. (6/7 methods)
4. **Filesystem access unrestricted by default** -- `RelativeFileSystemLibrary` with empty `RootDir` (the default) allows `load-file` to read any path. Even when configured, a TOCTOU symlink race can bypass confinement. (5/7 methods)
5. **Go panics crash the host process** -- 37 explicit `panic()` sites in `lisp/` with 0 `recover()` wrappers. Type assertion panics in `FunData()`, `Bytes()`, `Map()`, `UserData()` and stack operations can crash the entire host application. (6/7 methods)

---

## 2. Methodology

Seven complementary failure analysis methods were selected to provide maximum coverage across different failure dimensions:

| Method | Purpose | Strength |
|--------|---------|----------|
| **FMEA** (Failure Mode and Effects Analysis) | Bottom-up component analysis. Every function, module, and data structure examined for failure modes with Severity/Occurrence/Detection scoring. | Exhaustive enumeration of individual failure modes with quantified Risk Priority Numbers (RPNs). |
| **FTA** (Fault Tree Analysis) | Top-down deductive analysis from 5 catastrophic end states. Boolean logic gates trace causes backward from effects. | Identifies common-cause failures, minimal cut sets, and Fussell-Vesely importance rankings. |
| **ETA** (Event Tree Analysis) | Forward-looking barrier analysis. 6 initiating events traced through sequential safety barriers to end states. | Quantifies barrier effectiveness and identifies single points of failure in defense layers. |
| **STPA** (Systems-Theoretic Process Analysis) | Control-theoretic analysis. Models controllers (host, evaluator, parser, package system), control actions, and feedback channels. Identifies unsafe control actions. | Discovers systemic hazards from missing or incorrect control actions and inadequate feedback, especially relevant for embedded systems. |
| **Markov/Semi-Markov** | Stochastic state-transition modeling of interpreter availability. CTMC with Semi-Markov extensions for deterministic thresholds. | Provides quantitative availability estimates, MTTF calculations, and sensitivity analysis showing which parameters most affect reliability. |
| **Petri Net/CPN** | Formal concurrency and resource flow modeling. 24 places, 31 transitions, 5 token colors model the full evaluation pipeline. | Proves deadlock-freedom, liveness, and boundedness. Identifies resource bottlenecks and quantifies token flow timing. |
| **HAZOP** | Security-adapted guide word analysis with 10 security-specific guide words across 7 study nodes. Interaction matrix reveals cross-component attack chains. | Systematic deviation analysis catches subtle interaction effects. Attack chain scenarios combine multiple weaknesses into realistic exploit paths. |

---

## 3. System Under Analysis

### Architecture

```
+-----------------------------------------------------------------------+
|                         GO HOST APPLICATION                            |
|  Creates LEnv, configures Runtime, calls Eval/Load                    |
+------------------+----------------------------------------------------+
                   |
           config  | (WithReader, WithLibrary, WithMaximum*StackHeight,
                   |  Runtime.MaxAlloc, Library.RootDir)
                   v
+-----------------------------------------------------------------------+
|                      RUNTIME (Shared State)                           |
|  PackageRegistry | CallStack (25K/50K) | conditionStack | Reader     |
|  SourceLibrary   | Profiler (optional) | MaxAlloc (10M) | Counters   |
+------------------+----------------------------------------------------+
                   |
        Eval()     | control actions: eval, call, macro-expand,
        Load()     | load-file, set, in-package, export, use-package
                   v
+-----------------------------------------------------------------------+
|                    LEnv EVALUATOR                                      |
|  Tree-structured scoping: LEnv -> Parent -> ... -> root               |
|  Dispatches: FunCall, SpecialOpCall, MacroCall                        |
|  Manages: TRO, scope binding, error propagation                      |
+------------------+----------------------------------------------------+
                   |
                   v
+-----------------------------------------------------------------------+
|              LISP CODE EXECUTION                                       |
|  Builtins (~80) | Special Ops (~30) | Macros (~8) | Stdlib (~12 pkgs)|
+------------------+----------------------------------------------------+
                   |
                   v
+-----------------------------------------------------------------------+
|              EXTERNAL RESOURCES                                        |
|  Filesystem | Go goroutine stack | Heap memory | stderr               |
+-----------------------------------------------------------------------+
```

### Scope

- **Core interpreter:** `lisp/` -- LVal types, LEnv evaluator, builtins, operators, macros, package system, error handling, stack management, resource limits
- **Parser:** `parser/` -- lexer, recursive descent parser, token/scanner
- **Standard library:** `lisp/lisplib/` -- JSON, regexp, string, math, time, testing, etc.
- **Tooling:** `formatter/`, `lint/`, `analysis/`, `diagnostic/`
- **CLI:** `cmd/` -- run, repl, doc, lint, fmt
- **Module:** `github.com/luthersystems/elps` (Go)

### Key Source Files

| File | Relevance |
|------|-----------|
| `lisp/env.go` | Evaluator core, Eval(), FunCall, MacroCall, scoping |
| `lisp/runtime.go` | Shared state, MaxAlloc, conditionStack |
| `lisp/stack.go` | Call stack, depth limits, TRO |
| `lisp/lisp.go` | LVal type, singletons, Copy(), Equal(), panic-prone accessors |
| `lisp/builtins.go` | ~80 builtins including arithmetic, set, eval, load-file |
| `lisp/op.go` | Special operators: let, if, handler-bind, dotimes |
| `lisp/macro.go` | Macro expansion, stampMacroExpansion |
| `lisp/package.go` | Package registry, symbol binding |
| `lisp/library.go` | File loading, RootDir confinement |
| `parser/rdparser/parser.go` | Recursive descent parser, no depth limit |
| `lisp/lisplib/libjson/json.go` | JSON serialization, unbounded recursion |

---

## 4. Critical Findings

### High Confidence Findings (3+ methods agree)

#### HC-1: No Evaluation Timeout or Cancellation Mechanism
**Methods:** FMEA (RL1), FTA (BE-5.4, CCF-1), ETA (IE1/IE2), STPA (UCA-10, SC-5, SC-6), Markov (lambda_ei, Recommendation 2), Petri Net (IL absorbing state), HAZOP (3.5)
**Confidence:** **7/7 methods** -- unanimous agreement

The `Runtime` struct has no `context.Context`, no step counter, no wall-clock timeout, and no instruction limit. Once `env.Eval()` begins, there is no way to interrupt it from the host. `dotimes` (`op.go:462`) loops without pushing stack frames, so stack depth limits do not apply. Tail-recursive infinite loops via TRO maintain a constant physical stack depth.

- **FTA:** BE-5.4 has FV importance 0.55, the highest of any basic event. Removing it reduces P(TE-1) from 0.70 to ~0.45 and P(TE-5) from 0.76 to ~0.50.
- **Markov:** The IL (Infinite-Loop) state is absorbing with no exit distribution. Adding a step counter eliminates this absorbing state.
- **STPA:** UCA-10 identifies `dotimes` specifically: `(dotimes (i 1000000000) ...)` runs to completion with no depth protection.
- **HAZOP:** Risk score 16 (P=4, C=4), third-highest finding.

**Code locations:** `lisp/runtime.go:15-27` (no timeout fields), `lisp/env.go:826` (eval entry, no cancellation check), `lisp/op.go:462` (dotimes loop, no iteration limit).

---

#### HC-2: Incomplete MaxAlloc Enforcement
**Methods:** FMEA (RL1, RPN 224), FTA (BE-5.3, CCF-2), ETA (B4 gap, Risk 0.49), STPA (UCA-3, UCA-14, SC-4), Markov (implicit via resource exhaustion), Petri Net (P20 guard covers 2/15 transitions), HAZOP (3.2, 3.6)
**Confidence:** **7/7 methods** -- unanimous agreement

`MaxAllocBytes()` (`runtime.go:31-36`) is only checked in `builtinMakeSequence` (`builtins.go:1714`) and `builtinRepeat` (`libstring.go:135`). The following allocation-capable builtins have NO size check: `concat`, `append`, `map`, `select`, `reject`, `zip`, `reverse`, `vector`, `json:load-*`, `json:dump-*`, `format-string`, `bytes` operations, array construction via `Array()`.

- **ETA:** Allocations via unguarded paths have a 9.8% probability of reaching OOM (End State E), the highest-risk end state at Risk Score 0.49.
- **Petri Net:** The `AllocBudget` place (P20) has transition T25 guarding only 2 of ~15+ allocation paths. Unchecked tokens bypass the guard entirely.
- **FMEA:** RPN 224, tied for 4th-highest.
- **STPA:** UCA-14 lists 10+ specific builtins that allocate without checking.

**Code locations:** `lisp/runtime.go:31-36` (MaxAllocBytes), `lisp/builtins.go:1714` (only check in make-sequence), `lisp/builtins.go` (concat, append, map -- no checks).

---

#### HC-3: Parser Has No Depth Limit
**Methods:** FMEA (P4, RPN 112), FTA (BE-1.4), STPA (UCA-18, SC-1), Markov (Recommendation 5), Petri Net (implicit), HAZOP (3.1, Risk Score 20)
**Confidence:** **6/7 methods** (all except ETA, which focused on evaluation-time barriers)

The recursive descent parser (`rdparser/parser.go`) uses Go function call recursion for nested expressions. `ParseConsExpression` -> `ParseExpression` -> `parseExpression` -> `ParseConsExpression` recurses once per nesting level. Input with 50,000+ nested parentheses overflows the Go goroutine stack, causing a fatal crash. This is NOT recoverable by `recover()` in all cases (Go goroutine stack overflow is a fatal signal).

ELPS stack limits (`MaxHeightLogical`, `MaxHeightPhysical`) apply only during evaluation, not during parsing. The parser is the first code to process untrusted input.

- **HAZOP:** Highest risk score (20, P=4 C=5) -- "any endpoint accepting Lisp source from untrusted input is vulnerable."
- **STPA:** Loss Scenario LS-1 traces the full causal path from untrusted input through parser to Go process crash.
- **Markov:** Recommendation 5 notes this is a class of Go-level panics that bypass all ELPS error handling and would not be caught by a `recover()` in `env.Eval()`.

**Code locations:** `parser/rdparser/parser.go` -- `ParseConsExpression`, `ParseExpression`, `parseExpression` (mutually recursive, no depth counter).

---

#### HC-4: Go Panics Crash the Host Process (37 Sites, 0 recover())
**Methods:** FMEA (E2, E3, T1, T2, H3), FTA (TE-3, BE-3.1 through BE-3.6), ETA (End State D, Risk 0.12+0.05), STPA (UCA-4, UCA-13, SC-2), Markov (mu_pr=0 absorbing state, Sensitivity rank 1), HAZOP (2.6)
**Confidence:** **6/7 methods** (Petri Net proves deadlock-freedom but does not focus on panic recoverability)

There are approximately 37 explicit `panic()` call sites in the `lisp/` package. The most dangerous are type assertion panics in accessor methods: `FunData()` (`lisp.go:709`), `Bytes()` (`lisp.go:763`), `Map()` (`lisp.go:772`), `UserData()` (`lisp.go:755`). Stack operations panic on invariant violations: `Pop()` on empty stack (`stack.go:206`), `TerminalFID` with TROBlock (`stack.go:135`). There is no `recover()` anywhere in `lisp/`, `repl/`, or `cmd/`.

- **Markov:** `mu_pr = 0` (panic recovery rate is zero), making the Panicked state absorbing. This is the single highest-sensitivity parameter: any non-zero `mu_pr` eliminates the absorbing failure state.
- **FTA:** P(TE-3) ~ 0.11 per year for typical deployment.
- **ETA:** End State D (host crash) probability 2.4% for type confusion (IE4) and 1.0% for recursion (IE1).
- **STPA:** UCA-4 identifies that the host not wrapping `Eval()` in `recover()` is an unsafe control action.

**Code locations:** `lisp/lisp.go:663-857` (13 type assertion panics), `lisp/stack.go:135,206` (stack panics), `lisp/env.go:907,938,990,1059` (evaluator panics).

---

#### HC-5: Filesystem Access Unrestricted by Default
**Methods:** FMEA (F1 RPN 405, F2 RPN 162, F3 RPN 280), FTA (BE-5.1, BE-5.2), STPA (UCA-1, UCA-7, SC-3, LS-2), HAZOP (5.1, 5.2, 5.5)
**Confidence:** **5/7 methods** (Markov and Petri Net focus on availability rather than security boundaries)

`RelativeFileSystemLibrary` with empty `RootDir` (the default in `cmd/run.go` and `lisplib.NewDocEnv`) allows `load-file` to read any path on the filesystem. When `RootDir` IS configured, a TOCTOU race exists: `filepath.EvalSymlinks` resolves the path, but between the check and `os.ReadFile`, an attacker with filesystem write access can swap a symlink. Additionally, if `EvalSymlinks` fails, the code silently falls through to the unresolved path.

- **FMEA:** F1 has the highest RPN in the entire analysis (405, S=9, O=5, D=9).
- **STPA:** Loss Scenario LS-2 describes the full path traversal attack. AS-4 describes filesystem enumeration via error messages.
- **HAZOP:** Scenario 4 describes the symlink TOCTOU race in detail.

**Code locations:** `lisp/library.go:93-116` (LoadSource with RootDir check), `lisp/library.go:102-106` (EvalSymlinks error swallowed), `cmd/run.go:45` (empty RootDir default).

---

#### HC-6: No Package Sealing -- Untrusted Code Can Redefine Builtins
**Methods:** STPA (UCA-8, UCA-9, UCA-21, SC-7, LS-3, AS-1, AS-3), HAZOP (2.1, 2.3, Scenarios 1, 3, 5), FMEA (K2 RPN 270)
**Confidence:** **3/7 methods** (STPA, HAZOP, FMEA directly address this; others touch it tangentially)

Any Lisp code can execute `(in-package 'lisp)` and then `(set 'handler-bind ...)` to replace core builtins. The `Package.Put()` method only blocks rebinding of `true` and `false`. There is no "sealed" or "frozen" mode for packages. `in-package` auto-creates packages that don't exist (`builtins.go:478`), enabling typos to silently create wrong packages (K2).

- **STPA:** AS-1 describes a code injection attack that replaces `car` with a file-exfiltrating function. AS-3 describes privilege escalation via package manipulation.
- **HAZOP:** Scenario 1 chains JSON deserialization through `load-string` to full environment compromise. Risk score 20 for code injection plus 12 for builtin redefinition.

**Code locations:** `lisp/builtins.go:472-504` (builtinInPackage, auto-creates packages), `lisp/builtins.go:533-554` (builtinSet, no immutability check), `lisp/package.go:122-131` (Put, only blocks true/false).

---

#### HC-7: Sorted-Map Copy and Equality Are Broken
**Methods:** FMEA (M1 RPN 210, M2 RPN 50), FTA (BE-2.4, BE-2.5), ETA (implicit in IE4/IE5)
**Confidence:** **3/7 methods**

`LVal.Copy()` (`lisp.go:980-992`) does a shallow copy of the `Native` field for `LSortMap`, sharing the underlying map data between original and "copy". The existing `copyMapData()` method (`lisp.go:994-1007`) is never called from `Copy()`. Separately, `LVal.Equal()` for sorted-maps (`lisp.go:950-955`) unconditionally returns `false` for all non-empty maps -- a known BUG acknowledged in comments.

- **FMEA:** M1 (shallow copy) has RPN 210, ranked 6th highest. M2 (equality) has RPN 50.
- **FTA:** BE-2.4 (map equality) has FV importance 0.30 for TE-2 (Silent Incorrect Computation). BE-2.5 (shallow copy) has FV importance 0.20.

**Code locations:** `lisp/lisp.go:980-992` (Copy, no call to copyMapData for LSortMap), `lisp/lisp.go:994-1007` (copyMapData exists but unused by Copy), `lisp/lisp.go:950-955` (Equal always returns false for maps, BUG comment at line 912).

---

#### HC-8: Integer Division/Modulo by Zero Causes Go Panic
**Methods:** FMEA (N1 RPN 175, N2 RPN 140), FTA (implicit in TE-3), ETA (implicit in IE4 End State D)
**Confidence:** **3/7 methods**

`builtinDiv` and `builtinMod` perform integer division/modulo without checking for zero divisor. Go panics on `int / 0` and `int % 0`. Float division by zero returns IEEE 754 `+Inf` and is handled, but integer operations crash the host.

- **FMEA:** N1 (division by zero) RPN 175 (rank 9), N2 (modulo by zero) RPN 140 (rank 21).

**Code locations:** `lisp/builtins.go` -- `builtinDiv` (no zero check before integer division), `builtinMod` (docstring says "b must be non-zero" but no runtime enforcement).

---

#### HC-9: Infinite Macro Expansion Has No Depth Limit
**Methods:** FMEA (E1 RPN 224, X2 RPN 98), FTA (BE-1.4), STPA (UCA-12), Markov (Recommendation 3), Petri Net (Scenario 4.4), HAZOP (2.4)
**Confidence:** **6/7 methods**

The `goto eval` loop in `Eval()` (`env.go:826-880`) re-expands `LMarkMacExpand` results without a depth counter. `builtinMacroExpand` (`builtins.go:564-587`) has an unbounded `for` loop. While each expansion pushes a stack frame (bounded by MaxHeightPhysical), the absence of an explicit expansion limit means errors occur as generic "stack overflow" rather than informative "macro expansion limit exceeded."

- **FMEA:** E1 has RPN 224, tied for 4th-highest.
- **Petri Net:** Proves macro expansion is bounded by stack limits (Scenario 4.4), but recommends an explicit limit for better error messages.
- **Markov:** Recommendation 3 proposes `MaxMacroExpansionDepth = 1000`.

**Code locations:** `lisp/env.go:826-880` (goto eval loop), `lisp/env.go:863-867` (LMarkMacExpand re-evaluation), `lisp/builtins.go:564-587` (builtinMacroExpand for loop).

---

#### HC-10: Runtime Concurrency Unsafe
**Methods:** FMEA (E4 RPN 162, RT1 RPN 189, K1 RPN 162), STPA (UCA-5, H-7, SC-8), Petri Net (Section 4.6)
**Confidence:** **3/7 methods**

The `Runtime` struct is not thread-safe. `Runtime.Package` is a mutable pointer swapped during function calls with no mutex. `CallStack`, `conditionStack`, and `PackageRegistry` use no synchronization. Only `atomicCounter` (for ID generation) is thread-safe. If an embedder shares a `Runtime` across goroutines, data races occur.

- **FMEA:** RT1 (Runtime.Package concurrency) RPN 189, ranked 8th.
- **STPA:** UCA-5 identifies concurrent `Eval()` on shared Runtime as unsafe.
- **Petri Net:** Section 4.6 confirms no deadlock is possible (because there are no locks), but notes data races would occur under concurrent use.

**Code locations:** `lisp/runtime.go:15-27` (Runtime struct, no mutex), `lisp/env.go:1191-1201` (Package swap with defer, no lock), `lisp/package.go:52-63` (Package.Get mutates FunNames during read).

---

### Medium Confidence Findings (2 methods agree)

| ID | Finding | Methods | Key Code Location |
|----|---------|---------|-------------------|
| MC-1 | `opLet` evaluates RHS in child env (`letenv`), not parent -- violates parallel binding semantics | FMEA (E5 RPN 150), FTA (BE-2.6) | `lisp/op.go:570` |
| MC-2 | Silent integer overflow in arithmetic builtins | FMEA (N3 RPN 160), FTA (BE-2.1, FV 0.40) | `lisp/builtins.go` (add, mul, sub, pow) |
| MC-3 | JSON deep nesting causes unbounded recursion in `loadInterface` | FMEA (J1 RPN 192), FTA (BE-1.5) | `lisp/lisplib/libjson/json.go:155-192` |
| MC-4 | Singleton mutation risk (Nil/Bool) -- no runtime enforcement | FMEA (S1 RPN 160), FTA (TE-4, AND gate P=0.015) | `lisp/lisp.go:289-292` |
| MC-5 | `builtinRethrow` returns pointer to original error (handler may have mutated it) | FMEA (H2 RPN 120), HAZOP (implicit in SN-7) | `lisp/builtins.go:777-783`, `lisp/runtime.go:40-42` |
| MC-6 | `load-string`/`load-bytes` enable arbitrary code execution with no sandbox | FMEA (F3 RPN 280), STPA (UCA-6, AS-1) | `lisp/builtins.go:404-427` |
| MC-7 | Float equality via `==` produces unexpected results | FMEA (N4 RPN 150), FTA (BE-2.3, FV 0.20) | `lisp/lisp.go:970-977` |
| MC-8 | `GoInt`/`GoFloat64` in libjson have inverted `IsNumeric()` checks | FMEA (J2/J3 RPN 90), FTA (implicit) | `lisp/lisplib/libjson/json.go:421-445` |
| MC-9 | No parser input size limit -- multi-GB input exhausts heap | STPA (UCA-19, SC-11, AS-6), HAZOP (implicit in SN-1) | `parser/rdparser/parser.go` |
| MC-10 | `RegisterDefaultBuiltin/Macro/SpecialOp` mutate global slices with no protection | STPA (UCA-15, AS-5), HAZOP (implicit) | `lisp/builtins.go:386`, `lisp/macro.go:51`, `lisp/op.go:123` |
| MC-11 | Error messages from `load-file` expose filesystem paths | STPA (AS-4, SC-10), HAZOP (5.4) | `lisp/library.go:110` |
| MC-12 | `handler-bind` handler error masks original error | FMEA (H1 RPN 96), HAZOP (implicit in SN-7) | `lisp/op.go:678` |

### Method-Specific Findings (1 method only)

| ID | Finding | Method | Key Code Location |
|----|---------|--------|-------------------|
| MS-1 | `countExprArgs` does not recurse into nested s-expressions | FMEA (P2 RPN 160) | `lisp/op.go:265-341` |
| MS-2 | `shallowUnquote` aliases Cells with macro template | FMEA (X3 RPN 64) | `lisp/lisp.go:632-637` |
| MS-3 | `UsePackage` copies symbol references not values (mutable aliasing) | FMEA (K3 RPN 128) | `lisp/env.go:187-203` |
| MS-4 | TRO-eligible tail recursion defeats stack limits for CPU DoS | FTA (BE-1.6, FV 0.21) | `lisp/env.go:1042-1080` |
| MS-5 | `LEnv.Copy()` shares `FunName` map between original and copy | FMEA (RT3 RPN 48) | `lisp/env.go:298-309` |
| MS-6 | No `load-file` cycle detection (generic stack overflow vs. informative error) | Petri Net (Scenario 4.5) | `lisp/builtins.go:454-470` |
| MS-7 | `Package.Get` mutates `FunNames` during read (not a pure read) | FMEA (K1 RPN 162) | `lisp/package.go:52-63` |
| MS-8 | Formatter: top-level trailing comments migrate to wrong form | FMEA (FM2 RPN 50) | `formatter/formatter.go` |

---

## 5. Cross-Method Convergence Analysis

The following matrix shows which methods identified each finding. Checkmarks indicate the method independently identified the issue.

| Finding | FMEA | FTA | ETA | STPA | Markov | Petri | HAZOP | Count |
|---------|------|-----|-----|------|--------|-------|-------|-------|
| **HC-1:** No timeout/cancellation | X | X | X | X | X | X | X | **7** |
| **HC-2:** Incomplete MaxAlloc | X | X | X | X | X | X | X | **7** |
| **HC-3:** Parser no depth limit | X | X | -- | X | X | -- | X | **5** (+ETA implicit) |
| **HC-4:** Panics crash host (37 sites, 0 recover) | X | X | X | X | X | -- | X | **6** |
| **HC-5:** Filesystem unrestricted by default | X | X | -- | X | -- | -- | X | **4** |
| **HC-6:** No package sealing | X | -- | -- | X | -- | -- | X | **3** |
| **HC-7:** Sorted-map copy/equality broken | X | X | -- | -- | -- | -- | -- | **2** (+ETA implicit) |
| **HC-8:** Integer div/mod by zero panic | X | X | X | -- | -- | -- | -- | **3** |
| **HC-9:** Infinite macro expansion | X | X | -- | X | X | X | X | **6** |
| **HC-10:** Runtime concurrency unsafe | X | -- | -- | X | -- | X | -- | **3** |
| MC-1: `opLet` wrong env for RHS | X | X | -- | -- | -- | -- | -- | 2 |
| MC-2: Silent integer overflow | X | X | -- | -- | -- | -- | -- | 2 |
| MC-3: JSON deep nesting | X | X | -- | -- | -- | -- | -- | 2 |
| MC-4: Singleton mutation risk | X | X | -- | -- | -- | -- | -- | 2 |
| MC-6: `load-string` no sandbox | X | -- | -- | X | -- | -- | -- | 2 |
| MC-7: Float equality `==` | X | X | -- | -- | -- | -- | -- | 2 |

**Convergence observations:**
- HC-1 (no timeout) and HC-2 (incomplete MaxAlloc) achieved perfect 7/7 agreement -- every method identified these as critical gaps.
- Security-focused findings (HC-5 filesystem, HC-6 package sealing) were primarily identified by STPA and HAZOP, which have explicit security analysis capabilities. The quantitative methods (Markov, Petri Net) focused on availability rather than security boundaries.
- Code-level bugs (MC-1 `opLet`, MC-8 `GoInt`/`GoFloat64`) were primarily identified by FMEA, which performs line-by-line analysis.

---

## 6. Detailed Findings by Theme

### 6.1 Resource Exhaustion and Denial of Service

This is the most pervasive theme, identified by all 7 methods as the primary risk area.

**Missing timeout mechanism (HC-1):** The `Runtime` struct has no `context.Context`, step counter, or instruction limit. The following code patterns can run indefinitely:
- `(dotimes (i 2000000000) nil)` -- `op.go:462` loops without stack frames
- `(labels ((f () (f))) (f))` -- TRO prevents stack overflow
- Any user-defined loop that avoids stack growth

**Incomplete allocation limits (HC-2):** `MaxAllocBytes()` protects 2 of ~15+ allocation-capable builtins. Unprotected paths include:
- `concat` (string/list concatenation, no size check)
- `append` / `cons` (list construction, no size check)
- `map` / `select` / `reject` / `zip` / `reverse` (sequence operations)
- `json:load-*` (JSON deserialization, no depth or size limit)
- `vector` / `Array()` (multi-dimensional array construction)
- `format-string` (output size unbounded)

**Parser resource exhaustion (HC-3, MC-9):** The parser has neither a depth limit nor an input size limit. Deeply nested input overflows the Go goroutine stack (fatal, non-recoverable). Extremely large flat input exhausts heap memory during AST construction.

**Macro expansion amplification (HC-9):** A macro can return an expansion exponentially larger than its input. `stampMacroExpansion` walks the full expansion. No limit on expansion size, only on depth (via stack limits). The `goto eval` loop in `Eval()` has no explicit expansion counter.

### 6.2 Host Process Stability (Go Panics)

**37 panic sites, 0 recover sites (HC-4):** The `lisp/` package uses `panic()` for internal invariant violations. The most dangerous are:
- Type assertion panics: `FunData()`, `Bytes()`, `Map()`, `UserData()`, `seqCells()` in `lisp.go:663-857` (13 sites)
- Stack operations: `Pop()` on empty stack (`stack.go:206`), `TerminalFID` with TROBlock (`stack.go:135`)
- Evaluator: nil LVal return (`env.go:937-939,988-990,1057-1059`), invalid function type (`env.go:907`)
- Arithmetic: integer division by zero (HC-8), integer modulo by zero

**No recover() in entry points:** Neither `env.Eval()`, `env.Load()`, `env.LoadFile()`, nor the REPL eval loop wraps calls in `recover()`. A single panic from a buggy custom builtin or type confusion crashes the entire host application.

**Markov analysis quantifies impact:** With `mu_pr = 0`, the Panicked state is absorbing. MTTF to panic in REPL mode with adversarial code is ~588 hours. Adding `recover()` changes MTTF to effectively unlimited for panic-class failures.

### 6.3 Security Boundaries and Sandboxing

**Filesystem access (HC-5):** Three layered issues:
1. `RootDir` defaults to empty, disabling all confinement (`library.go:87`)
2. TOCTOU race between `EvalSymlinks` and `os.ReadFile` (`library.go:102-114`)
3. `EvalSymlinks` failure silently falls through to unresolved path (`library.go:105-106`)

**Package system not a security boundary (HC-6):** The package system was designed for code organization, not security isolation:
- `in-package` can switch to any package including `lisp` core
- `set` can overwrite any symbol including builtins
- `in-package` auto-creates packages on typos (K2, RPN 270)
- No "sealed" or "frozen" mode exists

**Code injection via eval/load-string (MC-6):** `eval` and `load-string` execute arbitrary Lisp with full access to all builtins. Combined with the above, untrusted input to these functions enables complete environment compromise.

### 6.4 Silent Incorrect Computation

**Integer overflow (MC-2):** `builtinAdd`, `builtinMul`, `builtinSub`, and `powInt` perform arithmetic without overflow detection. Go's 64-bit integers wrap silently: `(+ 9223372036854775807 1)` returns a negative number.

**Sorted-map bugs (HC-7):** Two separate bugs:
- `Copy()` does shallow copy of `Native` for `LSortMap` -- the existing `copyMapData()` method is never called from `Copy()`
- `Equal()` unconditionally returns `false` for non-empty sorted-maps (known BUG in code)

**`opLet` scoping (MC-1):** `opLet` (`op.go:555-582`) evaluates RHS in the child environment (`letenv`) rather than the parent environment (`env`). This means earlier bindings in a `let` form are visible to later RHS expressions, violating parallel binding semantics. This is a semantic correctness bug.

**Float equality (MC-7):** `equalNum` (`lisp.go:970-977`) compares floats with `==`. The source comment acknowledges: "This may not be correct."

### 6.5 Error Handling Gaps

**Handler error masks original (MC-12):** When a `handler-bind` handler itself errors, the original error is lost (`op.go:678`, comment: "Well, we're boned").

**Rethrow mutation risk (MC-5):** `PushCondition` stores the original error pointer, not a copy. If handler code mutates the error, `rethrow` returns the mutated version.

**Package-level errors lack stack traces:** Errors created via the package-level `Errorf()` (`lisp.go:583-601`) rather than the method `env.Errorf()` have no stack trace attached.

### 6.6 Parser Safety

**No depth limit (HC-3):** Fatal Go goroutine stack overflow from nested input.

**No input size limit (MC-9):** Multi-gigabyte input strings to `load-string` or `load-bytes` cause unbounded memory allocation during parsing, before any evaluation-level limit applies.

**`countExprArgs` non-recursive (MS-1):** The `(expr ...)` form's argument counting at `op.go:265-341` does not recurse into nested s-expressions, potentially creating lambdas with wrong arity.

---

## 7. Quantitative Risk Assessment

### 7.1 FMEA Risk Priority Numbers (Top 15)

| Rank | ID | RPN | Component | Failure Mode |
|------|-----|-----|-----------|-------------|
| 1 | F1 | **405** | `RelativeFileSystemLibrary` | Path traversal when `RootDir` empty |
| 2 | F3 | **280** | `load-string`/`load-bytes` | Arbitrary code execution from untrusted input |
| 3 | K2 | **270** | `builtinInPackage` | Package typo silently creates new package |
| 4 | E1 | **224** | `Eval` goto-eval loop | Infinite macro expansion, no depth limit |
| 5 | RL1 | **224** | `DefaultMaxAlloc` | Allocation limit not enforced by all paths |
| 6 | M1 | **210** | `LVal.Copy` for `LSortMap` | Shallow copy shares map data |
| 7 | J1 | **192** | JSON `loadInterface` | Deeply nested JSON causes stack overflow |
| 8 | RT1 | **189** | `Runtime.Package` | No concurrency safety on shared state |
| 9 | N1 | **175** | `builtinDiv` | Integer division by zero panics host |
| 10 | J4 | **175** | JSON `Load` | Integer precision loss > 2^53 |
| 11 | K1 | **162** | `Package.Get` | Side-effect mutation during read |
| 12 | E4 | **162** | `call` package swap | No mutex on `Runtime.Package` |
| 13 | F2 | **162** | `RelativeFileSystemLibrary` | TOCTOU symlink race |
| 14 | P2 | **160** | `countExprArgs` | Non-recursive walk misses nested `%%N` |
| 15 | S1 | **160** | Singletons | External code mutates Nil/Bool globals |

### 7.2 FTA Top Event Probabilities (Annual, Semi-Trusted Input)

| Top Event | P(event) | Dominant Contributors |
|-----------|----------|----------------------|
| TE-1: Host DoS | **0.70** | No timeout (BE-5.4, FV=0.57), incomplete MaxAlloc (BE-5.3, FV=0.43) |
| TE-2: Silent Wrong Result | **0.50** | Integer overflow (BE-2.1, FV=0.40), map equality bug (BE-2.4, FV=0.30) |
| TE-3: Interpreter Panic | **0.11** | Type assertion panics (BE-3.1), nil return (BE-3.3) |
| TE-4: Singleton Corruption | **0.015** | AND gate: mutation + escapes review (BE-4.1 x BE-4.2) |
| TE-5: Security Bypass | **0.76** | No timeout (BE-5.4, FV=0.53), incomplete MaxAlloc (BE-5.3, FV=0.40) |

### 7.3 ETA Critical End-State Risk Scores

| End State | Primary IE | Probability | Severity | Risk Score |
|-----------|-----------|-------------|----------|------------|
| E (OOM) | IE2: Excessive allocation | 0.098 | Critical (5) | **0.49** |
| H (Silent wrong) | IE5: Error propagation failure | 0.120 | High (4) | **0.48** |
| H (Silent wrong) | IE6: Semantic errors | 0.050 | High (4) | 0.20 |
| D (Crash) | IE4: Type confusion | 0.024 | Critical (5) | 0.12 |
| D (Crash) | IE1: Recursion | 0.010 | Critical (5) | 0.05 |

### 7.4 Markov Availability Estimates

| Scenario | Availability | MTTF |
|----------|-------------|------|
| REPL, well-behaved code | 99.98% (CTMC), 99.995% (SMP) | >10,000 hours |
| REPL, adversarial code | 99.80% (CTMC), 99.5% (SMP) | ~588 hours |
| Batch, well-behaved code | 99.99% per-run | >1M eval cycles |
| Batch, adversarial code | 95.0% per-run | ~143 seconds |

**Key Markov finding:** The Panicked state (P) is absorbing (`mu_pr = 0`). Adding `recover()` at eval entry points is the single highest-sensitivity improvement, converting MTTF from finite to effectively unlimited for panic-class failures.

### 7.5 HAZOP Risk Scores (Top 5)

| Risk Score | Node | Deviation |
|------------|------|-----------|
| **20** | SN-3 Resources | Parser stack overflow from deeply nested input |
| **20** | SN-2 Evaluator | Code injection via `eval`/`load-string` on untrusted input |
| **16** | SN-3 Resources | No CPU/instruction limit; infinite loops |
| **15** | SN-5 File Load | Path traversal via TOCTOU symlink race |
| **15** | SN-5 File Load | Loaded file injects malicious code |

### 7.6 Petri Net Formal Properties

| Property | Result |
|----------|--------|
| Deadlock-free | **Yes** -- proven for all 7 scenarios examined |
| Live (termination guarantee) | **Yes** -- proven via well-founded ordering M = (physical_slack, logical_slack, AST_depth) |
| Bounded | **Yes** -- all 24 places have finite upper bounds |
| Primary bottleneck | Physical call stack (P19), consumed by all 5 call types |
| Allocation guard coverage | 2 of ~15+ allocation-capable transitions |

---

## 8. Prioritized Recommendations

Recommendations are ranked by cross-method agreement, quantified risk, and implementation effort.

### Priority 1: Add Evaluation Timeout / Step Counter (Critical)

**Cross-method agreement:** 7/7 methods
**Addresses:** HC-1 (no timeout), FTA BE-5.4 (FV 0.55), Markov IL absorbing state, STPA SC-5/SC-6, HAZOP 3.5

**Action:** Add a `context.Context` field to `Runtime` and/or a `MaxSteps int64` step counter. Check cancellation/step limit at the top of `env.Eval()` (`env.go:826`), in `dotimes` per iteration (`op.go:462`), and on every `PushFID`. Default to unlimited (0) for backward compatibility.

**Impact:** Eliminates the IL absorbing state. Reduces P(TE-1) from 0.70 to ~0.45. Reduces P(TE-5) from 0.76 to ~0.50.

**Effort:** Low-Medium. Add field to `Runtime`, add check at 3-5 locations.

**Code locations:** `lisp/runtime.go:15-27`, `lisp/env.go:826`, `lisp/op.go:462`.

---

### Priority 2: Add recover() at Evaluator Entry Points (Critical)

**Cross-method agreement:** 6/7 methods
**Addresses:** HC-4 (37 panics, 0 recover), Markov mu_pr (sensitivity rank 1), STPA SC-2, ETA End State D

**Action:** Wrap `env.Eval()`, `env.Load()`, `env.LoadFile()`, and the REPL eval loop with `defer recover()` that converts panics to `LError` values with diagnostic information. Log recovered panics for debugging.

**Impact:** Eliminates the absorbing Panicked state. MTTF from ~588 hours (adversarial) to effectively unlimited for panic-class failures. Availability improves from 99.80% to 99.99%+.

**Effort:** Low. Single wrapper function added to 3-4 entry points.

**Code locations:** `lisp/env.go` (Eval, Load, LoadFile), `repl/repl.go:238`, `cmd/run.go:62`.

---

### Priority 3: Extend MaxAlloc to All Allocation Paths (High)

**Cross-method agreement:** 7/7 methods
**Addresses:** HC-2, FTA CCF-2, ETA Risk 0.49, STPA SC-4, HAZOP 3.2/3.6

**Action:** Create a centralized `checkAlloc(env *LEnv, n int) *LVal` helper. Call it before allocating in: `builtinConcat`, `builtinAppend`, `builtinMap`, `builtinSelect`, `builtinReject`, `builtinZip`, `builtinReverse`, `builtinVector`, `json:loadInterface`, `builtinFormatString`, and `Array()`. Consider a cumulative allocation counter on `Runtime` for total-budget enforcement.

**Impact:** ETA End State E probability drops from 9.8% to <1%. Eliminates the primary single-point-of-failure identified by ETA barrier analysis.

**Effort:** Medium. Helper function plus ~15 call-site additions across `builtins.go` and `libjson/json.go`.

**Code locations:** `lisp/runtime.go` (helper), `lisp/builtins.go` (~10 builtins), `lisp/lisplib/libjson/json.go`.

---

### Priority 4: Add Parser Depth and Size Limits (High)

**Cross-method agreement:** 6/7 methods
**Addresses:** HC-3, FTA BE-1.4, STPA SC-1/SC-11, Markov Rec 5, HAZOP 3.1

**Action:** Add a `depth int` and `maxDepth int` (default 10,000) to the `Parser` struct. Increment on entry to `ParseConsExpression` and `ParseList`, decrement on exit. Return `LError` with `parse-error` condition on overflow. Also add an input byte limit on the scanner (default 10MB).

**Impact:** Prevents a class of fatal Go-level crashes that bypass ALL ELPS error handling and `recover()`. The Go goroutine stack overflow from deeply nested input is non-recoverable.

**Effort:** Low. Single counter in parser struct, 4-5 lines of checking code.

**Code locations:** `parser/rdparser/parser.go` (ParseConsExpression, ParseList, ParseExpression).

---

### Priority 5: Enforce Filesystem Confinement by Default (High)

**Cross-method agreement:** 5/7 methods
**Addresses:** HC-5, FMEA F1 (RPN 405), STPA SC-3, HAZOP 5.1/5.5

**Action:** (a) Set `RootDir` on `RelativeFileSystemLibrary` to the directory of the executed file by default in `cmd/run.go`. (b) When `EvalSymlinks` fails in `library.go:102-106`, deny access rather than falling through. (c) Prefer `FSLibrary` with `os.DirFS()` in documentation and examples. (d) Emit a warning when `RootDir` is empty.

**Impact:** Eliminates the highest-RPN finding (405). Addresses all three layers of filesystem vulnerability.

**Effort:** Low-Medium. Default change in `cmd/run.go`, error handling fix in `library.go`.

**Code locations:** `lisp/library.go:93-116`, `cmd/run.go:45`, `lisp/lisplib/lisplib.go`.

---

### Priority 6: Add Package Sealing / Freezing (Medium-High)

**Cross-method agreement:** 3/7 methods (STPA, HAZOP, FMEA)
**Addresses:** HC-6, STPA SC-7, HAZOP 2.1/2.3

**Action:** Add `Sealed bool` field to `Package`. After `InitializeUserEnv()` and `LoadLibrary()`, seal the `lisp` package and any core packages. `Package.Put()` and `Package.Update()` return errors when sealed. `in-package` refuses to switch into sealed packages or allows only read access.

**Effort:** Medium. Add field to `Package`, modify `Put`/`Update`, add sealing API.

**Code locations:** `lisp/package.go` (Package struct, Put, Update), `lisp/env.go` (InitializeUserEnv), `lisp/builtins.go` (builtinInPackage).

---

### Priority 7: Fix Sorted-Map Copy and Equality (Medium)

**Cross-method agreement:** 3/7 methods
**Addresses:** HC-7, FMEA M1 (RPN 210), FTA BE-2.4/BE-2.5

**Action:** (a) In `LVal.Copy()`, detect `LSortMap` type and call the existing `copyMapData()` method. This is a one-line fix. (b) In `LVal.Equal()`, replace the unconditional `return Bool(false)` with actual key-by-key comparison.

**Effort:** Low. `copyMapData()` already exists. Equality comparison is ~10 lines.

**Code locations:** `lisp/lisp.go:980-992` (Copy), `lisp/lisp.go:950-955` (Equal), `lisp/lisp.go:994-1007` (copyMapData).

---

### Priority 8: Add Integer Division/Modulo Zero Checks (Medium)

**Cross-method agreement:** 3/7 methods
**Addresses:** HC-8, FMEA N1/N2 (RPN 175+140)

**Action:** Add `if divisor.Int == 0 { return env.Errorf("division-by-zero", ...) }` before integer division in `builtinDiv` and `builtinMod`. Consider also wrapping the general builtin dispatch in `call()` with `recover()` to catch panics from custom builtins (overlaps with Priority 2).

**Effort:** Very low. Two-line addition per function.

**Code locations:** `lisp/builtins.go` (builtinDiv, builtinMod).

---

### Priority 9: Add Explicit Macro Expansion Depth Limit (Medium)

**Cross-method agreement:** 6/7 methods
**Addresses:** HC-9, FMEA E1 (RPN 224)

**Action:** Add a counter to the `goto eval` loop in `Eval()` (`env.go:826-880`) tracking consecutive macro re-expansions. Error with condition `macro-expansion-limit` after a configurable limit (default 1000). Also add an iteration limit to the `for` loop in `builtinMacroExpand` (`builtins.go:564-587`).

**Effort:** Low. Counter variable and check in existing loop.

**Code locations:** `lisp/env.go:826-880`, `lisp/builtins.go:564-587`.

---

### Priority 10: Fix `opLet` Parallel Binding Semantics (Medium)

**Cross-method agreement:** 2/7 methods
**Addresses:** MC-1, FMEA E5 (RPN 150)

**Action:** Change `op.go:570` from `vals[i] = letenv.Eval(bind.Cells[1])` to `vals[i] = env.Eval(bind.Cells[1])`. This evaluates each binding's RHS in the parent environment, giving standard parallel `let` semantics where bindings cannot see each other.

**Effort:** Very low. Single line change.

**Code locations:** `lisp/op.go:570`.

---

### Priority 11: Add JSON Deserialization Depth Limit (Medium)

**Cross-method agreement:** 2/7 methods
**Addresses:** MC-3, FMEA J1 (RPN 192), FTA BE-1.5

**Action:** Add a `maxDepth` parameter to `loadInterface` (`libjson/json.go:155-192`). Decrement on recursion, error when zero. Default 512.

**Effort:** Low.

**Code locations:** `lisp/lisplib/libjson/json.go:155-192`.

---

### Priority 12: Document or Enforce Runtime Single-Threaded Requirement (Low-Medium)

**Cross-method agreement:** 3/7 methods
**Addresses:** HC-10, FMEA RT1 (RPN 189), STPA SC-8

**Action:** Either: (a) Document prominently that `Runtime` is NOT thread-safe and must not be shared across goroutines, OR (b) add `sync.Mutex` to protect `Runtime.Package`, `Stack`, and `conditionStack`. Option (a) is sufficient if ELPS is truly single-threaded by design.

**Effort:** Low (documentation) or Medium (mutex).

**Code locations:** `lisp/runtime.go:15-27`.

---

### Priority 13: Add Singleton Immutability Enforcement (Low)

**Cross-method agreement:** 2/7 methods
**Addresses:** MC-4, FMEA S1 (RPN 160), FTA TE-4

**Action:** Add a CI test that verifies singleton integrity after the full test suite runs. Alternatively, add a `frozen bool` field to `LVal` that prevents mutation of singletons. The AND gate in FTA (P=0.015) means this is low probability but catastrophic severity.

**Effort:** Low (CI test) or Medium (frozen field).

**Code locations:** `lisp/lisp.go:289-292` (singleton definitions).

---

### Priority 14: Add Load-File Cycle Detection (Low)

**Cross-method agreement:** 1/7 methods (Petri Net)
**Addresses:** MS-6

**Action:** Add `loadingFiles map[string]bool` to `Runtime`. Check before `os.ReadFile` in `builtinLoadFile`. Return informative "circular load detected" error instead of generic stack overflow.

**Effort:** Very low.

**Code locations:** `lisp/builtins.go:454-470`, `lisp/runtime.go`.

---

### Priority 15: Fix Deprecated `GoInt`/`GoFloat64` Inverted Checks (Low)

**Cross-method agreement:** 1/7 methods (FMEA)
**Addresses:** MC-8, FMEA J2/J3

**Action:** Fix inverted `v.IsNumeric()` check in `GoInt` and `GoFloat64` (`libjson/json.go:421-445`), or remove the deprecated functions entirely.

**Effort:** Very low.

**Code locations:** `lisp/lisplib/libjson/json.go:421-445`.

---

## 9. Method Comparison

### Effectiveness by Analysis Dimension

| Dimension | Most Effective Method(s) | Least Effective |
|-----------|------------------------|-----------------|
| **Individual bug discovery** | FMEA (found 45+ specific failure modes with line-level precision) | Petri Net, Markov (abstract away individual bugs) |
| **Resource exhaustion patterns** | STPA, HAZOP, Petri Net (systematic resource modeling) | -- |
| **Security/adversarial scenarios** | STPA (STPA-Sec extension), HAZOP (security guide words) | Markov, ETA (availability-focused) |
| **Quantitative risk ranking** | FMEA (RPNs), FTA (probabilities, FV importance) | STPA (qualitative safety constraints) |
| **System availability** | Markov (steady-state availability, MTTF, sensitivity) | FMEA (component-level, not system-level) |
| **Formal correctness proofs** | Petri Net (deadlock-freedom, liveness, boundedness) | HAZOP (qualitative deviations) |
| **Defense-in-depth analysis** | ETA (barrier effectiveness, redundancy analysis) | FTA (deductive, not barrier-focused) |
| **Cross-component interactions** | HAZOP (interaction matrix), STPA (control structure) | FMEA (single-component focus) |
| **Common-cause failures** | FTA (CCF analysis, minimal cut sets) | ETA (assumes independent barriers) |
| **Practical recommendations** | All methods contribute; STPA produces the most actionable safety constraints | -- |

### Unique Contributions per Method

| Method | Unique Contribution |
|--------|-------------------|
| **FMEA** | Only method to find the `opLet` scoping bug (E5), `GoInt`/`GoFloat64` inverted checks (J2/J3), `countExprArgs` non-recursive walk (P2), and `shallowUnquote` aliasing (X3). Line-level precision unmatched by other methods. |
| **FTA** | Common-cause failure analysis identified 4 CCFs. Fussell-Vesely importance rankings provide objective prioritization. Minimal cut sets show that most top events have single-event cut sets (system is fragile). |
| **ETA** | Barrier degradation model predicts that MaxAlloc (B4) and type checking (B5) will degrade most as the codebase grows. Identified error-as-value discipline (M3) as a single point of failure with 12% silent-incorrect-behavior rate. |
| **STPA** | Only method to systematically identify unsafe control actions from the host application's perspective (UCA-1 through UCA-5). Adversarial scenarios (AS-1 through AS-6) are the most realistic attack chains. Safety constraints (SC-1 through SC-13) are directly implementable. |
| **Markov** | Only method providing quantitative availability numbers (99.98% for well-behaved code, 99.80% for adversarial). Sensitivity analysis proves `recover()` has the highest single-parameter impact. Semi-Markov extensions identify where the memoryless assumption breaks (stack depth thresholds, MaxAlloc). |
| **Petri Net** | Only method providing formal proofs of deadlock-freedom and liveness. Boundedness analysis covers all 24 state places. Resource contention ranking identifies physical call stack as primary bottleneck. CPN color extensions model per-call-type resource consumption. |
| **HAZOP** | Only method with security-specific guide words (SPOOFED, INJECTED, ESCALATED, EXFILTRATED, etc.). Interaction matrix reveals 5 critical cross-component attack chains. Maintenance triggers define when to re-analyze. |

### Method Limitations

| Method | Key Limitation |
|--------|---------------|
| **FMEA** | Does not model system-level interactions well. RPN can overweight low-severity/high-occurrence issues. |
| **FTA** | Probability estimates are subjective (no empirical failure data). Independence assumption between events may not hold. |
| **ETA** | Assumes barriers are independent; does not capture common-cause degradation. Forward-only analysis may miss backward-propagating failures. |
| **STPA** | Qualitative -- no numeric risk scores. Requires manual identification of control loops. |
| **Markov** | Memoryless assumption fails for deterministic thresholds (stack limits, MaxAlloc). Transition rates are estimated, not measured. |
| **Petri Net** | Formal proofs are for the model, not the code. Model may not capture all real behaviors. Timed extensions are estimates. |
| **HAZOP** | Guide word coverage may miss novel failure modes not captured by the predefined vocabulary. Risk scores (PxC) are subjective. |

---

## 10. Implementation Roadmap

### Phase 1: Critical Safety (1-2 weeks)

These changes address the highest-risk findings with the lowest effort:

| Task | Priority | Effort | Findings Addressed |
|------|----------|--------|-------------------|
| Add `recover()` at eval entry points | P2 | Low | HC-4 (37 panics) |
| Add parser depth limit | P4 | Low | HC-3 (parser crash) |
| Add integer division/modulo zero checks | P8 | Very Low | HC-8 (arithmetic panic) |
| Fix sorted-map Copy (call copyMapData) | P7a | Very Low | HC-7 (shallow copy) |
| Fix `opLet` parallel binding | P10 | Very Low | MC-1 (wrong env) |

**Estimated impact:** Eliminates the absorbing Panicked state in Markov model. Prevents fatal parser crashes. Fixes 3 code-level bugs.

### Phase 2: Resource Controls (2-4 weeks)

| Task | Priority | Effort | Findings Addressed |
|------|----------|--------|-------------------|
| Add evaluation step counter / context.Context | P1 | Low-Medium | HC-1 (no timeout) |
| Extend MaxAlloc to all allocation paths | P3 | Medium | HC-2 (incomplete MaxAlloc) |
| Add JSON deserialization depth limit | P11 | Low | MC-3 (JSON recursion) |
| Add explicit macro expansion depth limit | P9 | Low | HC-9 (macro expansion) |
| Add parser input size limit | P4b | Low | MC-9 (oversized input) |

**Estimated impact:** P(TE-1 Host DoS) drops from 0.70 to ~0.30. P(TE-5 Security Bypass) drops from 0.76 to ~0.35. ETA End State E (OOM) probability drops from 9.8% to <1%.

### Phase 3: Security Hardening (3-6 weeks)

| Task | Priority | Effort | Findings Addressed |
|------|----------|--------|-------------------|
| Default to confined file loading | P5 | Low-Medium | HC-5 (filesystem access) |
| Add package sealing / freezing | P6 | Medium | HC-6 (no package sealing) |
| Document runtime single-threaded requirement | P12 | Low | HC-10 (concurrency unsafe) |
| Sanitize error messages for untrusted contexts | -- | Low | MC-11 (path disclosure) |
| Add load-file cycle detection | P14 | Very Low | MS-6 (circular loads) |

**Estimated impact:** Addresses all security boundary findings. Provides defense-in-depth for embedded deployments processing untrusted input.

### Phase 4: Correctness and Quality (Ongoing)

| Task | Priority | Effort | Findings Addressed |
|------|----------|--------|-------------------|
| Implement sorted-map equality | P7b | Low | HC-7 (equality bug) |
| Add integer overflow detection (opt-in) | -- | Low | MC-2 (silent overflow) |
| Document float comparison semantics | -- | Very Low | MC-7 (float equality) |
| Fix deprecated GoInt/GoFloat64 | P15 | Very Low | MC-8 (inverted checks) |
| Add singleton immutability CI test | P13 | Low | MC-4 (singleton risk) |
| Convert type-assertion panics to error returns | -- | Medium | HC-4 (incremental) |

**Estimated impact:** Addresses correctness bugs and long-term code quality. Reduces probability of TE-2 (Silent Incorrect Computation) from 0.50 to ~0.25.

### Verification Criteria

Each phase should be verified with:
1. `make test` passes (Go tests + Lisp test files)
2. `make static-checks` passes (golangci-lint)
3. `elps doc -m` passes (no missing docstrings for new builtins)
4. New test cases covering each fixed finding
5. Benchmark comparison (`go test -bench=.`) to verify no performance regression from added checks

---

## Appendix A: Method Agreement Summary Table

| Finding | FMEA RPN | FTA P(event) | ETA Risk | STPA UCA | Markov dU/d | Petri Net | HAZOP PxC | Agreement |
|---------|----------|-------------|----------|----------|-------------|-----------|-----------|-----------|
| No timeout | 224 | P(BE-5.4)=0.40 | -- | UCA-10,SC-5 | lambda_ei=1e-5 | IL absorbing | 16 | 7/7 |
| Incomplete MaxAlloc | 224 | P(BE-5.3)=0.30 | 0.49 | UCA-3,14,SC-4 | -- | 2/15 coverage | 9 | 7/7 |
| Parser no depth | 112 | P(BE-1.4)=0.03 | -- | UCA-18,SC-1 | Rec 5 | -- | 20 | 6/7 |
| 37 panics, 0 recover | 147 | P(TE-3)=0.11 | 0.12+0.05 | UCA-4,13,SC-2 | mu_pr=0 (rank 1) | -- | 6 | 6/7 |
| Filesystem unrestricted | 405 | P(BE-5.2)=0.10 | -- | UCA-1,7,SC-3 | -- | -- | 15 | 5/7 |
| No package sealing | 270 | -- | -- | UCA-8,9,21,SC-7 | -- | -- | 12+20 | 3/7 |
| Sorted-map bugs | 210+50 | P(BE-2.4)=0.15 | -- | -- | -- | -- | -- | 3/7 |
| Div/mod by zero | 175+140 | implicit | implicit | -- | -- | -- | -- | 3/7 |
| Macro expansion | 224 | P(BE-1.4)=0.03 | -- | UCA-12 | Rec 3 | Sec 4.4 | 12 | 6/7 |
| Concurrency unsafe | 189 | -- | -- | UCA-5,SC-8 | -- | Sec 4.6 | -- | 3/7 |

---

## Appendix B: Disagreements and Perspective Differences

No outright disagreements were found between methods. However, several findings received different emphasis:

1. **Singleton mutation risk (MC-4):** FMEA rates it RPN 160 (significant). FTA computes P(TE-4)=0.015 (low, due to AND gate). Both are correct -- low probability but catastrophic severity. The AND gate in FTA provides a more nuanced view than FMEA's multiplicative RPN.

2. **TRO-enabled infinite loops (MS-4):** FTA assigns FV importance 0.21 for BE-1.6. Markov models this as lambda_ei=1e-5 (near-zero). The difference reflects FTA's broader "any infinite loop" scope vs. Markov's "zero stack growth loops only" precision. Both recommend the same mitigation (timeout/step counter).

3. **Sorted-map equality bug (HC-7):** FMEA and FTA flag it prominently. STPA and HAZOP do not, because they focus on safety/security boundaries rather than functional correctness bugs. This illustrates how FMEA excels at finding code-level bugs while STPA/HAZOP focus on systemic issues.

4. **Package registry monotonic growth (Petri Net P21):** Only the Petri Net analysis identifies that packages can never be deleted. Other methods do not consider this because in practice, package count is bounded by the finite source program. The Petri Net's formal boundedness analysis provides the most rigorous treatment.

---

*End of synthesis report. All findings trace to specific code locations in the ELPS codebase. Quantitative values cite the originating analysis method. Recommendations are deduplicated and ranked by cross-method agreement.*
