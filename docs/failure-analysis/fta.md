# FTA -- Fault Tree Analysis

## Date and Scope
- **Date**: 2026-02-13
- **Scope**: Full system analysis of the ELPS embedded Lisp interpreter (module `github.com/luthersystems/elps`).
- **Analyst**: Claude Opus 4.6 (automated code-level FTA)
- **Method**: Top-down deductive analysis starting from catastrophic end states. Probability estimates based on static code analysis, architectural review, and known failure patterns.

---

## Top Event Definitions

### TE-1: Host Application Denial of Service (DoS)
**Rationale**: ELPS is embedded in Go applications. Untrusted or semi-trusted Lisp code could exhaust CPU, memory, or Go goroutine stack, rendering the host application unresponsive. This is catastrophic because the interpreter shares the process with the host.

### TE-2: Silent Incorrect Computation
**Rationale**: The interpreter produces a wrong result without signaling an error. This is arguably the most dangerous failure mode because downstream decisions in the host application are based on incorrect data. Numeric overflows, comparison bugs, and type confusion are primary contributors.

### TE-3: Interpreter Panic (Unrecoverable Crash)
**Rationale**: A Go `panic()` escapes to the host application, potentially crashing it. The ELPS codebase contains numerous explicit `panic()` calls on internal invariant violations, plus implicit panics from nil pointer dereferences and type assertions.

### TE-4: Singleton Corruption (Global State Poisoning)
**Rationale**: The three shared singleton values (`singletonNil`, `singletonTrue`, `singletonFalse` in `lisp/lisp.go:289-292`) are returned by ~190 call sites. If any code path mutates a singleton, every subsequent evaluation in the process is corrupted. This is a common-cause failure affecting all other top events.

### TE-5: Security Boundary Bypass via Untrusted Input
**Rationale**: JSON deserialization (`libjson`), regex compilation (`libregexp`), `load-string`/`load-file`, and `eval` can process attacker-controlled input. Failures here enable code injection, ReDoS, or resource exhaustion beyond configured limits.

---

## Fault Tree Structures

### TE-1: Host Application Denial of Service

```
TE-1: Host Application DoS
|
+-- [OR] Resource Exhaustion
|   |
|   +-- BE-1.1: Unbounded memory allocation via unchecked list growth
|   |   Source: builtinAdd (builtins.go:2402-2424) — no overflow check on
|   |   integer sum; builtinMul (builtins.go:2534-2561) — no overflow check
|   |   on integer product. Indirect: repeated cons/append without limit.
|   |
|   +-- BE-1.2: make-sequence allocation bypass
|   |   Source: builtinMakeSequence (builtins.go:1687-1723). MaxAlloc check
|   |   exists but only for element count. Using float step near zero creates
|   |   near-infinite iterations before the limit is hit, causing CPU spin.
|   |
|   +-- BE-1.3: Regex catastrophic backtracking (ReDoS)
|   |   Source: libregexp.go:62-71 uses Go's regexp.Compile which is RE2
|   |   (linear time). This is NOT a real risk — Go RE2 prevents ReDoS.
|   |   Probability effectively zero.
|   |
|   +-- BE-1.4: Deeply nested macro expansion consuming Go stack
|   |   Source: stampMacroExpansion (macro.go:233-251) is recursive with no
|   |   depth limit. Deep ASTs from macro expansion can overflow the Go
|   |   goroutine stack. Also: findAndUnquote (macro.go:286-322) recurses
|   |   through quasiquote nesting with no depth bound.
|   |
|   +-- BE-1.5: JSON deserialization of deeply nested structures
|       Source: libjson/json.go:155-192 — loadInterface recurses through
|       nested maps/arrays with no depth limit. Attacker-crafted JSON with
|       thousands of nesting levels overflows the Go stack.
|
+-- [OR] CPU Exhaustion
    |
    +-- BE-1.6: Infinite loop in user Lisp code when TRO defeats stack limit
    |   Source: env.go:1055-1080 — tail recursion optimization collapses
    |   stack frames, so infinite mutual recursion never hits
    |   MaxHeightLogical. The logical height tracks elided frames
    |   (env.go:1069), but only at the point of TRO. A simple
    |   (defun f () (f)) loops forever without exceeding the limit because
    |   the physical stack stays at height 1. This is BY DESIGN but means
    |   MaxHeightLogical does not protect against CPU exhaustion from
    |   tail-recursive infinite loops.
    |
    +-- BE-1.7: Pathological sorted-map operations
        Source: lisp/lisp.go:473-486 — SortedMap backed by a tree. copyMapData
        (lisp.go:994-1007) deep-copies on every non-mutating assoc/dissoc call.
        Repeated assoc on a large map in a loop causes O(n^2) total allocations.
```

### TE-2: Silent Incorrect Computation

```
TE-2: Silent Incorrect Computation
|
+-- [OR] Numeric Errors
|   |
|   +-- BE-2.1: Integer overflow in arithmetic (wraps silently)
|   |   Source: builtinAdd (builtins.go:2413-2416) sums ints with `sum += c.Int`
|   |   — no overflow detection. builtinMul (builtins.go:2557-2558) multiplies
|   |   with `x.Int * y.Int` — no overflow detection. builtinSub similarly.
|   |   powInt (builtins.go:2368-2385) multiplies without overflow check.
|   |   Go int is 64-bit; overflow wraps silently to negative.
|   |
|   +-- BE-2.2: Float-to-int truncation in to-int loses data silently
|   |   Source: builtinToInt (builtins.go:732) does `Int(int(val.Float))`.
|   |   Float values exceeding int64 range produce undefined behavior in Go
|   |   (implementation-specific). Values like 1e20 lose precision silently.
|   |
|   +-- BE-2.3: Float equality comparison (builtinEqNum)
|       Source: lisp.go:970-977 — equalNum compares floats with `==`. The
|       comment "This may not be correct" acknowledges the issue. Expressions
|       like (= 0.1 (- 0.3 0.2)) return false due to IEEE 754 rounding.
|
+-- [OR] Type Confusion
|   |
|   +-- BE-2.4: sorted-map equality always returns false
|   |   Source: lisp.go:950-955 — Equal() for LSortMap checks length but then
|   |   unconditionally returns `Bool(false)`. The BUG comment on line 912
|   |   confirms this. (equal? m1 m2) is always false even for identical maps.
|   |
|   +-- BE-2.5: Copy() shallow-copies map and bytes native data
|       Source: lisp.go:980-992 — Copy() does `*cp = *v` (shallow) then only
|       deep-copies Cells (and skips arrays). The Native field (holding
|       *MapData for sorted-maps, *[]byte for bytes) is shared between
|       original and copy. Mutating operations (assoc!, append-bytes!) on
|       either value affect both. This is acknowledged in the comment on
|       line 985 but is a correctness hazard for users.
|
+-- [OR] Scoping Errors
    |
    +-- BE-2.6: let* allows recursive self-reference (documented BUG)
    |   Source: op.go:602-617 — The BUG comment explains that a function
    |   bound in let* can reference itself recursively when it should not be
    |   able to. This can produce unexpected behavior vs. Common Lisp semantics.
    |
    +-- BE-2.7: Package swap during function call creates temporal coupling
        Source: env.go:1191-1201 — During env.call(), the runtime package is
        swapped to the function's package. If the function triggers an error
        during evaluation and the error handler reads env.Runtime.Package,
        it sees the callee's package, not the caller's. The defer restores
        it, but handler-bind handlers execute before the defer fires.
```

### TE-3: Interpreter Panic (Unrecoverable Crash)

```
TE-3: Interpreter Panic
|
+-- [OR] Explicit panic() calls on invariant violations
|   |
|   +-- BE-3.1: Type assertion panics on LVal.FunData(), Bytes(), Map(), etc.
|   |   Source: lisp.go:707-775 — FunData(), Bytes(), Map(), UserData() all
|   |   panic if called on wrong type. If any internal code path reaches
|   |   these with a wrong type (e.g., after silent type confusion), panic.
|   |
|   +-- BE-3.2: Stack.Pop() panics on empty stack
|   |   Source: stack.go:204-212 — Pop() panics if Frames is empty. Each
|   |   FunCall/MacroCall/SpecialOpCall pushes and defers Pop. If an
|   |   unbalanced push/pop occurs (e.g., error during PushFID followed by
|   |   spurious Pop), the interpreter panics.
|   |
|   +-- BE-3.3: Nil LVal returned from function call
|   |   Source: env.go:937-939, env.go:988-990, env.go:1057-1059 — Three
|   |   identical checks `if r == nil { panic(...) }`. A nil return from
|   |   env.call() indicates a fundamental evaluator bug (likely a builtin
|   |   that returns Go nil instead of lisp Nil()).
|   |
|   +-- BE-3.4: TRO blocked panic in TerminalFID
|       Source: stack.go:128-135 — If TROBlock is true on a terminal frame,
|       the stack dumps to stderr and calls log.Panicf. This can be triggered
|       by incorrect Terminal marking in custom builtins.
|
+-- [OR] Nil pointer dereference
|   |
|   +-- BE-3.5: env.Runtime.Stack.Top() returns nil when stack is empty
|   |   Source: stack.go:96-101 — Top() returns nil if Frames is empty.
|   |   Callers like evalSExprCells (env.go:1105) dereference .Terminal
|   |   on the result. If Top() returns nil, this panics. In practice the
|   |   stack should never be empty during evaluation, but a bug in stack
|   |   management could cause it.
|   |
|   +-- BE-3.6: LFunData.Env is nil for builtins
|       Source: lisp.go:165-170 — Builtin functions have LFunData.Env = nil
|       (by design). If code calls fun.Env() on a builtin and then
|       dereferences the result without a nil check, panic. The bind()
|       function (env.go:1231) does check `if funenv == nil` but other paths
|       may not.
|
+-- [AND] Singleton mutation leading to cascading panic
    |
    +-- BE-3.7: stampMacroExpansion mutates shared Nil singleton
    |   (Mitigated: macro.go:242-244 guards against empty SExpr)
    |
    +-- BE-3.8: Future code path mutates Bool singleton
        (Currently mitigated but fragile — no runtime enforcement)
```

### TE-4: Singleton Corruption (Global State Poisoning)

```
TE-4: Singleton Corruption
|
+-- [AND] All three conditions must hold:
    |
    +-- BE-4.1: New code adds mutation of Nil/Bool return value
    |   Source: singletonNil, singletonTrue, singletonFalse (lisp.go:289-292)
    |   are shared mutable pointers. The SAFETY comment (lisp.go:276-288)
    |   documents the audit but there is NO runtime enforcement (no freezing,
    |   no copy-on-write). Any future code change that does
    |   `v := Nil(); v.Cells = append(v.Cells, x)` corrupts globally.
    |
    +-- BE-4.2: Mutation escapes review
    |   Source: No automated test or lint check verifies singleton immutability.
    |   The only protection is the comment and code review.
    |
    +-- BE-4.3: Corruption propagates before detection
        Source: Since Nil/Bool are returned by ~190 call sites, corruption
        affects every subsequent evaluation. No integrity check exists to
        detect that a singleton has been modified.
```

### TE-5: Security Boundary Bypass via Untrusted Input

```
TE-5: Security Boundary Bypass
|
+-- [OR] Code Injection
|   |
|   +-- BE-5.1: eval on attacker-controlled string
|   |   Source: builtinEval (builtins.go:756-762) evaluates arbitrary
|   |   expressions. If the host application passes user input to eval
|   |   (directly or via load-string), the user can execute arbitrary
|   |   Lisp code including load-file for filesystem access.
|   |
|   +-- BE-5.2: load-file enables filesystem traversal
|       Source: builtinLoadFile (builtins.go:454-470) passes loc.Str
|       directly to env.root().LoadFile(). The SourceLibrary interface
|       (loader.go) controls resolution, but RelativeFileSystemLibrary
|       may allow path traversal (../../../etc/passwd).
|
+-- [OR] Resource Exhaustion Beyond Limits
|   |
|   +-- BE-5.3: MaxAlloc only protects make-sequence and string:repeat
|   |   Source: runtime.go:31-36 and builtins.go:1714. The MaxAlloc check
|   |   is not applied to: sorted-map creation, vector creation, concat,
|   |   append, cons chains, or recursive data structure construction.
|   |   An attacker can build arbitrarily large structures via repeated
|   |   cons/append without hitting any allocation limit.
|   |
|   +-- BE-5.4: No timeout mechanism for evaluation
|       Source: The Runtime struct (runtime.go:15-27) has no timeout or
|       cancellation context. Once evaluation begins, there is no way
|       to interrupt it from the host application. A malicious program
|       can run indefinitely.
|
+-- [OR] JSON Deserialization Attacks
    |
    +-- BE-5.5: JSON bomb (deeply nested or very wide structures)
        Source: libjson/json.go:155-192 — loadInterface has no depth or
        size limit. A 10MB JSON document with millions of keys creates
        millions of LVal allocations with no MaxAlloc enforcement.
```

---

## Minimal Cut Sets

A minimal cut set is the smallest combination of basic events sufficient to cause the top event.

### TE-1: Host Application DoS
| Cut Set | Basic Events | Description |
|---------|-------------|-------------|
| MCS-1.1 | {BE-1.6} | Single event: tail-recursive infinite loop bypasses stack depth limit |
| MCS-1.2 | {BE-5.4} | Single event: no evaluation timeout allows infinite CPU consumption |
| MCS-1.3 | {BE-1.4} | Single event: deeply nested macro expansion overflows Go stack |
| MCS-1.4 | {BE-1.5} | Single event: deeply nested JSON overflows Go stack |
| MCS-1.5 | {BE-5.3} | Single event: unchecked allocation via cons/append exhausts memory |
| MCS-1.6 | {BE-1.2} | Single event: make-sequence with near-zero float step spins CPU |

### TE-2: Silent Incorrect Computation
| Cut Set | Basic Events | Description |
|---------|-------------|-------------|
| MCS-2.1 | {BE-2.1} | Single event: integer overflow in arithmetic wraps silently |
| MCS-2.2 | {BE-2.4} | Single event: sorted-map equality always false |
| MCS-2.3 | {BE-2.5} | Single event: shallow copy causes shared-mutation data corruption |
| MCS-2.4 | {BE-2.3} | Single event: float equality with == produces wrong boolean |
| MCS-2.5 | {BE-2.6} | Single event: let* recursive self-reference changes semantics |

### TE-3: Interpreter Panic
| Cut Set | Basic Events | Description |
|---------|-------------|-------------|
| MCS-3.1 | {BE-3.1} | Single event: type assertion panic on wrong LVal type |
| MCS-3.2 | {BE-3.3} | Single event: nil return from builtin causes evaluator panic |
| MCS-3.3 | {BE-3.2} | Single event: empty stack Pop panic |

### TE-4: Singleton Corruption
| Cut Set | Basic Events | Description |
|---------|-------------|-------------|
| MCS-4.1 | {BE-4.1, BE-4.2} | AND gate: new mutation code + escapes review |

### TE-5: Security Boundary Bypass
| Cut Set | Basic Events | Description |
|---------|-------------|-------------|
| MCS-5.1 | {BE-5.1} | Single event: eval on attacker input |
| MCS-5.2 | {BE-5.2} | Single event: load-file path traversal |
| MCS-5.3 | {BE-5.4} | Single event: no evaluation timeout |
| MCS-5.4 | {BE-5.5} | Single event: JSON bomb |

---

## Probability Assignments

Probabilities are estimated per-year for a typical deployment where ELPS evaluates semi-trusted input (e.g., configuration scripts, business rules from non-developer users).

| Basic Event | P(event) | Justification |
|-------------|----------|---------------|
| BE-1.1 | 0.05 | Unbounded list growth requires specific usage pattern; most programs stay bounded |
| BE-1.2 | 0.02 | Float step near zero is an unusual usage; MaxAlloc mitigates most cases |
| BE-1.3 | 0.001 | Go RE2 prevents ReDoS; effectively zero |
| BE-1.4 | 0.03 | Deep macro nesting is unusual but possible with recursive macros |
| BE-1.5 | 0.08 | JSON from external sources is common; depth attacks are well-known |
| BE-1.6 | 0.15 | Tail-recursive infinite loops are a common programming error; TRO makes them undetectable by stack limits |
| BE-1.7 | 0.03 | Pathological map usage requires specific access patterns |
| BE-2.1 | 0.20 | Integer overflow is the most likely silent error; financial/counter use cases hit this |
| BE-2.2 | 0.05 | Float-to-int conversion is less common |
| BE-2.3 | 0.10 | Float comparison is a well-known trap; many programs compare computed floats |
| BE-2.4 | 0.15 | Any program using sorted-map equality (testing, diffing) will hit this |
| BE-2.5 | 0.10 | Copy-then-mutate patterns are natural in Lisp; users expect deep copy |
| BE-2.6 | 0.03 | let* with recursive lambdas is uncommon |
| BE-2.7 | 0.02 | Package confusion during error handling is rare |
| BE-3.1 | 0.04 | Type assertion panics require internal inconsistency; rare in normal operation |
| BE-3.2 | 0.01 | Empty stack is prevented by balanced push/pop pattern with defer |
| BE-3.3 | 0.02 | Nil return requires a broken builtin implementation |
| BE-3.4 | 0.01 | TRO blocked panic requires inconsistent builtin |
| BE-3.5 | 0.01 | Stack empty during eval is prevented by initialization |
| BE-3.6 | 0.02 | Nil env dereference requires missed nil check in new code |
| BE-4.1 | 0.05 | New code mutating singleton is possible; no automated guard |
| BE-4.2 | 0.30 | Review miss probability — mutation may not be obvious |
| BE-5.1 | 0.25 | eval/load-string on user input is a common anti-pattern |
| BE-5.2 | 0.10 | Path traversal depends on SourceLibrary implementation |
| BE-5.3 | 0.30 | Lack of MaxAlloc on most allocation paths is a design gap |
| BE-5.4 | 0.40 | No timeout is a fundamental architectural limitation |
| BE-5.5 | 0.15 | JSON bomb attacks are well-documented |

---

## Top Event Probability Calculations

Using Boolean algebra with independence assumption for OR gates and AND gates.

### TE-1: Host Application DoS
```
P(TE-1) = 1 - (1 - P(BE-1.6)) * (1 - P(BE-5.4)) * (1 - P(BE-1.5)) *
              (1 - P(BE-5.3)) * (1 - P(BE-1.4)) * (1 - P(BE-1.2)) *
              (1 - P(BE-1.1))
        = 1 - (0.85)(0.60)(0.92)(0.70)(0.97)(0.98)(0.95)
        = 1 - 0.297
        = 0.703
```
**P(TE-1) ~ 0.70** (high — dominated by missing timeout and TRO-enabled infinite loops)

### TE-2: Silent Incorrect Computation
```
P(TE-2) = 1 - (1 - P(BE-2.1)) * (1 - P(BE-2.4)) * (1 - P(BE-2.5)) *
              (1 - P(BE-2.3)) * (1 - P(BE-2.2)) * (1 - P(BE-2.6))
        = 1 - (0.80)(0.85)(0.90)(0.90)(0.95)(0.97)
        = 1 - 0.505
        = 0.495
```
**P(TE-2) ~ 0.50** (medium-high — integer overflow and map equality are dominant contributors)

### TE-3: Interpreter Panic
```
P(TE-3) = 1 - (1 - P(BE-3.1)) * (1 - P(BE-3.3)) * (1 - P(BE-3.2)) *
              (1 - P(BE-3.4)) * (1 - P(BE-3.5)) * (1 - P(BE-3.6))
        = 1 - (0.96)(0.98)(0.99)(0.99)(0.99)(0.98)
        = 1 - 0.893
        = 0.107
```
**P(TE-3) ~ 0.11** (low-medium — defer-based cleanup and type checks provide good protection)

### TE-4: Singleton Corruption
```
P(TE-4) = P(BE-4.1) * P(BE-4.2)
        = 0.05 * 0.30
        = 0.015
```
**P(TE-4) ~ 0.015** (low — AND gate, but consequences are catastrophic if it occurs)

### TE-5: Security Boundary Bypass
```
P(TE-5) = 1 - (1 - P(BE-5.1)) * (1 - P(BE-5.2)) * (1 - P(BE-5.4)) *
              (1 - P(BE-5.5)) * (1 - P(BE-5.3))
        = 1 - (0.75)(0.90)(0.60)(0.85)(0.70)
        = 1 - 0.243
        = 0.757
```
**P(TE-5) ~ 0.76** (high — dominated by missing timeout and missing allocation limits)

---

## Common Cause Failures

Common cause failures are basic events that appear across multiple fault trees:

### CCF-1: Missing Evaluation Timeout (BE-5.4)
- Appears in: TE-1 (CPU exhaustion), TE-5 (security bypass)
- Impact: Without a context.Context or timeout mechanism in the Runtime, any code that runs too long cannot be interrupted. This is the single highest-impact gap in the system.
- Source: `runtime.go:15-27` — Runtime struct has no timeout or cancellation field.

### CCF-2: Incomplete MaxAlloc Coverage (BE-5.3)
- Appears in: TE-1 (memory exhaustion), TE-5 (resource bypass)
- Impact: MaxAlloc is only checked in `make-sequence` and `string:repeat`. All other allocation paths (cons, append, vector, sorted-map, concat) have no size enforcement.
- Source: `runtime.go:31-36` defines MaxAllocBytes() but it is only called from `builtins.go:1714`.

### CCF-3: No Integer Overflow Detection (BE-2.1)
- Appears in: TE-2 (incorrect computation), TE-1 (via crafted inputs causing overflow in size calculations)
- Impact: Every arithmetic builtin silently wraps on overflow. Array dimension calculation (`lisp.go:449-453`) checks for overflow via `totalSize < 0` but arithmetic builtins do not.
- Source: `builtins.go:2402-2424` (add), `builtins.go:2534-2561` (mul), `builtins.go:2368-2385` (pow).

### CCF-4: Recursive Processing Without Depth Limits
- Appears in: TE-1 (Go stack overflow), TE-3 (panic from stack overflow), TE-5 (JSON bomb)
- Impact: Three independent recursive code paths have no depth bounds: `stampMacroExpansion` (macro.go:233), `findAndUnquote` (macro.go:286), `loadInterface` (libjson/json.go:155).

---

## Fussell-Vesely Importance

The Fussell-Vesely (FV) importance of a basic event measures the fraction of the top event probability that involves cut sets containing that event. Higher FV = more critical to system reliability.

### TE-1: Host Application DoS (P = 0.703)
| Basic Event | FV Importance | Rank |
|-------------|--------------|------|
| BE-5.4 (no timeout) | 0.569 | 1 |
| BE-1.6 (TRO infinite loop) | 0.213 | 2 |
| BE-5.3 (incomplete MaxAlloc) | 0.427 | 3 |
| BE-1.5 (JSON depth overflow) | 0.114 | 4 |
| BE-1.4 (macro depth overflow) | 0.043 | 5 |
| BE-1.1 (unbounded list growth) | 0.071 | 6 |
| BE-1.2 (make-seq float step) | 0.028 | 7 |

### TE-2: Silent Incorrect Computation (P = 0.495)
| Basic Event | FV Importance | Rank |
|-------------|--------------|------|
| BE-2.1 (integer overflow) | 0.404 | 1 |
| BE-2.4 (map equality bug) | 0.303 | 2 |
| BE-2.3 (float == comparison) | 0.202 | 3 |
| BE-2.5 (shallow copy mutation) | 0.202 | 4 |
| BE-2.2 (float-to-int truncation) | 0.101 | 5 |
| BE-2.6 (let* self-reference) | 0.061 | 6 |

### TE-5: Security Boundary Bypass (P = 0.757)
| Basic Event | FV Importance | Rank |
|-------------|--------------|------|
| BE-5.4 (no timeout) | 0.528 | 1 |
| BE-5.3 (incomplete MaxAlloc) | 0.396 | 2 |
| BE-5.1 (eval on user input) | 0.330 | 3 |
| BE-5.5 (JSON bomb) | 0.198 | 4 |
| BE-5.2 (load-file traversal) | 0.132 | 5 |

---

## Importance Rankings (Global)

Aggregating across all top events, the most critical basic events are:

| Rank | Basic Event | Aggregate FV | Primary Top Events |
|------|------------|-------------|-------------------|
| 1 | BE-5.4: No evaluation timeout | 0.55 | TE-1, TE-5 |
| 2 | BE-2.1: Integer overflow in arithmetic | 0.40 | TE-2 |
| 3 | BE-5.3: Incomplete MaxAlloc coverage | 0.41 | TE-1, TE-5 |
| 4 | BE-2.4: sorted-map equality always false | 0.30 | TE-2 |
| 5 | BE-5.1: eval on attacker-controlled input | 0.33 | TE-5 |
| 6 | BE-1.6: TRO enables infinite loops | 0.21 | TE-1 |
| 7 | BE-2.3: Float equality with == | 0.20 | TE-2 |
| 8 | BE-2.5: Shallow copy shares native data | 0.20 | TE-2 |
| 9 | BE-1.5: JSON recursive deserialization | 0.16 | TE-1, TE-5 |
| 10 | BE-4.1+4.2: Singleton mutation (AND) | 0.015 (but catastrophic) | TE-4 |

---

## Top 5 Recommendations

### R1: Add Evaluation Timeout via context.Context (Addresses BE-5.4, CCF-1)
**Priority**: Critical
**Effort**: Medium
Add a `context.Context` field to `Runtime` (`runtime.go:15`). Thread cancellation checks into `LEnv.Eval()` (the `eval:` label in `env.go:827`) and into `env.call()` (`env.go:1163`). Check `ctx.Err()` at each eval cycle — this is the single most impactful change, reducing P(TE-1) from 0.70 to ~0.45 and P(TE-5) from 0.76 to ~0.50.

### R2: Add Integer Overflow Detection to Arithmetic Builtins (Addresses BE-2.1, CCF-3)
**Priority**: High
**Effort**: Low
In `builtinAdd` (`builtins.go:2413-2416`), `builtinMul` (`builtins.go:2557-2558`), `builtinSub`, and `powInt` (`builtins.go:2375-2384`), add overflow checks using Go's `math/bits` package or the `math.MaxInt64` / `math.MinInt64` bounds. Return an error condition `integer-overflow-error` on overflow. This eliminates the highest-importance single basic event for TE-2.

### R3: Implement sorted-map Equality Comparison (Addresses BE-2.4)
**Priority**: High
**Effort**: Low
In `LVal.Equal()` (`lisp.go:950-955`), replace the unconditional `return Bool(false)` with an actual key-by-key comparison iterating `sortedMapEntries()`. The comment on line 912 acknowledges this as a known BUG. This is a 10-line fix that eliminates the second-highest contributor to TE-2.

### R4: Add Depth Limits to Recursive Processing (Addresses BE-1.4, BE-1.5, CCF-4)
**Priority**: High
**Effort**: Low-Medium
Add explicit depth counters to: (a) `stampMacroExpansion` in `macro.go:233`, (b) `findAndUnquote` in `macro.go:286`, and (c) `loadInterface` in `libjson/json.go:155`. Return an error when depth exceeds a configurable limit (default 1000). This prevents Go goroutine stack overflow from deeply nested inputs.

### R5: Extend MaxAlloc Enforcement to All Allocation Paths (Addresses BE-5.3, CCF-2)
**Priority**: Medium-High
**Effort**: Medium
Currently `MaxAllocBytes()` is only checked in `builtinMakeSequence`. Add checks to: `builtinConcat`/`builtinConcatSeq` (`builtins.go:1288`), `builtinAppend`/`builtinAppendMutate`, `builtinCons` (for accumulated list size), and `libjson/json.go:172` (for JSON array size). This significantly reduces P(TE-1) and P(TE-5) for memory exhaustion scenarios.

---

## Caveats and Limitations

1. **Probability estimates are subjective**: Based on code analysis and common usage patterns, not empirical failure data. Actual probabilities depend heavily on whether ELPS processes untrusted input.

2. **Concurrency not analyzed**: ELPS is designed as a single-threaded interpreter. The `atomicCounter` in `runtime.go:132` suggests some concurrent usage may exist, but the core evaluator is not thread-safe. Concurrent access to `LEnv` would introduce an entire class of failures not covered here.

3. **Host application integration not analyzed**: The fault trees assume ELPS is embedded in a Go application. The host's recovery mechanisms (e.g., `recover()` from panics, goroutine isolation) significantly affect the real-world impact of TE-3 and TE-1.

4. **Formatter and linter paths excluded from security analysis**: The formatter (`formatter/`) and linter (`lint/`) operate on trusted source and are not considered security-relevant.

5. **Go RE2 regex engine eliminates ReDoS**: Unlike most language interpreters, ELPS's use of Go's `regexp` package (RE2) means regex denial-of-service is not a realistic threat. This is a significant security advantage.
