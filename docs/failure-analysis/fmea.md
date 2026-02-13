# FMEA -- Failure Mode and Effects Analysis

**System:** ELPS Embedded Lisp Interpreter (github.com/luthersystems/elps)
**Date:** 2026-02-13
**Scope:** Full system -- lexer, parser, evaluator, standard library, formatter, linter, and CLI
**Analyst:** Claude Opus 4.6

---

## 1. Scale Definitions

### Severity Scale (S)

| Rating | Level        | Definition |
|--------|-------------|------------|
| 1      | None        | No observable effect on interpreter behavior |
| 2      | Cosmetic    | Incorrect formatting or diagnostic output; program logic unaffected |
| 3      | Minor       | Slightly misleading error message or degraded debug info; workaround obvious |
| 4      | Low         | Incorrect lint diagnostic or analysis result; no runtime impact |
| 5      | Moderate    | Silent wrong result for an edge case; user may not notice |
| 6      | Significant | Incorrect evaluation result that a user is likely to encounter |
| 7      | High        | Unhandled error propagation; program crashes with a Go panic |
| 8      | Very High   | Memory exhaustion or unbounded resource consumption (DoS for embedding host) |
| 9      | Severe      | Path traversal or sandbox escape via file loading |
| 10     | Critical    | Corruption of shared mutable state; interpreter produces silently wrong results across all subsequent evaluations |

### Occurrence Scale (O)

| Rating | Level          | Definition |
|--------|---------------|------------|
| 1      | Eliminated    | Impossible given current code structure |
| 2      | Remote        | Requires deliberate adversarial crafting of input |
| 3      | Very Low      | Exotic combination of features rarely used together |
| 4      | Low           | Uncommon usage pattern; requires specific argument shapes |
| 5      | Moderate-Low  | Plausible user scenario with specific but reasonable inputs |
| 6      | Moderate      | Reasonably common pattern that triggers the failure |
| 7      | Moderate-High | Common pattern; likely encountered in medium-sized programs |
| 8      | High          | Frequently used feature path; most non-trivial programs hit this |
| 9      | Very High     | Almost every program execution traverses this path |
| 10     | Certain       | Every invocation triggers this path |

### Detection Scale (D)

| Rating | Level          | Definition |
|--------|---------------|------------|
| 1      | Certain       | Compile-time check or type system prevents the failure entirely |
| 2      | Very High     | Existing unit test directly covers this exact failure mode |
| 3      | High          | Existing test exercises the code path; failure would cause test failure |
| 4      | Moderate-High | CI pipeline (golangci-lint, `make test`) would likely catch a regression |
| 5      | Moderate      | Test exists for related functionality but not this specific edge case |
| 6      | Low-Moderate  | Would be caught during manual testing or code review |
| 7      | Low           | Only detectable by careful runtime observation (e.g., profiling, benchmarks) |
| 8      | Very Low      | Requires adversarial fuzzing or security audit to discover |
| 9      | Remote        | Requires deep analysis of concurrent/stateful interaction; no existing test coverage |
| 10     | Impossible    | No mechanism exists to detect this failure before production |

---

## 2. FMEA Worksheet

### 2.1 Lexer (`parser/lexer/lexer.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| L1 | Unterminated string literal produces `ERROR` token | User sees scan-error; file partially parsed. Lexer line 107: `lex.errorf("unterminated string literal")` | Missing closing `"` or unescaped newline in source | Lexer emits `token.ERROR`; parser converts to `LError` with condition `scan-error` | 3 | 6 | 2 | 36 | None needed -- well handled |
| L2 | Raw string `"""..."""` scanning loops until EOF | CPU bound on large input without closing `"""`; lexer line 135-143 calls `scanner.Accept` in tight loop | Unclosed raw string literal | Loop terminates on EOF with error message; bounded by input size | 3 | 3 | 5 | 45 | Consider size limit for raw strings |
| L3 | `precedingNewlines`/`precedingSpaces` tracking lost on error recovery | Formatter produces incorrect blank-line preservation after error token | Error token path in `emit` still sets whitespace fields, but error recovery may leave stale state | Whitespace fields are set per-token via `lex.precedingNewlines`/`lex.precedingSpaces` in `emit()` (line 168-177) | 2 | 3 | 6 | 36 | Add formatter test with error-recovery scenarios |

### 2.2 Parser (`parser/rdparser/parser.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| P1 | Mismatched bracket `[...)` produces error but location may confuse user | Two-location diagnostic emitted via `CondMismatchedSyntax` | Parser detects wrong closing bracket type | `errorAtf` points to opening bracket; `diagnostic/` renders annotated snippet | 3 | 5 | 2 | 30 | None needed |
| P2 | `countExprArgs` (op.go line 265-341) does not recurse into nested s-expressions in `(expr ...)` body | `(expr (+ %%1 (f %%2)))` may create lambda with wrong arity because `countExprArgs` for `LSExpr` only checks direct children's `.Str` prefix, not deeper nesting | `countExprArgs` `LSExpr` case (line 291) iterates `expr.Cells` but each cell's check at line 296 only checks `cell.Str` which is empty for non-symbol types (nested LSExpr has `.Str=""`) | No test covers deeply nested `%%N` inside `(expr ...)` | 5 | 4 | 8 | **160** | Add recursive walk in `countExprArgs` or test proving current behavior is intentional |
| P3 | No limit on expression count in `ParseProgram` | Memory exhaustion parsing extremely large files with millions of top-level forms; unbounded `append` to `exprs` slice at line 108 | No expression-count limit | Relies on Go memory limits and OS OOM | 8 | 2 | 8 | **128** | Consider optional expression-count limit for embedded use |
| P4 | Deeply nested parentheses cause Go stack overflow in parser | `ParseConsExpression` and `ParseExpression` are mutually recursive via `parseExpression()` (line 153-193). 50000+ nested `((((...))))` could overflow Go goroutine stack before ELPS stack limits apply (those are eval-time, not parse-time) | No parse depth counter | None for parsing phase | 8 | 2 | 7 | **112** | Add parse depth counter with configurable limit (default: 10000) |

### 2.3 Evaluator Core (`lisp/env.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| E1 | `goto eval` loop in `Eval()` (line 826-880) has no macro expansion depth limit | Infinite macro expansion via mutually recursive macros causes Go goroutine stack overflow or CPU spin. Each `goto eval` at line 867 re-enters the loop without pushing an ELPS stack frame for the expansion iteration itself | User writes macro A that expands to call to macro B that expands to call to macro A. `MacroCall` pushes a frame with `TROBlock=true` per expansion, but the *outer* `goto eval` loop iterates without bound | `MacroCall` pushes a stack frame, but this only limits depth *within* a single expansion, not the total number of re-expansions | 8 | 4 | 7 | **224** | Add macro expansion depth counter (e.g., max 1000 re-expansions per `Eval` call) |
| E2 | Go panic from nil `LVal` return in `call()` | Host crash. Lines 1057-1059: `if r == nil { panic("nil LVal returned from function call") }`. Same pattern at lines 937-939 and 988-990 | Custom Go builtin returns nil instead of `*LVal` -- API contract violation | Three explicit nil checks with panic + debug stack dump to stderr. No `recover()` | 7 | 3 | 7 | **147** | Wrap builtin dispatch with `recover()` or replace panic with `env.Errorf` |
| E3 | `bind()` panics if `fun.Native` is not `*LFunData` | Go panic crashes host. `FunData()` at lisp.go line 711 does unchecked type assertion: `v.Native.(*LFunData)` | Corrupted `LVal` with `Type=LFun` but wrong `Native` type; possible via unsafe Go interop | `FunData()` panics on mismatch -- no nil/type guard | 7 | 2 | 8 | **112** | Add type check in `FunData()` returning error instead of panic |
| E4 | Package swap `env.Runtime.Package = inner` in `call()` (line 1194-1201) not protected by mutex | Concurrent goroutine evaluation using same `Runtime` sees wrong package during function dispatch | `Runtime` struct has no synchronization; `atomicCounter` used only for ID generation | No mutex; single-threaded by convention, not enforcement | 6 | 3 | 9 | **162** | Document non-thread-safety prominently or add `sync.Mutex` on `Runtime.Package` |
| E5 | `opLet` (op.go line 555-582) evaluates RHS in `letenv` not `env` | `let` bindings evaluated in child env where previous bindings from the same `let` form may be visible, violating parallel-binding semantics. Line 570: `vals[i] = letenv.Eval(bind.Cells[1])` -- `letenv` is the child env, not the parent `env` | `letenv` is created at line 557 as child of `env`; RHS should be evaluated in `env` for true parallel binding | No test specifically checks that `let` bindings cannot see each other. BUG comment in `opLetSeq` (line 602-617) discusses related scoping issue | 6 | 5 | 5 | **150** | Change line 570 to `vals[i] = env.Eval(bind.Cells[1])` |
| E6 | `evalSExprCells` at line 1124: dead code check `f.Type == LMarkTailRec` after `f.Type != LFun` already returned error | No runtime effect; the `LMarkTailRec` branch panics but is unreachable since `LMarkTailRec != LFun` would have returned error at line 1121-1123 | Defensive coding artifact | `log.Panicf` would fire but is unreachable | 1 | 1 | 2 | 2 | Remove dead code for clarity |
| E7 | `args.Cells = args.Cells[1:]` mutation in `opFlet`/`opLabels`/`opLet`/`opLetSeq` (op.go lines 403, 491, 529, 558) | Mutates the slice header of args LVal in-place. If the AST node is re-evaluated (macro expansion loop), truncation persists | Direct mutation for efficiency; `evalSExprCells` returns fresh `SExpr(newCells)` so aliasing is indirect | AST nodes from special ops receive unevaluated cells; mutation of `args.Cells` header does not modify the original parse tree cells slice backing array | 4 | 3 | 8 | **96** | Create `SExpr(args.Cells[1:])` rather than mutating `args.Cells` in-place |

### 2.4 Tail Recursion Optimization (`lisp/stack.go`, `lisp/env.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| T1 | `TerminalFID` (stack.go line 118-144) panics on `TROBlock` frame during terminal chain search | Go panic kills host. Line 135: `log.Panicf("tail-recursion-optimization is blocked -- inconsistent stack")` | Custom builtin incorrectly sets `Terminal=true` on a frame with `TROBlock` | Debug print to stderr + panic; intentional fail-fast | 7 | 2 | 7 | **98** | Return error instead of panicking; let caller handle |
| T2 | `Pop()` on empty stack panics (stack.go line 207) | Host crash: `panic("pop called on an empty stack")` | Mismatched Push/Pop from corrupted evaluator state or custom builtin | Explicit panic check | 7 | 2 | 7 | **98** | Return zero `CallFrame` and error instead of panic |
| T3 | `decrementMarkTailRec` (env.go line 1092-1097) directly mutates `mark.Cells[0].Int` | No current bug because `markTailRec` at line 654-658 creates fresh `Int` values; but fragile if code changes | Mutable integer decrement on value known to be freshly allocated | `markTailRec` always allocates new `Int` values | 5 | 1 | 5 | 25 | None needed |

### 2.5 Singleton Mutation Risk (`lisp/lisp.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| S1 | External code mutates field on `singletonNil`/`singletonTrue`/`singletonFalse` | All subsequent `Nil()`, `Bool(true)`, `Bool(false)` return corrupted value; global silent state corruption across entire interpreter | Go builtin or embedding code modifies `.Cells`, `.Str`, `.Source` on singleton | Extensive audit documented in comments (lisp.go lines 269-293); `stampMacroExpansion` guards empty SExpr (line 242-244); 127 `Nil()` and 63 `Bool()` call sites audited | 10 | 2 | 8 | **160** | Add runtime assertion in debug builds; freeze singletons; or add CI test verifying singleton integrity post-test-suite |
| S2 | `stampMacroExpansion` guard `v.Type == LSExpr && len(v.Cells) == 0` also skips user-created empty SExpr | Macro-expanded empty lists keep `<native code>` source instead of call site | Guard is intentionally broad to protect singletons | Comment at line 237-244 documents the trade-off | 3 | 3 | 5 | 45 | None needed -- acceptable |

### 2.6 Error Handling / Condition System (`lisp/op.go`, `lisp/runtime.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| H1 | `opHandlerBind` handler error masks original error (op.go line 678) | If handler itself errors ("Well, we're boned" comment), original error is lost. Handler's error returned without original context | Handler function raises an error; original was pushed to conditionStack at line 688 but return at line 678 bypasses it | Comment acknowledges issue; `defer PopCondition` at line 689 cleans up | 4 | 4 | 6 | **96** | Chain original error as cause/context in handler error |
| H2 | `builtinRethrow` (builtins.go line 777-783) returns pointer to original error that handler may have mutated | Rethrown error carries mutations made by handler code if handler stored references | `PushCondition` at runtime.go line 41 stores original `*LVal` pointer, not a copy | No copy on push; handler receives condition symbol + copied cells but conditionStack holds original | 5 | 3 | 8 | **120** | Copy the error on push to isolate from handler mutations |
| H3 | `ErrorCondition` (env.go line 744) panics when called with mixed `error` and `*LVal` arguments | Go panic: `panic("invalid error argument")` at line 755 | Caller passes `(error, *LVal)` to variadic `v ...interface{}`; narg>1 check triggers | Comment documents this is invalid usage; only env methods and builtins call this | 7 | 2 | 6 | **84** | Return error LVal instead of panicking |
| H4 | `opHandlerBind` deep copy of error data via `val.Copy().Cells` (op.go line 691) expensive for large payloads | Performance degradation with large error data (sorted-map or large list in error cells) | `Copy()` creates full deep copy including all nested cells | No alternative path; copy needed for safety | 3 | 3 | 7 | 63 | Consider shallow copy of cells slice only |

### 2.7 Package System (`lisp/package.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| K1 | `Package.Get` (line 52-63) mutates `FunNames` as side effect during read | `Get` is not a pure read: updates `FunNames[fid]` tracking. Concurrent reads on same package race | `FunNames[fid]` tracks most recent name used to reference function (line 58-62) | No mutex; single-threaded assumption | 6 | 3 | 9 | **162** | Remove mutation from `Get`; FunNames already populated in `put()` (line 151-152). Or document thread-unsafety |
| K2 | `builtinInPackage` (builtins.go line 472-504) silently creates new package on first use | Typo in package name creates wrong package instead of error. `(in-package my-pkge)` with typo creates "my-pkge" and auto-imports lisp package | `Registry.DefinePackage` called when package not found (line 481) | No warning or confirmation mechanism | 5 | 6 | 9 | **270** | Add linter check for undeclared package names |
| K3 | `UsePackage` (env.go line 187-203) copies symbol references not values | Mutable values (sorted-maps) shared by reference across packages. `assoc!` on shared map mutates both packages' view | `Put(Symbol(sym), v)` at line 200 copies the `*LVal` pointer | By-reference is intentional for functions | 4 | 4 | 8 | **128** | Document that mutable values shared across packages are aliased |
| K4 | `Exports()` has O(n*m) duplicate detection (package.go line 95-109) | Performance degradation for packages with many exports | Linear scan `for _, s := range pkg.Externals` for each new symbol | Typically small number of exports | 2 | 3 | 7 | 42 | Use map for O(1) dedup; low priority |

### 2.8 File Loading / Source Library (`lisp/library.go`, `lisp/builtins.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| F1 | Path traversal via `load-file` when `RootDir` empty | Lisp code reads arbitrary files: `(load-file "/etc/passwd")` parsed as Lisp. `RelativeFileSystemLibrary` at library.go line 93-116 with empty `RootDir` (default) | `RootDir` defaults to empty string, disabling confinement (library.go line 87 comment) | `RootDir` field exists with symlink-resolving confinement (lines 98-111) but is opt-in. `FSLibrary` provides alternative. CLI `cmd/run.go` does not set RootDir | 9 | 5 | 9 | **405** | Default `RootDir` to CWD for embedded use; require explicit opt-out for unrestricted access |
| F2 | Symlink-based `RootDir` bypass (library.go line 102-108) | TOCTOU: `EvalSymlinks` resolves at check time but file may change between check and `os.ReadFile` at line 114. If `EvalSymlinks` fails (permission denied), code falls back to un-resolved path (lines 102-106: `if resolved, err := filepath.EvalSymlinks(loc); err == nil { resolvedLoc = resolved }`) | `EvalSymlinks` error silently swallowed, using unresolved `loc` for check | Confinement check at line 109 uses `resolvedLoc` which may be un-resolved on error | 9 | 2 | 9 | **162** | Deny access when `EvalSymlinks` fails; use fd-based verification |
| F3 | `load-string`/`load-bytes` enable arbitrary code evaluation from untrusted input | Embedding app passing untrusted input to `(load-string ...)` enables full ELPS code execution including `load-file`, `set`, package manipulation | No sandbox mechanism for dynamically loaded code | `TROBlock` prevents TRO unwinding past load boundary (builtins.go line 421); package restored via defer | 7 | 4 | 10 | **280** | Provide sandboxed eval mode; document security implications |
| F4 | `env.root()` in `builtinLoadString` (builtins.go line 422) loads code in root environment | Loaded code can `(set 'x ...)` in caller's package scope, overwriting caller's bindings without restriction | Intentional: loaded code shares package but not lexical scope | `TROBlock` set; package restored via defer in `load()` | 5 | 5 | 5 | **125** | Document that load-string can modify current package bindings |

### 2.9 Sorted Map (`lisp/maps.go`, `lisp/lisp.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| M1 | `LVal.Copy()` (lisp.go line 980-992) does shallow copy of `Native` for `LSortMap` -- map data shared | Mutating a "copied" sorted-map mutates the original. `Copy()` at line 985: `*cp = *v` copies Native pointer; no special case for `LSortMap` | `copyMapData()` method exists (line 994-1007) but is NOT called by `Copy()`. For `LArray`, cells are deliberately NOT deep-copied (line 986-988), but `LSortMap` stores data in `Native`, not `Cells` | BUG comment at line 912: "sorted-map comparison is not implemented" hints at incomplete sorted-map support | 6 | 5 | 7 | **210** | Call `copyMapData()` in `Copy()` for `LSortMap` type |
| M2 | `LVal.Equal` for `LSortMap` (lisp.go line 951-956) always returns false | `(equal? (sorted-map :a 1) (sorted-map :a 1))` returns `false` for all non-empty maps | BUG comment: "sorted-map comparison is not implemented" (line 912); after length check, unconditionally returns `Bool(false)` at line 955 | Known issue documented in code | 5 | 5 | 2 | 50 | Implement sorted-map equality |

### 2.10 Arithmetic / Numeric Operations (`lisp/builtins.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| N1 | `builtinDiv` integer division by zero causes Go panic | Host crash: Go runtime panics on `int / 0`. `(/ 1 0)` with two integers triggers unrecoverable panic | No zero-check before integer division | None; float division by zero returns `+Inf` (IEEE 754) which is handled | 7 | 5 | 5 | **175** | Add explicit zero check before integer division in `builtinDiv` |
| N2 | `builtinMod` integer modulo by zero causes Go panic | Host crash: `(mod 5 0)` -- Go `int % 0` panics | Docstring says "b must be non-zero" but no runtime enforcement | Documentation only | 7 | 4 | 5 | **140** | Add runtime zero check returning descriptive error |
| N3 | `builtinAdd`/`builtinMul` integer overflow wraps silently | `(+ 9223372036854775807 1)` returns negative number on 64-bit. Go integer arithmetic wraps without error | No overflow detection in arithmetic builtins | None | 5 | 4 | 8 | **160** | Document overflow behavior; optionally add checked arithmetic |
| N4 | `equalNum` (lisp.go line 970-977) float comparison via `==` | `(= 0.1 (+ 0.05 0.05))` may give unexpected results. Comment at line 975: "This may not be correct" | Standard IEEE 754 equality; no epsilon | Acknowledged in source comment | 5 | 6 | 5 | **150** | Document float comparison semantics; consider epsilon option |
| N5 | `builtinPow` returns `+Inf` for large exponents | `(pow 2 10000)` returns `+Inf` float, not error | `math.Pow` does not error on overflow | No overflow check on result | 5 | 3 | 7 | 105 | Check for `math.IsInf` and return error |

### 2.11 JSON Serialization (`lisp/lisplib/libjson/json.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| J1 | Deeply nested JSON causes unbounded recursion in `loadInterface` (json.go line 155-192) | Go goroutine stack overflow from `loadInterface` calling itself for each nesting level | Adversarial JSON like `[[[[...]]]]` nested thousands of levels | No depth limit on recursive calls | 8 | 3 | 8 | **192** | Add depth limit parameter to `loadInterface` (default: 512) |
| J2 | `GoInt` (json.go line 421-429): `v.IsNumeric()` check is inverted | Returns `(0, false)` when value IS numeric; always returns false for numbers. Line 422: `if v.IsNumeric()` should be `if !v.IsNumeric()` | Logic bug; inverted boolean | Marked `Deprecated` but still public API | 6 | 3 | 5 | 90 | Fix inverted condition or remove deprecated function |
| J3 | `GoFloat64` (json.go line 437-445): same inverted `v.IsNumeric()` bug | Returns `(0, false)` for all numeric values | Same pattern as J2 | Marked `Deprecated` | 6 | 3 | 5 | 90 | Fix or remove |
| J4 | JSON numbers decoded as `float64` lose precision for values > 2^53 | `(json:load-string "{\"id\":9007199254740993}")` returns 9007199254740992.0 | Standard `encoding/json` behavior: numbers -> `float64` | `UseStringNumbers` option exists as workaround | 5 | 5 | 7 | **175** | Document precision loss; recommend `:string-numbers` for large integers |

### 2.12 Regexp (`lisp/lisplib/libregexp/libregexp.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| R1 | `BuiltinIsMatch` with string pattern recompiles on every call | Performance degradation in loops: `getRegexp` (line 103-119) calls `regexp.Compile` when given a string | No pattern cache | User can pre-compile with `regexp-compile`; documented in docstring | 3 | 7 | 7 | **147** | Consider LRU cache for recently compiled patterns |
| R2 | Go RE2 engine prevents ReDoS | No vulnerability -- Go `regexp` guarantees linear-time matching | RE2 design | Inherent in Go stdlib | 1 | 1 | 1 | 1 | None needed |

### 2.13 Macro System (`lisp/macro.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| X1 | `stampMacroExpansion` infinite recursion on cyclic AST | Stack overflow. `stampMacroExpansion` (macro.go line 233-251) walks children recursively without cycle detection | User macro produces cyclic cell structure | No depth limit or cycle detection | 8 | 1 | 9 | 72 | Add depth limit to recursive walk |
| X2 | `builtinMacroExpand` (builtins.go line 564-587) infinite loop | CPU hang: `for {}` loop calls `macroExpand1` until head is no longer a macro; non-terminating macro chains loop forever | User-defined macro chain that always produces another macro call | No expansion depth limit in the `for` loop | 7 | 2 | 7 | **98** | Add iteration counter; error after 1000 expansions |
| X3 | `shallowUnquote` (lisp.go line 632-637) aliases Cells with template | Macro result shares Cells slice with template via `*cp = *v`. If expanded result is later mutated, template corrupted for subsequent invocations | Shallow copy semantics; comment at line 630 calls this "an artifact" | User macros use quasiquote which builds new structures; builtin macros return fresh SExpr | 4 | 2 | 8 | 64 | Audit mutation paths after shallowUnquote |

### 2.14 Resource Limits (`lisp/runtime.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| RL1 | `MaxAlloc` only checked in specific builtins (`string:repeat`, `make-sequence`) | Other allocation paths (`concat`, `vector`, `map`, array construction, JSON deserialization) can create arbitrarily large structures without limit | `MaxAlloc` is opt-in per-builtin, not enforced globally | Individual builtins check `MaxAllocBytes()` where implemented | 8 | 4 | 7 | **224** | Add allocation checks to `Array()`, `Vector()`, `concat`, JSON `loadInterface` |
| RL2 | `Runtime.Stack` defaults protect against stack overflow | Stack overflow produces clean ELPS error via `LogicalStackOverflowError`/`PhysicalStackOverflowError` | Recursive ELPS code exceeding limits | `PushFID` checks both limits; defaults: logical=50000, physical=25000 | 3 | 6 | 2 | 36 | None needed |

### 2.15 Runtime Shared State (`lisp/runtime.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| RT1 | `Runtime.Package` shared mutable pointer with no concurrency safety | Data race: two goroutines evaluating with same Runtime see/modify each other's package context | ELPS is single-threaded by design but `atomicCounter` at line 132-136 suggests awareness of concurrent access patterns; all other fields unprotected | No mutex; `atomic` only on counters | 7 | 3 | 9 | **189** | Document single-threaded requirement prominently; or add `sync.Mutex` |
| RT2 | `conditionStack` unbounded growth if `PopCondition` missed | Memory leak from embedding misuse | `PushCondition` appends; `PopCondition` pops. `opHandlerBind` uses `defer` for cleanup | `defer` in `opHandlerBind` ensures cleanup even on panic | 4 | 1 | 8 | 32 | Low risk; acceptable |
| RT3 | `LEnv.Copy()` (env.go line 298-309) copies `Scope` but shares `FunName` map | Mutations to function name tracking in copy affect original | `Copy()` does `cp.Scope = make(...)` but not `cp.FunName = make(...)` | No test verifies FunName isolation | 3 | 2 | 8 | 48 | Copy `FunName` map in `Copy()` or document shared semantics |

### 2.16 Formatter (`formatter/formatter.go`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| FM1 | Formatter produces code that parses differently from input | Semantic change to source code after formatting | Bug in indent rules, comment handling, or bracket preservation | `TestASTPreservation` and `TestRepoFileRoundTrip` verify `parse(original) == parse(formatted)` for comprehensive patterns and all repo `.lisp` files | 5 | 2 | 2 | 20 | Good controls; continue expanding test corpus |
| FM2 | Top-level trailing comments after `))` closing brackets become leading comments on next form | Comment moves to wrong location in formatted output | Known limitation documented in MEMORY.md | `TestASTPreservation` tests AST preservation but not comment placement | 2 | 5 | 5 | 50 | Document limitation; add test case |

### 2.17 Linter / Analysis (`lint/`, `analysis/`)

| # | Failure Mode | Effect | Cause | Current Controls | S | O | D | RPN | Recommended Action |
|---|-------------|--------|-------|-----------------|---|---|---|-----|-------------------|
| A1 | `builtin-arity` false positive for user threading macros | Arity warning for user macros that inject extra arguments (like `thread-first` does) | `aritySkipNodes` hardcoded for `thread-first`/`thread-last` only | User can suppress with `; nolint:builtin-arity` | 4 | 4 | 5 | 80 | Allow configurable skip patterns |
| A2 | `analysis.prescan` does not recognize `macrolet`-defined macros | Symbols from `macrolet` macros may appear as unresolved in phase 1 | `prescan` handles `defun`, `defmacro`, `deftype`, `set`, `use-package`, `in-package` but not local forms | Phase 2 `analyzeExpr` handles scoped forms correctly | 4 | 4 | 5 | 80 | None needed -- phase 2 handles it |

---

## 3. Cross-Stage Failure Propagation Matrix

| Source Component | Propagation Path | Downstream Effect |
|-----------------|------------------|-------------------|
| **Lexer error token** | Lexer -> Parser -> `LError` with `scan-error` condition | Evaluator never reached; clean error to user |
| **Parser overflow (P3/P4)** | Parser -> Memory/stack exhaustion -> Go OOM/crash | Host process killed; all components affected |
| **Singleton mutation (S1)** | Any code mutating singleton -> `Nil()`/`Bool()` globally corrupted | Every subsequent eval returns wrong values; evaluator, builtins, macros, stdlib all affected |
| **Sorted-map shallow copy (M1)** | `Copy()` -> shared map -> `assoc!` mutates "copy" | Original map silently corrupted; affects any code holding reference |
| **Path traversal (F1)** | `load-file` -> `LoadSource` -> arbitrary file read -> parser -> evaluator | Untrusted code execution in embedding host's environment |
| **Macro infinite expansion (E1)** | User macro -> `Eval` -> `MacroCall` -> `markMacExpand` -> `goto eval` loop | Go goroutine stack exhaustion or CPU spin |
| **Integer division by zero (N1)** | `builtinDiv` -> Go panic -> deferred Stack.Pop() fires -> partial cleanup | Host process crashes; no ELPS error handling possible |
| **JSON deep nesting (J1)** | `Load` -> `loadInterface` recursion -> Go stack overflow | Host process crashes; no recovery |
| **Package typo (K2)** | `in-package` creates wrong package -> symbols bound in wrong namespace | Silent wrong behavior; symbols "disappear" from expected package |
| **`let` scoping (E5)** | `opLet` -> RHS evaluated in `letenv` not `env` -> binding can see prior bindings | Semantic divergence from standard `let`; subtle logic bugs in user code |
| **Concurrent Runtime (E4/RT1)** | Two goroutines share `Runtime` -> `Package` field race | Data race; unpredictable evaluation results |
| **Rethrow mutation (H2)** | Handler mutates condition -> `builtinRethrow` returns mutated original | Rethrown error has unexpected data; outer handler sees corrupted error |

---

## 4. Ranked List by RPN (Highest First)

| Rank | # | RPN | Component | Failure Mode | S | O | D |
|------|---|-----|-----------|-------------|---|---|---|
| 1 | F1 | **405** | `RelativeFileSystemLibrary` | Path traversal when `RootDir` empty | 9 | 5 | 9 |
| 2 | F3 | **280** | `load-string`/`load-bytes` | Arbitrary code evaluation from untrusted input | 7 | 4 | 10 |
| 3 | K2 | **270** | `builtinInPackage` | Package typo silently creates new package | 5 | 6 | 9 |
| 4 | E1 | **224** | `Eval` goto-eval loop | Infinite macro expansion, no depth limit | 8 | 4 | 7 |
| 5 | RL1 | **224** | `DefaultMaxAlloc` | Allocation limit not enforced by all code paths | 8 | 4 | 7 |
| 6 | M1 | **210** | `LVal.Copy` for `LSortMap` | Shallow copy shares map data | 6 | 5 | 7 |
| 7 | J1 | **192** | `Serializer.Load` | Deeply nested JSON causes stack overflow | 8 | 3 | 8 |
| 8 | RT1 | **189** | `Runtime.Package` | No concurrency safety on shared state | 7 | 3 | 9 |
| 9 | N1 | **175** | `builtinDiv` | Integer division by zero panics | 7 | 5 | 5 |
| 10 | J4 | **175** | `Serializer.Load` | JSON integer precision loss > 2^53 | 5 | 5 | 7 |
| 11 | K1 | **162** | `Package.Get` | Side-effect mutation of `FunNames` during read | 6 | 3 | 9 |
| 12 | E4 | **162** | `call` package swap | No mutex on `Runtime.Package` | 6 | 3 | 9 |
| 13 | F2 | **162** | `RelativeFileSystemLibrary` | TOCTOU symlink race | 9 | 2 | 9 |
| 14 | P2 | **160** | `countExprArgs` | Non-recursive walk misses nested `%%N` in `(expr ...)` | 5 | 4 | 8 |
| 15 | S1 | **160** | Singletons | External code mutates shared Nil/Bool singletons | 10 | 2 | 8 |
| 16 | N3 | **160** | `builtinAdd`/`builtinMul` | Silent integer overflow | 5 | 4 | 8 |
| 17 | E5 | **150** | `opLet` | RHS evaluated in wrong environment | 6 | 5 | 5 |
| 18 | N4 | **150** | `equalNum` | Float equality via `==` | 5 | 6 | 5 |
| 19 | R1 | **147** | `BuiltinIsMatch` | Pattern recompiled on every call | 3 | 7 | 7 |
| 20 | E2 | **147** | `call()` nil return | Go panic from nil builtin return | 7 | 3 | 7 |
| 21 | N2 | **140** | `builtinMod` | Integer modulo by zero panics | 7 | 4 | 5 |
| 22 | P3 | **128** | `ParseProgram` | No expression count limit | 8 | 2 | 8 |
| 23 | K3 | **128** | `UsePackage` | Shared mutable values by reference | 4 | 4 | 8 |
| 24 | F4 | **125** | `builtinLoadString` | Loaded code modifies caller's package | 5 | 5 | 5 |
| 25 | H2 | **120** | `builtinRethrow` | Rethrown error carries handler mutations | 5 | 3 | 8 |

---

## 5. Top 5 Recommendations

### Recommendation 1: Enforce filesystem confinement by default (F1, F2 -- RPN 405 + 162)

**Code Locations:**
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/library.go` lines 93-116
- `/Users/swood/work/src/github.com/luthersystems/elps/cmd/run.go`

**Action:** Set `RootDir` on `RelativeFileSystemLibrary` to the directory of the executed file by default. In `library.go`, change the `EvalSymlinks` error handling at lines 102-106 to deny access when resolution fails rather than falling through to the un-resolved path. For embedding, recommend `FSLibrary` with `os.DirFS()` as the secure default.

**Observable Metric:** Test that `(load-file "../../../etc/hostname")` returns access-denied error when RootDir is set.

### Recommendation 2: Add macro expansion depth limit and allocation guards (E1, RL1 -- RPN 224 + 224)

**Code Locations:**
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/env.go` lines 826-880 (the `goto eval` loop)
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/runtime.go` `MaxAllocBytes()`

**Action:** (a) Add counter to `Eval` tracking macro re-expansion iterations; error after configurable limit (default 1000). (b) Apply `MaxAllocBytes()` checks to `Array()`, `Vector()`, `concat`, and JSON `loadInterface` -- currently only `string:repeat` and `make-sequence` check this limit.

**Observable Metric:** Test that mutually recursive macros produce clear "macro-expansion-limit" error. Test that `(vector (make-sequence 0 100000000))` respects `MaxAlloc`.

### Recommendation 3: Guard against Go panics from arithmetic operations (N1, N2 -- RPN 175 + 140)

**Code Locations:**
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/builtins.go` -- `builtinDiv`, `builtinMod`

**Action:** Add explicit zero checks in `builtinDiv` and `builtinMod` before performing integer division/modulo. Return `LError` with descriptive `"division-by-zero"` condition. Also wrap builtin dispatch in `call()` with `recover()` to convert any remaining panics from custom builtins into LError values (addressing E2, RPN 147).

**Observable Metric:** Test `(/ 1 0)` and `(mod 5 0)` return LError not Go panic. `go test -race` passes.

### Recommendation 4: Fix sorted-map shallow copy in `LVal.Copy()` (M1 -- RPN 210)

**Code Locations:**
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisp.go` lines 980-992 (`Copy()`), lines 994-1007 (`copyMapData()`)

**Action:** In `LVal.Copy()`, detect `LSortMap` type and call the existing `copyMapData()` method (which is already implemented but never called from `Copy()`). This is a one-line fix with high impact.

**Observable Metric:** Test `(let ([m (sorted-map :a 1)] [m2 (identity m)]) (assoc! m2 :a 2) (get m :a))` returns 1, not 2.

### Recommendation 5: Add linter check for undeclared package names (K2 -- RPN 270)

**Code Locations:**
- `/Users/swood/work/src/github.com/luthersystems/elps/lint/analyzers.go` -- new analyzer
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/builtins.go` lines 472-504 (`builtinInPackage`)

**Action:** Add new `package-exists` lint analyzer that checks `(in-package name)` calls against known package names: stdlib packages, previously declared packages in the file, and workspace packages. Flag names that don't match any known package as potential typos.

**Observable Metric:** Linting a file with `(in-package my-pkge)` (typo for `my-pkg`) produces a warning.

---

## Summary of Key Findings

The ELPS codebase is well-structured with consistent error propagation using `LVal` with `LError` type and good test coverage. The highest-priority risks cluster in two areas:

1. **Security boundary gaps** (RPN 405, 280, 162): Filesystem access defaults to unrestricted; `load-string` enables arbitrary code execution; symlink resolution has TOCTOU vulnerability.

2. **Host application stability** (RPN 224, 192, 175, 147): Multiple paths can crash the host via Go panics (division by zero, nil returns, stack overflow), unbounded allocations (sequences, JSON), or infinite loops (macro expansion). The use of `panic()` across the lisp package means unexpected state can bring down the embedding application without graceful degradation.

Three code-level bugs were identified:
- `opLet` evaluates RHS in wrong environment (E5)
- `GoInt`/`GoFloat64` have inverted `IsNumeric()` checks (J2, J3)
- `LVal.Copy()` for sorted-maps does not call the existing `copyMapData()` method (M1)
