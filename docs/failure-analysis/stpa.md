# STPA -- Systems-Theoretic Process Analysis

**Date:** 2026-02-13
**Scope:** Full ELPS interpreter system
**Analysis Type:** Systems-Theoretic Process Analysis (STPA) with STPA-Sec extensions

---

## 1. System-Level Losses

| ID | Loss Description |
|----|--------------------|
| L-1 | **Host application crash** -- the Go process hosting ELPS terminates unexpectedly (via Go panic, OS signal, or goroutine stack exhaustion) |
| L-2 | **Host application resource exhaustion** -- memory, CPU, or file descriptors consumed to the point the host cannot serve its primary purpose |
| L-3 | **Unauthorized filesystem access** -- Lisp code reads or writes files outside the intended scope, leading to data exfiltration or corruption |
| L-4 | **Data corruption** -- Lisp code mutates shared state (package registry, runtime, sorted maps, arrays, singleton LVals) in a way that silently corrupts host application data |
| L-5 | **Information disclosure** -- internal host state, file contents, stack traces, or runtime metadata is exposed to untrusted Lisp code or end users |
| L-6 | **Logic bypass** -- error-handling, type-checking, or package-isolation mechanisms are circumvented, causing the host to operate on invalid assumptions |
| L-7 | **Denial of service to co-embedded workloads** -- one Lisp evaluation monopolizes the shared Runtime or Go goroutine, starving other work |

## 2. System-Level Hazards (Linked to Losses)

| ID | Hazard Description | Losses |
|----|-------------------|--------|
| H-1 | Unbounded recursion or allocation exhausts Go goroutine stack or heap memory | L-1, L-2, L-7 |
| H-2 | Lisp code causes a Go `panic()` that is not recovered by the host | L-1 |
| H-3 | `load-file` accesses arbitrary filesystem paths outside the intended root | L-3 |
| H-4 | Untrusted Lisp code redefines builtins, macros, or special operators, subverting intended semantics | L-4, L-6 |
| H-5 | Error conditions are silently swallowed, causing the host to treat failures as successes | L-4, L-6 |
| H-6 | Lisp code executes for an unbounded duration with no cancellation mechanism | L-2, L-7 |
| H-7 | Shared mutable state (Runtime, package registry) is concurrently accessed from multiple goroutines | L-1, L-4 |
| H-8 | Stack traces, error messages, or debug output disclose host-internal information | L-5 |
| H-9 | `eval`, `load-string`, `load-bytes` allow dynamic code execution that bypasses static analysis | L-3, L-4, L-6 |
| H-10 | Go `panic()` in type-assertion or accessor methods crashes the host when Lisp code provides unexpected types | L-1 |

## 3. Control Structure

```
+-----------------------------------------------------------------------+
|                         GO HOST APPLICATION                            |
|  (Controller C0: creates LEnv, configures Runtime, calls Eval/Load)   |
+------------------+----------------------------------------------------+
                   |
           config  | (WithReader, WithLibrary, WithMaximum*StackHeight,
           calls   |  Runtime.MaxAlloc, Library.RootDir)
                   v
+-----------------------------------------------------------------------+
|                      RUNTIME (Shared State)                           |
| +-------------------+  +-------------------+  +-------------------+   |
| | PackageRegistry   |  | CallStack         |  | conditionStack    |   |
| | .Packages map     |  | .Frames []        |  | []*LVal           |   |
| | .Lang string      |  | .MaxHeightLogical |  +-------------------+   |
| +-------------------+  | .MaxHeightPhysical|                          |
|                        +-------------------+                          |
| +-------------------+  +-------------------+  +-------------------+   |
| | Reader            |  | SourceLibrary     |  | Profiler          |   |
| | (parser)          |  | (filesystem I/O)  |  | (optional)        |   |
| +-------------------+  +-------------------+  +-------------------+   |
+------------------+----------------------------------------------------+
                   |
        Eval()     | control actions: eval, function call, macro expand,
        Load()     | load-file, set, set!, in-package, export, use-package
                   v
+-----------------------------------------------------------------------+
|                    LEnv EVALUATOR (Controller C1)                     |
|  Tree-structured scoping: LEnv -> Parent -> ... -> root               |
|  Dispatches: FunCall, SpecialOpCall, MacroCall                        |
|  Manages: TRO (tail recursion optimization), scope binding            |
+------------------+----------------------------------------------------+
                   |
                   | evaluates
                   v
+-----------------------------------------------------------------------+
|                   LISP CODE EXECUTION (Controlled Process)            |
|  +---------------------+  +--------------------+                     |
|  | Builtins (~80)       |  | Special Ops (~30)  |                     |
|  | load-file, set,      |  | let, if, lambda,   |                     |
|  | eval, concat,        |  | handler-bind,      |                     |
|  | make-sequence, ...   |  | quote, cond, ...   |                     |
|  +---------------------+  +--------------------+                     |
|  +---------------------+  +--------------------+                     |
|  | Macros (~8)          |  | Stdlib (~12 pkgs)  |                     |
|  | defun, defmacro,     |  | json, regexp,      |                     |
|  | deftype, defconst    |  | string, math, ...  |                     |
|  +---------------------+  +--------------------+                     |
+-----------------------------------------------------------------------+
                   |
        feedback   | error returns (LError), stack traces,
                   | type checks, stack height checks
                   v
+-----------------------------------------------------------------------+
|                    EXTERNAL RESOURCES                                  |
|  Filesystem (via RelativeFileSystemLibrary / FSLibrary)                |
|  Go goroutine stack, heap memory, stderr                              |
+-----------------------------------------------------------------------+
```

**Feedback channels:**
- Error propagation: `LVal.Type == LError` return values from every function
- Stack depth: `CallStack.PushFID()` -> `checkHeightPush()` -> error
- Allocation limits: `Runtime.MaxAllocBytes()` checked in `make-sequence`, `string:repeat`
- Type checks: runtime type assertions in builtins (`args.Cells[0].Type != LString`)
- Filesystem confinement: `RelativeFileSystemLibrary.RootDir` with symlink resolution

## 4. Unsafe Control Actions (UCA Table)

### Controller C0: Go Host Application

| ID | Control Action | UCA Type | UCA Description | Hazard |
|----|---------------|----------|-----------------|--------|
| UCA-1 | Configure Runtime.Library | Not provided | Host does not set `RootDir` on `RelativeFileSystemLibrary`, leaving filesystem access unbounded. The default in `cmd/run.go` line 45 creates `&lisp.RelativeFileSystemLibrary{}` with empty `RootDir`. | H-3 |
| UCA-2 | Configure stack limits | Not provided | Host creates `LEnv` without calling `InitializeUserEnv` (which does not set stack limits -- they come from `StandardRuntime()` defaults). If host manually creates a `Runtime` with zero MaxHeight values, stack checks are disabled (`stack.go` lines 180, 190: `if s.MaxHeightPhysical <= 0 { return nil }`). | H-1 |
| UCA-3 | Configure MaxAlloc | Not provided | Host leaves `Runtime.MaxAlloc` at zero. While `MaxAllocBytes()` falls back to `DefaultMaxAlloc` (10M), only `make-sequence` and `string:repeat` check this limit. Most allocation paths (`concat`, `append`, `map`, `json:load-*`) do not check any allocation limit. | H-1, H-2 |
| UCA-4 | Recover panics | Not provided | Host calls `env.Eval()` without wrapping in `recover()`. Approximately 35 `panic()` call sites in `lisp/` (e.g., `stack.go:206` Pop on empty stack, `lisp.go:709` FunData on non-function, `env.go:907` invalid function type) can crash the host. | H-2, H-10 |
| UCA-5 | Concurrent access | Provided when not safe | Host shares a single `Runtime` across multiple goroutines and calls `Eval()` concurrently. `Runtime` fields (`Package`, `Stack`, `conditionStack`, `Registry`) use no synchronization. Only `atomicCounter` (env/sym IDs) is thread-safe. | H-7 |

### Controller C1: LEnv Evaluator

| ID | Control Action | UCA Type | UCA Description | Hazard |
|----|---------------|----------|-----------------|--------|
| UCA-6 | `eval` (Eval method, `env.go:826`) | Provided when not needed | `builtinEval` (`builtins.go:756`) allows Lisp code to evaluate arbitrary runtime-constructed expressions. Combined with `load-string` and string manipulation, this enables code injection from untrusted data. | H-9 |
| UCA-7 | `load-file` (`builtins.go:454`) | Provided with wrong parameters | When `Library.RootDir` is empty, `load-file` can read any path on the filesystem including `../../etc/passwd`. The path is passed directly from Lisp code: `env.root().LoadFile(loc.Str)`. | H-3 |
| UCA-8 | `set` (`builtins.go:533`) | Provided when not needed | `set` can overwrite any symbol in the current package, including builtins. `env.PutGlobal(v.Cells[0], v.Cells[1])` writes directly to `Package.Symbols`. Lisp code can do `(set 'if my-if)` to replace the `if` special operator. | H-4, H-6 |
| UCA-9 | `in-package` (`builtins.go:472`) | Wrong timing/order | `in-package` creates packages if they do not exist and switches into them. Lisp code can switch into the `lisp` package and modify core builtins: `(in-package 'lisp) (set 'car (lambda (x) x))`. | H-4, H-6 |
| UCA-10 | Stack depth check (`stack.go:148`) | Stopped too soon | `checkHeightPush()` only fires on `PushFID`. However, `dotimes` (`op.go:462`) loops `count.Int` iterations without pushing stack frames, so a `(dotimes (i 1000000000) ...)` runs to completion with no depth protection, consuming CPU indefinitely. | H-6, H-1 |
| UCA-11 | `handler-bind` error dispatch (`op.go:641`) | Wrong timing/order | `handler-bind` uses `defer env.Runtime.PopCondition()` inside a `for` loop over bindings (`op.go:689`). The handler evaluation at line 692 (`env.Eval(SExpr(expr))`) may itself enter another `handler-bind` or call `rethrow`. If a nested handler pushes its own condition, `CurrentCondition()` (`runtime.go:56-62`) returns the innermost condition -- `rethrow` may re-throw the wrong error if the condition stack ordering does not match the caller's expectation. | H-5, H-6 |
| UCA-12 | Macro expansion (`env.go:912`) | Applied too long | `MacroCall` has no recursion limit on macro re-expansion. The result of macro expansion goes through `goto eval` in `Eval()` (`env.go:867`), which can trigger further macro calls. A self-expanding macro creates infinite expansion. The macro call pushes a frame with `TROBlock = true` (`env.go:933`), so each expansion consumes a physical frame -- but if `MaxHeightPhysical` is disabled (set to 0), expansion is unbounded. | H-1, H-6 |
| UCA-13 | Type feedback on panic paths | Not provided | Over 30 locations in `lisp/lisp.go` call `panic()` on type assertion failure (e.g., `FunData()` at line 709, `Bytes()` at line 763, `Map()` at line 772, `UserData()` at line 755). These are reached when Lisp code produces a structurally invalid `LVal`. The host receives no `LError` -- only a Go panic. | H-2, H-10 |
| UCA-14 | Allocation control in `concat` | Not provided | `builtinConcat` and its string/bytes helpers compute output sizes from input lengths and allocate without checking `MaxAllocBytes()`. An attacker can create large strings/bytes and concatenate them to exhaust memory. Similarly, `builtinMap`, `builtinSelect`, `builtinReject`, `builtinZip`, `builtinReverse`, and `builtinAppend` all allocate output sequences without size checks. | H-1, H-2 |
| UCA-15 | `RegisterDefaultBuiltin/Macro/SpecialOp` | Provided when not needed | These package-level functions (`builtins.go:386`, `macro.go:51`, `op.go:123`) mutate global slices (`userBuiltins`, `userMacros`, `userSpecialOps`) that persist across all `LEnv` instances. If called from init() in multiple packages, or at runtime, they silently extend the default language for ALL environments created afterward. No locking, no isolation. | H-4, H-7 |
| UCA-16 | TRO `markTailRec` decrement (`env.go:1091`) | Provided incorrectly | `decrementMarkTailRec` mutates `mark.Cells[0].Int` in place. If TRO bookkeeping is incorrect (e.g., a custom special op marks terminal incorrectly), the decrement can underflow to negative values, and the `mark.Cells[0].Int <= 0` check at line 1096 will always return true. While this prevents infinite looping, the logical height tracking at `env.go:999/1069` may accumulate incorrect values. | H-1 |
| UCA-17 | `stampMacroExpansion` (`macro.go:233`) | Provided when not needed | Mutates `v.Source` in place on expanded AST nodes. If two macro expansions share AST nodes (e.g., a macro returning a cached sub-tree), the source location of one expansion will overwrite the other, producing misleading error locations. Singleton nil check at line 242 prevents mutation of shared Nil, but other shared nodes are not protected. | H-5 |

### Controller C2: Parser (rdparser)

| ID | Control Action | UCA Type | UCA Description | Hazard |
|----|---------------|----------|-----------------|--------|
| UCA-18 | Parse deeply nested input | Applied too long | The recursive descent parser (`rdparser/parser.go`) parses nested s-expressions via recursive `ParseExpression()` calls. Deeply nested input like 50,000 nested parens `(((((...))))` exhausts the Go goroutine stack before any ELPS-level stack limit applies. No parser-level depth limit exists. | H-1, H-2 |
| UCA-19 | Parse oversized input | Not provided (no limit) | The parser has no input byte limit. A multi-gigabyte input string via `load-string` or `load-bytes` causes the parser to allocate unbounded memory for the token stream and AST before any evaluation-level limit is checked. | H-1 |

### Controller C3: Package System

| ID | Control Action | UCA Type | UCA Description | Hazard |
|----|---------------|----------|-----------------|--------|
| UCA-20 | `use-package` (`env.go:187`) | Wrong timing/order | `use-package` copies all externals from the source package into the current package's symbol table. If the source package is later modified (new exports added), those do not propagate. However, the initial copy overwrites same-named symbols silently. A package can shadow critical builtins in the consuming package. | H-4, H-6 |
| UCA-21 | Package isolation | Not provided | There is no access control on packages. Any Lisp code can call `(in-package 'lisp)` and modify the core language package, then `(in-package 'user)` to return. The `in-package` builtin creates packages on demand (`builtins.go:478`), meaning untrusted code can create arbitrary packages. | H-4, H-9 |

## 5. Detailed Loss Scenarios

### Loss Scenario LS-1: Host Crash via Parser Stack Overflow (UCA-18 -> H-1 -> L-1)

**Causal path:** An attacker supplies deeply nested Lisp source (e.g., 100,000 nested parentheses) to `env.Load()` or `env.LoadFile()`. The recursive descent parser's `ParseExpression()` recurses once per nesting level. Each Go function call consumes approximately 1-4KB of goroutine stack. At ~100K depth, the default 1MB goroutine stack (or 8MB with `GOGC` tuning) overflows. Go issues a fatal "goroutine stack exceeds limit" error, which is NOT recoverable via `recover()`. The entire Go process crashes.

**Why feedback fails:** The parser has no depth counter or depth limit. The ELPS `CallStack.MaxHeightPhysical` only applies to the interpreter's call stack, not the parser's Go-level recursion. The Go runtime's stack overflow is a fatal signal, not a panic.

**Impact:** L-1 (host crash). If ELPS is embedded in a server, all concurrent requests fail.

### Loss Scenario LS-2: Filesystem Traversal via load-file (UCA-7 -> H-3 -> L-3, L-5)

**Causal path:** The host application uses the default `RelativeFileSystemLibrary{}` without setting `RootDir` (as in `cmd/run.go` line 45). Untrusted Lisp code executes `(load-file "/etc/shadow")` or `(load-file "../../sensitive-config.yaml")`. The `LoadSource` method at `library.go:93` resolves the path relative to the current source context, calls `filepath.Clean`, and then `os.ReadFile(loc)` without any access restriction (the `RootDir` check at line 98 is skipped when `RootDir` is empty).

The file contents are parsed as Lisp. While the file may not be valid Lisp, the parser error message may disclose partial file contents. Additionally, if the file happens to contain valid Lisp expressions, they are evaluated with full privileges.

**Why feedback fails:** The only feedback is the `LError` returned if parsing fails. But the error message includes the filename and potentially partial content. There is no audit trail or access control log.

**Impact:** L-3 (unauthorized filesystem access), L-5 (information disclosure through error messages).

### Loss Scenario LS-3: Silent Builtin Overwrite Leading to Logic Bypass (UCA-8, UCA-9 -> H-4 -> L-4, L-6)

**Causal path:** Untrusted Lisp code executes:
```lisp
(in-package 'lisp)
(set 'error (lambda (condition &rest args) ()))
(in-package 'user)
```
This replaces the core `error` function with a no-op. All subsequent error signaling in the host application's Lisp logic silently succeeds, returning nil instead of propagating errors. Business logic that depends on error propagation (e.g., validation, authorization checks) is completely bypassed.

The `set` builtin at `builtins.go:538` calls `env.PutGlobal()`, which writes directly to the package's symbol table. The `Package.Put()` method at `package.go:122` only checks for `true`/`false` constant rebinding -- it does not prevent overwriting builtins.

**Why feedback fails:** `set` returns the newly bound value, not an error. There is no "immutable binding" or "sealed package" concept. The host has no notification that a core function was replaced.

**Impact:** L-4 (data corruption from missed validation), L-6 (logic bypass of error handling).

### Loss Scenario LS-4: Memory Exhaustion via Unbounded concat (UCA-14 -> H-1 -> L-1, L-2)

**Causal path:** Lisp code constructs a very large string through repeated concatenation:
```lisp
(let ((s "A"))
  (dotimes (i 30)
    (set! s (concat 'string s s))))
```
Each iteration doubles the string length. After 30 iterations, the string is 2^30 bytes (1 GB). The `builtinConcat` function uses `bytes.NewBuffer(make([]byte, 0, size))` without checking against `MaxAllocBytes()`. The `MaxAlloc` check exists only in `make-sequence` and `string:repeat`.

`dotimes` at `op.go:462` does not push stack frames per iteration, so the 50K stack limit does not apply. The loop runs 30 iterations, each allocating exponentially more memory.

**Why feedback fails:** `concat` has no allocation limit check. `dotimes` has no iteration limit. The only thing that stops this is the OS killing the process (OOM killer).

**Impact:** L-1 (crash from OOM), L-2 (resource exhaustion), L-7 (other workloads starved).

### Loss Scenario LS-5: Host Crash via Panic in Type Assertion (UCA-13, UCA-4 -> H-2, H-10 -> L-1)

**Causal path:** A Go-defined builtin creates an `LVal` with `Type: LFun` but a nil `Native` field (missing `LFunData`). When the ELPS evaluator encounters this function and calls `fun.FunData()` (`lisp.go:707`), the type assertion `v.Native.(*LFunData)` panics with "interface conversion: interface is nil, not *lisp.LFunData". The evaluator does not wrap builtin calls in `recover()`.

This can also occur through the embedding API: if a Go application registers a builtin that returns a structurally invalid `LVal` (e.g., an `LBytes` with `nil` Native), subsequent operations like `v.Bytes()` (`lisp.go:763`) panic.

Even within pure ELPS, the `json:dump-message` builtin at `libjson/json.go:239` has `panic("unexpected lval: " + val.Type.String())` -- if an LVal type not handled by the JSON encoder reaches this path, the host crashes.

**Why feedback fails:** Go panics from type assertions bypass the ELPS error system entirely. The `LError` propagation chain is not involved. Unless the host wraps `Eval()` in `defer recover()`, the process terminates.

**Impact:** L-1 (host crash).

## 6. STPA-Sec: Adversarial Scenarios

### AS-1: Code Injection via eval + load-string (Exploits UCA-6, UCA-9)

**Attacker model:** Attacker controls a string input that is passed to ELPS (e.g., a user-provided template, configuration value, or JSON field that is interpolated into a Lisp expression).

**Attack:** The attacker crafts a string like `"(progn (in-package 'lisp) (set 'car (lambda (x) (load-file \"/etc/passwd\"))))"` and passes it to a context where `eval` or `load-string` processes it. This replaces the `car` function with one that exfiltrates file contents. All subsequent calls to `car` throughout the application execute the attacker's code.

**Why existing controls fail:** `eval` and `load-string` are standard builtins with no sandboxing or capability restrictions. The package system allows unrestricted switching via `in-package`. `set` allows unrestricted overwriting.

### AS-2: Denial of Service via CPU-bound Loop (Exploits UCA-10, UCA-6)

**Attacker model:** Attacker can provide Lisp expressions for evaluation (e.g., via a scripting API, formula evaluation, or configuration DSL).

**Attack:** `(dotimes (i 2000000000) ())` -- a 2-billion iteration loop. Each iteration is minimal but the `dotimes` implementation at `op.go:462` has no timeout, no iteration limit, and does not push stack frames (so stack depth limits do not apply). The Go goroutine is blocked for the duration. Since ELPS has no `context.Context` integration or timeout mechanism, there is no way to cancel the evaluation.

Alternatively: `(labels ((f () (f))) (f))` creates infinite recursion. While the stack limit catches this, the attacker can use `(dotimes (i 1000000000) (let ((x 1)) x))` where the inner `let` creates and discards scope objects, causing GC pressure without hitting stack limits.

### AS-3: Privilege Escalation via Package Manipulation (Exploits UCA-9, UCA-20, UCA-21)

**Attacker model:** Application loads untrusted Lisp code in a "sandboxed" package, relying on package isolation for security.

**Attack:** The attacker's code calls `(in-package 'lisp)` to switch into the core language package, then uses `(set ...)` to modify any builtin. It can also call `(export ...)` to make new symbols visible, and `(use-package ...)` to import from any package including application-internal packages. After modifying the `lisp` package, `(in-package 'user)` returns to the user package, where the modified builtins are now in effect.

**Why existing controls fail:** `in-package` (`builtins.go:472-491`) has no access control. It creates packages on demand and switches freely. The package system was designed for code organization, not security isolation.

### AS-4: Information Disclosure via Error Messages (Exploits UCA-7, H-8)

**Attacker model:** Attacker can observe error messages from ELPS evaluation (e.g., via an API that returns error details).

**Attack:** The attacker sends expressions like `(load-file "/etc/hostname")`. Even if the file does not contain valid Lisp, the parser error message may include the file path and the byte offset where parsing failed, confirming the file exists and its approximate size. The attacker can iterate over paths to enumerate the filesystem.

Additionally, `(debug-stack)` prints the full call stack to stderr, and `(debug-print ...)` writes to stderr. If stderr is logged and accessible, the attacker gains visibility into runtime state.

### AS-5: Supply-Chain Injection via RegisterDefault* (Exploits UCA-15)

**Attacker model:** Attacker controls a Go package imported by the host application (e.g., a transitive dependency).

**Attack:** The attacker's package uses `init()` to call `lisp.RegisterDefaultBuiltin("harmless-name", ...)` with a function that exfiltrates data or modifies state. Since `RegisterDefaultBuiltin` appends to a global slice (`userBuiltins`) and `DefaultBuiltins()` includes all registered builtins, every `LEnv` created afterward includes the attacker's builtin. This persists for the lifetime of the Go process.

**Why existing controls fail:** `RegisterDefaultBuiltin` has no authentication, no logging, and no way for the host to audit what has been registered. The global slices are not protected by any access control.

### AS-6: Parser Bomb via Oversized Input (Exploits UCA-19)

**Attacker model:** Attacker can supply Lisp source code via `load-string` or `load-bytes`.

**Attack:** The attacker supplies a multi-gigabyte source string consisting of millions of simple atoms (e.g., `1 1 1 1 ...`). The parser allocates an `[]*LVal` slice proportional to the input. Since the parser has no input size limit, this causes unbounded memory allocation before any ELPS-level evaluation limit is checked. Unlike deeply nested input (AS-2), flat input does not overflow the Go stack -- it exhausts heap memory instead.

**Why existing controls fail:** The parser has no byte limit or node limit. `MaxAllocBytes` is only checked at evaluation time by specific builtins, not during parsing.

## 7. Feedback Gap Analysis

| Feedback Channel | What It Covers | Gaps |
|-----------------|---------------|------|
| **LError return values** | Runtime errors, type mismatches, unbound symbols | Does not cover Go panics (~35 panic sites). Does not cover resource limits (no timeout, no memory tracking). Errors can be silently swallowed by `ignore-errors`. No compile-time enforcement that every call site checks for LError. |
| **CallStack depth checks** | Stack overflow from recursive function calls | Only applies to ELPS-level function calls, not parser recursion or Go-level recursion. `dotimes` loops do not increment the stack. TRO-elided frames count toward logical height but physical frames do not grow. Logical height overflow gives a soft error, not a hard limit on memory. |
| **MaxAllocBytes** | Single-allocation memory bombs (`make-sequence`, `string:repeat`) | Not checked by `concat`, `append`, `map`, `select`, `reject`, `zip`, `reverse`, `json:load-*`, `json:dump-*`, `bytes` operations, or general list construction. Most allocation paths are unprotected. |
| **Library.RootDir** | Filesystem path traversal | Not set by default. Only exists on `RelativeFileSystemLibrary`, not as a Runtime-level control. Symlink resolution has TOCTOU window between `filepath.EvalSymlinks` and `os.ReadFile`. |
| **Package system exports** | Symbol visibility across packages | No access control on `in-package`, `set`, or `use-package`. Externals list controls `use-package` import but any code can switch packages and access symbols directly via qualified names. |
| **Type assertions** | Structural invariants on LVal | Checked via `panic()` not `LError`. Host receives no graceful error. ~35 panic sites in `lisp/` alone. |
| **Condition stack** | Error handling context for `handler-bind`/`rethrow` | Simple push/pop with no pairing validation. No feedback to detect mismatched push/pop. `rethrow` returns `CurrentCondition()` which is the stack top -- may be incorrect under nested error handling. |
| **Singleton immutability** | Nil/Bool shared values | Enforced only by code audit and convention (comments in `lisp.go:269-293`). `stampMacroExpansion` has a guard for empty SExpr but no runtime enforcement for other mutation vectors. A future bug in a registered builtin could corrupt singletons globally. |
| **MISSING: Timeout/cancellation** | None | ELPS has zero `context.Context` integration. No way to cancel a running evaluation. No CPU time limits. No wall-clock timeout. No instruction counter. |
| **MISSING: Allocation tracking** | None | No per-evaluation memory budget. No total allocation counter. Go's runtime memory stats are not consulted. |
| **MISSING: Audit/capability system** | None | No way to restrict which builtins are available. No capability tokens. No sandboxing. No audit log of operations performed. |
| **MISSING: Concurrency safety** | None | `Runtime` struct is not thread-safe. No mutex, no atomic fields (except env/sym counters). Documentation does not warn against concurrent use. |

## 8. Safety/Security Constraints (Derived from UCAs)

| ID | Constraint | Addresses |
|----|-----------|-----------|
| SC-1 | The parser MUST enforce a maximum nesting depth and return an error (not panic/crash) when exceeded. | UCA-18 |
| SC-2 | All Go `panic()` sites in the `lisp/` package that are reachable from user-provided Lisp code MUST be converted to `LError` returns, or the evaluator MUST wrap calls in `recover()`. | UCA-13, UCA-4 |
| SC-3 | `RelativeFileSystemLibrary.RootDir` SHOULD be non-empty by default, or the host MUST be warned when filesystem access is unconstrained. | UCA-1, UCA-7 |
| SC-4 | All allocation paths (concat, append, map, select, reject, zip, reverse, json load/dump, array construction) MUST check against `MaxAllocBytes()` before allocating. | UCA-3, UCA-14 |
| SC-5 | The evaluator MUST support a cancellation/timeout mechanism (e.g., `context.Context` integration or an evaluation step counter) to bound execution time. | UCA-10 |
| SC-6 | `dotimes` and similar loop constructs MUST either push stack frames or check an iteration limit to prevent unbounded CPU consumption. | UCA-10 |
| SC-7 | The package system SHOULD support a "sealed" or "frozen" mode where core packages cannot be modified after initialization. | UCA-8, UCA-9, UCA-21 |
| SC-8 | The `Runtime` struct MUST either be documented as single-goroutine only or protected with synchronization primitives. | UCA-5 |
| SC-9 | `RegisterDefaultBuiltin/Macro/SpecialOp` SHOULD be restricted to `init()` time or require explicit opt-in, with an audit mechanism for what has been registered. | UCA-15 |
| SC-10 | Error messages from `load-file` failures SHOULD NOT include file path information when operating in an untrusted-input context. | AS-4 |
| SC-11 | The parser SHOULD enforce an input size limit (bytes or node count) to prevent heap exhaustion during parsing. | UCA-19 |
| SC-12 | The `eval`, `load-string`, and `load-bytes` builtins SHOULD be documented as security-sensitive. A restricted evaluation mode (no file I/O, no package switching) SHOULD be available for untrusted input processing. | UCA-6, H-9 |
| SC-13 | The condition stack (`Runtime.conditionStack`) SHOULD validate push/pop pairing. At minimum, `PopCondition` should log or error if the stack is empty. | UCA-11 |

## 9. Top 5 Recommendations

### Recommendation 1: Add context.Context support for evaluation cancellation
**Addresses:** SC-5, SC-6, H-6, L-2, L-7

This is the highest-impact gap. ELPS has zero mechanism to cancel or time-limit a running evaluation. The `Runtime` struct should accept a `context.Context`. The `Eval()` method should check `ctx.Done()` periodically (e.g., every N evaluation steps, or on every `PushFID`). `dotimes` should check cancellation each iteration. This single change addresses CPU-bound DoS, unbounded loops, and co-tenant starvation.

### Recommendation 2: Convert panics to LError returns or add recover() wrapper
**Addresses:** SC-2, UCA-4, UCA-13, H-2, H-10, L-1

There are approximately 35 `panic()` call sites in `lisp/` reachable from user-provided code. The most dangerous are type-assertion panics in accessor methods (`FunData()`, `Bytes()`, `Map()`, `UserData()`). Two approaches: (a) replace panics with error returns where possible, or (b) add a `recover()` wrapper around the main `Eval()` entry point that converts panics to `LError`. Approach (b) is simpler and provides a safety net. Approach (a) is more correct and should be done incrementally. Additionally, the parser should have a depth limit to prevent Go goroutine stack overflow, which is NOT recoverable by `recover()`.

### Recommendation 3: Enforce MaxAllocBytes uniformly across all allocation paths
**Addresses:** SC-4, UCA-3, UCA-14, H-1, L-2

Currently only `make-sequence` and `string:repeat` check `MaxAllocBytes()`. The `concat`, `append`, `map`, `select`, `reject`, `zip`, `reverse`, `json:load-*`, and array construction paths allocate memory without any limit check. Create a helper function `env.CheckAlloc(n int) *LVal` that returns an `LError` if `n` exceeds the limit, and call it before every significant allocation. The JSON deserializer should also limit depth and output size.

### Recommendation 4: Add package sealing to prevent runtime modification of core builtins
**Addresses:** SC-7, UCA-8, UCA-9, UCA-21, H-4, L-4, L-6

Add a `Sealed bool` field to `Package`. After `InitializeUserEnv()` and `LoadLibrary()` complete, the host can seal the `lisp` package and any other core packages. `Package.Put()` and `Package.Update()` should return errors when the package is sealed. This prevents Lisp code from overwriting builtins or modifying the core language. `in-package` should refuse to switch into sealed packages (or only allow read access). This is the most important security boundary for embedding ELPS in multi-tenant or untrusted-input scenarios.

### Recommendation 5: Add parser depth and size limits
**Addresses:** SC-1, SC-11, UCA-18, UCA-19, H-1, H-2, L-1

The parser is the first code to process untrusted input and currently has no resource limits. Add: (a) a nesting depth counter in `ParseList`/`ParseConsExpression` that returns a parse error when a configurable maximum (default: 1000) is exceeded, and (b) an input byte limit on the scanner (default: 10MB) that refuses to read past the threshold. These prevent both Go goroutine stack overflow (which is fatal and non-recoverable) and heap exhaustion from oversized flat input.

---

## Key Files Referenced

- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/env.go` -- LEnv evaluator, Eval(), FunCall, MacroCall, bind, load
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/runtime.go` -- Runtime shared state, MaxAllocBytes, conditionStack
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/stack.go` -- CallStack, depth limits, PushFID, TerminalFID
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/library.go` -- SourceLibrary, RelativeFileSystemLibrary, RootDir confinement
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/package.go` -- PackageRegistry, Package, Put/Update/Get
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/builtins.go` -- builtins including load-file, set, eval, concat, make-sequence
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/op.go` -- special operators including handler-bind, dotimes, let, in-package
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/macro.go` -- macro system, stampMacroExpansion, RegisterDefaultMacro
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisp.go` -- LVal type, panic-prone accessor methods, singletons
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/config.go` -- Config functions, WithMaximumStackHeight, WithLibrary
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisplib/libjson/json.go` -- JSON serializer with unbounded allocation
- `/Users/swood/work/src/github.com/luthersystems/elps/lisp/lisplib/libregexp/libregexp.go` -- regexp using Go RE2 (safe from ReDoS)
- `/Users/swood/work/src/github.com/luthersystems/elps/cmd/run.go` -- CLI entry point with unconstrained filesystem library
- `/Users/swood/work/src/github.com/luthersystems/elps/parser/rdparser/parser.go` -- Recursive descent parser, no depth/size limits
