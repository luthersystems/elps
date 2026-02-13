# HAZOP -- Hazard and Operability Study

## Date and Scope

| Field | Value |
|-------|-------|
| Date | 2026-02-13 |
| Scope | Full system HAZOP of the ELPS embedded Lisp interpreter |
| Module | `github.com/luthersystems/elps` |
| Method | Security-adapted HAZOP with guide words applied to interpreter subsystems |
| Analyst | Automated HAZOP (Claude Opus 4.6), subject to expert review |

---

## 1. Study Node Definitions

| ID | Study Node | Design Intent | Key Source Files |
|----|-----------|---------------|------------------|
| SN-1 | Parser and Lexer | Tokenize and parse Lisp source into a well-formed AST of `LVal` nodes, rejecting malformed input with clear errors | `parser/rdparser/parser.go`, `parser/lexer/lexer.go`, `parser/token/scanner.go` |
| SN-2 | Evaluator and Scoping | Walk the AST with tree-structured scoping, evaluate expressions, dispatch function calls, perform tail-recursion optimization, expand macros | `lisp/env.go`, `lisp/op.go`, `lisp/macro.go` |
| SN-3 | Resource Control and Stack Management | Enforce bounded computation via call stack depth limits (`MaxHeightLogical`, `MaxHeightPhysical`), allocation caps (`MaxAlloc`), and tail-recursion optimization | `lisp/stack.go`, `lisp/runtime.go`, `lisp/builtins.go` (make-sequence) |
| SN-4 | Package System and Symbol Binding | Manage namespaced symbol tables, package imports/exports, symbol resolution (qualified and unqualified), and binding immutability for constants (`true`, `false`) | `lisp/package.go`, `lisp/env.go` (Get/Put/Update) |
| SN-5 | File Loading and Source Library | Load and evaluate external Lisp source from the filesystem, resolve relative paths, enforce directory confinement (`RootDir`), handle symlinks | `lisp/library.go`, `lisp/loader.go`, `lisp/env.go` (Load/LoadFile) |
| SN-6 | Standard Library (JSON, Regexp, String) | Provide stdlib packages for JSON serialization/deserialization, regex compilation/matching, string operations, with protection against untrusted input | `lisp/lisplib/libjson/json.go`, `lisp/lisplib/libregexp/libregexp.go`, `lisp/lisplib/libstring/libstring.go` |
| SN-7 | Error Handling and Condition System | Propagate errors as first-class `LError` values with stack traces, provide `handler-bind`/`rethrow` condition handling, maintain condition stack integrity | `lisp/error.go`, `lisp/conditions.go`, `lisp/op.go` (opHandlerBind), `lisp/builtins.go` (builtinRethrow) |

---

## 2. Security-Adapted Guide Word Definitions

| Guide Word | Definition | Example in ELPS Context |
|-----------|-----------|------------------------|
| SPOOFED | A value, identity, or type is forged or misrepresented | A symbol pretends to be a package-qualified name to access internals |
| INJECTED | Unauthorized code or data is inserted into the processing pipeline | Untrusted string passed to `load-string` executes arbitrary Lisp |
| REPLAYED | A previously valid input or state is reused out of context | A captured closure referencing a stale environment is re-invoked |
| ESCALATED | An operation gains privileges or access beyond its intended scope | User code redefines a builtin to bypass safety checks |
| EXFILTRATED | Data is leaked through an unintended channel | Error messages expose internal file paths or Go stack traces |
| TAMPERED | Data integrity is compromised through unauthorized modification | Singleton `Nil()` or `Bool()` values are mutated, corrupting global state |
| BYPASSED | A safeguard or validation check is circumvented | Path traversal with `..` escapes `RootDir` confinement in `load-file` |
| EXHAUSTED | A resource is consumed to the point of denial of service | Deeply nested s-expressions blow the Go goroutine stack before `MaxHeight` is checked |
| PERSISTED | Unintended state persists across boundaries (calls, packages, sessions) | Runtime `conditionStack` leaks between handler-bind invocations due to missing cleanup |
| ENUMERATED | Internal structure is discovered through probing | Package names and symbol bindings are enumerable, revealing the application's API surface |

---

## 3. Guide Word Application Matrix (All Nodes)

The following matrix shows which guide word / study node combinations produce meaningful deviations. Cells marked with risk indicators: H = High, M = Medium, L = Low, -- = Not applicable or negligible.

| Guide Word | SN-1 Parser | SN-2 Evaluator | SN-3 Resources | SN-4 Packages | SN-5 File Load | SN-6 Stdlib | SN-7 Errors |
|-----------|------------|----------------|----------------|---------------|----------------|-------------|-------------|
| SPOOFED | L | M | -- | H | M | M | L |
| INJECTED | M | H | -- | M | H | H | L |
| REPLAYED | L | M | -- | L | L | L | M |
| ESCALATED | L | H | M | H | M | M | L |
| EXFILTRATED | L | L | L | M | M | M | H |
| TAMPERED | M | H | M | M | L | L | M |
| BYPASSED | L | M | H | M | H | M | M |
| EXHAUSTED | H | H | H | L | M | H | L |
| PERSISTED | L | M | L | M | L | M | H |
| ENUMERATED | L | L | L | H | M | L | M |

---

## 4. Detailed Worksheets -- Top 3 Highest-Risk Study Nodes

### 4.1 Worksheet: SN-2 Evaluator and Scoping (Highest Risk)

**Design Intent:** Walk the AST with tree-structured scoping, evaluate expressions correctly, dispatch function calls, perform TRO safely, and expand macros faithfully.

| # | Guide Word | Deviation | Possible Causes | Consequences | Existing Safeguards | P | C | Risk (PxC) | Recommended Actions |
|---|-----------|-----------|-----------------|--------------|---------------------|---|---|------------|---------------------|
| 2.1 | INJECTED | Arbitrary code execution via `eval`, `load-string`, or `load-bytes` on untrusted input | Application passes user-controlled strings to `builtinLoadString` or `builtinEval` (`builtins.go:404-427`, `builtins.go:756-762`). `eval` calls `shallowUnquote` then `env.Eval`, executing anything. | Full access to all builtins including `load-file` (filesystem read), `debug-print` (stderr write), package manipulation, and potentially custom builtins registered by the embedding application. | `load-string` evaluates in root env (line 422), TROBlock set (line 421). No sandboxing of builtins available to evaluated code. | 4 | 5 | **20** | Provide a `SafeEnv` or configurable builtin allowlist so embedders can restrict what `eval`/`load-string` can access. Document that these functions MUST NOT receive untrusted input without sandboxing. |
| 2.2 | TAMPERED | Singleton mutation corrupts global interpreter state | `stampMacroExpansion` in `macro.go:233-251` walks expanded AST and writes `v.Source = callSite`. If a macro expansion returns a shared singleton (e.g., `singletonNil`), the guard at line 242 checks `len(v.Cells) == 0` but only for `LSExpr`. A future code change or new singleton type could bypass this guard. | All subsequent `Nil()` calls would return a value with a corrupted `Source` field, leading to wrong error locations across the entire interpreter session. | Guard in `stampMacroExpansion` (line 242-244) skips empty `LSExpr`. Comment in `lisp.go:275-288` documents the invariant. No runtime assertion enforces immutability. | 2 | 5 | **10** | Add a `frozen` flag or use Go's `sync.Once` pattern to make singletons provably immutable. Consider making `stampMacroExpansion` check `v == singletonNil || v == singletonTrue || v == singletonFalse` explicitly rather than relying on structural checks. |
| 2.3 | ESCALATED | User code redefines builtins or special operators | `builtinSet` in `builtins.go:533-554` calls `env.PutGlobal` which calls `pkg.Put` (`package.go:122-131`). `Put` only blocks rebinding `true` and `false`. A user can `(set 'if ...)` or `(set 'eval ...)` to replace any builtin, including security-critical ones. | Overriding `handler-bind` could suppress error handlers. Overriding `load-file` could redirect file loading. Overriding `set!` could remove mutation guards. In an embedding scenario, this allows untrusted code to compromise the host application's assumptions. | Constants `true`/`false` are protected in `Put` (line 126-128). No other symbols are protected from redefinition. | 3 | 4 | **12** | Add an optional `frozen` set of symbols per package that cannot be rebound after initialization. At minimum, protect the `lisp` lang package symbols after `InitializeUserEnv` completes. |
| 2.4 | EXHAUSTED | Macro expansion produces exponential AST growth | A user-defined macro can return an expansion that is quadratically or exponentially larger than its input. `MacroCall` in `env.go:912-960` expands, stamps source locations via `stampMacroExpansion` (which walks the full expansion), then the expansion is re-evaluated by `Eval` (line 866-867). No limit on expansion size. | Memory exhaustion or extreme latency before stack depth limits are hit. The Go process OOMs. | Stack depth limits (`MaxHeightLogical` = 50000, `MaxHeightPhysical` = 25000) bound recursion depth but not breadth. `TROBlock` prevents TRO from hiding depth, but width explosion is unbounded. | 3 | 4 | **12** | Add a configurable macro expansion depth counter (number of successive macro expansions) and an AST node count limit. Abort expansion when either threshold is exceeded. |
| 2.5 | BYPASSED | TRO incorrectly collapses stack frames, losing state | `TerminalFID` in `stack.go:113-145` scans backward for terminal frames. If `TROBlock` is not set correctly by a special operator (e.g., a custom builtin using `env.Terminal`), TRO may collapse frames that contain live bindings needed by unevaluated expressions in the collapsed `let`/`handler-bind`. | Silent incorrect evaluation -- bindings resolve to wrong values or become unbound. Extremely difficult to debug since the result looks valid but is semantically wrong. | `TROBlock` is set in `opHandlerBind` (line 665), `opIgnoreErrors` (line 708), `MacroCall` (line 933), and `builtinLoadString` (line 421). `TerminalFID` panics on TROBlock inconsistency (line 135). | 2 | 4 | **8** | Add a test suite that specifically verifies TRO correctness for all special operators. Consider making `TROBlock` the default for custom builtins registered via `RegisterDefaultBuiltin`, requiring explicit opt-in for TRO. |
| 2.6 | PERSISTED | Package swap via `defer` in `env.call` can be corrupted by panic | In `env.go:1191-1201`, `env.call` saves `outer := env.Runtime.Package`, swaps to the function's package, and defers restoration. If the function body triggers a Go panic (e.g., via a nil pointer in a native builtin), the deferred restore runs but the caller may catch the panic and continue with a partially valid state. | `env.Runtime.Package` is restored but the call stack (`Stack.Pop` in the caller's `defer`) may be in an inconsistent state. Subsequent evaluations see the wrong package context. | `defer` in `env.go:1197-1199` always restores. `Stack.Pop` in callers (`funCall`, `SpecialOpCall`, `MacroCall`) also uses `defer`. Both run on panic, but ordering between multiple defers across stack frames depends on recovery location. | 2 | 3 | **6** | Add a `recover()` guard in `env.call` that validates stack and package state consistency before re-panicking. Consider a single cleanup struct that atomically restores all state. |

### 4.2 Worksheet: SN-5 File Loading and Source Library (Second Highest Risk)

**Design Intent:** Load and evaluate external Lisp source from the filesystem safely, resolving relative paths, enforcing directory confinement, and preventing unauthorized filesystem access.

| # | Guide Word | Deviation | Possible Causes | Consequences | Existing Safeguards | P | C | Risk (PxC) | Recommended Actions |
|---|-----------|-----------|-----------------|--------------|---------------------|---|---|------------|---------------------|
| 5.1 | BYPASSED | Path traversal escapes `RootDir` confinement | `RelativeFileSystemLibrary.LoadSource` (`library.go:93-116`) resolves symlinks via `filepath.EvalSymlinks` (line 102-108) and checks `strings.HasPrefix`. Race condition: a symlink could be created after `EvalSymlinks` but before `os.ReadFile` (TOCTOU). Also, if `EvalSymlinks` fails (returns error), the unresolved `loc` is used at line 105-106 (`resolvedLoc := loc`), meaning a path with `..` components that cannot be symlink-resolved would be checked against `root` using the cleaned-but-not-fully-resolved path. | Attacker-controlled Lisp code reads arbitrary files on the host filesystem via `(load-file "../../../etc/passwd")`. In an embedding scenario, this could leak secrets or configuration. | `filepath.Clean` (line 97) normalizes `..` components. `strings.HasPrefix` check (line 109). `EvalSymlinks` resolves symlinks. `FSLibrary` uses `fs.FS` which inherently rejects `..`. | 3 | 5 | **15** | Prefer `FSLibrary` over `RelativeFileSystemLibrary` in security-sensitive contexts. For `RelativeFileSystemLibrary`, open the file first, then verify the opened file descriptor's real path via `os.Stat`/`os.Readlink` to eliminate TOCTOU. Document the TOCTOU risk. |
| 5.2 | INJECTED | Loaded file contains malicious code that modifies the host environment | `builtinLoadFile` (`builtins.go:454-470`) calls `env.root().LoadFile(loc.Str)` which evaluates all expressions in the loaded file in the root environment. The loaded code has full access to define packages, override builtins, and call `load-file` recursively. | Loaded code can redefine builtins, install trojan functions, modify package exports, and chain-load additional files. A single trusted `load-file` of a compromised source file compromises the entire environment. | `TROBlock` set on the stack frame (line 464). Code runs in root env (line 465), not the caller's lexical env. No restriction on what builtins the loaded code can access. | 3 | 5 | **15** | Provide an option to load files in a restricted environment that cannot define packages or override existing bindings. Add a file-hash verification mechanism for trusted source loading. |
| 5.3 | EXHAUSTED | Recursive `load-file` calls exhaust filesystem handles or stack | A file A loads file B which loads file A, creating infinite recursion. Each `LoadFile` call opens a file handle, reads it, and pushes a stack frame. The stack depth limit will eventually trigger, but each frame holds file contents in memory. | Memory exhaustion from accumulated file contents before the stack limit is hit. On systems with low file descriptor limits, file handle exhaustion. | Stack depth limits (`MaxHeightLogical`/`MaxHeightPhysical`) in `stack.go`. `TROBlock` prevents TRO from hiding the recursion depth. Files are read into memory fully via `os.ReadFile` before parsing. | 2 | 3 | **6** | Add a `load-file` recursion depth limit (or loaded-file set to detect cycles). Consider a configurable total loaded-bytes limit. |
| 5.4 | EXFILTRATED | Error messages reveal filesystem structure | When `LoadSource` fails (e.g., file not found), the error message includes the full resolved path (`library.go:110`): `"access denied: %s is outside root directory %s"`. Normal `os.ReadFile` errors also include the path. | Attacker can probe the filesystem structure by observing error messages from `load-file` calls with various paths. Reveals `RootDir` configuration and file existence. | None -- error messages are passed directly to the Lisp environment and visible to user code via `handler-bind`. | 3 | 3 | **9** | Sanitize error messages from `LoadSource` to not reveal the absolute `RootDir` path. Return a generic "file not found or access denied" message without path details. |
| 5.5 | SPOOFED | Malicious file masquerades as trusted source | If `RootDir` is not set (the default for `RelativeFileSystemLibrary`), any file path is accepted. An attacker who can write a file to a predictable location (e.g., `/tmp`) can trick the application into loading it. | Arbitrary code execution in the ELPS environment context. | `RootDir` confinement (when configured). `FSLibrary` provides inherent confinement. Neither is the default -- `RelativeFileSystemLibrary{}` with empty `RootDir` is used by `lisplib.NewDocEnv()` (line 82). | 3 | 4 | **12** | Make `RootDir` (or `FSLibrary`) the default in documentation and examples. Emit a warning when `RelativeFileSystemLibrary` is used without `RootDir`. |

### 4.3 Worksheet: SN-3 Resource Control and Stack Management (Third Highest Risk)

**Design Intent:** Enforce bounded computation through call stack depth limits, allocation size caps, and tail-recursion optimization to prevent denial-of-service from runaway or malicious Lisp code.

| # | Guide Word | Deviation | Possible Causes | Consequences | Existing Safeguards | P | C | Risk (PxC) | Recommended Actions |
|---|-----------|-----------|-----------------|--------------|---------------------|---|---|------------|---------------------|
| 3.1 | EXHAUSTED | Deeply nested s-expressions exhaust the Go goroutine stack during parsing | The recursive-descent parser (`rdparser/parser.go`) uses Go function call recursion for nested expressions. `ParseConsExpression` calls `ParseExpression` which calls `parseExpression` which calls `ParseConsExpression` again. No depth limit in the parser. Input like `(((((...))))` with depth > 8000 overflows the default 1MB goroutine stack. | Go runtime panic (`runtime: goroutine stack exceeds ...`), crashing the entire Go process -- not just the ELPS interpreter. This is a hard crash, unrecoverable by `handler-bind`. | None in the parser. The ELPS `CallStack` limits only apply during evaluation, not parsing. Go's goroutine stack grows dynamically but has a default maximum (1GB or `runtime.GOMAXPROCS`-dependent). | 4 | 5 | **20** | Add a parse depth counter in the `Parser` struct. Increment on `ParseConsExpression`/`ParseList` entry, decrement on exit. Return an `LError` when depth exceeds a configurable limit (e.g., 10000). |
| 3.2 | EXHAUSTED | `make-sequence` allocation limit is per-call, but chained calls bypass it | `builtinMakeSequence` (`builtins.go:1714-1718`) checks `maxAlloc := env.Runtime.MaxAllocBytes()` per call. But `(concat 'list (make-sequence 0 10000000) (make-sequence 0 10000000))` creates two 10M-element lists, then concatenates them into a 20M-element list. The concatenation itself (`builtinConcat`) has no allocation limit check. | Memory exhaustion through multiple individually-within-limit allocations that together exceed available memory. The 10MB default per allocation still allows ~80MB of integer list data per call. | `MaxAlloc` check in `builtinMakeSequence`. `string:repeat` also checks. `builtinConcat` does not check. | 3 | 3 | **9** | Add allocation limit checks to `builtinConcat`, `builtinAppend`, and other functions that create new sequences from existing ones. Consider a global memory budget (total allocated LVal count) in addition to per-call limits. |
| 3.3 | BYPASSED | Logical stack height tracking is defeated by non-recursive patterns | `MaxHeightLogical` tracks the "effective" recursion depth by incrementing `HeightLogical` on push (`stack.go:150-152`). TRO collapses frames but maintains `HeightLogical` (`env.go:999`, `env.go:1069`). However, a program that creates many small lambdas and calls them in sequence (not recursively) has physical stack growth limited by `MaxHeightPhysical` (25000) but each lambda call resets -- the logical height only increments by 1 per nested call, not per sequential call. | With `MaxHeightPhysical = 25000`, each frame consuming ~hundreds of bytes of Go stack, the Go goroutine could grow to tens of MB. If many goroutines each run ELPS interpreters, combined memory pressure could cause OOM. | `MaxHeightPhysical` limits the physical frame count. `MaxHeightLogical` limits effective depth including TRO-elided frames. Default limits are generous (50K/25K). | 2 | 3 | **6** | Document that embedders running multiple ELPS interpreters concurrently should tune stack limits downward. Consider an optional wall-clock timeout or instruction counter for CPU-bound DoS prevention. |
| 3.4 | TAMPERED | `decrementMarkTailRec` directly mutates an `LVal.Cells[0].Int` field | `decrementMarkTailRec` in `env.go:1091-1097` does `mark.Cells[0].Int--`. This mutates the `Int` field of an `LVal` created by `markTailRec` (`lisp.go:654-658`). The `Int(npop)` call at line 657 creates a fresh LVal, so this is safe today. But if `Int()` were ever changed to return cached/interned small integers (a common optimization), this mutation would corrupt the cache. | Silent corruption of small integer values throughout the interpreter if integer interning is added in the future. | `Int()` currently always allocates a fresh `LVal` (`lisp.go:306-311`). No integer interning exists. | 1 | 5 | **5** | Add a comment at `decrementMarkTailRec` explicitly noting the mutation and that `Int()` must not intern values. Alternatively, use a non-LVal counter to avoid mutating LVals entirely. |
| 3.5 | BYPASSED | No CPU/instruction limit -- infinite loop without recursion is unbounded | A loop like `(dotimes (i 999999999) nil)` runs for ~1 billion iterations. `dotimes` (`op.go:428-480`) has no check against an iteration/instruction limit. The stack depth doesn't grow (single `loopenv`), and there is no allocation. | CPU denial-of-service. The ELPS interpreter blocks the Go goroutine indefinitely. In a server context, this is a resource exhaustion attack. | None. No instruction counter or timeout mechanism exists. `dotimes` is bounded by its `count` argument, but that can be `INT_MAX`. | 4 | 4 | **16** | Add a configurable instruction counter or step limit to the `Runtime`. Decrement on each `Eval` call. Return an error when exhausted. Alternatively, support `context.Context` cancellation for cooperative timeout. |
| 3.6 | EXHAUSTED | `format-string` with large repetition creates unbounded output | `builtinFormatString` substitutes `{}` placeholders. If a placeholder value is a very large string and there are many placeholders, the resulting string can be much larger than `MaxAlloc`. `format-string` does not check output size. | Memory exhaustion through string concatenation in format operations. | None for `format-string`. `string:repeat` checks `MaxAlloc`. | 3 | 3 | **9** | Add an output size check to `builtinFormatString` that compares the total output length against `MaxAllocBytes()`. |

---

## 5. Interaction Matrix

This NxN matrix shows how a deviation in one study node (row) can cause or amplify a deviation in another (column). Entries describe the interaction mechanism.

| Affected -> | SN-1 Parser | SN-2 Evaluator | SN-3 Resources | SN-4 Packages | SN-5 File Load | SN-6 Stdlib | SN-7 Errors |
|------------|------------|----------------|----------------|---------------|----------------|-------------|-------------|
| **SN-1 Parser** | -- | Malformed AST causes eval panic | Deep nesting exhausts Go stack before Resource limits apply | -- | Parsed load-file path is attacker-controlled | -- | Parser error messages leak source structure |
| **SN-2 Evaluator** | eval/load-string re-enters parser | -- | Macro expansion amplifies resource consumption | Eval resolves symbols, can redefine packages | Eval dispatches load-file calls | Eval calls stdlib builtins | Eval errors propagate through condition system |
| **SN-3 Resources** | -- | Stack overflow terminates eval with LError | -- | -- | Each load-file consumes stack + memory | Stdlib alloc limits interact with global limits | Resource errors are LError values |
| **SN-4 Packages** | -- | Symbol shadowing changes eval semantics | Redefining builtins can remove resource checks | -- | in-package changes load context | Stdlib functions are package-scoped | Error condition names are symbols |
| **SN-5 File Load** | Loaded source is re-parsed | Loaded expressions are evaluated | Each load adds stack frames + memory | Loaded code can define/modify packages | -- | Loaded code can call stdlib | Load errors propagate as LError |
| **SN-6 Stdlib** | JSON/regex accept untrusted input strings | Stdlib builtins call env.FunCall for callbacks | string:repeat, make-sequence have alloc limits | Stdlib packages use package system | json:load-string can trigger eval-like behavior | -- | Stdlib errors use condition system |
| **SN-7 Errors** | -- | handler-bind alters control flow | Condition stack uses memory | Error messages may reference package names | File load errors reveal paths | -- | -- |

---

## 6. Critical Interaction Scenarios

### Scenario 1: Untrusted Input -> Code Injection -> Full Environment Compromise

**Chain:** SN-6 (JSON) -> SN-2 (Evaluator) -> SN-4 (Packages) -> SN-5 (File Loading)

An embedding application deserializes untrusted JSON via `json:load-string`, producing an ELPS sorted-map. The application extracts a string field and passes it to `load-string` for "template evaluation." The injected string contains:

```lisp
(progn
  (set 'load-file (lambda (path) (debug-print "intercepted" path) ()))
  (in-package "lisp")
  (set 'handler-bind (lambda (&rest args) ())))
```

This redefines `load-file` to intercept all subsequent file loads and `handler-bind` to silently swallow all errors. The application continues running but is now blind to errors and loading attacker-controlled code.

**Risk:** P=3, C=5, Score=15. Triggered whenever an embedder bridges untrusted data to `eval`/`load-string`.

### Scenario 2: Parser Stack Overflow -> Go Process Crash -> Service Denial

**Chain:** SN-1 (Parser) -> SN-3 (Resources) -> Host Process

An attacker submits a Lisp source string containing 50,000 nested parentheses: `(((((...))))`. The recursive-descent parser in `rdparser/parser.go` recurses through `ParseConsExpression` -> `ParseExpression` -> `parseExpression` -> `ParseConsExpression`. Each level consumes Go stack space (~200-400 bytes per frame). At ~50K depth, the Go goroutine stack (default max 1GB, but grows slowly) causes a `runtime.StackOverflow` panic that cannot be caught by `recover()` in all Go versions and crashes the entire process.

This bypass occurs entirely in the parser, before any ELPS resource limits (`MaxHeightLogical`, `MaxHeightPhysical`) are checked, because those limits apply only during evaluation.

**Risk:** P=4, C=5, Score=20. Any endpoint accepting Lisp source from untrusted input is vulnerable.

### Scenario 3: Infinite Loop + Error Suppression -> Silent Service Hang

**Chain:** SN-2 (Evaluator) -> SN-7 (Errors) -> SN-3 (Resources)

Untrusted code executes:

```lisp
(handler-bind ((condition (lambda (&rest args) ())))
  (dotimes (i 2000000000) nil))
```

The `handler-bind` suppresses all errors (though `dotimes` won't produce any). The loop runs for 2 billion iterations with no stack growth (single loop environment in `opDoTimes`), no allocation, and no existing mechanism to interrupt it. The Go goroutine is blocked for hours or until the process is killed.

Even if the application tries to enforce a timeout at the Go level, ELPS provides no cooperative cancellation mechanism (no `context.Context` integration, no instruction counter).

**Risk:** P=4, C=4, Score=16. Any scenario where untrusted code can execute loops.

### Scenario 4: Symlink Race -> File Confinement Bypass -> Data Exfiltration

**Chain:** SN-5 (File Load) -> SN-7 (Errors) -> SN-6 (Stdlib)

When `RelativeFileSystemLibrary` is used with `RootDir` set:

1. Attacker creates `/rootdir/link` -> `/rootdir/safe.lisp` (passes `EvalSymlinks` check).
2. Between `EvalSymlinks` (line 102-106) and `os.ReadFile` (line 114), attacker atomically replaces `/rootdir/link` with a symlink to `/etc/shadow`.
3. `os.ReadFile` follows the new symlink and reads the target.
4. The file contents (which are not valid Lisp) produce a parse error containing the raw bytes of the target file in the error message.
5. The error is caught by `handler-bind` and the error message (containing file data) is extractable.

**Risk:** P=2, C=5, Score=10. Requires attacker with filesystem write access within `RootDir` and precise timing.

### Scenario 5: Builtin Redefinition -> Safety Bypass -> Memory Exhaustion

**Chain:** SN-4 (Packages) -> SN-2 (Evaluator) -> SN-3 (Resources)

Untrusted code runs:

```lisp
(in-package "lisp")
(set 'make-sequence
  (let ([original make-sequence])
    (lambda (start stop &rest step)
      ;; Remove the allocation check by directly building the list
      (foldl (lambda (acc i) (cons i acc)) '()
             (funcall original start (min stop (+ start 100)))))))
```

Wait -- this doesn't actually bypass the check directly because the original `make-sequence` still enforces it. But the attacker could instead do:

```lisp
(defun bomb () (concat 'list (bomb) (bomb)))
```

This creates an exponentially growing list. Each `concat` doubles the size. The resource limit on `make-sequence` does not apply to `concat`. After ~30 recursive doublings (within the 50K stack limit), the concatenated list would require 2^30 = 1 billion elements, exhausting memory.

**Risk:** P=3, C=4, Score=12. Exploits the gap between per-call allocation limits and global memory.

---

## 7. Prioritized Risk Scores

All deviations scored P >= 2 and C >= 3, sorted by risk score:

| Rank | Risk Score | Node | # | Deviation Summary |
|------|-----------|------|---|-------------------|
| 1 | **20** | SN-3 | 3.1 | Parser stack overflow from deeply nested input crashes Go process |
| 2 | **20** | SN-2 | 2.1 | Code injection via `eval`/`load-string` on untrusted input |
| 3 | **16** | SN-3 | 3.5 | No CPU/instruction limit; infinite loops block goroutine indefinitely |
| 4 | **15** | SN-5 | 5.1 | Path traversal via TOCTOU symlink race bypasses `RootDir` |
| 5 | **15** | SN-5 | 5.2 | Loaded file injects malicious code into host environment |
| 6 | **12** | SN-2 | 2.3 | User code redefines builtins to bypass safety checks |
| 7 | **12** | SN-2 | 2.4 | Macro expansion produces exponential AST growth |
| 8 | **12** | SN-5 | 5.5 | Unconfigured `RootDir` allows loading from arbitrary paths |
| 9 | **10** | SN-2 | 2.2 | Singleton mutation via `stampMacroExpansion` corrupts global state |
| 10 | **9** | SN-3 | 3.2 | Chained allocations bypass per-call `MaxAlloc` limit |
| 11 | **9** | SN-3 | 3.6 | `format-string` output size is unbounded |
| 12 | **9** | SN-5 | 5.4 | Error messages reveal filesystem structure |
| 13 | **8** | SN-2 | 2.5 | TRO incorrectly collapses frames, causing wrong evaluation |
| 14 | **6** | SN-2 | 2.6 | Package swap corruption on panic in `env.call` |
| 15 | **6** | SN-3 | 3.3 | Sequential calls bypass logical stack height tracking |
| 16 | **6** | SN-5 | 5.3 | Recursive `load-file` exhausts memory before stack limit triggers |
| 17 | **5** | SN-3 | 3.4 | `decrementMarkTailRec` mutates LVal, fragile to future interning |

---

## 8. Team Composition and Effort Estimate

### Recommended HAZOP Team

| Role | Skills Required | Responsibility |
|------|----------------|----------------|
| Study Leader | HAZOP methodology, facilitation, risk assessment | Facilitate sessions, maintain worksheets, track actions |
| Interpreter Architect | Deep knowledge of `lisp/env.go`, `lisp/op.go`, evaluation semantics | Explain design intent, identify deviations in evaluator and TRO |
| Security Engineer | Application security, input validation, sandboxing patterns | Assess exploitation feasibility, validate safeguards, score risk |
| Go Systems Engineer | Go runtime internals (goroutine stacks, GC, panic/recover) | Assess Go-level impacts (goroutine stack overflow, memory, concurrency) |
| Embedding Application Developer | Domain knowledge of how ELPS is embedded | Identify real-world exposure scenarios, validate consequence severity |
| Scribe | Technical writing, HAZOP documentation | Record deviations, causes, consequences, actions |

### Effort Estimate

| Activity | Duration | Participants |
|----------|----------|-------------|
| Preparation (materials, node definitions, pre-read) | 2 days | Study Leader, Scribe |
| Session 1: SN-1 Parser, SN-5 File Loading | 4 hours | Full team |
| Session 2: SN-2 Evaluator, SN-4 Packages | 6 hours | Full team |
| Session 3: SN-3 Resources, SN-6 Stdlib, SN-7 Errors | 4 hours | Full team |
| Session 4: Interaction scenarios, cross-node effects | 3 hours | Full team |
| Post-study: Documentation, action tracking, risk ranking | 3 days | Study Leader, Scribe, Security Engineer |
| **Total** | **~5 days (35 person-hours of sessions + 5 days prep/doc)** | **6 people** |

---

## 9. Living HAZOP Maintenance Triggers

The HAZOP should be re-studied (partially or fully) when any of the following occur:

| Trigger | Scope | Rationale |
|---------|-------|-----------|
| New builtin, special operator, or macro added | Affected study node (SN-2, SN-3, SN-4) | New operations may introduce new deviation patterns or bypass existing safeguards |
| Change to `LVal` type system (new `LType` constant) | Full re-study | `LVal` is the universal type; changes affect every node |
| Change to `CallStack`, TRO logic, or `MaxHeight` defaults | SN-3 Resource Control | Directly affects the primary DoS safeguard |
| New `SourceLibrary` implementation or change to path resolution | SN-5 File Loading | Path traversal and confinement are high-risk areas |
| New stdlib package added (especially I/O, network, or OS access) | SN-6 Stdlib + interaction matrix | New external interfaces expand the attack surface |
| Singleton or interning optimization introduced | SN-2 Evaluator (mutation risks) | Singleton mutation is a latent high-consequence risk |
| ELPS used in a new embedding context (e.g., multi-tenant, network-facing) | Full re-study with new threat model | Different embedding contexts have different exposure profiles |
| `context.Context` or cancellation mechanism added | SN-3 Resource Control | Changes the CPU exhaustion risk profile |
| Go runtime version upgrade affecting goroutine stack behavior | SN-1 Parser, SN-3 Resources | Parser stack overflow risk depends on Go stack growth behavior |
| Any security vulnerability reported or CVE filed | Affected node + adjacent nodes in interaction matrix | Real-world validation may reveal missed deviations |

---

## 10. Top 5 Recommendations

### Recommendation 1: Add Parser Depth Limit (Risk Score 20, SN-3 #3.1)

**Problem:** The recursive-descent parser has no depth limit. Deeply nested input (50K+ levels) causes a Go goroutine stack overflow that crashes the entire host process, bypassing all ELPS-level error handling.

**Action:** Add a `maxDepth` counter to the `Parser` struct in `rdparser/parser.go`. Increment on entry to `ParseConsExpression` and `ParseList`, decrement on exit. Return an `LError` with condition `parse-error` when depth exceeds a configurable limit (default: 10,000). This is a 1-file change with high impact.

**Files:** `parser/rdparser/parser.go`

### Recommendation 2: Add Instruction Counter / Timeout Mechanism (Risk Score 16, SN-3 #3.5)

**Problem:** ELPS has no mechanism to bound CPU time. An infinite loop in `dotimes`, a tight `labels` recursion within stack limits, or any long-running computation blocks the Go goroutine indefinitely with no way to interrupt it.

**Action:** Add an optional `StepLimit uint64` field to `Runtime`. Decrement in `LEnv.Eval` on each invocation. When it reaches zero, return an `LError` with condition `step-limit-exceeded`. Alternatively or additionally, integrate `context.Context` into the `Runtime` and check `ctx.Done()` periodically during evaluation.

**Files:** `lisp/runtime.go`, `lisp/env.go`

### Recommendation 3: Provide Builtin Freezing / Sandboxing for Untrusted Code (Risk Scores 20+12, SN-2 #2.1, #2.3)

**Problem:** `eval`, `load-string`, and `load-bytes` execute arbitrary Lisp with full access to all builtins. User code can redefine any symbol including `if`, `handler-bind`, `load-file`, etc. Embedders have no built-in way to restrict what untrusted code can do.

**Action:** (a) Add a `Package.Frozen bool` flag that prevents `Put` from modifying any existing binding. Set it on the `lisp` package after `InitializeUserEnv`. (b) Provide a `RestrictedEnv` constructor that omits dangerous builtins (`load-file`, `load-string`, `load-bytes`, `debug-print`, `in-package`) for evaluating untrusted code. Document that `eval`/`load-string` MUST NOT be used on untrusted input without these restrictions.

**Files:** `lisp/package.go`, `lisp/env.go`, `lisp/builtins.go`

### Recommendation 4: Add Allocation Limits to All Sequence-Creating Builtins (Risk Score 9, SN-3 #3.2, #3.6)

**Problem:** `MaxAlloc` is checked in `make-sequence` and `string:repeat`, but not in `concat`, `append`, `zip`, `map`, `format-string`, or other builtins that create new sequences. Chained calls can produce arbitrarily large data structures.

**Action:** Add `MaxAlloc` checks to `builtinConcat`, `builtinAppend`, `builtinZip`, `builtinMap` (output vector/list size), and `builtinFormatString` (output string length). Use a helper function `checkAlloc(env, n) *LVal` that centralizes the check.

**Files:** `lisp/builtins.go`, `lisp/lisplib/libstring/libstring.go`

### Recommendation 5: Default to Confined File Loading (Risk Scores 15+12, SN-5 #5.1, #5.5)

**Problem:** `RelativeFileSystemLibrary` with empty `RootDir` (the current default in `lisplib.NewDocEnv` and most examples) allows loading any file on the filesystem. Even when `RootDir` is set, a TOCTOU race condition with symlinks can bypass confinement.

**Action:** (a) Change `lisplib.NewDocEnv` and all examples to use `FSLibrary` with `os.DirFS()` instead of `RelativeFileSystemLibrary`. (b) In `RelativeFileSystemLibrary.LoadSource`, use `os.Open` + `fd.Stat` to verify the real path of the opened file descriptor, eliminating TOCTOU. (c) Emit a runtime warning (to `Runtime.Stderr`) when `RelativeFileSystemLibrary` is used without `RootDir` set.

**Files:** `lisp/library.go`, `lisp/lisplib/lisplib.go`
