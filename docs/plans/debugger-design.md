# ELPS Debugger Design Document

**Date:** 2026-02-12
**Status:** Draft / RFC
**Author:** Claude (with Sam)
**Related:** PR #21 (2020, never merged), `docs/plans/lsp-support-enhancements.md`

---

## 1. Context & Motivation

PR #21 (2020) attempted to add a debugger with two backends: Delve and DAP.
That code was never merged and no longer exists in the repo. This document
revisits the idea with a modern lens, considering:

- The current ELPS architecture (no debugger code exists today)
- Modern tooling (DAP ecosystem maturation, editor landscape changes)
- The **embedded use case** (substrate and similar Go applications hosting ELPS)
- The non-negotiable requirement of **zero overhead on the production hot path**

### 1.1 Why not Delve?

Delve debugs **Go code**, not languages hosted on Go. It operates at the Go
runtime level (goroutines, Go stack frames, Go variables). It cannot:

- Set breakpoints on Lisp source lines
- Step through Lisp expressions
- Inspect Lisp variable bindings or the ELPS call stack
- Understand ELPS packages, macros, or tail recursion

Delve is the wrong tool. The 2020 PR's Delve backend panicked immediately
because Delve doesn't understand the interpreter's virtual call stack. A
debugger for ELPS must be **interpreter-level**, not runtime-level.

### 1.2 Why DAP?

The **Debug Adapter Protocol** (Microsoft, same lineage as LSP) is now the
universal standard for editor-debugger communication:

| Editor       | DAP Support | Notes |
|-------------|-------------|-------|
| VS Code     | Native      | DAP was designed for VS Code |
| Neovim      | nvim-dap    | Mature plugin, widely used |
| Helix       | Built-in    | Experimental but improving |
| JetBrains   | LSP4IJ      | DAP client plugin (all IDEs), CLion native since 2025.3 |
| Emacs       | dap-mode    | Stable, wraps DAP over LSP |
| Zed         | Built-in    | DAP support added 2025 |

**One DAP server = every editor.** This is the same bet LSP made for language
servers, and it paid off.

#### Go library: `google/go-dap` (v0.10.0)

- Provides DAP **types and codec** (message encoding/decoding)
- Does NOT provide a server framework (you build the TCP listener + handler)
- Used by Delve's own DAP server as the codec layer
- Apache 2.0 license, maintained by Google

#### Precedent: Go-hosted interpreters with debuggers

| Interpreter | Language | Debugger Approach |
|------------|----------|-------------------|
| **Yaegi**  | Go       | Internal `Debugger` type with SetBreakpoints, StepInto/Over/Out, Continue |
| **Goja**   | JavaScript | DAP-compatible debugger with CLI frontend |
| **GraalVM/Truffle** | Polyglot | Built-in DAP server, language-agnostic via AST tags |

Yaegi's approach (internal debugger interface + DAP adapter) is closest to
what ELPS should do.

---

## 2. Architecture Overview

Two cleanly separated layers:

```
┌─────────────────────────────────────────────────────────┐
│                    Editor / IDE                          │
│              (VS Code, Neovim, etc.)                     │
└────────────────────┬────────────────────────────────────┘
                     │ DAP over TCP/stdio
┌────────────────────▼────────────────────────────────────┐
│              DAP Server (Layer 2)                        │
│         lisp/x/debugger/dapserver/                       │
│                                                          │
│  - Listens on TCP or communicates via stdio              │
│  - Translates DAP messages ↔ Debugger interface calls    │
│  - Uses google/go-dap for codec                          │
│  - Manages session lifecycle                             │
└────────────────────┬────────────────────────────────────┘
                     │ Go interface calls
┌────────────────────▼────────────────────────────────────┐
│           Debugger Engine (Layer 1)                      │
│              lisp/debugger.go                            │
│                                                          │
│  - Debugger interface on Runtime                         │
│  - Breakpoint storage & matching                         │
│  - Step state machine (into/over/out/continue)           │
│  - Variable inspection (scope walking)                   │
│  - Expression evaluation in suspended context            │
│  - Call stack translation                                │
└────────────────────┬────────────────────────────────────┘
                     │ Hook calls (nil-checked)
┌────────────────────▼────────────────────────────────────┐
│           ELPS Interpreter Core                          │
│              lisp/env.go                                 │
│                                                          │
│  Eval() → EvalSExpr() → funCall() → call()              │
│  (existing hot path, untouched when debugger is nil)     │
└─────────────────────────────────────────────────────────┘
```

### 2.1 Why two layers?

**Layer 1 (Debugger Engine)** lives in the `lisp/` package (or a sub-package)
and has zero external dependencies. It provides the core debugging primitives:
pause, resume, step, breakpoints, inspect. This layer is usable by embedders
who want programmatic debugging without DAP (e.g., a custom REPL debugger, a
test harness, or a substrate-specific debug UI).

**Layer 2 (DAP Server)** is an optional adapter in `lisp/x/debugger/` that
translates between the DAP wire protocol and the Layer 1 interface. It depends
on `google/go-dap` but the interpreter core does not.

This separation means:
- **Substrate** can embed Layer 1 without pulling in DAP dependencies
- **The `elps` CLI** can start Layer 2 for editor integration
- **Tests** can exercise Layer 1 without network/protocol overhead
- **Future protocols** (if DAP is ever superseded) only replace Layer 2

---

## 3. Layer 1: Debugger Engine

### 3.1 The Debugger interface

```go
// lisp/debugger.go

// Debugger is called by the interpreter at key execution points.
// When Runtime.Debugger is nil, these calls are never made (zero cost).
type Debugger interface {
    // OnEval is called before evaluating any expression.
    // Returns true if the debugger wants execution to pause.
    // The env provides access to scope, stack, and current package.
    OnEval(env *LEnv, expr *LVal) bool

    // OnFunCall is called before a function is invoked (after args are evaluated).
    // fun is the function value, args is the evaluated argument list.
    OnFunCall(env *LEnv, fun, args *LVal)

    // OnFunReturn is called after a function returns.
    // fun is the function, result is the return value.
    OnFunReturn(env *LEnv, fun, result *LVal)

    // WaitIfPaused blocks until the debugger allows execution to continue.
    // Called when OnEval returns true.
    // Returns the action to take (Continue, StepInto, StepOver, StepOut).
    WaitIfPaused(env *LEnv, expr *LVal) DebugAction
}

type DebugAction int

const (
    DebugContinue DebugAction = iota
    DebugStepInto
    DebugStepOver
    DebugStepOut
)
```

### 3.2 Hook points in the interpreter

Only **three** touch points in `env.go`:

**Hook 1: `Eval()`** — expression-level granularity for line stepping

```go
func (env *LEnv) Eval(v *LVal) *LVal {
    env.Loc = v.Source
    if env.Runtime.Debugger != nil {  // nil check = zero cost when disabled
        if env.Runtime.Debugger.OnEval(env, v) {
            env.Runtime.Debugger.WaitIfPaused(env, v)
        }
    }
    // ... existing eval logic unchanged ...
}
```

**Hook 2: `funCall()`** — function entry (replaces/extends profiler hook)

```go
func (env *LEnv) funCall(fun, args *LVal) *LVal {
    if env.Runtime.Debugger != nil {
        env.Runtime.Debugger.OnFunCall(env, fun, args)
    }
    if env.Runtime.Profiler != nil {
        defer env.trace(fun)()
    }
    // ... existing funCall logic unchanged ...
}
```

**Hook 3: `funCall()` return path** — function exit

```go
    // At the end of funCall, before returning r:
    if env.Runtime.Debugger != nil {
        env.Runtime.Debugger.OnFunReturn(env, fun, r)
    }
    return r
```

### 3.3 Performance analysis: zero-cost when disabled

The critical question: **does adding these hooks slow down production code?**

When `Runtime.Debugger` is `nil`:
- Each hook is a **nil pointer check** (`CMPQ $0, offset(reg)` + `JE`)
- This is 1-2 nanoseconds, branch-predicted as not-taken
- The Go compiler may even inline and optimize this away
- Comparable to the existing `Runtime.Profiler != nil` check already in `funCall()`

**Verification plan:** Run the existing benchmarks (`BenchmarkEnvFunCallBuiltin`,
`BenchmarkEnvFunCallRecursion`, etc.) before and after, using `benchstat` to
confirm no statistically significant regression. The benchmark CI
(`.github/workflows/benchmark.yml`) will catch any drift.

The `Eval()` hook is the most frequently called — every expression traversal
hits it. But the nil check is effectively free on modern CPUs due to branch
prediction. The Profiler hook already proves this pattern works in `funCall()`.

### 3.4 Breakpoint storage

```go
// Breakpoint represents a location where execution should pause.
type Breakpoint struct {
    ID        int
    File      string
    Line      int
    Condition string           // optional: Lisp expression to evaluate
    HitCount  int              // how many times hit (for conditional breaks)
    Enabled   bool
}
```

Breakpoints are stored in the debugger engine (not on `Runtime`), indexed by
`file:line` for O(1) lookup during `OnEval`. The `OnEval` implementation
checks `expr.Source` against the breakpoint map.

### 3.5 Step state machine

The debugger engine tracks stepping state:

```
          ┌──────────┐
          │  Running  │ ←── Continue command
          └────┬─────┘
               │ breakpoint hit / step complete
          ┌────▼─────┐
          │  Paused   │ ←── WaitIfPaused blocks here
          └────┬─────┘
               │ user command
     ┌─────────┼──────────┬────────────┐
     ▼         ▼          ▼            ▼
  Continue  StepInto   StepOver    StepOut
```

**StepInto**: Pause on the next `OnEval` call regardless of depth.

**StepOver**: Record current stack depth. Pause on next `OnEval` where
stack depth <= recorded depth (i.e., don't descend into function calls).

**StepOut**: Record current stack depth. Pause on next `OnEval` where
stack depth < recorded depth (i.e., wait until current function returns).

Stack depth is available via `len(env.Runtime.Stack.Frames)`.

### 3.6 Variable inspection

When paused, the debugger can inspect variables by walking the `LEnv` chain:

```go
// InspectScope returns all bindings visible from the given environment.
func InspectScope(env *LEnv) []ScopeBinding {
    var bindings []ScopeBinding
    current := env
    for current != nil {
        for name, val := range current.Scope {
            bindings = append(bindings, ScopeBinding{
                Name:  name,
                Value: val,
                Env:   current,
            })
        }
        current = current.Parent
    }
    return bindings
}
```

This naturally produces scoped results: local bindings first, then enclosing
scopes, then package globals.

### 3.7 Expression evaluation in debug context

When paused, users expect to evaluate expressions in the current context
(the "Debug Console" / REPL). This is straightforward:

```go
// EvalInContext parses and evaluates an expression in the paused environment.
func EvalInContext(env *LEnv, source string) *LVal {
    expr, err := env.Runtime.Reader.Read("debug-eval", strings.NewReader(source))
    if err != nil {
        return env.ErrorConditionf("debug-eval", "%v", err)
    }
    return env.Eval(expr[0])
}
```

The paused `env` already has the correct scope chain, package context, and
runtime state. This is one of the advantages of an interpreter-level debugger.

### 3.8 Tail recursion interaction

ELPS performs tail recursion optimization (TRO) by collapsing stack frames.
During debugging, TRO makes stepping confusing because the stack appears to
"jump" when frames are elided.

**Recommendation:** When a debugger is attached, disable TRO. This means:
- The logical and physical stack heights are always equal
- Step commands behave predictably
- Stack frames are never elided

Implementation: In `funCall()`, skip the `TerminalFID()` check when
`Runtime.Debugger != nil`. This only affects debug sessions — production
code retains TRO.

**Trade-off:** Deeply recursive programs may hit the stack limit during
debugging. This is acceptable and matches how most debuggers work (e.g.,
Python's default recursion limit is much lower than production capacity).

---

## 4. Layer 2: DAP Server

### 4.1 Package structure

```
lisp/x/debugger/
    debugger.go          // Debugger engine implementation (Layer 1 impl)
    breakpoint.go        // Breakpoint storage, matching, conditions
    stepper.go           // Step state machine
    inspector.go         // Scope/variable inspection
    dapserver/
        server.go        // TCP listener, session management
        handler.go       // DAP message → Debugger method dispatch
        translate.go     // ELPS types → DAP types (stack frames, variables, etc.)
```

### 4.2 DAP message handling

The DAP server handles these key request types:

| DAP Request | ELPS Action |
|------------|-------------|
| `initialize` | Declare capabilities (breakpoints, stepping, eval, etc.) |
| `launch` | Start ELPS execution in a goroutine, attach debugger |
| `attach` | Attach to an already-running embedded ELPS instance |
| `setBreakpoints` | Update breakpoint map for a source file |
| `configurationDone` | Begin/resume execution |
| `continue` | Set action to DebugContinue, unblock WaitIfPaused |
| `next` | StepOver |
| `stepIn` | StepInto |
| `stepOut` | StepOut |
| `stackTrace` | Translate `Runtime.Stack.Frames` → DAP StackFrame[] |
| `scopes` | Return Local / Package / Global scope references |
| `variables` | Walk `env.Scope` for the requested scope reference |
| `evaluate` | EvalInContext for debug console expressions |
| `disconnect` | Detach debugger, resume execution |

### 4.3 The `attach` use case (embedded/substrate/shirotester)

This is the **critical differentiator** for ELPS. Most DAP servers only
support `launch` (the debugger starts the program). ELPS must also support
`attach` — connecting to an ELPS instance already running inside a Go
application like substrate or shirotester.

#### Does the embedder need its own debugger?

**No.** The ELPS debugger is sufficient. Here's why:

When an embedder (substrate, shirotester) registers custom builtins via
`elpsutil.PackageLoader` or `env.AddBuiltins()`, those builtins are stored
in the standard `Runtime.Registry` as regular `LFun` values. They are
**indistinguishable from ELPS-defined functions** at the package registry
level. The debugger sees them automatically:

| What the embedder registers | What the debugger sees |
|----------------------------|------------------------|
| Custom builtin (Go func) | Call entry + exit, args, return value, package name |
| Custom special op | Same as builtin — entry/exit/args/return |
| Custom macro | Expansion result + post-expansion source locations |
| `.lisp` library files | **Full debugging** — breakpoints, stepping, variable inspection |
| Go native values (`LVal.Native`) | Type name only (opaque `interface{}`) |

The key insight: **embedder-registered builtins are opaque at the Go boundary
but fully visible at the Lisp boundary.** When a user steps through code that
calls `substrate:do-something`, the debugger:

1. Shows `substrate:do-something` in the call stack with source location
2. Shows the evaluated arguments passed to it
3. Steps OVER the Go implementation (cannot step into Go code)
4. Shows the return value when it comes back
5. Resumes stepping through the calling Lisp code

This is the correct behavior — it matches how every language debugger handles
FFI calls (Python's C extensions, Ruby's C gems, etc.).

#### When would an embedder need to do extra work?

Only in these optional, advanced cases:

1. **Custom variable formatters.** If the embedder stores complex Go structs
   in `LVal.Native`, the default debugger shows them as
   `<native:*mypackage.MyStruct>`. The embedder could register a
   `VariableFormatter` to provide richer display:

   ```go
   dbg.RegisterFormatter("*mypackage.MyStruct", func(v interface{}) string {
       s := v.(*mypackage.MyStruct)
       return fmt.Sprintf("{name: %q, count: %d}", s.Name, s.Count)
   })
   ```

   This is Phase 3 / nice-to-have. The debugger works without it.

2. **Source path mapping.** If the embedder loads `.lisp` files from embedded
   resources (e.g., `//go:embed`) rather than the filesystem, breakpoints by
   file path won't match. The embedder would need to provide a source mapper
   so the DAP server can resolve `"substrate/lib/auth.lisp"` to the actual
   embedded content. This is solvable via the existing `SourceLibrary`
   interface.

3. **Conditional debug activation.** In production, the embedder would NOT
   attach a debugger (zero overhead). In development/test, a flag like
   `--debug-port=4711` would activate it. This is just a configuration
   concern — the embedder decides when to call `WithDebugger()`.

#### Embedder integration API

```go
// In the host application (e.g., substrate or shirotester):

env := lisp.NewEnv(nil)
lisp.InitializeUserEnv(env)
lisplib.LoadLibrary(env)

// Register custom packages (these are automatically visible to debugger)
rc := elpsutil.Load(env, elpsutil.PackageLoader(&substrate.CorePackage{}))
rc = elpsutil.Load(env, elpsutil.PackageLoader(&substrate.AuthPackage{}))
// ... more custom packages ...

// Optionally attach debugger (e.g., behind a --debug flag)
if debugMode {
    dbg := debugger.New(env.Runtime)
    server := dapserver.New(dbg, dapserver.Options{
        Addr: fmt.Sprintf("localhost:%d", debugPort),
    })
    go server.ListenAndServe()
    log.Printf("DAP debugger listening on %s", server.Addr())
}

// Continue normal execution — if debugger is attached, hooks are active;
// if not, Runtime.Debugger is nil and there is zero overhead.
env.LoadFile("main.lisp")
```

**Shirotester specifically:** Since shirotester runs ELPS-based tests, the
debug flow would be: set breakpoints in `.lisp` test files, attach the
debugger, run the test. The debugger pauses at breakpoints in the test code.
Calls to shirotester's custom test builtins (assertions, setup/teardown)
appear in the stack and can be stepped over. The `.lisp` test files
themselves are fully debuggable — breakpoints, stepping, variable inspection
all work. No changes to shirotester's builtin registration are needed.

The embedder starts the DAP server in a goroutine. The editor connects to it.
The debugger hooks pause execution when breakpoints are hit, and the DAP
server communicates the state back to the editor.

**Key constraint:** The ELPS execution and the DAP server communicate via
channels. The execution goroutine blocks on `WaitIfPaused()` (which reads
from a channel). The DAP server goroutine writes commands to that channel.
This is thread-safe by design.

### 4.4 The `launch` use case (CLI)

For standalone ELPS files:

```bash
elps debug --dap --port 4711 myfile.lisp
```

The CLI creates the env, attaches the debugger, starts the DAP server, and
waits for the editor to connect before executing `myfile.lisp`.

---

## 5. What the debugger CAN and CANNOT see

### Can see (Lisp-level):
- ELPS call stack (function names, source locations, packages)
- Local variable bindings (via scope walking)
- Package-level globals (including embedder-registered symbols)
- Function formal parameters
- Macro expansion results (post-expansion source locations)
- Error condition stack (what's being handled by `handler-bind`)
- Expression values at breakpoints
- All packages in `Runtime.Registry` (including embedder-registered packages)
- Embedder `.lisp` files loaded via `load-file` or `SourceLibrary`

### Cannot see (Go-level):
- Go goroutine state
- Go variables inside builtin/special-op implementations
- Internal LVal memory layout
- The Go call stack beneath the interpreter
- Native Go values beyond type name (opaque `interface{}` in `LVal.Native`)

### Partial visibility (the embedder boundary):

This is the most important section for substrate/shirotester users.

- **Embedder Go builtins** (registered via `AddBuiltins`/`elpsutil`): The
  debugger sees the call into the builtin and the return value, but cannot
  step inside the Go implementation. The function appears in the call stack
  with its registered name and package. Arguments are fully inspectable
  (they're evaluated LVals). This is the expected behavior — identical to
  how Python debuggers handle C extensions or how Java debuggers handle
  JNI calls.

- **Embedder `.lisp` files**: Fully debuggable. If substrate loads
  `lib/auth.lisp` via `load-file` or a `SourceLibrary`, breakpoints,
  stepping, and variable inspection all work. These are first-class ELPS
  source — the debugger treats them identically to user code.

- **Embedder special ops**: Same as builtins — entry/exit visible, internals
  opaque. Arguments are NOT evaluated (that's what makes them special ops),
  so the debugger shows the raw unevaluated forms.

- **Embedder macros**: The debugger sees the expanded result. Source locations
  on macro-generated code point to the call site (via `stampMacroExpansion`).
  Stepping through macro-expanded code may feel surprising if the expansion
  is complex, but breakpoints on the call site work correctly.

- **`LVal.Native` values**: The debugger can show the Go type name
  (`*substrate.Transaction`, `*shirotester.TestContext`, etc.) but cannot
  inspect fields. The optional `VariableFormatter` extension (Phase 3) lets
  embedders provide richer display for their native types.

### What this means for embedders

**The ELPS debugger is the only debugger needed.** Embedders do NOT need to:
- Implement their own debugger
- Register their builtins specially for debugging
- Change their `PackageLoader` or `AddBuiltins` calls
- Provide any debug-specific metadata

Everything works out of the box because custom builtins are stored as regular
`LFun` values in the package registry. The debugger discovers them through
the same `Runtime.Registry` that `elps doc` and the linter already use.

---

## 6. Complexity Assessment

### Estimated effort by component

| Component | Files | Estimated LOC | Difficulty | Notes |
|-----------|-------|---------------|------------|-------|
| Debugger interface + hooks | 1-2 | ~80 | Low | 3 nil-checked hook points in env.go |
| Breakpoint engine | 1 | ~150 | Low | Map lookup, condition eval |
| Step state machine | 1 | ~200 | Medium | StepOver/StepOut need depth tracking |
| Variable inspector | 1 | ~150 | Low | Scope chain walking |
| Debug eval (REPL) | 0 | ~30 | Low | Reuses existing Eval + Reader |
| DAP server scaffold | 2 | ~400 | Medium | TCP listener, session lifecycle |
| DAP message handler | 1 | ~600 | Medium-High | ~15 request types to implement |
| DAP type translation | 1 | ~200 | Medium | LVal → DAP Variable, Stack → DAP Frame |
| CLI integration | 1 | ~80 | Low | `elps debug` command |
| Tests | 3-4 | ~500 | Medium | Need mock debugger, DAP client tests |
| **Total** | **~12** | **~2400** | **Medium** | |

### Risk assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Hot path performance regression | Low | High | Benchmark CI, nil-check pattern proven by Profiler |
| TRO disabling causes stack overflow in debug | Medium | Low | Configurable, increase stack limit in debug mode |
| DAP protocol complexity (edge cases) | Medium | Medium | Start with VS Code only, add editors incrementally |
| Thread safety between DAP server + eval | Low | High | Channel-based communication, single-writer |
| Expression eval side effects in debug console | Medium | Medium | Document that debug eval can mutate state |
| Breakpoint in macro expansion | Medium | Medium | Use post-expansion source locations (stampMacroExpansion) |

### Phasing

**Phase 1 — Minimum Viable Debugger (~1200 LOC)**
- Debugger interface + hooks in env.go
- Breakpoint engine (by file:line)
- Continue + StepInto only
- Variable inspection (locals + globals)
- CLI `elps debug` with stdin/stdout DAP
- VS Code launch configuration

**Phase 2 — Full Stepping (~600 LOC)**
- StepOver + StepOut
- Conditional breakpoints
- Function breakpoints (by name)
- Debug eval (REPL in paused context)
- `attach` mode for embedded use

**Phase 3 — Polish (~600 LOC)**
- Hit count breakpoints
- Log points (print without pausing)
- Exception breakpoints (break on LError)
- Watch expressions
- Source mapping for macro expansions
- Editor-specific launch configs (Neovim, JetBrains)

---

## 7. Impact on Existing Code

The debugger is designed to be **almost entirely additive**. No changes to the
AST (`LVal`), parser, token system, package registry, call stack, or any
existing interface. The core interpreter requires only **surgical, minimal
modifications** to 3 existing files.

### 7.1 Files that DO NOT change

These files/structures are untouched. The debugger reads them but never
modifies their definitions:

| File/Structure | Why no change needed |
|---------------|---------------------|
| `lisp/lisp.go` — `LVal` struct | Debugger uses existing `Source *token.Location` for breakpoints and `Cells`/`Scope` for inspection. No new fields. |
| `lisp/lisp.go` — `LType` enum | No new types. Debugger operates on existing types. |
| `lisp/lisp.go` — `LFunData` | Already has `FID`, `Package`, `Builtin`, `Env` — everything the debugger needs to identify functions. |
| `lisp/lisp.go` — `SourceMeta` | Formatter metadata. Irrelevant to debugger. |
| `parser/` (all files) | Source locations are already tracked (`token.Location` with File, Line, Col). No parser changes needed. |
| `parser/token/token.go` — `Location` | Already has File, Line, Col, Pos. Sufficient for breakpoint matching. |
| `lisp/stack.go` — `CallStack`, `CallFrame` | Already has Source, FID, Package, Name, HeightLogical, Terminal, TROBlock. The debugger reads these, never extends them. |
| `lisp/package.go` — `Package`, `PackageRegistry` | Debugger reads `pkg.Symbols` for variable inspection. No changes. |
| `lisp/builtins.go` | All builtins unchanged. `debug-print` and `debug-stack` remain as-is. |
| `lisp/op.go` | All special operators unchanged. |
| `lisp/macro.go` | `stampMacroExpansion` already handles source locations. No changes. |
| `lisp/profiler.go` — `Profiler` interface | Remains independent. Debugger is a separate concern. |
| `lisp/error.go` | Error types and stack trace attachment unchanged. |
| `formatter/` | Completely unrelated to runtime debugging. |
| `lint/`, `analysis/` | Static analysis, unrelated to runtime. |
| `elpsutil/` | Embedding helpers unchanged. |
| `elpstest/` | Test framework unchanged. |

### 7.2 Files that change (3 files, ~15 lines of modifications)

#### `lisp/runtime.go` — Add one field

```diff
 type Runtime struct {
     Registry       *PackageRegistry
     Package        *Package
     Stderr         io.Writer
     Stack          *CallStack
     Reader         Reader
     Library        SourceLibrary
     Profiler       Profiler
+    Debugger       Debugger          // nil = disabled (zero overhead)
     MaxAlloc       int
     conditionStack []*LVal
     numenv         atomicCounter
     numsym         atomicCounter
 }
```

**Impact:** Adding a field to a struct. Zero risk. The field is `nil` by
default (`StandardRuntime()` does not set it). No existing code references it.
Size increase: 16 bytes per Runtime (one interface = pointer pair). There is
exactly one Runtime per interpreter instance.

#### `lisp/env.go` — Add nil-checked hooks at 3 points

**Point 1: `Eval()` (line ~831)** — expression-level hook

```diff
 func (env *LEnv) Eval(v *LVal) *LVal {
 eval:
     if v.Spliced {
         return env.Errorf("spliced value used as expression")
     }
     env.Loc = v.Source
+    if env.Runtime.Debugger != nil {
+        if env.Runtime.Debugger.OnEval(env, v) {
+            env.Runtime.Debugger.WaitIfPaused(env, v)
+        }
+    }
     if v.Quoted {
         return v
     }
```

**Impact:** 4 lines added. When `Debugger` is nil, this is a single
pointer comparison (`CMPQ $0, offset(reg)`) that the CPU branch-predicts
as not-taken. Identical to the existing `Profiler != nil` pattern in
`funCall()`. The `Eval` function is the hottest path — this is the change
that matters most. Benchmark verification is mandatory.

**Point 2: `funCall()` (line ~1034)** — function entry hook

```diff
 func (env *LEnv) funCall(fun, args *LVal) *LVal {
     if fun.Type != LFun {
         return env.Errorf("not a function: %v", fun.Type)
     }
     if fun.IsSpecialFun() {
         return env.Errorf("not a regular function: %v", fun.FunType)
     }

+    if env.Runtime.Debugger != nil {
+        env.Runtime.Debugger.OnFunCall(env, fun, args)
+    }
     if env.Runtime.Profiler != nil {
         defer env.trace(fun)()
     }
```

**Impact:** 3 lines added, immediately before the existing Profiler check.
Same nil-check pattern. Fires once per function call (not per expression).

**Point 3: `funCall()` return path (line ~1079)** — function exit hook

```diff
+    if env.Runtime.Debugger != nil {
+        env.Runtime.Debugger.OnFunReturn(env, fun, r)
+    }
     return r
 }
```

**Impact:** 3 lines added at the end of `funCall()`. Same nil-check pattern.

**Point 4: TRO conditional disable (line ~1042)** — optional

```diff
-    npop := env.Runtime.Stack.TerminalFID(fun.FID())
+    var npop int
+    if env.Runtime.Debugger == nil {
+        npop = env.Runtime.Stack.TerminalFID(fun.FID())
+    }
```

**Impact:** Wraps existing TRO check in a nil guard. When debugger is nil,
behavior is identical. When debugger is attached, TRO is disabled for
predictable stepping. 2 lines changed.

#### `lisp/config.go` — Add one Config function

```diff
+// WithDebugger returns a Config that attaches a debugger to the runtime.
+// When a debugger is attached, tail recursion optimization is disabled
+// to provide predictable stepping behavior.
+func WithDebugger(d Debugger) Config {
+    return func(env *LEnv) *LVal {
+        env.Runtime.Debugger = d
+        return Nil()
+    }
+}
```

**Impact:** Purely additive. 7 lines. Follows the existing pattern
(`WithReader`, `WithStderr`, `WithLibrary`, `WithMaximum*`).

### 7.3 New files (purely additive)

| File | Contents | Depends on |
|------|----------|-----------|
| `lisp/debugger.go` | `Debugger` interface, `DebugAction` enum, `Breakpoint` type | `lisp` package only (no new deps) |
| `lisp/x/debugger/debugger.go` | Engine implementation | `lisp` |
| `lisp/x/debugger/breakpoint.go` | Breakpoint storage + matching | `lisp`, `parser/token` |
| `lisp/x/debugger/stepper.go` | Step state machine | `lisp` |
| `lisp/x/debugger/inspector.go` | Scope/variable inspection | `lisp` |
| `lisp/x/debugger/dapserver/server.go` | TCP listener | `google/go-dap` |
| `lisp/x/debugger/dapserver/handler.go` | DAP message dispatch | `google/go-dap`, debugger engine |
| `lisp/x/debugger/dapserver/translate.go` | ELPS→DAP type mapping | `google/go-dap`, `lisp` |
| `cmd/debug.go` | `elps debug` CLI command | `lisp/x/debugger` |

### 7.4 New dependency

```
google/go-dap v0.10.0
```

This dependency is **only imported by `lisp/x/debugger/dapserver/`**. The
core `lisp/` package has zero new dependencies. An embedder that uses Layer 1
(the `Debugger` interface) directly does not pull in `go-dap`.

### 7.5 Summary: change surface

```
Existing code modified:    ~15 lines across 3 files
Existing structs modified: 1 field added to Runtime (nil by default)
AST changes:               NONE
Parser changes:            NONE
Token changes:             NONE
Call stack changes:         NONE
Package registry changes:  NONE
Builtin changes:           NONE
New external dependency:   1 (go-dap, isolated to dapserver/ subpackage)
New files:                 ~10
New LOC:                   ~2400
```

The debugger is essentially a **plugin that reads existing structures**. It
adds one field to Runtime, adds nil-checked calls at 3 points in the eval
loop, and everything else is new code in new files. If the debugger feature
were ever removed, reverting those ~15 lines would leave the codebase exactly
as it was.

---

## 8. Alternatives Considered

### 8.1 Extend the Profiler interface instead of adding Debugger

**Pros:** Reuses existing hook point, no new field on Runtime.
**Cons:** Profiler semantics are wrong — Start/Stop is for timing, not
pause/resume. Mixing concerns makes both profiling and debugging worse.
A debugger needs to block execution; a profiler must never block.
**Verdict:** Rejected. Keep Profiler and Debugger as separate concerns.

### 8.2 Chrome DevTools Protocol (CDP) instead of DAP

**Pros:** Rich, well-documented, good for web-based UIs.
**Cons:** Designed for JavaScript/browser debugging, poor editor integration
(no native support in VS Code for non-JS languages, no Neovim plugin).
Much larger protocol surface area. No Go library ecosystem.
**Verdict:** Rejected. DAP has better editor coverage.

### 8.3 Custom protocol (like the 2020 PR's Delve approach)

**Pros:** Maximum flexibility.
**Cons:** Requires building editor extensions from scratch for every editor.
Enormous maintenance burden. The 2020 PR required a custom VS Code extension.
**Verdict:** Rejected. DAP eliminates this entire class of work.

### 8.4 GDB/MI protocol

**Pros:** Supported by older tools.
**Cons:** Legacy protocol, poor editor support outside GDB frontends,
designed for native code debugging. Declining ecosystem.
**Verdict:** Rejected.

### 8.5 No debugger — just improve debug-print/debug-stack

**Pros:** Zero complexity.
**Cons:** Doesn't solve the actual problem. Printf debugging is the baseline,
not the goal. Modern developers expect breakpoint/step/inspect workflows.
**Verdict:** Rejected as the sole approach, but `debug-print` and
`debug-stack` remain useful as lightweight alternatives.

---

## 9. Interaction with Existing Systems

### 9.1 Profiler

The Debugger and Profiler are independent. Both can be active simultaneously:
- Profiler fires in `funCall()` via `defer env.trace(fun)()`
- Debugger fires in `Eval()` via `OnEval()` and in `funCall()` via `OnFunCall()`
- No ordering dependency between them

### 9.2 LSP

The LSP support enhancements (`docs/plans/lsp-support-enhancements.md`) and
the debugger are complementary:
- LSP provides editing features (completion, diagnostics, go-to-definition)
- Debugger provides runtime features (breakpoints, stepping, inspection)
- They share no state and can be developed independently
- The LSP's semantic analyzer (P3) could eventually provide richer variable
  type information to the debugger, but this is not required for v1

### 9.3 Formatter and Linter

No interaction. The debugger operates at runtime, not on static source.

### 9.4 Error handling (handler-bind / rethrow)

The debugger can provide "break on exception" functionality by hooking into
the condition system. When `opHandlerBind` pushes to `Runtime.conditionStack`,
the debugger can optionally pause. This is a Phase 3 feature.

---

## 10. Configuration API

```go
// Config function for embedders
func WithDebugger(d Debugger) Config {
    return func(env *LEnv) *LVal {
        env.Runtime.Debugger = d
        return Nil()
    }
}

// Usage:
env := lisp.NewEnv(nil)
dbg := debugger.New()
lisp.InitializeUserEnv(env, lisp.WithDebugger(dbg))
```

This follows the established Config pattern (`WithReader`, `WithStderr`,
`WithLibrary`). The debugger is optional and nil by default.

---

## 11. Open Questions

1. **Should the debugger disable TRO globally or per-session?** If globally,
   it simplifies the implementation but means attaching a debugger changes
   execution behavior. Per-session would require tracking which call chains
   are being debugged.

2. **Should `OnEval` fire for every expression or only expressions with
   source locations?** Synthetic expressions (from macro expansion with
   `Pos < 0`) may not be meaningful to step through. Filtering them reduces
   overhead but may miss debugging opportunities.

3. **Should the DAP server support both TCP and stdio?** VS Code typically
   launches the debug adapter as a child process (stdio). Embedded attach
   requires TCP. Supporting both is straightforward but adds complexity.

4. **Should debug eval be sandboxed?** Evaluating expressions in the debug
   console can mutate program state (e.g., `(set! x 42)`). Some debuggers
   prevent mutations; others allow them. ELPS should probably allow them
   (REPL philosophy) but document the risk.

5. **Goroutine model for the DAP server?** The Delve DAP server uses a
   single-client model (restart for each session). This is simpler and
   sufficient for v1.

---

## 12. Summary

| Aspect | Decision |
|--------|----------|
| Protocol | DAP (Debug Adapter Protocol) |
| Go library | `google/go-dap` v0.10.0 (codec only) |
| Architecture | Two layers: internal Debugger interface + DAP adapter |
| Hook mechanism | Nil-checked calls in Eval() and funCall() |
| Hot path cost | Zero when debugger is nil (branch-predicted nil check) |
| Embedded support | `attach` mode via WithDebugger() Config |
| CLI support | `elps debug --dap` command |
| Editor coverage | VS Code (Phase 1), Neovim/JetBrains/Helix (Phase 3) |
| Total estimated LOC | ~2400 across ~12 files |
| Overall difficulty | Medium — no novel algorithms, well-trodden patterns |
| Biggest risk | DAP protocol edge cases in message handler |
| Biggest win | One implementation serves every editor |
