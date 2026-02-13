# Petri Net / Colored Petri Net Analysis

**Date:** 2026-02-13
**Scope:** Full ELPS interpreter evaluation pipeline -- recursive evaluator, macro system, error handling, tail-recursion optimization, package system, file loading
**Analysis Type:** Formal concurrency and resource contention analysis using Place/Transition nets extended with Colored Petri Net (CPN) token types and timed stochastic extensions

---

## 1. Resource Model Table

All capacities derived from source code constants and structural analysis.

| Resource | Capacity (Default) | Source Constant / Code Location | Acquired At | Released At |
|---|---|---|---|---|
| Physical Call Stack | 25,000 frames | `DefaultMaxPhysicalStackHeight` (`runtime.go:77`) | `CallStack.PushFID()` (`stack.go:148-165`) | `CallStack.Pop()` via `defer` (`env.go:929`, `976`, `1049`) |
| Logical Call Stack | 50,000 logical frames | `DefaultMaxLogicalStackHeight` (`runtime.go:76`) | `PushFID` increments `HeightLogical` (`stack.go:150-152`); TRO accumulation (`env.go:999`, `1069`) | `Pop()` via `defer`; logical height is monotonically increasing within a call chain -- NOT decremented on pop |
| Single Allocation | 10,485,760 elements or bytes | `DefaultMaxAlloc` (`runtime.go:68`) | `builtinMakeSequence` (`builtins.go:1714-1718`), `builtinRepeat` (`libstring.go:135-142`) | Immediate release to Go GC; budget resets per-operation |
| Condition Stack | Unbounded (Go `[]*LVal` slice) | `Runtime.conditionStack` (`runtime.go:24`) | `PushCondition()` (`runtime.go:40-42`) | `PopCondition()` via `defer` in `opHandlerBind` (`op.go:688-689`) |
| Package Registry | Monotonically growing (Go `map[string]*Package`) | `PackageRegistry.Packages` (`package.go:9`) | `DefinePackage()` (`package.go:20-28`) | Never released -- packages cannot be deleted |
| LEnv Scope Chain | Proportional to call depth; unbounded in absolute terms | `LEnv.Scope map[string]*LVal` (`env.go:93`) | `NewEnv(parent)` / `newEnvN(parent, n)` (`env.go:121-147`) | Go GC when scope exits and all closures referencing it are collected |
| Scanner Buffer | 131,072 bytes (128 KiB) per parse | `make([]byte, 128<<10)` (`scanner.go:49`) | `token.NewScanner()` | Go GC after parse completes |
| File System Handle | 1 per `ReadFile` call | OS file descriptor limit | `os.ReadFile(loc)` (`library.go:114`) | Immediate -- `ReadFile` opens, reads, closes |
| Runtime Singleton | 1 per env tree | `StandardRuntime()` (`runtime.go:81-90`) | Process/env-tree initialization | Process lifetime |
| Profiler Mutex | 1 `sync.Mutex` | `callgrindProfiler` (`callgrind.go:43`) | `Profiler.Start()` | Returned stop callback `func()` |
| Atomic Counters | 2 per Runtime | `numenv`, `numsym` (`runtime.go:25-26`) | `GenEnvID()` / `GenSym()` (`runtime.go:92-98`) | Never reset; monotonically increasing |
| Interactive Parser RWMutex | 1 `sync.RWMutex` | `interact.go:21` (in `rdparser`) | Parser `Interact()` call | Unlock after parse step |

---

## 2. Full Evaluation Pipeline Token Flow

### 2.1 Places (States)

| Place ID | Name | Description | Initial Marking |
|---|---|---|---|
| P0 | SourceInput | Raw source text (string, bytes, file path) | 1 (program input) |
| P1 | ScannerActive | Lexer initialized with 128 KiB buffer, scanning characters | 0 |
| P2 | TokenStream | Token sequence produced by lexer state machine | 0 |
| P3 | ParsedAST | `[]*LVal` expression list from recursive-descent parser | 0 |
| P4 | EvalReady | Expression(s) queued for evaluation (`env.load()` loop at `env.go:287-293`) | 0 |
| P5 | SymbolResolving | Symbol lookup traversing scope chain -> parent -> package (`env.go:363-423`) | 0 |
| P6 | FunctionDispatching | Function type identified, dispatching to call path (`env.go:899-908`) | 0 |
| P7 | ArgumentBinding | Formals/actuals binding via `env.bind()` (`env.go:1226-1282`) | 0 |
| P8 | BuiltinExecuting | Native Go builtin function running (`env.go:1172-1182`) | 0 |
| P9 | SpecialOpExecuting | Special operator body evaluating (`env.go:963-1011`) | 0 |
| P10 | MacroExpanding | Macro body executing to produce expansion AST (`env.go:912-959`) | 0 |
| P11 | MacroReEval | Expanded AST returned with `LMarkMacExpand`, awaiting re-evaluation (`env.go:863-867`) | 0 |
| P12 | UserFunExecuting | User-defined lambda body evaluating (`env.go:1027-1080`) | 0 |
| P13 | TRODecision | Tail recursion optimization: `TerminalFID` scan + `markTailRec` check (`stack.go:113-145`, `env.go:1042-1053`) | 0 |
| P14 | FileLoadActive | `load-file` / `load-string` re-entering the full pipeline (`env.go:205-293`) | 0 |
| P15 | ErrorRaised | `LError` value produced by any evaluation step | 0 |
| P16 | HandlerSearching | Scanning `handler-bind` binding list for matching condition (`op.go:669-694`) | 0 |
| P17 | ConditionActive | Condition pushed onto `conditionStack` for `rethrow` access (`op.go:688`) | 0 |
| P18 | ResultProduced | Final value returned to caller | 0 |
| P19 | StackCapacity | Available physical call stack frame slots | 25,000 |
| P20 | AllocBudget | Per-operation allocation limit tokens | 10,485,760 |
| P21 | PackageState | Shared mutable package registry + current package pointer | 1 (singleton) |
| P22 | ArgEvalLoop | S-expression cell evaluation loop (`evalSExprCells` at `env.go:1099-1159`) | 0 |
| P23 | PackageSaved | Saved package context for restoration after load/call (`env.go:279-284`, `env.go:1191-1201`) | 0 |

### 2.2 Transitions (Events)

| Transition | Name | Input Places | Output Places | Guard Condition |
|---|---|---|---|---|
| T0 | InitScanner | P0 | P1 | `Runtime.Reader != nil` (`env.go:233-234`) |
| T1 | Lexing | P1 | P2 | scanner not at EOF; rune dispatch in lexer state machine |
| T2 | Parsing | P2 | P3 | valid token sequence; bracket matching succeeds |
| T3 | EnqueueForEval | P3 | P4 | `len(exprs) > 0` (`env.go:273`) |
| T4 | EvalDispatch | P4 | P5 / P6 / P18 | based on `v.Type`: LSymbol -> P5; LSExpr -> P22 -> P6; LQuote -> T4 (goto eval); other -> P18 |
| T5 | SymbolResolve | P5, P21 | P18 / P15 | symbol found in scope chain or package (`env.go:363-423`); else error |
| T6 | SExprCellEval | P22 | P6 | head evaluates to `LFun` (`env.go:1121`); args evaluated if not special fun |
| T7 | DispatchBuiltin | P6, P19 | P7, P8 | `fun.FunType == LFunNone && fun.Builtin() != nil`; stack push succeeds |
| T8 | DispatchSpecialOp | P6, P19 | P9 | `fun.FunType == LFunSpecialOp`; stack push succeeds |
| T9 | DispatchMacro | P6, P19 | P10 | `fun.FunType == LFunMacro`; stack push succeeds; sets `TROBlock = true` |
| T10 | DispatchUserFun | P6, P19 | P7, P12 | `fun.FunType == LFunNone && fun.Builtin() == nil`; stack push succeeds; TRO check first |
| T11 | BuiltinComplete | P8 | P18, P19 | execution returns non-error `*LVal`; `defer Pop()` fires |
| T12 | SpecialOpComplete | P9 | P18, P19 | body evaluation returns non-error; may produce `LMarkTerminal` (`env.go:1177-1179`) |
| T13 | MacroComplete | P10 | P11, P19 | expansion produces non-error; `stampMacroExpansion` + `shallowUnquote` applied (`env.go:950-959`) |
| T14 | MacroReEvaluate | P11 | P4 | `LMarkMacExpand` unwrapped; `goto eval` at `env.go:867` |
| T15 | UserFunComplete | P12 | P18, P19 | body evaluation returns non-error; terminal expression marked at `env.go:1215-1217` |
| T16 | TROFire | P13 | P6 | `TerminalFID(fid) > 0` (`stack.go:113-145`); `goto callf` at `env.go:1074` |
| T17 | TROAccumulate | P13 | P12 | `HeightLogical += elided` (`env.go:1069`); `CheckHeight()` passes (`env.go:1070-1072`) |
| T18 | LoadFileEnter | P8 | P14, P23 | `builtinLoadFile` called; saves current package (`env.go:279`) |
| T19 | LoadFileReturn | P14 | P18, P23 -> P21 | load completes; `defer` restores package (`env.go:280-284`) |
| T20 | RaiseError | P8/P9/P10/P12 | P15 | `result.Type == LError` |
| T21 | HandleError | P15, P16 | P17, P4 | handler condition matches error condition (`op.go:672`); `PushCondition` at `op.go:688` |
| T22 | PropagateError | P15 | P18 | no handler matches; error passes through return chain |
| T23 | StackPush | P19 | P6 | `len(Frames) < MaxHeightPhysical` (`stack.go:180-187`) AND `HeightLogical < MaxHeightLogical` (`stack.go:189-200`) |
| T24 | StackPop | P6/P8/P9/P10/P12 | P19 | `defer env.Runtime.Stack.Pop()` fires |
| T25 | AllocCheck | P20 | P8 | allocation size `< MaxAllocBytes()` (`runtime.go:31-36`) |
| T26 | AllocReject | P20 | P15 | allocation size `>= MaxAllocBytes()`; returns `LError` |
| T27 | ConditionPop | P17 | P18 | handler returns; `defer PopCondition()` fires (`op.go:689`) |
| T28 | TerminalMark | P8/P9 | P4 | `LMarkTerminal` returned; `env.call()` sets `Terminal = true` and re-evaluates (`env.go:1177-1179`) |
| T29 | PackageSwap | P21 | P23 | `env.call()` swaps `Runtime.Package` for user-fun package (`env.go:1191-1201`) |
| T30 | PackageRestore | P23 | P21 | `defer` restores `Runtime.Package` to outer package (`env.go:1197-1199`) |

### 2.3 Primary Token Flow Arcs

**Main evaluation pipeline:**
```
P0 --T0--> P1 --T1--> P2 --T2--> P3 --T3--> P4
                                                |
                        +--<------T14------P11--+
                        |                       |
                        v                       |
                       P4 --T4--> P5 --T5--> P18
                        |
                        +--T4--> P22 --T6--> P6
                                              |
                    +----+----+----+----------+
                    |    |    |    |
                    T7   T8   T9   T10
                    |    |    |    |
                    v    v    v    v
                   P8   P9  P10  P12
                    |    |    |    |
                   T11  T12  T13  T15
                    |    |    |    |
                    v    v    v    v
                   P18  P18 P11  P18
```

**Tail recursion optimization cycle:**
```
P12 --> T13 (check TerminalFID) --> P13 --> T16 (TRO fire) --> P6 (restart call)
                                        \--> T17 (accumulate logical height)
                                              --> check HeightLogical < max
                                              --> P12 (goto callf)
```

**Error handling flow:**
```
P8/P9/P10/P12 --T20--> P15 (ErrorRaised)
                          |
                          +--> T21 (handler matches) --> P17 (condition pushed)
                          |                               |
                          |                               +--> P4 (eval handler body)
                          |                               |
                          |                               +--> T27 (defer PopCondition)
                          |                                     --> P18
                          |
                          +--> T22 (no handler) --> P18 (propagate error up)
```

**File loading re-entrant path:**
```
P8 (builtinLoadFile) --T18--> P14 --T0--> P1 ... (full pipeline re-entry)
                                |
                          P23 (saved package)
                                |
                          T19 (return) --> P23 --T30--> P21 (restore package)
```

**Package swap during user function call:**
```
P6 (dispatch user fun) --T29--> P23 (save outer package, install fun's package)
                                  |
P12 (body executing)              |
                                  |
T15 (return) --T30--> P23 --> P21 (restore outer package via defer)
```

---

## 3. Colored Petri Net (CPN) Extension

### 3.1 Color Set Definitions

```
-- Token colors for evaluation type
colorset CallType = enum { Builtin, SpecialOp, Macro, UserFun, FileLoad }

-- Token colors for value types (all 18 LType constants from lisp.go:27-113)
colorset ValueType = enum {
    Invalid, Int, Float, Error, Symbol, QSymbol,
    SExpr, Fun, Quote, String, Bytes, SortMap,
    Array, Native, TaggedVal,
    MarkTerminal, MarkTailRec, MarkMacExpand
}

-- Stack operations
colorset StackOp = union {
    Push of { fid: string, pkg: string, name: string },
    Pop,
    TROElide of { frames: int, logical_acc: int }
}

-- Allocation operations (only 2 builtins currently check)
colorset AllocOp = union {
    SeqAlloc of { count: int },      -- make-sequence (builtins.go:1714)
    StringRepeat of { bytes: int },   -- string:repeat (libstring.go:135)
    Unchecked of { desc: string }     -- concat, append, map, json, etc.
}

-- Package operations
colorset PkgOp = union {
    Define of { name: string },
    InPackage of { name: string },
    UsePackage of { name: string },
    Export of { sym: string },
    PutSymbol of { name: string, value: ValueType },
    GetSymbol of { name: string },
    SwapPackage of { from: string, to: string }
}

-- Error/condition handling operations
colorset ErrorOp = union {
    Raise of { condition: string },
    Handle of { condition: string, handler: Fun },
    Rethrow,
    IgnoreErrors,
    Propagate
}

-- Composite evaluation token: the primary token flowing through the net
colorset EvalToken = record {
    call_type: CallType,
    value_type: ValueType,
    physical_depth: int,       -- len(stack.Frames) at this point
    logical_depth: int,        -- stack.Top().HeightLogical
    is_terminal: bool,         -- stack.Top().Terminal
    tro_blocked: bool,         -- stack.Top().TROBlock
    package_name: string,      -- env.Runtime.Package.Name
    source_location: Location  -- env.Loc
}
```

### 3.2 Resource Consumption Matrix per Color

| Color (CallType) | Physical Frames (+/-) | Logical Height Delta | TRO Eligible | TROBlock Set | Allocation Pattern | Package Ops | Scope Creation |
|---|---|---|---|---|---|---|---|
| **Builtin** | +1 / -1 (defer) | +1 (HeightLogical increments on push) | Yes (`TerminalFID` check at `env.go:1042`) | No | Variable: 0 for most; up to `MaxAlloc` for `make-sequence` | `Get` (most), `Put` for `set`/`export`, `InPackage`/`UsePackage`/`DefinePackage` | 0 (uses caller's env) |
| **SpecialOp** | +1 / -1 (defer) | +1 | Limited: `goto callf` at `env.go:1004-1005` for TRO passthrough | Yes for `handler-bind` (`op.go:665`), `ignore-errors` (`op.go:708`) | 0 (pure control flow) | Scoped `Put` in `let`, `let*`, `flet`, `labels`, `macrolet` | 1 per `let*`/`flet`/`labels` binding form via `newEnvN()` |
| **Macro** | +1 / -1 (defer) | +1 | No (TROBlock = true always, `env.go:933`) | Yes (always) | AST nodes for expansion template | `set` for `defun`/`defmacro` (via expanded `(progn (set ...))`) | 1 via `env.Lambda()` for the macro body env |
| **UserFun** | +1 / -1 (defer) | +1 (or +n via TRO accumulation) | Yes (primary TRO candidate via `TerminalFID`) | No | Per body expression evaluation | Package swap: `outer -> fun.Package()` (`env.go:1191-1201`) | 1 via `fun.Env().Copy()` in `bind()` (`env.go:1231`) |
| **FileLoad** | +1 / -1 (via load-file builtin frame) + N nested | +1 + N | No (load frames contain TROBlock builtins) | Implicit (via builtins) | Scanner buffer (128 KiB) + AST `[]*LVal` | `DefinePackage`, `InPackage`, `UsePackage`, `Export`; package saved/restored via defer | Multiple: new scopes for each form evaluated in loaded file |

### 3.3 Guard Functions per Color (Formal CPN Guards)

```
guard(T7, Builtin):
    len(stack.Frames) < MaxHeightPhysical    -- stack.go:180-187
    AND (len(stack.Frames) == 0 OR
         stack.Top().HeightLogical < MaxHeightLogical)  -- stack.go:189-200

guard(T8, SpecialOp):
    len(stack.Frames) < MaxHeightPhysical
    AND (len(stack.Frames) == 0 OR
         stack.Top().HeightLogical < MaxHeightLogical)

guard(T9, Macro):
    len(stack.Frames) < MaxHeightPhysical
    AND (len(stack.Frames) == 0 OR
         stack.Top().HeightLogical < MaxHeightLogical)
    -- Note: sets TROBlock = true AFTER push succeeds (env.go:933)

guard(T10, UserFun):
    len(stack.Frames) < MaxHeightPhysical
    AND (len(stack.Frames) == 0 OR
         stack.Top().HeightLogical < MaxHeightLogical)
    -- Note: TerminalFID check occurs BEFORE push (env.go:1042)
    -- If TRO eligible: markTailRec(npop, fun, args) is returned AFTER push

guard(T16, TROFire):
    TerminalFID(fid) > 0                     -- stack.go:113-145
    AND no TROBlock in terminal chain         -- stack.go:123-127

guard(T17, TROAccumulate):
    stack.Top().HeightLogical + elided < MaxHeightLogical  -- env.go:1000-1003, 1070-1072

guard(T25, AllocCheck):
    requested_size < env.Runtime.MaxAllocBytes()  -- runtime.go:31-36

guard(T18, FileLoad):
    env.Runtime.Library != nil               -- env.go:216-217
    AND env.Runtime.Reader != nil            -- env.go:233-234
    AND file accessible via OS               -- library.go:114
    AND len(stack.Frames) < MaxHeightPhysical  -- via PushFID in builtinLoadFile
```

---

## 4. Deadlock Analysis

A deadlock in a Petri net is a marking where no transition is enabled. For ELPS, this means: can the interpreter reach a state where evaluation cannot proceed and cannot produce a result?

### 4.1 Scenario 1: Physical Stack Exhaustion

**Question:** When all 25,000 physical stack slots are consumed, can evaluation proceed?

**Analysis:** When `len(Frames) >= MaxHeightPhysical`, `checkHeightPhysical()` at `stack.go:179-187` returns `&PhysicalStackOverflowError{len(s.Frames) + 1}`. This Go error is converted to an `*LVal` of type `LError` at the call site:
- `MacroCall`: `env.go:926-928` -- `err := env.Runtime.Stack.PushFID(...)` -> `return env.Error(err)`
- `SpecialOpCall`: `env.go:972-975` -- same pattern
- `funCall`: `env.go:1045-1048` -- same pattern

The error `*LVal` is returned through the normal return path. Each caller checks `if result.Type == LError { return result }`. The `defer Pop()` on each existing frame fires during Go stack unwinding (Go `defer` unwinds regardless of control flow). The net transitions from the execution place (P8/P9/P10/P12) to P15 (ErrorRaised) via T20, then to P18 (ResultProduced) via T22 (PropagateError) if no handler exists, or via T21 if a `handler-bind` matches.

**Verdict:** **No deadlock.** Stack exhaustion produces an LError that propagates through the return value chain. The `defer Pop()` pattern guarantees all frame slots are released. The system always reaches P18 (Result).

### 4.2 Scenario 2: Logical Stack Overflow with TRO

**Question:** Can TRO accumulate logical height to the maximum without the system being able to proceed?

**Analysis:** When TRO fires, `HeightLogical` is increased by the number of elided frames (`env.go:999`, `1069`). Immediately after, `CheckHeight()` is called (`env.go:1000-1003`, `1070-1072`). If `MaxHeightLogical < Top().HeightLogical`, `CheckHeight()` returns `&LogicalStackOverflowError{...}`, which is converted to `LError` and returned. The `goto callf` does NOT fire when `CheckHeight` fails.

The physical stack still has room (TRO reduced physical usage), so `defer Pop()` will fire normally, returning physical frame slots.

**Verdict:** **No deadlock.** Logical overflow produces an error that propagates normally. Physical frames are released by defers.

### 4.3 Scenario 3: Condition Stack Leak

**Question:** Can `PushCondition` occur without a matching `PopCondition`, permanently occupying condition stack slots?

**Analysis:** `PushCondition` at `op.go:688` is immediately followed by `defer env.Runtime.PopCondition()` at `op.go:689`. The `defer` is in the scope of `opHandlerBind`, which means it fires when `opHandlerBind` returns -- regardless of whether the handler succeeds, fails, or panics (Go runtime unwinds defers on panic).

Critically, the `return env.Eval(SExpr(expr))` at `op.go:692` is a normal function call. If this evaluation raises a further error, the error is returned as an `*LVal` (not thrown as a Go panic), so the defer still fires normally. If a Go panic occurs (e.g., nil pointer), Go's defer mechanism still fires.

The condition stack depth is bounded by the number of active `handler-bind` handler executions, which is bounded by the physical stack depth (each `handler-bind` occupies at least one stack frame for the `opHandlerBind` special op call).

**Verdict:** **No deadlock.** The defer-based push/pop pattern is ironclad. Condition stack is bounded by physical stack depth.

### 4.4 Scenario 4: Infinite Macro Expansion Cycle

**Question:** Can a macro `M` expand to a form that calls `M` again, creating an infinite loop in the `goto eval` loop at `env.go:863-867`?

**Analysis:** The `goto eval` at `env.go:867` fires when `res.Type == LMarkMacExpand`. This loop itself does not push a stack frame -- it merely re-enters `Eval()` with the expanded expression. However, when the expanded expression is `(M ...)` (another call to macro M), `EvalSExpr` is called, which calls `MacroCall`, which calls `PushFID` at `env.go:925`. The push either succeeds (consuming one physical frame) or fails with stack overflow.

Since macros set `TROBlock = true` (`env.go:933`), TRO cannot collapse macro frames via `TerminalFID` (the search stops at any TROBlock frame per `stack.go:123-127`). Therefore, physical stack depth strictly increases with each macro expansion. After at most 25,000 nested expansions, `PhysicalStackOverflowError` terminates the chain.

**Verdict:** **No deadlock.** The `goto eval` loop cannot spin infinitely because each re-expansion requires a stack frame, and the stack is bounded.

### 4.5 Scenario 5: Mutual `load-file` Recursion

**Question:** File A loads file B, file B loads file A. Deadlock?

**Analysis:** Each `load-file` call enters `builtinLoadFile` which calls `env.LoadFile()` (`env.go:215-225`). This is a builtin, so it consumes one stack frame. Within the loaded file, expressions are evaluated sequentially by `env.load()` (`env.go:272-294`). If file B contains `(load-file "a.lisp")`, that becomes another `builtinLoadFile` call consuming another frame.

There is no file-level lock or visited-file tracking -- the system does not detect the cycle. However, each `load-file` pushes at least one stack frame. After at most 25,000 / 2 = 12,500 round trips (file A loads B, B loads A = 2 frames), physical stack overflow occurs.

File system handles are not held open across evaluations -- `os.ReadFile` at `library.go:114` reads and closes immediately. No file handle deadlock is possible.

**Verdict:** **No deadlock.** Stack overflow terminates the cycle. No resource deadlock. However, the lack of cycle detection means the error message is "physical stack height exceeded maximum" rather than a more informative "circular load detected."

### 4.6 Scenario 6: Package Registry Contention

**Question:** Can concurrent access to the package registry create deadlock?

**Analysis:** The `PackageRegistry` (`package.go:8-11`) uses plain `map[string]*Package`. `Package.Symbols` and `Package.FunNames` are also plain maps. The `Runtime` is documented as belonging to one env tree (`env.go:105-106`). ELPS is single-threaded by design.

The only synchronization primitives in the entire system are:
1. `sync.Mutex` in `callgrindProfiler` (`callgrind.go:43`) -- optional profiler, not in core path
2. `sync.RWMutex` in `rdparser/interact.go:21` -- used only by interactive REPL input
3. `atomic.AddUint64` in `atomicCounter` (`runtime.go:134-136`) -- for ID generation

None of these protect the package registry. There are no locks to deadlock on.

**Verdict:** **No deadlock possible.** The system is fundamentally single-threaded. If an embedder violates the single-tree contract by sharing a `Runtime` across goroutines, data races (not deadlocks) would occur.

### 4.7 Scenario 7: Terminal Expression Loop in `env.call()`

**Question:** Can the `LMarkTerminal` handling at `env.go:1177-1179` create an infinite loop?

**Analysis:** When a builtin returns `LMarkTerminal`, `env.call()` sets `Terminal = true` on the top frame and evaluates `val.Cells[0]`:
```go
if val.Type == LMarkTerminal {
    env.Runtime.Stack.Top().Terminal = true
    return val.Native.(*LEnv).Eval(val.Cells[0])
}
```
This is a single recursive `Eval` call, not a loop. The terminal flag enables TRO for the subsequent evaluation, but the evaluation itself goes through normal `Eval` dispatch. If the evaluated expression is itself a function call, `PushFID` is called, consuming a stack frame.

The `Terminal = true` flag on the CURRENT frame does not cause looping -- it tells future `TerminalFID` scans that this frame can be collapsed. The expression is evaluated exactly once.

**Verdict:** **No infinite loop.** Terminal marking enables TRO but does not create a cycle.

### 4.8 Global Deadlock Freedom Proof

**Theorem:** The ELPS Petri net is deadlock-free.

**Proof:** In every reachable marking, at least one transition is enabled:
1. If P4 (EvalReady) has tokens: T4 (EvalDispatch) is enabled (type switch always resolves).
2. If P6 (FunctionDispatching) has tokens: at least one of T7/T8/T9/T10 is enabled (function type is determined) OR stack overflow produces an error (T20 -> P15).
3. If P15 (ErrorRaised) has tokens: either T21 (handler matches) or T22 (propagate) is enabled (handler list iteration is finite).
4. If P8/P9/P10/P12 has tokens: execution is in progress and will eventually produce a result (bounded by Go's own stack + ELPS stack limits), enabling T11/T12/T13/T15 or T20.
5. If no execution places have tokens and P18 has tokens: evaluation is complete.

Since every execution path either makes progress toward P18 or reaches P15 (which also progresses toward P18), and all loops are bounded by resource constraints, no deadlock state is reachable. **QED.**

---

## 5. Liveness Proof Sketch

**Theorem:** Every evaluation in ELPS eventually terminates (reaches P18), given the default resource bounds.

**Proof by well-founded ordering:**

Define the measure function:
```
M(state) = (MaxHeightPhysical - physical_height,
            MaxHeightLogical - logical_height,
            AST_depth_remaining)
```
with lexicographic ordering on (non-negative integers)^3.

**Base cases (immediate termination, M unchanged or at floor):**
- `LInt`, `LFloat`, `LString`, `LBytes`, `LNative`: returned immediately from `Eval` (`env.go:877-878`). No stack operations.
- `LSymbol` (non-keyword, unqualified): `getSimple` loop at `env.go:403-415` terminates because the LEnv parent chain is finite (rooted at `Parent == nil`).
- `LSymbol` (keyword like `:foo`): returned immediately (`env.go:843`).
- Quoted values (`v.Quoted`): returned immediately (`env.go:832-834`).

**Inductive cases (M strictly decreases):**

1. **Function call (any type):** `PushFID` at `env.go:925/972/1045` either:
   - Succeeds: `physical_height` increases by 1, so `M[0]` decreases by 1. The body has finite AST nodes, so `M[2]` is finite. After all body expressions are evaluated (finite list traversal), a result is produced and `defer Pop()` restores `M[0]`.
   - Fails: `PhysicalStackOverflowError` returned as `LError`. Transition to P15/P18. `M` reaches its terminal state.

2. **Tail recursion optimization:** `goto callf` at `env.go:1005/1074` does NOT push a new physical frame. Instead, `HeightLogical` increases by the number of elided frames (`env.go:999/1069`). So `M[1]` decreases. If `M[1]` reaches 0 (`HeightLogical >= MaxHeightLogical`), `CheckHeight()` returns error. Therefore TRO iterations are bounded by `MaxHeightLogical`.

3. **Macro expansion:** `MacroCall` at `env.go:912-959` pushes one frame (M[0] decreases), evaluates the body (bounded), then the `goto eval` at `env.go:867` re-enters `Eval` with the expanded form. The expanded form is a finite AST (produced by the macro body), so `M[2]` is finite. The re-evaluation goes through normal dispatch, requiring another stack frame for any function call.

4. **Special operators with body evaluation:** `opProgn`, `opLet`, `opCond`, `opIf`, etc. iterate over a finite list of cells (`args.Cells`), evaluating each. The list length is determined at parse time and is finite. Each iteration may recursively call `Eval`, which decreases `M` by the arguments above.

5. **`dotimes` loop:** Bounded by the `count` integer argument (evaluated once at `op.go:453-459`). Each iteration evaluates a finite body.

6. **File loading (`load`):** Pushes at least one frame (for the `load-file` builtin). Within the loaded file, `env.load()` at `env.go:272-294` iterates over `exprs`, which is the finite list of parsed expressions. Each expression evaluation decreases `M` per the above cases.

7. **Error propagation:** An `LError` value is returned immediately at each call site. The `if ret.Type == LError { return ret }` checks at `env.go:289`, `env.go:1209`, etc. cause immediate unwinding. Each `defer Pop()` fires during unwinding, restoring M[0].

**Conclusion:** `M` is bounded below by `(0, 0, 0)` and strictly decreases on every recursive evaluation step. By the well-founded ordering principle, evaluation must terminate. **QED.**

**Caveats:**
- This proof assumes finite input (finite source text, finite file set). An infinite input stream would never finish parsing (T1/T2 would not terminate).
- Native Go builtins that call external code (e.g., `regexp.Compile` via `libregexp`) are black boxes to this analysis. A pathological regex could make T7 take arbitrarily long, but it would still eventually return.
- The proof shows termination, not progress toward the intended result. A stack overflow during legitimate deep recursion is "termination" per this proof but not useful computation.

---

## 6. Boundedness Analysis per Place

| Place | Bounded? | Upper Bound | Justification |
|---|---|---|---|
| P0 (SourceInput) | Yes | 1 per active load operation | `load-file` reads one file at a time; `load-string` processes one string |
| P1 (ScannerActive) | Yes | 1 | One scanner per parse invocation; synchronous |
| P2 (TokenStream) | Yes | O(source_length) | Lexer produces at most one token per character |
| P3 (ParsedAST) | Yes | O(tokens) | Parser produces one AST node per token (approximately) |
| P4 (EvalReady) | Yes | O(load_depth * exprs_per_file) | Sequential evaluation in `env.load()` loop; bounded by source size |
| P5 (SymbolResolving) | Yes | 1 | Synchronous scope chain walk; immediate return |
| P6 (FunctionDispatching) | Yes | 25,000 | One per physical stack frame |
| P7 (ArgumentBinding) | Yes | 1 per active call | `bind()` runs synchronously per function invocation |
| P8 (BuiltinExecuting) | Yes | 25,000 | Bounded by physical stack depth |
| P9 (SpecialOpExecuting) | Yes | 25,000 | Bounded by physical stack depth |
| P10 (MacroExpanding) | Yes | 25,000 | Bounded by physical stack; TROBlock prevents frame elision |
| P11 (MacroReEval) | Yes | 25,000 | One per macro expansion; consumed by T14 before next macro can expand |
| P12 (UserFunExecuting) | Yes | 25,000 | Bounded by physical stack depth |
| P13 (TRODecision) | Yes | 50,000 | Bounded by logical stack limit; each TRO check either fires or does not |
| P14 (FileLoadActive) | Yes | 25,000 | Each load-file is a builtin call consuming a frame |
| P15 (ErrorRaised) | Yes | 1 per eval frame | Error is returned immediately; not accumulated |
| P16 (HandlerSearching) | Yes | N_handlers per binding list | Finite list iteration per handler-bind |
| P17 (ConditionActive) | Yes | 25,000 | Bounded by handler-bind nesting, bounded by stack depth; `defer PopCondition()` guarantees cleanup |
| P18 (ResultProduced) | Yes | 1 per eval call | Synchronous return value |
| P19 (StackCapacity) | Yes | 25,000 | `DefaultMaxPhysicalStackHeight`; hard-coded constant |
| P20 (AllocBudget) | Yes | 10,485,760 per operation | `DefaultMaxAlloc`; resets per allocation call |
| P21 (PackageState) | **Weakly bounded** | Grows monotonically with `in-package` calls | No package deletion; bounded by number of `in-package` calls in finite source; but no explicit cap |
| P22 (ArgEvalLoop) | Yes | O(max_args * call_depth) | Finite number of cells per s-expression |
| P23 (PackageSaved) | Yes | 25,000 | One saved context per load/call; bounded by call depth |

**Key finding:** All places are bounded. P21 (PackageState) is the only monotonically growing accumulator without an explicit cap, but it is bounded by the finite source program (each package requires an `in-package` call in source text). There is no mechanism for a running program to dynamically create unbounded packages -- `builtinInPackage` at `builtins.go:72-77` requires a symbol/string argument that comes from source text.

---

## 7. Bottleneck Ranking

Ranked by contention severity: how often the resource is the binding constraint on evaluation throughput.

| Rank | Resource (Place) | Contention Pattern | Utilization Frequency | Impact on Programs |
|---|---|---|---|---|
| 1 | **Physical Call Stack (P19)** | Every function call, special op, macro, and file load consumes one slot. Non-TRO-eligible calls (macros with `TROBlock`, `handler-bind`, `ignore-errors`) accumulate without optimization. | Every function call | Primary throughput limiter for deeply recursive code. 25,000 is generous for typical programs but chains like `load-file -> defun -> handler-bind -> recursive-fn` consume frames at 3:1 ratio per conceptual operation. |
| 2 | **Logical Call Stack (P13)** | TRO-optimized tail-recursive loops accumulate logical height without consuming physical frames. 50,000 is the ceiling for the logical depth including elided frames. | Every TRO-eligible tail call | Limits the number of tail-recursive iterations. A `(labels [(loop (i) (if (= i 50001) i (loop (+ i 1))))] (loop 0))` overflows logical stack. |
| 3 | **Package Registry (P21)** | Every `set`, `defun`, `defmacro`, `export`, `in-package`, `use-package` operation touches this. Single shared mutable state pointer (`Runtime.Package`) swapped on every cross-package function call. | Every evaluation that reads/writes symbols | No concurrent contention (single-threaded), but structural bottleneck for future parallelism. Package swap in `env.call()` (`env.go:1191-1201`) with `defer` is a non-trivial cost per cross-package call. |
| 4 | **Per-Operation Allocation (P20)** | Only 2 of ~15+ allocation-capable builtins check `MaxAllocBytes()`: `make-sequence` and `string:repeat`. The others (`concat`, `append`, `map`, `foldl`, `stable-sort`, `zip`, `vector`, JSON deserialization) allocate without limit. | Rare for checked builtins; frequent for unchecked ones | The 10MB per-operation cap is a bottleneck for legitimate large data processing but is easily circumvented by using unchecked allocation paths. |
| 5 | **LEnv Scope Allocation** | Every `let`, `let*`, `flet`, `labels`, `macrolet`, lambda creation, and function call copies or creates a new scope (`NewEnv`, `newEnvN`, `env.Copy()`). | Every function call + every binding form | GC pressure proportional to call rate. The `newEnvN(parent, n)` optimization (`env.go:126-147`) pre-sizes the map but still allocates. `fun.Env().Copy()` in `bind()` (`env.go:1231`) copies the entire scope map for each user function call. |
| 6 | **Scanner Buffer** | 128 KiB per parse. Non-trivial for embedded systems with tight memory. | Once per `load-file`, `load-string`, `load-bytes` | Negligible for typical usage. Could matter with hundreds of nested `load-file` calls (128 KiB * load_depth). |
| 7 | **Condition Stack** | Occupied only during `handler-bind` handler execution. Very short lifetime (handler eval + defer cleanup). | Only during error handling | Extremely low pressure; bounded by nesting depth. |

---

## 8. Timed Extensions

Modeled as a Stochastic Petri Net (SPN) with transition firing time distributions. Parameters estimated from code structure analysis and Go runtime characteristics.

| Transition | Distribution | Parameters | Justification |
|---|---|---|---|
| T0 (InitScanner) | Deterministic | ~1 us | `make([]byte, 128<<10)` allocation + scanner struct init |
| T1 (Lexing) | Exponential | mean ~0.3 us/token | Character-by-character `ReadRune` with state machine dispatch; `scanner.go` rune loop |
| T2 (Parsing) | Exponential | mean ~1.5 us/expression | Recursive descent; `LVal` allocation per node; bracket matching |
| T3 (EnqueueForEval) | Deterministic | ~0.01 us | Slice assignment; no allocation |
| T4 (EvalDispatch) | Deterministic | ~0.05 us | Type switch on `v.Type` at `env.go:835`; branch prediction friendly |
| T5 (SymbolResolve) | Log-normal | median ~0.1 us, sigma 0.5 | `getSimple` loop: scope map lookup O(1) amortized; chain depth varies. Qualified symbols add split + registry lookup. Boolean singletons short-circuit (`env.go:370-375`). |
| T6 (SExprCellEval) | Exponential | mean ~(0.5 + N * T_eval) us | Allocates `make([]*LVal, 1, len(cells))` at `env.go:1104`; iterates cells evaluating each for non-special funs |
| T7 (DispatchBuiltin) | Log-normal | median ~0.3 us, sigma 3 us | Extreme variance: `identity`/`type-of` ~0.05 us; `sort` O(n log n); `json:load-bytes` unbounded |
| T8 (DispatchSpecialOp) | Exponential | mean ~2 us + body eval | Control flow operators: `if` evaluates 1-2 branches; `let*` binds N variables; `handler-bind` sets up handlers |
| T9 (DispatchMacro) | Exponential | mean ~5 us + body eval + stamp | Macro body evaluation + `stampMacroExpansion` AST walk (`macro.go:233-251`) + `shallowUnquote` (`lisp.go:632-637`) |
| T10 (DispatchUserFun) | Exponential | mean ~1.5 us overhead + body | `bind()`: `fun.Env().Copy()` (map copy) + formal/actual matching; then body evaluation |
| T13 (MacroComplete) | Deterministic | ~0.5 us | `stampMacroExpansion` walk: O(expansion_size); `shallowUnquote`: O(1) copy |
| T14 (MacroReEvaluate) | Deterministic | ~0.05 us | `goto eval`; no allocation |
| T16 (TROFire) | Deterministic | ~0.3 us | `TerminalFID` scan: O(terminal chain length) at `stack.go:113-145`. Typically 1-3 frames. |
| T17 (TROAccumulate) | Deterministic | ~0.1 us | Integer addition + comparison |
| T18 (LoadFileEnter) | Log-normal | median ~200 us | File I/O (`os.ReadFile`), scanner init, full parse, package save |
| T20 (RaiseError) | Deterministic | ~1 us | `Stack.Copy()`: `make([]CallFrame, len(frames))` + `copy()` at `stack.go:86-93` |
| T21 (HandleError) | Exponential | mean ~2 us + handler body | Linear scan of bindings list (`op.go:669-694`) + `PushCondition` + handler evaluation |
| T23 (StackPush) | Deterministic | ~0.15 us | `append(Frames, CallFrame{...})` + `checkHeightPush()`: 2 integer comparisons |
| T24 (StackPop) | Deterministic | ~0.05 us | Slice truncation + zero fill (`stack.go:204-212`) |
| T25 (AllocCheck) | Deterministic | ~0.02 us | Single integer comparison against `MaxAllocBytes()` |
| T29 (PackageSwap) | Deterministic | ~0.1 us | Pointer assignment + defer setup for restoration |
| T30 (PackageRestore) | Deterministic | ~0.05 us | Pointer assignment (defer fires) |

**Critical path for `(f x y)` where `f` is a user-defined function:**
```
T4 (0.05) -> T6 (0.5 + 2*T5) -> T23 (0.15) -> T10 (1.5 + body) -> T15 (0.05) -> T24 (0.05)
           = ~0.05 + ~0.7 + 0.15 + 1.5 + body_time + 0.05 + 0.05
           = ~2.5 us overhead + body evaluation time
```

**Critical path for `(f x y)` where `f` is a builtin:**
```
T4 (0.05) -> T6 (0.5 + 2*T5) -> T23 (0.15) -> T7 (0.3) -> T11 (0.05) -> T24 (0.05)
           = ~1.1 us overhead + builtin execution time
```

---

## 9. Key Invariants

### Invariant 1: Stack Frame Conservation (S-invariant)

**Formal:** `|tokens_in_P19| + |active_frames| = MaxHeightPhysical` is a constant for all reachable markings.

**Where** `|active_frames|` = `|tokens in P6| + |tokens in P8| + |tokens in P9| + |tokens in P10| + |tokens in P12|`.

**Mechanism:** Every `PushFID` (T23) removes one token from P19 and places one in an execution place. Every `Pop` (T24, via `defer`) removes one from the execution place and returns one to P19. The `defer` pattern in Go guarantees execution even during panic unwinding.

**Proof:** `PushFID` is always paired with `defer Pop()`:
- `MacroCall`: push at `env.go:925`, defer pop at `env.go:929`
- `SpecialOpCall`: push at `env.go:972`, defer pop at `env.go:976`
- `funCall`: push at `env.go:1045`, defer pop at `env.go:1049`

No other code path pushes or pops frames. The invariant holds.

### Invariant 2: Condition Stack Integrity (S-invariant)

**Formal:** `len(conditionStack) = |active handler-bind handlers currently executing|`

**Mechanism:** `PushCondition()` at `op.go:688` and `defer PopCondition()` at `op.go:689` form a balanced pair within `opHandlerBind`. This is the ONLY call site for `PushCondition` in the entire codebase.

**Verification via grep:** `PushCondition` appears only at `runtime.go:40-42` (definition) and `op.go:688` (single call site). `PopCondition` appears only at `runtime.go:45-52` (definition) and `op.go:689` (single defer site).

### Invariant 3: Package Monotonicity (P-invariant)

**Formal:** Once `Registry.Packages[name] != nil`, it remains non-nil for all subsequent states.

**Mechanism:** `DefinePackage` at `package.go:20-28` only creates packages, never deletes. There is no `DeletePackage` or `RemovePackage` function in the codebase. The `Packages` map only grows.

### Invariant 4: Package Context Restoration (behavioral)

**Formal:** After `env.load()`, `env.LoadFile()`, `WithLoader()`, or `env.call()` (for cross-package calls) completes, `env.Runtime.Package` equals its value before the call.

**Mechanism:**
- `env.load()` at `env.go:279-284`: `defer func() { env.Runtime.Package = currPkg }()`
- `WithLoader()` at `config.go:40-46`: `defer func() { env.InPackage(Symbol(pkg)) }()`
- `env.call()` at `env.go:1196-1199`: `defer func() { env.Runtime.Package = outer }()`

### Invariant 5: TRO Correctness (behavioral)

**Formal:** For any tail-recursive call chain, `physical_height + sum_of_elided_frames = logical_height`.

**Mechanism:** When TRO fires at `env.go:1067-1076`:
```go
env.Runtime.Stack.Top().HeightLogical += r.tailRecElided()
```
The logical height accumulates the elided count. `CheckHeight()` at `env.go:1070-1072` ensures `logical_height < MaxHeightLogical`. The physical frame is reused (`goto callf`) without pushing a new one.

When TRO does NOT fire (non-terminal or different FID), the normal push increments logical height by 1 (`stack.go:150-152`):
```go
heff := 0
if len(s.Frames) > 0 {
    heff = s.Top().HeightLogical + 1
}
```

### Invariant 6: TROBlock Prevents Incorrect Elision (safety)

**Formal:** `TerminalFID()` never returns a chain that crosses a TROBlock frame.

**Mechanism:** The `TerminalFID` scan at `stack.go:118-145` checks `s.Frames[i].TROBlock` at `stack.go:123`. If encountered, it panics (indicating an implementation bug) because a terminal frame should never have TROBlock set -- the TROBlock is set AFTER terminal is set to false. The `opHandlerBind` and `opIgnoreErrors` set TROBlock directly on the current frame, which prevents their frames from being marked terminal.

### Invariant 7: Allocation Limit Per-Operation (partial)

**Formal:** Any single call to `builtinMakeSequence` or `builtinRepeat` (string:repeat) allocates at most `MaxAllocBytes()` elements or bytes.

**Caveat:** This invariant does NOT hold for the ~13 other allocation-capable builtins (`concat`, `append`, `map`, `foldl`, `stable-sort`, `zip`, `vector`, JSON deserialization, etc.) which allocate without checking `MaxAllocBytes()`. This is a known gap documented in prior analyses.

### Invariant 8: Error Source Association (diagnostic quality)

**Formal:** Every `LError` value reaching the user has either a `CallStack` (set at creation via `env.Error`/`env.Errorf`) or gets one associated via `ErrorAssociate` (`env.go:803-818`).

**Mechanism:** `env.Error()` at `env.go:731` calls `env.ErrorCondition()` which copies `env.Runtime.Stack` at `env.go:760`. `ErrorAssociate` fills in missing stacks at `env.go:807-808`. The `ErrorAssociate` call appears in `Eval` (symbol resolution errors, `env.go:858`), `EvalSExpr` (`env.go:892`), and `getSimple`/`packageGet` (`env.go:397-400`, `419-422`).

---

## 10. Top 5 Recommendations

### Recommendation 1: Add Cycle Detection for `load-file` (Priority: High)

**Petri Net Finding:** The file loading re-entrant path (P8 -> T18 -> P14 -> T0 -> ... -> P8) creates a cycle in the net that is only bounded by the physical stack limit (25,000 frames). A mutual `load-file` cycle between 2 files consumes frames at a rate of ~2-5 frames per round trip, giving ~5,000-12,500 iterations before the generic "physical stack height exceeded" error appears.

**Recommendation:** Add a `loadingFiles map[string]bool` to `Runtime` (or a `[]string` stack). In `builtinLoadFile`, check if the resolved file path is already in the set. If so, return a clear error: "circular load detected: a.lisp -> b.lisp -> a.lisp". This transforms the deadlock-free-but-wasteful cycle into an immediate, informative failure. Cost: one map lookup per `load-file`.

### Recommendation 2: Extend `MaxAllocBytes` to All Allocation Paths (Priority: High)

**Petri Net Finding:** The `AllocBudget` resource place (P20) has the transition `T25` (AllocCheck) guarding only 2 out of ~15+ allocation-capable operations. The other paths fire `T7` (BuiltinComplete) with `Unchecked` color tokens, bypassing the allocation guard entirely. This means P20 provides no protection for `concat`, `append`, `map`, `foldl`, `stable-sort`, `zip`, `vector`, `bytes`, JSON load/dump, and general list construction.

**Recommendation:** Create a helper `func (env *LEnv) CheckAlloc(n int) *LVal` that returns `LError` if `n > env.Runtime.MaxAllocBytes()`. Insert calls before all significant allocations: `builtinConcat` (`builtins.go`), `builtinAppend`, `builtinMap`, `builtinVector`, JSON deserialization (`libjson.go`), etc. This unifies all allocation paths under a single resource guard.

### Recommendation 3: Add Per-Evaluation Cumulative Allocation Tracking (Priority: Medium)

**Petri Net Finding:** Even with per-operation limits (Recommendation 2), the net allows unbounded token accumulation in practice because P20 resets per-operation. A program executing `(dotimes (i 1000) (make-sequence 0 5000000))` creates 5 trillion elements across 1000 operations, each individually under the 10MB limit. The Go GC handles this in practice, but embedded ELPS processing untrusted input has no defense.

**Recommendation:** Add `TotalAllocUsed int64` and `MaxTotalAlloc int64` to `Runtime`. Increment `TotalAllocUsed` in `CheckAlloc`. When `TotalAllocUsed > MaxTotalAlloc` (and `MaxTotalAlloc > 0`), return an error. Default `MaxTotalAlloc = 0` (unlimited) for backward compatibility. This transforms P20 from a per-operation reset to a cumulative counter.

### Recommendation 4: Add Explicit Condition Stack Depth Bound (Priority: Low)

**Petri Net Finding:** P17 (ConditionActive) is bounded by the physical stack depth (Invariant 2), but there is no explicit check on `conditionStack` length. The implicit bound is sufficient for correctness, but a future code change that introduces a non-stack-bounded path to `PushCondition` would silently break the invariant.

**Recommendation:** Add `MaxConditionDepth int` to `Runtime` (default: 1000). Check in `PushCondition()`: `if len(r.conditionStack) >= r.MaxConditionDepth { return error }`. This provides defense-in-depth and makes the bound explicit rather than relying on the structural coupling to the physical stack.

### Recommendation 5: Instrument TRO and Package Swap Overhead (Priority: Low)

**Petri Net Finding:** The timed analysis reveals that package swap (`env.go:1191-1201`) occurs on every cross-package function call and involves a `defer` setup + pointer swap + restoration. Similarly, the `TerminalFID` scan (`stack.go:113-145`) runs on every user function call regardless of whether TRO is applicable, iterating backward through the frame stack.

**Recommendation:** Add optional counters to `Runtime` (gated by `Profiler != nil` or a debug flag): `TROAttempts`, `TROSuccesses`, `TROFramesElided`, `PackageSwapCount`, `TerminalFIDScanDepth` (histogram). This enables performance diagnosis for programs that unexpectedly overflow the logical stack (TRO not firing because functions are not in tail position) or that suffer from frequent package swaps.

---

## Summary

The ELPS evaluation pipeline, modeled as a Colored Petri Net with 24 places, 31 transitions, and 5 token color types, exhibits the following formal properties:

- **Deadlock-free:** No reachable marking disables all transitions. Seven scenarios were examined (stack exhaustion, logical overflow, condition leak, macro cycles, mutual file loading, registry contention, terminal loops). All resolve via error propagation through the return value chain. The `defer`-based cleanup pattern is the key mechanism.
- **Live:** Every evaluation terminates, proven via the well-founded measure `M = (physical_slack, logical_slack, AST_depth)` with lexicographic ordering. Each recursive evaluation step strictly decreases M. Resource limits provide hard floors.
- **Bounded:** All 24 places have finite upper bounds. Physical stack (25,000 frames), logical stack (50,000), and per-operation allocation (10,485,760) are hard ceilings derived from code constants. The package registry grows monotonically but is bounded by the finite source program.
- **Primary bottleneck:** Physical call stack (P19) is the most contested resource, consumed by all five call types. Non-TRO-eligible paths (macros, handler-bind, ignore-errors, load-file) accumulate without optimization.
- **Key gap:** Allocation safety (P20/T25) covers only 2 of ~15+ allocation-capable builtins. The unchecked allocation color class bypasses the resource guard entirely.
- **Single-threaded invariant:** The absence of mutexes on core data structures (PackageRegistry, Package.Symbols, CallStack, conditionStack) means the net executes sequentially. This eliminates concurrency hazards but precludes parallel evaluation.
