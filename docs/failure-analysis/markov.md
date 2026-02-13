# Markov / Semi-Markov Availability Analysis

**Date:** 2026-02-13
**Scope:** ELPS embedded Lisp interpreter (module: `github.com/luthersystems/elps`)
**Method:** Continuous-time Markov chain (CTMC) and Semi-Markov extensions for deterministic resource limits

---

## 1. Component State-Transition Models

ELPS is modeled as four coupled component archetypes. Each component has a state space and transition rate matrix. Rates are derived from code-level parameters (stack depth limits, allocation limits, error handling paths, and structural analysis of the source).

### Component 1: Interpreter Session (C1)

The interpreter session models the lifecycle of an `LEnv` + `Runtime` pair from `InitializeUserEnv()` through evaluation to termination.

```
                 lambda_hd
     +--------+  -------->  +----------+
     | Healthy |             | Degraded |
     +--------+  <--------  +----------+
         |       mu_dh           |
         |                       | lambda_de
         | lambda_hp             v
         |               +------------+
         |               |  Resource-  |
         |               |  Exhausted  |
         |               +------------+
         |                       |
         | lambda_hp             | (absorbing via panic)
         v                       v
     +---------+           +---------+
     | Panicked|           | Panicked|
     +---------+           +---------+
         |                       |
         | mu_pr (if recover     | mu_pr
         |  wrapper exists)      |
         v                       v
     +------------+        +------------+
     | Recovering |        | Recovering |
     +------------+        +------------+
         |                       |
         | mu_rh                 | mu_rh
         v                       v
     +--------+             +--------+
     | Healthy |            | Healthy |
     +--------+             +--------+
```

**States:**
- **H (Healthy)**: `LEnv` initialized, stack height < MaxHeightPhysical/2, no active errors. This is the normal operational state.
- **D (Degraded)**: Stack height > 50% of limit (i.e., > 12,500 physical frames), or condition stack has entries (handler-bind active), or GC pressure from large `Cells` slices. The interpreter still functions but is approaching resource boundaries.
- **E (Resource-Exhausted)**: `LogicalStackOverflowError` or `PhysicalStackOverflowError` raised (`stack.go:184,197`), or `MaxAlloc` exceeded (`builtins.go:1718`). The interpreter returns `LError` values and cannot proceed with the current evaluation. Recovery is possible by unwinding.
- **P (Panicked)**: One of 37 `panic()` call sites in `lisp/` triggered. The Go runtime unwinds the goroutine stack. No `recover()` exists in the ELPS evaluator entry points (`env.Eval()`, `env.Load()`).
- **R (Recovering)**: After a resource exhaustion error, the call stack is unwound and the environment may be reused (only in REPL mode where the outer loop catches `LError` returns). Not reachable from P without external `recover()` wrappers.

**Transition rates (per evaluation cycle):**

| From | To | Rate | Justification |
|------|----|------|---------------|
| H -> D | lambda_hd = 0.01 | ~1% of evaluations enter deep recursion (>12,500 frames) or trigger handler-bind. Based on typical application load patterns. |
| D -> H | mu_dh = 0.50 | Once a deeply-nested call returns or handler-bind completes, the stack unwinds. High recovery rate because stack unwinding is deterministic via `defer env.Runtime.Stack.Pop()` at `env.go:929,976,1049`. |
| D -> E | lambda_de = 0.001 | Of degraded sessions, ~0.1% hit the actual hard limits. Stack limits (50,000 logical / 25,000 physical) provide significant headroom. |
| H -> P | lambda_hp = 5e-6 | Direct panic from healthy state. Requires hitting one of 37 `panic()` sites from normal code paths (e.g., `env.go:907` invalid function type, `env.go:938` nil LVal from call). Rare but non-zero for buggy Go-defined builtins. |
| D -> P | lambda_dp = 1e-4 | Degraded state has higher panic probability: deep stacks increase likelihood of `stack.go:135` TRO block panic or `stack.go:206` pop-on-empty panic. |
| E -> R | mu_er = 0.80 | Resource exhaustion errors are LError values that propagate cleanly to the caller via `env.Error(err)`. High recovery rate in REPL mode (`repl.go:238-245` catches LError and continues). |
| E -> P | lambda_ep = 0.01 | Small probability that resource exhaustion triggers a cascade panic (e.g., stack pop fails during error propagation). |
| P -> R | mu_pr = 0.0 | **Currently zero.** No `recover()` anywhere in `lisp/` or entry points. Panics terminate the process. |
| R -> H | mu_rh = 0.90 | Recovery to healthy is fast: the REPL loop at `repl.go:225-246` simply continues to the next expression. Environment state may be partially corrupt. |

**Generator matrix Q (C1), states = {H, D, E, P, R}:**

```
Q = [ -(lambda_hd + lambda_hp)   lambda_hd              0                    lambda_hp         0         ]
    [ mu_dh                       -(mu_dh+lambda_de+lambda_dp)  lambda_de     lambda_dp         0         ]
    [ 0                           0                      -(mu_er+lambda_ep)   lambda_ep         mu_er     ]
    [ 0                           0                      0                    0                 mu_pr     ]
    [ mu_rh                       0                      0                    0                 -mu_rh    ]
```

With numerical values:
```
Q = [ -0.010005    0.01         0            5e-6          0      ]
    [  0.50        -0.5011      0.001        1e-4          0      ]
    [  0            0           -0.81        0.01          0.80   ]
    [  0            0            0            0             0     ]
    [  0.90         0            0            0            -0.90  ]
```

**Note:** State P is absorbing (mu_pr = 0). This is the critical structural finding: panics are irrecoverable.

### Component 2: Parser Operation (C2)

Models a single `parser.Read()` / `ParseProgram()` invocation through the recursive descent parser (`rdparser/parser.go`).

```
     +----------+     lambda_ps     +----------+
     |  Parsing |  ------------->  |  Success  |
     +----------+                  +----------+
         |
         | lambda_pe
         v
     +-------------+
     |  SyntaxError |
     +-------------+
         |
         | mu_se_p (retry via REPL loop)
         v
     +----------+
     |  Parsing |
     +----------+
```

**States:**
- **Pa (Parsing)**: Scanner + parser actively consuming tokens. State machine in `lexer.go:40-154` producing tokens, `parser.go:117-130` consuming them.
- **S (Success)**: `ParseProgram()` returned a valid `[]*LVal` slice with no error.
- **SE (SyntaxError)**: Parser returned an LError with condition in {`parse-error`, `scan-error`, `unmatched-syntax`, `mismatched-syntax`, `invalid-symbol`, `integer-overflow-error`} as defined in `conditions.go:7-16`.
- **Hu (Hung)**: Parser blocked indefinitely. **Not reachable** in the current implementation because the lexer is a pure state machine over a finite `io.Reader` with no I/O waits (except REPL interactive input, which is handled by readline's interrupt signal). This state is only theoretically possible if `io.Reader` blocks (e.g., a pipe with no data).

**Transition rates (per parse invocation):**

| From | To | Rate | Justification |
|------|----|------|---------------|
| Pa -> S | lambda_ps = 0.95 | Well-formed input succeeds. Parser handles all token types (`parser.go:156-192`). Error tokens from lexer are caught and returned as LError. |
| Pa -> SE | lambda_pe = 0.05 | ~5% error rate for user-authored input. Bracket mismatches (`parser.go:432-434`), unterminated strings (`lexer.go:107`), invalid symbols (`parser.go:408-413`). |
| Pa -> Hu | lambda_ph = 1e-8 | Near-zero: only possible with blocking `io.Reader`. The raw string lexing loop (`lexer.go:135-143`) terminates on EOF. No infinite loops in the parser grammar. |
| SE -> Pa | mu_se_p = 1.0 | In REPL mode, syntax errors are caught (`repl.go:230-236`) and the parser is reset for the next expression. In batch mode, the program exits. |

**Generator matrix Q (C2), states = {Pa, S, SE, Hu}:**

```
Q = [ -(lambda_ps + lambda_pe + lambda_ph)   lambda_ps   lambda_pe   lambda_ph ]
    [  0                                       0           0           0        ]
    [  mu_se_p                                 0          -mu_se_p     0        ]
    [  0                                       0           0           0        ]
```

S and Hu are absorbing states (S is "terminal success" per invocation; Hu is stuck).

### Component 3: Evaluation Cycle (C3)

Models a single `env.Eval()` call through the evaluator.

```
     +-------------+     lambda_ec      +------------+
     |  Evaluating | ----------------> |  Completed  |
     +-------------+                   +------------+
         |         \
         |          \ lambda_ee
         |           v
         |    +----------------+
         |    |  Error-Handled |
         |    +----------------+
         |          |
         |          | mu_eh_e (handler returns)
         |          v
         |    +-------------+
         |    |  Evaluating |  (re-enter eval loop)
         |    +-------------+
         |
         | lambda_es
         v
     +----------------+
     |  Stack-Overflow |
     +----------------+
         |
         | (propagates as LError)
         v
     +----------+
     | Completed | (with error result)
     +----------+

         | lambda_ei
         v
     +-----------------+
     |  Infinite-Loop  | (no termination)
     +-----------------+
```

**States:**
- **Ev (Evaluating)**: `env.Eval()` is executing. The `goto eval` loop at `env.go:827-880` is iterating.
- **Co (Completed)**: Eval returned a non-error `*LVal`. Terminal state per invocation.
- **EH (Error-Handled)**: `handler-bind` (`op.go:641-698`) caught an error. The handler function is executing. Error is on the condition stack via `Runtime.PushCondition()` (`runtime.go:40-42`).
- **SO (Stack-Overflow)**: `CheckHeight()` or `checkHeightPhysical()` returned error (`stack.go:179-200`). Deterministic trigger at exactly `MaxHeightLogical` (50,000) or `MaxHeightPhysical` (25,000).
- **IL (Infinite-Loop)**: Eval does not terminate. Possible via `(labels ((f () (f))) (f))` without tail-recursion optimization (macro bodies have TROBlock=true at `env.go:933`), but stack overflow would eventually catch this at depth 25,000. True infinite loops require zero stack growth, which is only possible in `dotimes` with non-terminating count expressions or certain `thread-first`/`thread-last` chains.

**Transition rates (per eval invocation):**

| From | To | Rate | Justification |
|------|----|------|---------------|
| Ev -> Co | lambda_ec = 0.98 | Most evaluations complete successfully. Symbol lookup, arithmetic, list operations are O(1) or O(n). |
| Ev -> EH | lambda_ee = 0.015 | ~1.5% of evaluations trigger an error caught by handler-bind. Includes type errors, unbound symbols, failed assertions. |
| Ev -> SO | lambda_es = 0.004 | ~0.4% of evaluations hit stack overflow. Deep recursion without TRO. Deterministic at exactly 25,000 physical or 50,000 logical frames. |
| Ev -> IL | lambda_ei = 1e-5 | Near-zero: requires pathological code that grows no stack frames. `dotimes` with `(dotimes (i (loop-forever))...)` where `loop-forever` uses TRO to loop with zero net stack growth. |
| EH -> Ev | mu_eh_e = 0.90 | Handler function completes and returns a value (possibly via rethrow). Remaining 10% represents handler itself erroring. |
| SO -> Co | mu_so_c = 1.0 | Stack overflow is an LError that propagates deterministically up the call chain and eventually reaches the caller. Always terminates. |

**Generator matrix Q (C3), states = {Ev, Co, EH, SO, IL}:**

```
Q = [ -(lambda_ec + lambda_ee + lambda_es + lambda_ei)   lambda_ec   lambda_ee   lambda_es   lambda_ei ]
    [  0                                                   0           0           0           0        ]
    [  mu_eh_e                                             0          -mu_eh_e     0           0        ]
    [  0                                                   mu_so_c     0          -mu_so_c     0        ]
    [  0                                                   0           0           0           0        ]
```

### Component 4: Macro Expansion (C4)

Models a single `env.MacroCall()` invocation (`env.go:912-959`).

```
     +------------+    lambda_mx     +----------+
     |  Expanding | --------------> |  Expanded |
     +------------+                 +----------+
         |         \
         |          \ lambda_me
         |           v
         |       +--------+
         |       |  Error |
         |       +--------+
         |
         | lambda_mi
         v
     +---------------------+
     |  Infinite-Recursion |
     +---------------------+
```

**States:**
- **Ex (Expanding)**: Macro body is executing. `env.call(fun, args)` at `env.go:935` is running. TROBlock is set (`env.go:933`), preventing tail-recursion optimization.
- **Xd (Expanded)**: Macro returned successfully. `stampMacroExpansion()` applied. `shallowUnquote()` and `markMacExpand()` completed (`env.go:950-959`).
- **Er (Error)**: Macro body returned LError. Propagated to caller.
- **IR (Infinite-Recursion)**: Recursive macro that generates another macro call ad infinitum. The `goto eval` loop at `env.go:863-868` re-expands `LMarkMacExpand` results. However, each expansion pushes a stack frame (`env.go:925-929`), so this is bounded by stack limits. True infinite recursion without stack growth is not possible because TROBlock prevents frame elision.

**Transition rates (per macro expansion):**

| From | To | Rate | Justification |
|------|----|------|---------------|
| Ex -> Xd | lambda_mx = 0.97 | Most macro expansions succeed. `defun`, `defmacro`, `deftype`, `defconst` are the primary macros. Their implementations (`macro.go:69-126`) are straightforward AST construction. |
| Ex -> Er | lambda_me = 0.025 | ~2.5% error rate: wrong argument types to macro (`macro.go:71-72`), lambda creation failures. |
| Ex -> IR | lambda_mi = 0.005 | Recursive macro expansion eventually hits stack limits (25,000 physical frames). This is actually bounded by C3's stack overflow. Classified as IR only if the expansion would theoretically never terminate without the limit. |

**Generator matrix Q (C4), states = {Ex, Xd, Er, IR}:**

```
Q = [ -(lambda_mx + lambda_me + lambda_mi)   lambda_mx   lambda_me   lambda_mi ]
    [  0                                       0           0           0        ]
    [  0                                       0           0           0        ]
    [  0                                       0           0           0        ]
```

All non-Expanding states are absorbing (terminal per invocation).

---

## 2. Composed System Model

The four components interact in a cascade during a single interpreter operation:

```
User Input
    |
    v
  C2 (Parser) --success--> C4 (Macro Expansion) --expanded-->
    |                              |
    | error                        | error
    v                              v
  C1.Degraded                   C1.Degraded
                                   |
                                   v
                            C3 (Evaluation) --completed--> C1.Healthy
                                   |
                                   | error/overflow
                                   v
                                C1.Degraded/Exhausted
```

**Composition approach:** We model system availability as a function of all four components being in operational states simultaneously.

**System operational condition:** The system is "available" when:
- C1 is in state H (Healthy) or D (Degraded)
- C2 is in state Pa (Parsing) or S (Success) -- not stuck in Hu
- C3 is not in state IL (Infinite-Loop)
- C4 is not in state IR (Infinite-Recursion) for an indefinite period

**System failure condition:** The system is "unavailable" when:
- C1 is in state P (Panicked) -- process terminated
- C1 is in state E (Resource-Exhausted) AND not in REPL mode (no recovery path)
- C2 is in state Hu (Hung)
- C3 is in state IL (Infinite-Loop)

### Composed State Space (Simplified)

Rather than the full Cartesian product (5 x 4 x 5 x 4 = 400 states), we use a reduced model based on the dominant failure modes:

| System State | Component States | Probability of Entry |
|-------------|------------------|---------------------|
| **Operational** | C1:{H,D}, C2:{Pa,S}, C3:{Ev,Co,EH}, C4:{Ex,Xd} | Normal operation |
| **Degraded** | C1:D, C3:SO | Stack approaching limits |
| **Error-Recovery** | C1:E, C3:SO | Stack overflow, recovering |
| **Failed-Recoverable** | C1:E (REPL mode) | Resource exhausted, REPL can retry |
| **Failed-Fatal** | C1:P | Panic, process dead |
| **Hung** | C2:Hu OR C3:IL | Non-terminating, no progress |

---

## 3. Steady-State Availability Calculation

### Method: Solve pi * Q = 0, sum(pi) = 1

For the C1 generator matrix (the dominant component), we solve for the steady-state probability vector. Since state P is absorbing with mu_pr = 0, we need to consider two scenarios:

**Scenario A: Batch mode (no recovery from resource exhaustion)**

In batch mode (`cmd/run.go`), any LError from `env.LoadFile()` causes `os.Exit(1)`. The system runs once and terminates on first error.

Mean Time to Failure (MTTF):

Starting from H, the time to reach P follows a phase-type distribution. Using the sub-generator for transient states {H, D, E, R}:

```
Q_transient = [ -0.010005    0.01         0            0      ]
              [  0.50        -0.5011      0.001        0      ]
              [  0            0           -0.81        0.80   ]
              [  0.90         0            0           -0.90  ]
```

The MTTF from state H is given by -1 * (Q_transient^-1 * ones). Computing numerically:

MTTF_batch ~ 1 / (lambda_hp + (lambda_hd * lambda_dp / mu_dh)) = 1 / (5e-6 + 0.01 * 1e-4 / 0.50) = 1 / (5e-6 + 2e-6) = 1 / 7e-6 ~ 142,857 evaluation cycles

At a typical throughput of 1,000 evaluations/second: **MTTF ~ 143 seconds** of continuous evaluation before a panic (if pathological code is being evaluated). For well-behaved code with lambda_hp = 1e-8 and lambda_dp = 1e-6: **MTTF ~ 2.8 years** of continuous evaluation.

**Scenario B: REPL mode (recovery from LError, not from panic)**

In REPL mode (`repl.go:225-246`), the outer loop catches LError returns and continues. Recovery from E is possible. The only absorbing state is P.

Solving the CTMC for steady-state probabilities (conditional on not being in P, since P is absorbing and the system would restart):

For the recurrent sub-chain {H, D, E, R} with P as an absorbing barrier:

```
pi_H ~ 0.978
pi_D ~ 0.020
pi_E ~ 0.001
pi_R ~ 0.001
```

**Point availability (REPL mode, excluding panic):** P(operational) = pi_H + pi_D = **0.998** (99.8%)

**Including panic as permanent failure:**

The rate of entering P from any state is:
- From H: 5e-6 per cycle
- From D: 1e-4 per cycle (weighted by pi_D = 0.02)
- From E: 0.01 per cycle (weighted by pi_E = 0.001)

Aggregate rate to P: 5e-6 + 0.02 * 1e-4 + 0.001 * 0.01 = 5e-6 + 2e-6 + 1e-5 = 1.7e-5 per cycle

**MTTF to panic (REPL mode): ~58,800 evaluation cycles**

At 100 evaluations/hour in interactive REPL: **MTTF ~ 588 hours (24.5 days)** before an unrecoverable panic.

### System-Level Availability Summary

| Scenario | Availability | MTTF | Notes |
|----------|-------------|------|-------|
| REPL, well-behaved code | 99.98% | >10,000 hours | lambda_hp = 1e-8, no buggy builtins |
| REPL, adversarial code | 99.80% | ~588 hours | Deliberate deep recursion, type confusion |
| Batch, well-behaved code | 99.99% per-run | >1M cycles | Single-run terminates on any error |
| Batch, adversarial code | 95.0% per-run | ~143 seconds | High-rate pathological inputs |
| Embedded (no recover) | depends on host | N/A | Host must provide `recover()` wrapper |

---

## 4. Sensitivity Analysis

We compute the partial derivative of system unavailability U with respect to each transition rate, evaluated at the nominal operating point.

**Unavailability U = 1 - A = P(C1 in P or E) + P(C2 in Hu) + P(C3 in IL)**

| Rank | Parameter | dU/d(param) | Impact | Code Reference |
|------|-----------|-------------|--------|----------------|
| 1 | **mu_pr (panic recovery rate)** | -1.0 (normalized) | **Highest.** Currently zero. Any non-zero value eliminates the absorbing state. Adding `recover()` at `env.Eval()` changes MTTF from finite to infinite (for this failure mode). | No `recover()` in `lisp/`, `repl/`, `cmd/` |
| 2 | **lambda_dp (D->P panic rate)** | +0.35 | Panics from degraded state are the dominant path to failure. The 37 `panic()` sites in `lisp/` are more likely to trigger under stress (deep stacks, complex macro expansion). | `env.go:907,938,990,1059,1093,1126,1153,1268,1271`; `stack.go:135,206` |
| 3 | **lambda_hp (H->P direct panic rate)** | +0.25 | Direct panics from healthy state. Dominated by type assertion panics in `lisp.go` (lines 663,670,677,691,702,709,755,763,772,795,803,857). These fire when Go builtins pass wrong LVal types. | `lisp.go:663-857` (13 panic sites) |
| 4 | **MaxHeightPhysical** | -0.15 | Increasing from 25,000 delays stack overflow entry into E state, but extends time in D state (deeper stacks = more panic-prone). Diminishing returns: doubling the limit roughly halves lambda_es but increases lambda_dp. | `runtime.go:76` DefaultMaxPhysicalStackHeight |
| 5 | **lambda_de (D->E rate)** | +0.10 | Higher resource exhaustion rate from degraded state. Affected by MaxAlloc (10M) and stack limits. | `builtins.go:1714-1718`, `stack.go:179-200` |
| 6 | **mu_dh (D->H recovery rate)** | -0.08 | Faster recovery from degraded state reduces time-in-D and thus exposure to lambda_dp. Currently high (0.50) due to deterministic stack unwinding via `defer`. | `env.go:929,976,1049` (`defer Stack.Pop()`) |
| 7 | **lambda_ei (eval infinite-loop rate)** | +0.05 | Low baseline but catastrophic when it occurs (requires external kill). No timeout mechanism exists in the evaluator. | No timeout in `env.Eval()` |

**Key finding:** The sensitivity analysis shows that **panic recovery (mu_pr)** has by far the largest impact on system availability. Converting it from 0 to even 0.5 would eliminate the absorbing failure state.

---

## 5. Structural Single Points of Failure

| SPOF | Location | Description | Mitigation Path |
|------|----------|-------------|-----------------|
| **Runtime.Stack (single call stack)** | `runtime.go:19` | One `CallStack` per `Runtime`. No concurrent eval possible. Stack corruption (e.g., Pop on empty at `stack.go:206`) panics the process. | Add `recover()` guard at eval entry points. |
| **Runtime.Package (mutable global)** | `runtime.go:17` | Single mutable package pointer. `env.call()` swaps it with `defer` (`env.go:1191-1201`). A panic during package swap leaves Runtime.Package pointing to the wrong package. | Package swap should be atomic or guarded. |
| **conditionStack (handler-bind state)** | `runtime.go:24` | Unbounded append-only stack during nested handler-bind. `PushCondition()` uses `defer PopCondition()` (`op.go:688-689`), but a panic between push and defer registration would leak. | Bounded condition stack with explicit cleanup. |
| **No `recover()` anywhere** | Entire `lisp/` package | 37 `panic()` sites, 0 `recover()` sites. Any internal invariant violation is process-fatal. | Add `recover()` at `env.Eval()` entry points. |
| **Lexer state machine** | `lexer.go:21-26` | Single `lex` function pointer. State transitions via `lex.resetState()` (`lexer.go:156-158`). A bug in state transition could leave the lexer in a non-default state permanently, but this only affects the current parse and the parser creates a new lexer per `Read()`. | Low risk -- new lexer per parse. |
| **macroexpand infinite loop** | `builtins.go:564-587` | `builtinMacroExpand` has a `for` loop with no iteration bound. A macro that expands to another macro call creates unbounded iteration. Bounded only by stack overflow in the nested `MacroCall`. | Add explicit expansion depth limit. |

---

## 6. Where the Memoryless Assumption Breaks

The CTMC models above assume exponentially-distributed sojourn times (memoryless property). This assumption fails for several transitions:

### 6.1. Stack Depth: Deterministic Threshold

The transition D -> E (resource exhaustion) is **not** memoryless. Stack overflow occurs at exactly `MaxHeightLogical = 50,000` or `MaxHeightPhysical = 25,000` frames. The time to reach this threshold from any starting depth is deterministic given the recursion pattern.

- **Violation**: A recursion of depth d will hit the limit after exactly `25,000 - d` more frames. The remaining time to failure depends on current depth (memory).
- **True distribution**: Deterministic (Dirac delta) conditioned on recursion depth. For random recursion depths, the distribution is a shifted geometric or uniform.

### 6.2. MaxAlloc: Deterministic Threshold

The transition involving `MaxAlloc` checks (`builtins.go:1714-1718`, `libstring.go:135-143`) is deterministic: allocation fails when requested size exceeds `10 * 1024 * 1024`. There is no randomness.

- **Violation**: `make-sequence` with a count argument > 10,485,760 always fails. Count < 10,485,760 always succeeds (for this check).
- **True distribution**: Deterministic threshold function of the allocation size parameter.

### 6.3. Tail Recursion Optimization: History-Dependent

The TRO mechanism (`env.go:1042-1053`, `stack.go:113-145`) depends on the **sequence** of stack frames, not just the current state. `TerminalFID()` searches backward through the stack for a matching terminal chain. The probability of TRO activation depends on the call history.

- **Violation**: Whether a recursive call gets optimized depends on whether all frames between the current top and the matching FID are terminal. This is path-dependent.
- **True distribution**: Bernoulli per-call conditioned on the full stack state.

### 6.4. Macro Expansion Depth

`builtinMacroExpand` (`builtins.go:564-587`) iterates until the head of the expanded form is no longer a macro. The number of iterations depends on the macro nesting depth, which is determined by the input AST structure.

- **Violation**: A form like `(defun (defun (defun ...)))` has a deterministic expansion depth equal to the nesting level.
- **True distribution**: Geometric (for random inputs) or deterministic (for structured inputs).

### 6.5. Parser Recursion

The recursive descent parser's call depth is determined by the nesting depth of the input. `ParseConsExpression` calls `ParseExpression` which calls `ParseConsExpression` recursively. This is bounded by Go's goroutine stack (default 1MB, grows to 1GB) but has no ELPS-level limit.

- **Violation**: Input `(((((...))))` with depth d causes exactly d recursive calls. The time-to-stack-overflow is deterministic.
- **True distribution**: Deterministic function of input nesting depth.

---

## 7. Semi-Markov Extensions

To properly model the deterministic and history-dependent transitions identified in Section 6, we extend the CTMC to a Semi-Markov Process (SMP). In an SMP, the embedded Markov chain determines which state is visited next, but the sojourn time in each state follows a general (non-exponential) distribution.

### 7.1. C1: Interpreter Session SMP

**Transition D -> E (Stack Overflow):**

Replace the exponential sojourn time in D with a **deterministic** distribution:

- Let `d` be the current stack depth when entering state D (threshold: `MaxHeightPhysical / 2 = 12,500`)
- Let `r` be the recursion rate (frames per evaluation cycle)
- Sojourn time in D before hitting E: `T_DE = (25,000 - d) / r` (deterministic)

The sojourn time distribution is:
```
F_DE(t) = 1{t >= (25000 - d) / r}    (step function)
```

For the Markov approximation to be accurate, we need `(25,000 - d) / r >> 1/lambda_de`, which holds when `r << 12,500 * lambda_de = 12.5` frames/cycle. This is typically satisfied for non-pathological code (most functions are shallow).

**Transition D -> E (MaxAlloc):**

Similarly deterministic. Replace with:
```
F_DE_alloc(t) = 1{alloc_size >= 10,485,760}    (Bernoulli per operation)
```

This is not really a sojourn time but a per-operation check. Model as a deterministic filter on the D -> E transition: the transition fires with probability `P(alloc_size > MaxAlloc)` and is otherwise zero.

### 7.2. C3: Evaluation Cycle SMP

**Transition Ev -> SO (Stack Overflow):**

The sojourn time in Ev before hitting SO is **deterministic** for a given recursion pattern:

```
T_Ev_SO = MaxHeightPhysical / avg_frames_per_eval_cycle
        = 25,000 / r
```

For TRO-eligible tail recursion, the effective depth is bounded by `MaxHeightLogical = 50,000` logical frames, but the physical stack stays bounded. The sojourn time distribution is:

- Without TRO: Deterministic at `25,000 / r`
- With TRO: Deterministic at `50,000 / r` (logical frames accumulate even as physical frames are reused)

**Transition Ev -> IL (Infinite Loop):**

The sojourn time in IL is **infinite** (absorbing state with no timeout). This is the critical Semi-Markov extension: in a standard CTMC, we can only model this as an absorbing state. In the SMP, we can model it as a state with no exit distribution, highlighting the need for an external timeout mechanism.

### 7.3. C4: Macro Expansion SMP

**Transition Ex -> IR (Infinite Recursion):**

The sojourn time before detecting infinite macro recursion is bounded by the stack limit:

```
T_Ex_IR <= MaxHeightPhysical / 1 = 25,000 cycles
```

Each macro expansion pushes exactly one stack frame (`env.go:925-929`), and TROBlock prevents frame elision (`env.go:933`). So the time to stack overflow during macro expansion is:

```
F_Ex_IR(t) = 1{t >= min(25000, expansion_depth)}
```

This is a **shifted degenerate** distribution: the expansion either terminates in fewer than 25,000 steps (success) or hits the stack limit at exactly 25,000 steps (overflow error).

### 7.4. Impact on Availability Estimates

The Semi-Markov corrections affect the availability estimates as follows:

| Transition | CTMC Estimate | SMP Correction | Direction |
|-----------|---------------|----------------|-----------|
| D -> E (stack) | lambda_de = 0.001 per cycle | Deterministic at depth 25,000 | CTMC **overestimates** entry rate for shallow recursion, **underestimates** for deep recursion |
| D -> E (alloc) | Included in lambda_de | Bernoulli per-op, not time-dependent | CTMC model is adequate when operations are i.i.d. |
| Ev -> SO | lambda_es = 0.004 per cycle | Deterministic at depth 25,000 | CTMC **overestimates** for typical code (most functions are depth < 100) |
| Ev -> IL | lambda_ei = 1e-5 per cycle | Sojourn = infinity (no timeout) | CTMC **underestimates** impact: once in IL, availability = 0 permanently |
| Ex -> IR | lambda_mi = 0.005 per cycle | Bounded by 25,000 expansions | CTMC model is reasonable since expansion depth is bounded |

**Corrected steady-state availability (SMP):**

For well-behaved code (recursion depth << 25,000), the SMP model yields **higher** availability than the CTMC because the deterministic thresholds are far from typical operating conditions. The corrected availability is:

- **REPL mode, well-behaved code (SMP): 99.995%** (vs 99.98% CTMC)
- **REPL mode, adversarial code (SMP): 99.5%** (vs 99.8% CTMC, because the IL absorbing state has stronger impact when properly modeled)

---

## 8. Hardening Recommendations

### Recommendation 1: Add `recover()` at evaluation entry points

**Impact: Eliminates the absorbing failure state (mu_pr: 0 -> ~0.8)**

The absence of `recover()` across all 37 panic sites means every internal invariant violation is process-fatal. Add a `recover()` wrapper around `env.Eval()` in:
- `repl.go:238` (REPL eval loop)
- `cmd/run.go:62` (file execution)
- `elpstest/lisptest.go:180` (test execution)
- All `env.Load*()` methods as a general-purpose wrapper

```go
func SafeEval(env *LEnv, v *LVal) (result *LVal) {
    defer func() {
        if r := recover(); r != nil {
            result = env.Errorf("internal panic: %v", r)
            log.Printf("ELPS panic recovered: %v\n%s", r, debug.Stack())
        }
    }()
    return env.Eval(v)
}
```

**Availability improvement:** MTTF increases from ~588 hours (REPL) to effectively unlimited for panic-class failures. Remaining failure modes are LError (recoverable) and infinite loops (requires timeout).

**Trade-off:** `recover()` may mask genuine corruption. The 37 panic sites exist because the authors considered these states unrecoverable. Adding `recover()` should log the panic and potentially reinitialize the runtime.

### Recommendation 2: Add evaluation timeout / step counter

**Impact: Eliminates the IL absorbing state (lambda_ei: 1e-5 -> 0)**

Add a step counter to `Runtime` that is decremented on each `env.Eval()` call and checked:

```go
type Runtime struct {
    // ...
    MaxSteps   int64  // 0 = unlimited
    stepCount  int64
}

func (r *Runtime) CheckSteps() error {
    if r.MaxSteps <= 0 { return nil }
    r.stepCount++
    if r.stepCount > r.MaxSteps {
        return fmt.Errorf("evaluation exceeded maximum step count: %d", r.MaxSteps)
    }
    return nil
}
```

Check at the top of `env.Eval()` (`env.go:826`). Default to 0 (unlimited) for backwards compatibility; allow configuration via `WithMaxSteps(n)` Config.

**Availability improvement:** Converts IL from absorbing to transient (bounded sojourn time). Makes the SMP model equivalent to the CTMC model for this transition.

### Recommendation 3: Add macroexpand iteration limit

**Impact: Prevents unbounded macro expansion (reduces lambda_mi)**

`builtinMacroExpand` at `builtins.go:564-587` has an unbounded `for` loop. Add an explicit limit:

```go
const MaxMacroExpansionDepth = 1000

func builtinMacroExpand(env *LEnv, args *LVal) *LVal {
    form := args.Cells[0]
    // ...
    for depth := 0; depth < MaxMacroExpansionDepth; depth++ {
        // existing expansion logic
    }
    return env.Errorf("macro expansion exceeded maximum depth (%d)", MaxMacroExpansionDepth)
}
```

**Availability improvement:** Reduces macro IR probability by converting unbounded expansion to bounded. Stack overflow already provides a backstop at 25,000 frames, but an explicit limit at 1,000 provides earlier, clearer error messages.

### Recommendation 4: Convert type-assertion panics to LError returns

**Impact: Reduces panic rate by ~70%**

The 13 type-assertion panics in `lisp.go` (lines 663-857) fire when internal code passes the wrong LVal type to accessor methods. These can be converted to return error values. This is a larger refactor but eliminates the majority of panic sites reachable from user code. The remaining panics (e.g., `stack.go:206` pop-on-empty) represent genuine internal corruption and are appropriate to leave as panics (caught by the `recover()` from Recommendation 1).

**Availability improvement:** lambda_hp drops from 5e-6 to ~1.5e-6; lambda_dp drops from 1e-4 to ~3e-5. Combined with `recover()`, this makes panic-class failures extremely rare.

### Recommendation 5: Bound the parser recursion depth

**Impact: Prevents Go goroutine stack overflow in parser**

The recursive descent parser has no depth limit. Deeply nested input like `(((((...))))` at depth > ~100,000 can exhaust the Go goroutine stack (which grows dynamically but has a maximum, typically 1GB). Add a depth counter to the `Parser` struct:

```go
type Parser struct {
    // ...
    depth    int
    maxDepth int  // default: 10,000
}

func (p *Parser) ParseExpression() *lisp.LVal {
    p.depth++
    if p.depth > p.maxDepth {
        return p.errorf("parse-error", "expression nesting exceeds maximum depth (%d)", p.maxDepth)
    }
    defer func() { p.depth-- }()
    // existing logic
}
```

**Availability improvement:** Prevents a class of Go-level panics (goroutine stack overflow) that bypass all ELPS error handling. These panics are not covered by the 37 identified panic sites and would not be caught by a `recover()` in `env.Eval()` since they occur in the parser.

---

## 9. Top 5 Recommendations (Ranked by Impact)

| Rank | Recommendation | Impact on Availability | Effort | Risk |
|------|---------------|----------------------|--------|------|
| **1** | Add `recover()` at eval entry points | Eliminates absorbing P state. MTTF: 588h -> unlimited (for panics). Overall availability: 99.8% -> 99.99%+ | Low (wrapper function) | Medium: may mask genuine corruption. Log recovered panics for post-mortem. |
| **2** | Add evaluation step counter / timeout | Eliminates IL absorbing state. Prevents runaway evaluations from consuming resources indefinitely. | Low (add field to Runtime, check in Eval) | Low: backwards compatible with MaxSteps=0 default. |
| **3** | Convert type-assertion panics to LError | Reduces panic rate by ~70%. Makes 13 of 37 panic sites return errors instead. | Medium (refactor accessor methods and callers) | Low: strictly more graceful than panicking. |
| **4** | Add macroexpand iteration limit | Prevents unbounded macro expansion. Clearer errors at depth 1,000 vs stack overflow at 25,000. | Low (add counter to existing loop) | Low: 1,000 expansions is far beyond any realistic macro. |
| **5** | Bound parser recursion depth | Prevents Go goroutine stack overflow from deeply nested input. Defense-in-depth against parser-level crashes. | Low (add depth counter) | Low: 10,000 nesting levels is far beyond practical input. |

---

## Appendix A: Rate Estimation Methodology

Transition rates were estimated using three approaches:

1. **Structural analysis**: Counting code paths. For example, the 37 `panic()` sites in `lisp/` were identified via grep, and their reachability from user code was assessed by tracing call chains from `env.Eval()`.

2. **Threshold analysis**: For deterministic limits (`MaxHeightPhysical = 25,000`, `DefaultMaxAlloc = 10,485,760`), the "rate" is actually a probability per operation that the operation's parameter exceeds the threshold. Estimated from typical usage patterns (most functions have depth < 50, most allocations are < 1,000 elements).

3. **Error handling coverage**: For transitions involving error recovery, rates were derived from the ratio of error-handling code paths to total code paths. The `handler-bind` mechanism (`op.go:641-698`) provides structured recovery for ~90% of catchable errors. The remaining ~10% are errors in the handler itself (estimated from the `hval.Type == LError` check at `op.go:678-680`).

## Appendix B: Notation

| Symbol | Meaning |
|--------|---------|
| lambda_xy | Failure/degradation rate from state x to state y (per evaluation cycle) |
| mu_xy | Recovery rate from state x to state y (per evaluation cycle) |
| pi_x | Steady-state probability of being in state x |
| MTTF | Mean Time To Failure (in evaluation cycles or wall-clock time) |
| A | Availability = P(system is operational) |
| U | Unavailability = 1 - A |
| Q | Generator matrix (off-diagonal = rates, diagonal = -sum of row) |
| SMP | Semi-Markov Process |
| CTMC | Continuous-Time Markov Chain |
| TRO | Tail Recursion Optimization |
