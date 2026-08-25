// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"
	"io"
	"os"
	"sync/atomic"
	"time"
)

// Runtime is an object underlying a family of tree of LEnv values.  It is
// responsible for holding shared environment state, generating identifiers,
// and writing debugging output to a stream (typically os.Stderr).
//
// Step Limits: Runtime supports optional instruction counting via MaxSteps.
// Context cancellation is handled per-evaluation via LEnv.evalCtx, which
// is set by the *Context methods on LEnv (e.g. EvalContext) or the
// WithContext Config option.  When neither context nor step limits are
// configured, limit checks are two nil/zero comparisons with negligible
// overhead.
//
// Concurrency: Runtime and its associated LEnv tree are NOT safe for
// concurrent use from multiple goroutines.  All calls to Eval, Load, and
// any other methods that read or mutate Runtime or LEnv state must be
// serialized by the caller.  To evaluate ELPS code concurrently, create a
// separate Runtime (and LEnv tree) per goroutine.
//
// The only thread-safe operations are GenEnvID and GenSym, which use atomic
// counters internally.  All other fields — including Registry, Package,
// Stack, conditionStack, and the LEnv Scope maps — are unprotected.
//
// Field order is layout-sensitive: pointer-bearing fields lead so the GC scan
// extent stops at 112 bytes instead of 144. Add scalars below conditionStack.
type Runtime struct {
	Registry               *PackageRegistry
	Package                *Package
	Stderr                 io.Writer
	Stack                  *CallStack
	Reader                 Reader
	Library                SourceLibrary
	Profiler               Profiler
	Debugger               Debugger  // nil = disabled (zero overhead on hot path)
	LoadCache              LoadCache // nil = disabled (the load path is then byte-identical to having no hook); see lisp/loadcache.go
	conditionStack         []*LVal
	MaxAlloc               int           // Per-operation allocation size cap (0 = use default). Not cumulative.
	MaxMacroExpansionDepth int           // Maximum macro expansion iterations (0 = use default).
	MaxEvalNesting         int           // Evaluator recursion depth cap (0 = use default, negative = disabled).
	MaxSleep               time.Duration // Hard ceiling on a single time:sleep (0 or negative = none). See MaxSleepCeiling.
	evalDepth              int           // Re-entrancy depth of top-level evaluation entry points.
	loadCacheActive        bool          // Guards LoadCache re-entrancy; see (*LEnv).readCached.
	evalNesting            int           // Current recursion depth of LEnv.eval (the Go-stack guard).
	maxSteps               int64         // Per-evaluation step limit (0 = unlimited).
	steps                  int64         // Steps consumed by the current top-level evaluation.
	totalSteps             int64         // Steps consumed by all completed top-level evaluations.
	numenv                 atomicCounter
	numsym                 atomicCounter
	macroExpSeq            int64 // monotonic counter for macroExpansionInfo.ID
}

// MaxAllocBytes returns the effective per-operation allocation size cap.
// Each builtin that allocates a buffer or sequence checks its output size
// against this limit independently — it is NOT a cumulative memory tracker.
// If MaxAlloc is zero, DefaultMaxAlloc is returned.
func (r *Runtime) MaxAllocBytes() int {
	if r.MaxAlloc > 0 {
		return r.MaxAlloc
	}
	return DefaultMaxAlloc
}

// MaxMacroExpansions returns the effective maximum macro expansion depth.
// If MaxMacroExpansionDepth is zero, DefaultMaxMacroExpansionDepth is returned.
func (r *Runtime) MaxMacroExpansions() int {
	if r.MaxMacroExpansionDepth > 0 {
		return r.MaxMacroExpansionDepth
	}
	return DefaultMaxMacroExpansionDepth
}

// MaxEvalNestingDepth returns the effective evaluator recursion-depth cap.
// Zero means "use DefaultMaxEvalNesting"; a negative value disables the check
// and is reported as zero.
func (r *Runtime) MaxEvalNestingDepth() int {
	if r.MaxEvalNesting == 0 {
		return DefaultMaxEvalNesting
	}
	if r.MaxEvalNesting < 0 {
		return 0
	}
	return r.MaxEvalNesting
}

// EvalNesting reports how deeply LEnv.eval is currently recursed into itself.
// It is zero outside of an evaluation.  See DefaultMaxEvalNesting for what the
// quantity measures and why it is bounded separately from stack height.
func (r *Runtime) EvalNesting() int {
	return r.evalNesting
}

// evalNestingExceeded reports whether the evaluator has recursed past its
// depth cap.  It runs once per LEnv.eval call -- the interpreter's hottest
// path -- so it is deliberately a couple of loads and compares against a
// counter the caller has already incremented, with no function call in the
// common case (it inlines).
func (r *Runtime) evalNestingExceeded() bool {
	limit := r.MaxEvalNesting
	if limit == 0 {
		limit = DefaultMaxEvalNesting
	} else if limit < 0 {
		return false
	}
	return r.evalNesting > limit
}

// CheckAlloc returns a non-empty error message if n exceeds the per-operation
// allocation size cap.  This is a point-in-time check for a single operation,
// not a cumulative memory tracker.  Callers should use this before allocating
// buffers or sequences whose size is determined by user input.
func (r *Runtime) CheckAlloc(n int) string {
	maxAlloc := r.MaxAllocBytes()
	if n > maxAlloc {
		return fmt.Sprintf("allocation size %d exceeds maximum (%d)", n, maxAlloc)
	}
	return ""
}

// Steps returns the number of steps consumed by the current top-level
// evaluation — or, if no evaluation is in progress, by the most recent one.
// Four things increment the counter by one: each call to Eval, each
// tail-recursion iteration, each macro re-expansion, and each turn of a
// dotimes loop.  The last of those exists because an empty-bodied dotimes
// evaluates nothing and would otherwise consume no budget at all -- see
// opDoTimes, which also records the measured per-turn cost.
//
// The counter is reset when a new top-level evaluation begins (see
// WithMaxSteps), so it is not a lifetime total.  Use TotalSteps for that.
func (r *Runtime) Steps() int64 {
	return r.steps
}

// TotalSteps returns the number of steps consumed over the lifetime of the
// Runtime, across every top-level evaluation.  Unlike Steps it is not reset
// when a new evaluation begins.
func (r *Runtime) TotalSteps() int64 {
	return r.totalSteps + r.steps
}

// ResetSteps resets the current evaluation's step counter to zero.  It does
// not affect TotalSteps.
func (r *Runtime) ResetSteps() {
	r.totalSteps += r.steps
	r.steps = 0
}

// beginEval marks entry into a top-level evaluation and returns a function
// that must be deferred by the caller to mark the exit.
//
// The per-evaluation step budget (see WithMaxSteps) is reset only on the
// outermost entry, so every nested evaluation — recursive Eval calls from a
// builtin, the tail-call loops in funCall/specialOpCall, forms evaluated by
// load — shares the enclosing budget rather than silently refilling it.
func (r *Runtime) beginEval() func() {
	r.evalDepth++
	if r.evalDepth == 1 {
		r.totalSteps += r.steps
		r.steps = 0
	}
	return r.endEval
}

func (r *Runtime) endEval() {
	if r.evalDepth > 0 {
		r.evalDepth--
	}
}

// PushCondition pushes an error onto the condition stack, making it available
// to rethrow within a handler-bind handler.
func (r *Runtime) PushCondition(err *LVal) {
	r.conditionStack = append(r.conditionStack, err)
}

// PopCondition removes and returns the top condition from the stack.
func (r *Runtime) PopCondition() *LVal {
	n := len(r.conditionStack)
	if n == 0 {
		return nil
	}
	err := r.conditionStack[n-1]
	r.conditionStack = r.conditionStack[:n-1]
	return err
}

// CurrentCondition returns the condition currently being handled, or nil.
func (r *Runtime) CurrentCondition() *LVal {
	n := len(r.conditionStack)
	if n == 0 {
		return nil
	}
	return r.conditionStack[n-1]
}

// DefaultMaxAlloc is the per-operation allocation size cap (in bytes for
// strings, in elements for sequences) enforced by builtins like concat,
// append, map, zip, reverse, make-sequence, and JSON load.  Each operation
// checks its own output size independently — this is not a cumulative memory
// budget.  It prevents a single malicious or accidental call from exhausting
// memory.  Applications can override this via Runtime.MaxAlloc.
const DefaultMaxAlloc = 10 * 1024 * 1024 // 10 million (bytes or elements)

// DefaultMaxMacroExpansionDepth is the maximum number of successive macro
// expansions allowed before Eval returns an error.  This prevents infinite
// macro expansion from exhausting memory or running forever.
const DefaultMaxMacroExpansionDepth = 1000

// Default evaluation depth limits.
//
// # DefaultMaxPhysicalStackHeight — the memory guard, keep it on
//
// This bounds the number of frames physically present on the call stack. It
// protects against unbounded *non-tail* recursion exhausting the Go
// goroutine stack, which aborts the whole process with an unrecoverable
// "stack overflow" that no handler-bind can catch. Measured on linux/amd64
// with the limit disabled, ELPS non-tail recursion survives 200,000 levels
// (~678MB of Go stack) and crashes the runtime at 400,000. 25000 sits 8-16x
// below that threshold, and the margin is load-bearing: the crash point
// moves with Go's stack settings, per-frame size, and architecture. Do not
// raise it casually and do not default it off.
//
// Frames elided by tail-call optimization do not count toward it, so a
// tail-recursive loop runs at constant physical height no matter how many
// iterations it performs.
//
// It bounds FRAMES, not evaluation depth. A frame is pushed when a function is
// invoked, and LEnv.evalSExprCells deliberately evaluates a call's arguments
// BEFORE pushing anything (pushing there would corrupt the error messages and
// stack dumps produced while an argument is being evaluated). Nested arguments
// therefore recurse through the Go evaluator at physical height zero, which is
// exactly the shape this limit exists to stop and exactly the shape it cannot
// see. DefaultMaxEvalNesting covers that path; see issue #316.
//
// # DefaultMaxEvalNesting — the same memory guard, on the evaluator's own recursion
//
// This bounds how deeply LEnv.eval may recurse into itself, which is the true
// measure of Go stack consumed by the evaluator. Every nested evaluation
// passes through eval — a call's arguments, a special operator's subforms, a
// builtin re-entering via env.Eval — so a single counter incremented on entry
// and decremented on exit bounds all of them, and unlike a stack frame it
// costs no allocation and does not appear in a stack trace.
//
// Measured on linux/amd64 against the 1GB default goroutine stack:
// argument-nesting depth 700,000 completes, 800,000 aborts the process with
// "fatal error: stack overflow" (~1.4KB of Go stack per level). 100,000 sits
// 7-8x below that, matching the margin DefaultMaxPhysicalStackHeight keeps.
//
// THAT MARGIN IS POINTER-SIZE DEPENDENT. Go caps a goroutine stack at 1GB on
// 64-bit but 250MB on 32-bit (runtime/proc.go: maxstacksize). At ~1.4KB per
// level the default costs ~140MB, so the same 100,000 is a 7-8x margin on
// amd64 and roughly 1.8x on a 32-bit build -- and an embedder whose own call
// stack is already deep when it enters Eval eats into what is left. Nothing
// here is exercised on 32-bit: CI builds ubuntu-latest and windows-latest,
// both amd64. A 32-bit embedder should lower this with WithMaxEvalNesting
// rather than trust the default.
//
// It also sits comfortably above ordinary recursion. Measured with
// (defun sum (n) (if (<= n 0) 0 (+ n (sum (- n 1))))), the physical limit
// binds first for any MaxEvalNesting of 38,000 or more: that recursion costs
// ~1.5 eval levels per physical frame, so it reaches physical height 25,001
// at nesting ~38,000. Only code whose body nests expressions more than ~4
// deep per recursion level can reach 100,000 nesting before 25,000 frames --
// such code costs 4 eval levels per frame, ~2.6x what ordinary recursion
// costs, so it really is consuming that much more Go stack per frame.
//
// The two limits bound different quantities and neither implies the other.
// Deeply nested arguments raise nesting at constant height; a long chain of
// tail-position calls raises neither.
//
// Before this limit existed the only thing bounding evaluator recursion was
// rdparser.DefaultMaxParseDepth (10,000) — incidentally, because nesting had
// to be parsed before it could be evaluated. That bound was never sound: a
// recursive macro generates nesting at expansion time from an integer
// argument, so ~120 bytes of source reached the fatal overflow with every
// documented limit at its default. Parse depth is no longer load-bearing for
// stack safety.
//
// # DefaultMaxTailIterations — the runaway-loop backstop
//
// This bounds how many turns a single tail-recursive loop may take. Its unit
// is loop turns, so it is a knob an operator can reason about.
//
// It is a backstop against a loop that never terminates, NOT a time bound.
// One million turns of a trivial O(1) body costs ~4s of interpreter
// overhead, but turns say nothing about the work done per turn: a body that
// conses a list, concatenates a string, or calls any O(n) builtin can run
// for minutes — or effectively forever — inside the same turn budget, and a
// step limit does not help either because an O(n) builtin is one step. To
// bound wall-clock time, pass a context with a deadline (WithContext, or the
// *Context methods on LEnv). That is the only limit here that measures time.
//
// # DefaultMaxLogicalStackHeight — off by default
//
// Logical height accumulates every frame elided by tail-call optimization,
// so it is a running total of *elided frames*, not a nesting depth and not
// an iteration count. One turn of a tail loop adds the length of the elided
// terminal chain — 2 for a trivial body, more when the body nests terminal
// forms more deeply — so the same numeric limit permits a different number
// of iterations depending on the shape of the loop. That makes it unusable
// as a default runaway-loop bound: it fails data-dependently with a message
// that reads like runaway recursion when the program is a correct constant-
// space loop. MaxTailIterations does that job in honest units instead.
//
// Logical height remains valuable as a stack-trace diagnostic, and callers
// who specifically want to bound it can opt in via
// WithMaximumLogicalStackHeight.
//
// # Bounding total work
//
// None of these limits catch a non-recursive infinite loop (a host-provided
// `while`, for example) because such a loop neither grows the stack nor
// performs tail calls. WithMaxSteps bounds the number of evaluation steps,
// which does catch that shape.
//
// None of them — including WithMaxSteps — bound elapsed time, because a
// single step may perform an arbitrary amount of work inside a builtin. A
// context deadline is the only real time bound; see WithContext and the
// *Context methods on LEnv.
//
// # Bounding total memory
//
// Runtime.MaxAlloc is PER-OPERATION, not cumulative: each builtin checks its
// own output size against it, so a program that allocates a thousand buffers
// of MaxAlloc-1 bytes passes every check. Nothing here tracks total heap. The
// incidental bound is whatever limit stops the loop doing the allocating —
// MaxTailIterations for a tail loop, MaxSteps for a stepped one — multiplied
// by MaxAlloc, which is not a memory budget in any useful sense. A host that
// must bound total memory has to do it outside the interpreter (a cgroup, a
// container limit, or Go's GOMEMLIMIT plus a watchdog); see Runtime.MaxAlloc
// and CheckAlloc.
const (
	DefaultMaxLogicalStackHeight  = 0
	DefaultMaxPhysicalStackHeight = 25000
	DefaultMaxTailIterations      = 1000000
	DefaultMaxEvalNesting         = 100000
)

// DefaultMaxSleep bounds a single (time:sleep d) that does not pass an
// explicit :max, and is the last of the execution limits because it bounds
// WALL CLOCK rather than work.
//
// Every other limit here counts something the interpreter does — steps,
// frames, iterations, bytes, nesting. A sleeping goroutine does none of
// them, which is why time:sleep was unbounded by all of them at once and
// "9223372036854775807ns" blocked for ~292 years (issue #314). Bounding it
// needs a wall-clock number, and there is no work-based limit that implies
// one.
//
// One hour is chosen to be far above any plausible legitimate sleep in an
// embedded interpreter — a backoff, a poll interval, a test delay are all
// orders of magnitude below it — while still refusing the 292-year shape
// immediately rather than after 292 years. A caller who genuinely wants
// longer says so with :max, which is the point: the length becomes explicit
// at the call site instead of being an accident of arithmetic.
//
// :max raises the per-call cap but CANNOT exceed Runtime.MaxSleep when the
// embedder has set one. That split is deliberate. Program source and the
// embedder are different trust domains: downstream (luthersystems/substrate)
// the program is customer-supplied phylum running as chaincode, so a cap
// that program source could raise would be decorative. DefaultMaxSleep is a
// guard against accidents, which the program may relax; Runtime.MaxSleep is
// a containment bound, which only the host may relax.
const DefaultMaxSleep = time.Hour

// MaxSleepCeiling returns the hard upper bound on a single sleep, or 0 when
// the embedder has set none.  A negative Runtime.MaxSleep disables the bound
// explicitly and is also reported as 0.
//
// This is the ceiling on what (time:sleep d :max m) may request, NOT the cap
// applied when :max is absent — that is DefaultMaxSleep. See its doc comment
// for why the two are separate.
func (r *Runtime) MaxSleepCeiling() time.Duration {
	if r.MaxSleep <= 0 {
		return 0
	}
	return r.MaxSleep
}

// StandardRuntime returns a new Runtime with an empty package registry and
// Stderr set to os.Stderr.
func StandardRuntime() *Runtime {
	return &Runtime{
		Registry: NewRegistry(),
		Stderr:   os.Stderr,
		Stack: &CallStack{
			MaxHeightLogical:  DefaultMaxLogicalStackHeight,
			MaxHeightPhysical: DefaultMaxPhysicalStackHeight,
			MaxTailIterations: DefaultMaxTailIterations,
		},
	}
}

func (r *Runtime) GenEnvID() uint {
	return r.getEnvID()
}

func (r *Runtime) GenSym() string {
	return fmt.Sprintf("gen%08d", r.gensym())
}

func (r *Runtime) getStderr() io.Writer {
	if r.Stderr != nil {
		return r.Stderr
	}
	return os.Stderr
}

func (r *Runtime) getEnvID() uint {
	return r.numenv.Add(1)
}

func (r *Runtime) gensym() uint {
	return r.numsym.Add(1)
}

// nextMacroExpID returns the next unique macro expansion node ID.
// Only called when a debugger is attached.
func (r *Runtime) nextMacroExpID() int64 {
	r.macroExpSeq++
	return r.macroExpSeq
}

// sourceContext uses the CallStack to determine the location/name of the
// currently executing file (i.e. the file containing the function call
// `(load-file ...)` that is being evaluated).
func (r *Runtime) sourceContext() SourceContext {
	top := r.Stack.Top()
	if top != nil {
		// A frame's Source is nil when the call was constructed by Go code
		// rather than read from a source file (issue #362: constructors no
		// longer stamp a shared "<native code>" location).  Report the same
		// synthetic native location those frames historically carried so
		// SourceLibrary implementations see an unchanged context.
		src := top.Source
		if src == nil {
			loc := nativeLocation()
			src = &loc
		}
		return &sourceContext{
			name: src.File,
			loc:  src.Path,
		}
	}
	return &sourceContext{
		name: "",
		loc:  "",
	}
}

type atomicCounter uint64

func (c *atomicCounter) Add(n uint) uint {
	return uint(atomic.AddUint64((*uint64)(c), uint64(n)))
}
