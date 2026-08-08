// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"
	"io"
	"os"
	"sync/atomic"
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
	Debugger               Debugger // nil = disabled (zero overhead on hot path)
	conditionStack         []*LVal
	MaxAlloc               int   // Per-operation allocation size cap (0 = use default). Not cumulative.
	MaxMacroExpansionDepth int   // Maximum macro expansion iterations (0 = use default).
	evalDepth              int   // Re-entrancy depth of top-level evaluation entry points.
	maxSteps               int64 // Per-evaluation step limit (0 = unlimited).
	steps                  int64 // Steps consumed by the current top-level evaluation.
	totalSteps             int64 // Steps consumed by all completed top-level evaluations.
	numenv                 atomicCounter
	numsym                 atomicCounter
	macroExpSeq            int64 // monotonic counter for MacroExpansionInfo.ID
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
// evaluates nothing and would otherwise consume no budget at all — see
// opDoTimes.
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
const (
	DefaultMaxLogicalStackHeight  = 0
	DefaultMaxPhysicalStackHeight = 25000
	DefaultMaxTailIterations      = 1000000
)

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
		return &sourceContext{
			name: top.Source.File,
			loc:  top.Source.Path,
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
