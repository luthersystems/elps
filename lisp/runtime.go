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
// Concurrency: Runtime and its associated LEnv tree are NOT safe for
// concurrent use from multiple goroutines.  All calls to Eval, Load, and
// any other methods that read or mutate Runtime or LEnv state must be
// serialized by the caller.  To evaluate ELPS code concurrently, create a
// separate Runtime (and LEnv tree) per goroutine.
//
// The only thread-safe operations are GenEnvID and GenSym, which use atomic
// counters internally.  All other fields — including Registry, Package,
// Stack, conditionStack, and the LEnv Scope maps — are unprotected.
type Runtime struct {
	Registry               *PackageRegistry
	Package                *Package
	Stderr                 io.Writer
	Stack                  *CallStack
	Reader                 Reader
	Library                SourceLibrary
	Profiler               Profiler
	Debugger               Debugger // nil = disabled (zero overhead on hot path)
	MaxAlloc               int      // Per-operation allocation size cap (0 = use default). Not cumulative.
	MaxMacroExpansionDepth int // Maximum macro expansion iterations (0 = use default).
	conditionStack         []*LVal
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
	max := r.MaxAllocBytes()
	if n > max {
		return fmt.Sprintf("allocation size %d exceeds maximum (%d)", n, max)
	}
	return ""
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

// Default stack depth limits. These match the values used by the test harness
// and provide protection against unbounded recursion exhausting the Go
// goroutine stack. Applications that need deeper stacks can override these
// via WithMaximumLogicalStackHeight and WithMaximumPhysicalStackHeight.
const (
	DefaultMaxLogicalStackHeight  = 50000
	DefaultMaxPhysicalStackHeight = 25000
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
