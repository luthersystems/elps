// Copyright © 2018 The ELPS authors

package lisp

import (
	"context"
	"io"
	"time"
)

// Config is a function that configures a root environment or its runtime.
type Config func(env *LEnv) *LVal

// WithMaximumLogicalStackHeight returns a Config that will prevent an
// execution environment from allowing the logical stack height to exceed n.
// The logical height of the stack is the stack's physical height plus the
// number of stack frames which have been elided due to tail recursive call
// optimizations.
//
// This limit is disabled by default (see DefaultMaxLogicalStackHeight).  Its
// unit is elided frames, not loop turns: one turn of a tail loop adds the
// length of the elided terminal chain, which varies with the shape of the
// loop body.  To bound how many turns a tail loop may take, use
// WithMaxTailIterations instead; to bound total evaluation work, use
// WithMaxSteps.
func WithMaximumLogicalStackHeight(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stack.MaxHeightLogical = n
		return Nil()
	}
}

// WithMaximumPhysicalStackHeight returns a Config that will prevent an
// execution environment from allowing the physical stack height to exceed n.
// The physical stack height is the literal number of frames in the call stack
// and does not account for stack frames elided due to tail recursive call
// optimizations.
func WithMaximumPhysicalStackHeight(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stack.MaxHeightPhysical = n
		return Nil()
	}
}

// WithMaxEvalNesting returns a Config that bounds how deeply the evaluator may
// recurse into itself while evaluating a single expression.
//
// This is a distinct quantity from stack height and is not implied by it.  A
// call's arguments are evaluated before the call's frame is pushed, so
// ((lambda (x) x) ((lambda (x) x) ... )) recurses through the Go evaluator
// while the physical stack height stays at zero — the exact shape
// WithMaximumPhysicalStackHeight exists to stop and the one shape it cannot
// see (issue #316).  Nesting is bounded here instead.
//
// A value of 0 selects DefaultMaxEvalNesting.  A negative value disables the
// check, which re-exposes the host process to an unrecoverable
// "fatal error: stack overflow"; do that only when some other bound on
// expression depth is guaranteed.
func WithMaxEvalNesting(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.MaxEvalNesting = n
		return Nil()
	}
}

// WithMaxSleep returns a Config that sets a HARD CEILING on how long a single
// (time:sleep d) may block, in the host's hands rather than the program's.
//
// This is the containment bound, and it is not the same knob as
// DefaultMaxSleep. A sleep with no explicit :max is capped at
// DefaultMaxSleep, which program source may raise per call with
// (time:sleep d :max m). That is a guard against accidents. The ceiling set
// here is what :max may not exceed, so a program cannot opt itself out of
// it — which matters when the program is untrusted, as customer-supplied
// phylum source is downstream in luthersystems/substrate.
//
// Zero or negative means no ceiling: :max may name any duration. That is the
// default, because the interpreter cannot know what wall-clock budget the
// host is willing to spend.
//
// Note what this does NOT do: it bounds one sleep call, not their sum. A
// loop of N sleeps each just under the ceiling still blocks for N times the
// ceiling. Bounding total elapsed time is what a context deadline is for
// (see WithContext) -- sleep observes that too, and refuses immediately
// rather than blocking to the deadline.
func WithMaxSleep(d time.Duration) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.MaxSleep = d
		return Nil()
	}
}

// WithMaxTailIterations returns a Config that bounds the number of tail-call
// iterations a single stack frame may perform.  Tail calls run in constant
// stack space, so neither stack-height limit can bound a runaway tail loop;
// this is the limit that does.  Its unit is loop turns, independent of how
// many frames each turn elides.
//
// A value of 0 disables the check.  The default is
// DefaultMaxTailIterations, chosen as a runaway-loop backstop rather than a
// business limit.
func WithMaxTailIterations(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stack.MaxTailIterations = n
		return Nil()
	}
}

// WithLoader returns a Config that executes fn and ensures that the
// environment's working package is reset following execution of fn.  Despite
// fn having the same signature as a Config WithLoader allows a Loader to
// function more like the LEnv methods LoadFile, LoadString, etc.
func WithLoader(fn Loader) Config {
	return func(env *LEnv) (lerr *LVal) {
		pkg := env.Runtime.Package.Name
		defer func() {
			e := env.InPackage(Symbol(pkg))
			if e.Type == LError && lerr.Type != LError {
				lerr = e
			}
		}()
		return fn(env)
	}
}

// WithReader returns a Config that makes environments use r to parse source
// streams.  There is no default Reader for an environment.
func WithReader(r Reader) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Reader = r
		return Nil()
	}
}

// WithStderr returns a Config that makes environments write debugging output
// to w instead of the default, os.Stderr.
func WithStderr(w io.Writer) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Stderr = w
		return Nil()
	}
}

// WithLibrary returns a Config that makes environments use l
// as a source library.
func WithLibrary(l SourceLibrary) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Library = l
		return Nil()
	}
}

// WithMaxMacroExpansionDepth returns a Config that limits the number of
// successive macro expansions during evaluation.  This prevents infinite
// macro expansion from exhausting memory.
func WithMaxMacroExpansionDepth(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.MaxMacroExpansionDepth = n
		return Nil()
	}
}

// WithMaxAlloc returns a Config that sets the per-operation allocation size
// cap (in bytes for strings, in elements for sequences).  This limits the
// output size of any single builtin call, not cumulative memory usage.
func WithMaxAlloc(n int) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.MaxAlloc = n
		return Nil()
	}
}

// WithContext returns a Config that sets the initial context.Context for the
// root environment.  The context is checked at each evaluation step; if it is
// cancelled or its deadline expires, evaluation returns a CondContextCancelled
// error.  For per-call context control, use the *Context methods on LEnv
// instead.
func WithContext(ctx context.Context) Config {
	return func(env *LEnv) *LVal {
		env.evalCtx = ctx
		return Nil()
	}
}

// WithMaxSteps returns a Config that sets the maximum number of evaluation
// steps before evaluation returns a CondStepLimitExceeded error.  A step is
// counted for each Eval entry, each TRO iteration, each macro re-expansion,
// and each turn of a dotimes loop.  A value of 0 means unlimited (the
// default).
//
// The dotimes turn is counted because an empty-bodied loop evaluates nothing:
// (dotimes (i 2147483647)) consumed no budget and could not be interrupted at
// all.  It costs exactly one extra step per turn, so a dotimes-heavy program
// now uses more budget than it did -- proportionally most for a small body (a
// constant body goes from 1 step per turn to 2; a three-form body from 12 to
// 13).  A budget pinned tightly against a previously measured figure may need
// raising.  opDoTimes carries the full measurement table.
//
// The budget is per top-level evaluation: the counter is reset each time an
// exported entry point (Eval, EvalContext, EvalSExpr, FunCall,
// FunCallContext, SpecialOpCall, MacroCall, or any Load*) is entered from
// outside an evaluation.  Nested evaluation — a builtin calling back into Eval, the
// tail-call loops, forms evaluated by load — shares the enclosing budget and
// does not refill it.  Without the reset the limit would be a lifetime quota
// that permanently kills a long-lived Runtime once it was reached.
//
// Runtime.Steps reports the current evaluation's usage; Runtime.TotalSteps
// reports the lifetime total.
//
// A step budget is the only limit here that bounds an infinite loop which
// neither recurses nor tail-calls; the stack limits cannot see such a loop.
func WithMaxSteps(n int64) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.maxSteps = n
		return Nil()
	}
}

// WithDebugger returns a Config that attaches a debugger to the runtime.
// When a debugger is attached, tail recursion optimization is disabled to
// provide predictable stepping behavior and stack traces.
func WithDebugger(d Debugger) Config {
	return func(env *LEnv) *LVal {
		env.Runtime.Debugger = d
		return Nil()
	}
}
