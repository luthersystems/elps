// Copyright © 2018 The ELPS authors

package lisp

import (
	"context"
	"io"
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
// counted for each Eval entry, each TRO iteration, and each macro
// re-expansion.  A value of 0 means unlimited (the default).
//
// The budget is per top-level evaluation: the counter is reset each time an
// exported entry point (Eval, EvalContext, EvalSExpr, FunCall,
// FunCallContext, SpecialOpCall, or any Load*) is entered from outside an
// evaluation.  Nested evaluation — a builtin calling back into Eval, the
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
