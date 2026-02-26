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
