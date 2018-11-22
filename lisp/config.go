// Copyright © 2018 The ELPS authors

package lisp

import "io"

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
