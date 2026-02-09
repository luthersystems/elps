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
type Runtime struct {
	Registry       *PackageRegistry
	Package        *Package
	Stderr         io.Writer
	Stack          *CallStack
	Reader         Reader
	Library        SourceLibrary
	Profiler       Profiler
	MaxAlloc       int // Maximum single allocation size in bytes (0 = use default).
	conditionStack []*LVal
	numenv         atomicCounter
	numsym         atomicCounter
}

// MaxAllocBytes returns the effective maximum single allocation size.
// If MaxAlloc is zero, DefaultMaxAlloc is returned.
func (r *Runtime) MaxAllocBytes() int {
	if r.MaxAlloc > 0 {
		return r.MaxAlloc
	}
	return DefaultMaxAlloc
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

// DefaultMaxAlloc is the maximum size of a single allocation (in bytes for
// strings, in elements for sequences) allowed by builtins like string:repeat
// and make-sequence. This prevents memory exhaustion from untrusted input.
// Applications can override this via Runtime.MaxAlloc.
const DefaultMaxAlloc = 10 * 1024 * 1024 // 10 million (bytes or elements)

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
