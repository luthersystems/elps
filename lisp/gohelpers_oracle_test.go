// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
)

// The language builtins the LEnv sequence helpers return what they return,
// called as the oracle.
var (
	refKeys   = lisp.BuiltinFunc("keys")
	refLength = lisp.BuiltinFunc("length")
	refEmpty  = lisp.BuiltinFunc("empty?")
	refFirst  = lisp.BuiltinFunc("first")
	refSecond = lisp.BuiltinFunc("second")
)

// seqHelpers pairs each helper with its builtin.  samePtr says whether the
// helper must return the very value the builtin returns.
var seqHelpers = []struct {
	name    string
	builtin lisp.BuiltinRef
	fn      func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	samePtr bool
}{
	{"keys", refKeys, (*lisp.LEnv).MapKeys, false},
	{"length", refLength, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
		n, lerr := env.Length(v)
		if lerr != nil {
			return lerr
		}
		return lisp.Int(n)
	}, false},
	{"empty?", refEmpty, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
		empty, lerr := env.IsEmpty(v)
		if lerr != nil {
			return lerr
		}
		return lisp.Bool(empty)
	}, false},
	{"first", refFirst, (*lisp.LEnv).First, true},
	{"second", refSecond, (*lisp.LEnv).Second, true},
}

// TestSeqHelpersMatchBuiltins holds MapKeys, Length, IsEmpty, First and
// Second to keys, length, empty?, first and second on an input of every type:
// the same value (or the same element), or the same error condition.
func TestSeqHelpersMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, h := range seqHelpers {
		for _, in := range oracle.Values() {
			want := env.CallBuiltin(h.builtin, in.V)
			got := h.fn(env, in.V)
			if h.samePtr {
				oracle.SameObject(t, h.name+"/"+in.Label, got, want)
			} else {
				oracle.Same(t, h.name+"/"+in.Label, got, want)
			}
		}
	}
}

// TestSeqHelpersAllocLimit holds MapKeys to keys under a small allocation
// limit: a map over it raises the same condition.
func TestSeqHelpersAllocLimit(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	env.Runtime.MaxAlloc = 1
	for _, in := range oracle.Values() {
		oracle.Same(t, "keys/"+in.Label, env.MapKeys(in.V), env.CallBuiltin(refKeys, in.V))
	}
}

// TestSeqHelpersCancelled holds each helper to its builtin at every context
// check: each makes its one check before anything else.
func TestSeqHelpersCancelled(t *testing.T) {
	// Each run gets its own input: a value belongs to one runtime.
	for _, h := range seqHelpers {
		for i, in := range oracle.Values() {
			oracle.Sweep(t, h.name+"/"+in.Label,
				func(env *lisp.LEnv) *lisp.LVal { return h.fn(env, oracle.Values()[i].V) },
				func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(h.builtin, oracle.Values()[i].V) })
		}
	}
}
