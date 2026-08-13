// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestCycleContainersSkipFunctionSubtrees pins the rule that keeps
// FuzzCyclicValueWalks inside the value it built: containers() may only offer
// knot() nodes the input constructed, and everything below an LFun -- its
// formals and its body -- belongs to whoever defined the function.
//
// fuzzval hands back the environment's own global function objects (fun()
// kinds 2 and 3, reached from the seed corpus by []byte{kindFun, 2}), whose
// Cells[0] is the process-wide formals list from the builtin table.  Before
// issue #398 containers() collected that list and knot() appended to it, so
// running the seed corpus rewrote lisp:car's signature for the rest of the
// process.
//
// This is the attributable half of the guard pair.  TestMain's
// builtin-formals snapshot says the standard library was corrupted; this says
// by whom, and fails on the collection rather than on its consequence.
func TestCycleContainersSkipFunctionSubtrees(t *testing.T) {
	env := newCycleEnv(t)
	for _, name := range []string{"lisp:car", "lisp:identity", "lisp:map", "lisp:let", "lisp:defmacro"} {
		fn := env.GetGlobal(lisp.Symbol(name))
		if fn.Type != lisp.LFun {
			t.Fatalf("%s is not a function: %v", name, fn.Type)
		}
		if got := containers(fn); len(got) != 0 {
			t.Errorf("containers(%s) offered knot() %d node(s) inside a function value -- the first is a %v: %s"+
				"\nthose cells are %s's own formals and body, shared by every environment in the process,"+
				"\nnot structure this input built", name, len(got), got[0].Type, got[0], name)
		}
		// The same function nested inside a container the input DID build:
		// the list is writable, nothing under the function is.
		wrapper := lisp.QExpr([]*lisp.LVal{lisp.Int(1), fn})
		if got := containers(wrapper); len(got) != 1 || got[0] != wrapper {
			t.Errorf("containers(list holding %s) collected %d node(s), want exactly the list itself", name, len(got))
		}
	}
}
