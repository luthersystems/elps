// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
)

// TestMacroReturningARuntimeListBindingLeavesItUnlocated is the ELPS-level
// shape of issue #582.  A global list built by (list ...) is unsealed
// syntax with no location; a macro whose body returns it hands the stamp a
// binding.  The stamp used to write the macro call site onto the binding
// and its cells, for the rest of the process.
func TestMacroReturningARuntimeListBindingLeavesItUnlocated(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	// Only the list header is unlocated: (list 1 2) builds a fresh SExpr
	// over the reader's own (sealed, located) literal nodes.
	if rc := env.LoadString("binding.lisp", "(set 'l (list 1 2))\n(defmacro m () l)\n"); rc.Type == lisp.LError {
		t.Fatalf("fixture: %v", rc)
	}
	l := env.Runtime.Package.Get(lisp.Symbol("l"))
	if lisp.SourceRefForTest(l) != nil {
		t.Fatalf("fixture: the runtime list is located before the expansion: %v", lisp.SourceRefForTest(l))
	}
	cellSources := make([]*token.Location, len(l.Cells))
	for i, c := range l.Cells {
		cellSources[i] = lisp.SourceRefForTest(c)
	}

	// (1 2) is not callable, so the expansion errors -- after the stamp
	// has run over it, which is all this test needs.
	rc := env.LoadString("call.lisp", "\n\n(m)\n")
	if rc.Type != lisp.LError {
		t.Fatalf("expected (m) to fail evaluating (1 2), got %v", rc)
	}
	if got := lisp.SourceRefForTest(l); got != nil {
		t.Errorf("the binding acquired a location from the macro expansion (issue #582): %v", got)
	}
	for i, c := range l.Cells {
		if got := lisp.SourceRefForTest(c); got != cellSources[i] {
			t.Errorf("cell %d of the binding changed location: %v -> %v", i, cellSources[i], got)
		}
	}

	// The stamp still does its job on the tree that IS evaluated: an
	// unlocated head built at runtime -- (gensym) yields a symbol with no
	// location -- is stamped on the copy, so the unbound-symbol error
	// reports the macro CALL site, not <native code>.
	rc = env.LoadString("broken.lisp", "(defmacro broken () (list (gensym) 1))\n\n(broken)\n")
	if rc.Type != lisp.LError {
		t.Fatalf("expected an error from the broken expansion, got %v", rc)
	}
	if loc, ok := rc.Source(); !ok || loc.File != "broken.lisp" || loc.Line != 3 {
		t.Errorf("error from an expansion built with (list ...) is located at %v (ok=%v), want broken.lisp:3\n%v", loc, ok, rc)
	}
}
