// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// newDetachOwnershipEnv returns a fully initialized root environment with
// its own private Runtime.  Unlike newOwnershipTestEnv it registers the
// builtin libraries so expressions like (+ 1 2) evaluate; package lisp
// cannot import the parser (import cycle), so tests here build ASTs with
// constructors instead of LoadString.
func newDetachOwnershipEnv(t *testing.T) *LEnv {
	t.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv failed: %v", rc)
	}
	if rc := env.InPackage(String(DefaultUserPackage)); rc.Type == LError {
		t.Fatalf("InPackage failed: %v", rc)
	}
	return env
}

// TestOwnershipCheck_DetachedValueCrossesRuntimes is the integration proof
// that Detach and the runtime ownership checker compose: Detach's whole
// purpose is a hermetic hand-off between Runtimes, so a value adopted by
// runtime A, detached, and then used in runtime B must NOT trip the tag.
// If it does, the detacher's seen-table leaked a node (or some backing
// payload) of the original graph into the copy.
//
// The un-detached original used in runtime B is the control: it MUST trip,
// proving the gate was armed while the detached copy passed.
func TestOwnershipCheck_DetachedValueCrossesRuntimes(t *testing.T) {
	envA := newDetachOwnershipEnv(t)
	envB := newDetachOwnershipEnv(t)

	// Build a compound value carrying every hop the checker walks through
	// binding and evaluation: a sorted map holding a list and nested cells.
	m := SortedMap()
	if lerr := m.MapSet("k", QExpr([]*LVal{Int(1), Float(2.5), String("s")})); lerr.Type == LError {
		t.Fatalf("map-set: %v", lerr)
	}
	if lerr := m.MapSet("v", QExpr([]*LVal{Int(7), Int(8), Int(9)})); lerr.Type == LError {
		t.Fatalf("map-set: %v", lerr)
	}
	orig := QExpr([]*LVal{m, String("tail")})

	// Runtime A adopts the graph: binding adopts the root, evaluating
	// adopts the nodes the evaluator visits.
	if lerr := envA.Put(Symbol("m"), orig); lerr.Type == LError {
		t.Fatalf("put in runtime A: %v", lerr)
	}
	if res := envA.Eval(orig); res.Type == LError {
		t.Fatalf("eval in runtime A: %v", res)
	}
	for _, cell := range orig.Cells {
		if res := envA.Eval(cell); res.Type == LError {
			t.Fatalf("eval cell in runtime A: %v", res)
		}
	}

	detached, err := orig.Detach()
	if err != nil {
		t.Fatalf("detach: %v", err)
	}

	// The detached copy must be freely usable by runtime B: binding and
	// evaluation both funnel through checkOwnership, and neither may see a
	// node runtime A already adopted.
	if lerr := envB.Put(Symbol("m"), detached); lerr.Type == LError {
		t.Fatalf("put detached copy in runtime B: %v", lerr)
	}
	if res := envB.Eval(detached); res.Type == LError {
		t.Fatalf("eval detached copy in runtime B: %v", res)
	}
	// Walk into the copy the way application code would, exercising nodes
	// the root-level Put/Eval alone might not have visited.
	for _, cell := range detached.Cells {
		if res := envB.Eval(cell); res.Type == LError {
			t.Fatalf("eval detached cell in runtime B: %v", res)
		}
	}
	dm := detached.Cells[0]
	if got, ok := dm.Map().Get(String("k")); !ok {
		t.Fatal("detached map lost key k")
	} else if res := envB.Eval(got); res.Type == LError {
		t.Fatalf("eval detached map value in runtime B: %v", res)
	}

	// Control: the original must still trip the gate in runtime B —
	// otherwise the pass above proves nothing.
	expectOwnershipPanic(t, func() {
		envB.Put(Symbol("stolen"), orig)
	})
}

// TestOwnershipCheck_DetachedASTCrossesRuntimes covers the motivating
// consumer shape (substrate's parse cache, #362/#363): a cached AST is
// evaluated in runtime A, detached, and the detached AST is evaluated in
// runtime B.  The AST nodes carry real per-node source locations (as
// parser output does — package lisp cannot import the parser itself), so
// the test also proves the detached copy's location storage is fresh.
// The cached original must still trip the gate in runtime B.
func TestOwnershipCheck_DetachedASTCrossesRuntimes(t *testing.T) {
	envA := newDetachOwnershipEnv(t)
	envB := newDetachOwnershipEnv(t)

	buildAST := func() *LVal {
		ast := SExpr([]*LVal{Symbol("+"), Int(1), Int(2)})
		for i, node := range append([]*LVal{ast}, ast.Cells...) {
			node.SetSource(&token.Location{
				File: "cached.lisp",
				Line: 1,
				Col:  i + 1,
				Pos:  i,
			})
		}
		return ast
	}

	ast := buildAST()
	if out := envA.Eval(ast.Copy()); out.Type == LError || out.Int != 3 {
		t.Fatalf("adopt AST in runtime A: %v", out)
	}
	// Eval consumed the copy; make sure the cached original's nodes are
	// adopted by A too, exactly as a cache hit inside A would leave them.
	if out := envA.Eval(ast); out.Type == LError || out.Int != 3 {
		t.Fatalf("eval cached AST in runtime A: %v", out)
	}

	detached, err := ast.Detach()
	if err != nil {
		t.Fatalf("detach AST: %v", err)
	}
	if out := envB.Eval(detached); out.Type == LError || out.Int != 3 {
		t.Fatalf("eval detached AST in runtime B: %v", out)
	}

	expectOwnershipPanic(t, func() {
		envB.Eval(ast)
	})
}
