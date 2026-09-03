// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// TestMacroExpansionStampNeverWritesTheExpansion pins the copy-on-write
// contract of stampMacroExpansion (issue #582): the value a macro body
// returns is never written to.  A node that needs a stamp, or whose cells
// changed, is replaced in the RETURNED tree by a private copy; everything
// else -- located syntax, sealed subtrees, located values -- is shared.
//
// The shapes are the ones the in-place stamp got wrong.  A runtime list that
// is itself a binding, `(set 'l (list 1 2))` returned by a macro, acquired
// the macro call site as its location.  A LOCATED unsealed list holding an
// unlocated value had that cell overwritten with the value's private header
// copy, so a later `append!` through the binding and through the expansion
// could diverge.
func TestMacroExpansionStampNeverWritesTheExpansion(t *testing.T) {
	env := initSafetyTestEnv(t)
	callSite := &token.Location{File: "stamp.lisp", Line: 5, Col: 1}
	realLoc := &token.Location{File: "real.lisp", Line: 1, Col: 1, Pos: 10}
	unlocatedFun := func() *LVal {
		fn := env.Lambda(Formals("x"), []*LVal{Symbol("x")})
		fn.SetSource(nil)
		return fn
	}

	t.Run("unlocated list root", func(t *testing.T) {
		head, arg := Symbol("+"), Int(1)
		v := SExpr([]*LVal{head, arg})
		got := stampMacroExpansion(v, callSite, nil, env.Runtime)
		if got == v {
			t.Fatalf("a root that needs a stamp was returned as itself")
		}
		if got.source != callSite || got.Cells[0].source != callSite || got.Cells[1].source != callSite {
			t.Errorf("the returned tree is not stamped throughout")
		}
		if got.Cells[0] == head || got.Cells[1] == arg {
			t.Errorf("an unlocated cell was shared instead of copied")
		}
		if &got.Cells[0] == &v.Cells[0] {
			t.Errorf("the copy shares the input's cell slice, so a cell write would land in the input")
		}
		if v.source != nil || head.source != nil || arg.source != nil || v.Cells[0] != head || v.Cells[1] != arg {
			t.Errorf("the input was written to")
		}
	})

	t.Run("located list with an unlocated value cell", func(t *testing.T) {
		fn := unlocatedFun()
		head := Symbol("call")
		head.SetSource(realLoc)
		v := SExpr([]*LVal{head, fn})
		v.SetSource(realLoc)
		got := stampMacroExpansion(v, callSite, nil, env.Runtime)
		if got == v {
			t.Fatalf("a root whose cell changed was returned as itself")
		}
		if got.source != realLoc {
			t.Errorf("a located root lost its location on the copy: %v", got.source)
		}
		if got.Cells[0] != head {
			t.Errorf("a located cell was copied needlessly")
		}
		if got.Cells[1] == fn || got.Cells[1].source != callSite || got.Cells[1].Native != fn.Native {
			t.Errorf("the value cell is not a stamped private header of the same function")
		}
		if v.Cells[1] != fn || fn.source != nil {
			t.Errorf("the input's value cell was written to (issue #582)")
		}
	})

	t.Run("unlocated list nested in a located binding", func(t *testing.T) {
		inner := SExpr([]*LVal{Symbol("inner")})
		head := Symbol("outer")
		head.SetSource(realLoc)
		v := SExpr([]*LVal{head, inner})
		v.SetSource(realLoc)
		got := stampMacroExpansion(v, callSite, nil, env.Runtime)
		if got == v || got.Cells[1] == inner {
			t.Fatalf("the nested unlocated list was not replaced on the copy")
		}
		if got.Cells[1].source != callSite || got.Cells[1].Cells[0].source != callSite {
			t.Errorf("the nested copy is not stamped")
		}
		if v.Cells[1] != inner || inner.source != nil || inner.Cells[0].source != nil {
			t.Errorf("the input's nested list was written to (issue #582)")
		}
	})

	t.Run("sealed subtree and located cells are shared", func(t *testing.T) {
		sealed := SExpr([]*LVal{Symbol("sealed"), Int(2)})
		sealed.SealAST()
		located := Symbol("located")
		located.SetSource(realLoc)
		v := SExpr([]*LVal{Symbol("head"), sealed, located})
		got := stampMacroExpansion(v, callSite, nil, env.Runtime)
		if got == v {
			t.Fatalf("a root that needs a stamp was returned as itself")
		}
		if got.Cells[1] != sealed || got.Cells[2] != located {
			t.Errorf("a sealed subtree or a located cell was copied instead of shared")
		}
		if _, ok := sealed.Source(); ok {
			t.Errorf("the sealed subtree was stamped")
		}
	})

	t.Run("debugger metadata lands on the copy only", func(t *testing.T) {
		ctx := &macroExpansionContext{CallSite: callSite, Name: "m"}
		v := SExpr([]*LVal{Symbol("+"), Int(1)})
		got := stampMacroExpansion(v, callSite, ctx, env.Runtime)
		if got.macroExpansion == nil || got.Cells[0].macroExpansion == nil {
			t.Errorf("the copy did not get macro-expansion metadata")
		}
		if v.macroExpansion != nil || v.Cells[0].macroExpansion != nil {
			t.Errorf("the input got macro-expansion metadata")
		}
	})
}

// TestMacroExpansionStampAllocatesNothingWhenNothingNeedsAStamp pins the
// cost of the copy-on-write stamp on the usual expansion shape: quasiquote
// output, whose every node carries its template's location.  Nothing needs a
// stamp, so the expansion is returned as is and the walk allocates nothing.
func TestMacroExpansionStampAllocatesNothingWhenNothingNeedsAStamp(t *testing.T) {
	env := initSafetyTestEnv(t)
	callSite := &token.Location{File: "stamp.lisp", Line: 5, Col: 1}
	realLoc := &token.Location{File: "real.lisp", Line: 1, Col: 1, Pos: 10}
	locate := func(v *LVal) *LVal { v.SetSource(realLoc); return v }
	sealed := SExpr([]*LVal{Symbol("arg"), Int(2)})
	sealed.SealAST()
	fn := env.Lambda(Formals("x"), []*LVal{Symbol("x")})
	fn.SetSource(realLoc)
	v := locate(SExpr([]*LVal{
		locate(Symbol("progn")),
		locate(SExpr([]*LVal{locate(Symbol("f")), sealed, fn})),
		locate(Int(3)),
	}))

	if got := stampMacroExpansion(v, callSite, nil, env.Runtime); got != v {
		t.Fatalf("an expansion with nothing to stamp was copied")
	}
	if allocs := testing.AllocsPerRun(100, func() {
		stampMacroExpansion(v, callSite, nil, env.Runtime)
	}); allocs != 0 {
		t.Errorf("stamping an expansion with nothing to stamp allocated %v times per run, want 0", allocs)
	}
}

// TestMacroExpansionStampMintsExpansionIDsOnlyForTheKeptWalk pins that a
// cyclic expansion consumes exactly as many expansion IDs as it has stamped
// nodes.  The walk that discovers the cycle is abandoned along with the
// copies it built; the IDs it minted go with it, so the rerun's IDs are
// contiguous and in pre-order, as they were when the stamp wrote in place.
func TestMacroExpansionStampMintsExpansionIDsOnlyForTheKeptWalk(t *testing.T) {
	env := initSafetyTestEnv(t)
	callSite := &token.Location{File: "stamp.lisp", Line: 5, Col: 1}
	ctx := &macroExpansionContext{CallSite: callSite, Name: "m"}
	v := SExpr([]*LVal{Symbol("x")})
	v.Cells = append(v.Cells, v) // (x . <self>)

	before := env.Runtime.macroExpSeq
	got := stampMacroExpansion(v, callSite, ctx, env.Runtime)
	if got == v || got.Cells[1] != got {
		t.Fatalf("the copy does not contain itself where the expansion did")
	}
	if got.macroExpansion == nil || got.Cells[0].macroExpansion == nil {
		t.Fatalf("the copy is missing expansion metadata")
	}
	root, child := got.macroExpansion.ID, got.Cells[0].macroExpansion.ID
	if root != before+1 || child != before+2 {
		t.Errorf("expansion IDs root=%d child=%d, want %d and %d (pre-order, contiguous)", root, child, before+1, before+2)
	}
	if env.Runtime.macroExpSeq != before+2 {
		t.Errorf("the runtime consumed %d IDs for a two-node cycle, want 2", env.Runtime.macroExpSeq-before)
	}
}
