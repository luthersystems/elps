// Copyright © 2018 The ELPS authors

package lisp

import (
	"reflect"
	"testing"
)

// noopBuiltin is an LBuiltin used as a stand-in body for constructor
// tests — none of the cases below invoke it.
func noopBuiltin(_ *LEnv, _ *LVal) *LVal { return Nil() }

// TestFunInPackage_SetsPackage guards the FunInPackage / MacroInPackage
// / SpecialOpInPackage constructor invariant for issue #271: each must
// produce an LFun whose FunData.Package equals the pkg argument. A
// regression in this layer would re-enable the BUG: GetFunName log
// spam that the libschema fix and surrounding API safeguards
// eliminated.
func TestFunInPackage_SetsPackage(t *testing.T) {
	formals := QExpr([]*LVal{Symbol("x")})

	t.Run("FunInPackage", func(t *testing.T) {
		v := FunInPackage("my-pkg", "fid-1", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
		if got := v.FID(); got != "fid-1" {
			t.Errorf("FID() = %q, want %q", got, "fid-1")
		}
	})

	t.Run("MacroInPackage", func(t *testing.T) {
		v := MacroInPackage("my-pkg", "fid-2", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if v.FunType != LFunMacro {
			t.Errorf("FunType = %v, want LFunMacro", v.FunType)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
	})

	t.Run("SpecialOpInPackage", func(t *testing.T) {
		v := SpecialOpInPackage("my-pkg", "fid-3", formals, noopBuiltin)
		if v.Type != LFun {
			t.Fatalf("Type = %v, want LFun", v.Type)
		}
		if v.FunType != LFunSpecialOp {
			t.Errorf("FunType = %v, want LFunSpecialOp", v.FunType)
		}
		if got := v.Package(); got != "my-pkg" {
			t.Errorf("Package() = %q, want %q", got, "my-pkg")
		}
	})

	// Empty pkg is a valid input — it documents the (deprecated)
	// "caller will set Package later" path. Verify the constructor
	// doesn't reject or rewrite it.
	t.Run("FunInPackage empty pkg", func(t *testing.T) {
		v := FunInPackage("", "fid-4", formals, noopBuiltin)
		if got := v.Package(); got != "" {
			t.Errorf("Package() = %q, want empty", got)
		}
	})
}

// TestBatchedEntryValuesMatchTheirConstructors is the guard on sortedmap's
// entry batching (issue #379, item 6).
//
// Entries builds its pair and key LVals as struct literals carved out of
// arrays instead of calling QExpr and String n times, because the per-entry
// allocation was the single largest object-count site in the libjson
// benchmark suite.  Those literals are copies of the constructors' bodies, so
// they are correct only for as long as the constructors stay what they are
// today.  A field added to String or QExpr and not added here would produce
// map entries that differ from every other value of their type in a way
// nothing else would notice.
//
// reflect.DeepEqual reads unexported fields, so the comparisons below cover
// every field of the struct, not just the ones a literal names.
//
// Red-proof: dropping `quoted: true` from the pair literal in
// sortedmap.Entries, or adding an initialised field to String, fails this.
func TestBatchedEntryValuesMatchTheirConstructors(t *testing.T) {
	for _, s := range []string{"", "k", "a key", "\x00<&>"} {
		batched := LVal{Type: LString, Str: s}
		if want := String(s); !reflect.DeepEqual(&batched, want) {
			t.Errorf("a batched LString for %q is not what String builds:\n got %#v\nwant %#v",
				s, batched, *want)
		}
	}

	cells := []*LVal{String("k"), Int(1)}
	batchedPair := LVal{Type: LSExpr, quoted: true, Cells: cells}
	// mklist copies its arguments into a slice of its own, so the two pairs
	// hold different slices with equal contents; DeepEqual compares the
	// contents, which is what matters.
	if wantPair := mklist(cells[0], cells[1]); !reflect.DeepEqual(&batchedPair, wantPair) {
		t.Errorf("a batched entry pair is not what mklist builds:\n got %#v\nwant %#v",
			batchedPair, *wantPair)
	}
}
