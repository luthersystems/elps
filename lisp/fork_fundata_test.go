// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"testing"
)

// TestForkRebuildsFunDataPerHeaderIndistinguishably is the measurement
// behind the *funData row in memoExemptions.
//
// The forker rebuilds a *funData per HEADER (`cp.Native = &funData{...}`)
// and memoises nothing, which is the #576 / #585 shape in outline: FunRef
// puts two headers on ONE *funData deliberately -- that is what makes a
// reference a reference -- and the fork gives them one each.  The row says
// that split cannot be observed, and this is why: a funData is write-once.
// Every one in the package is built by a composite literal and no field of
// one is ever assigned afterwards, so the two copies are field-identical,
// and the single field that could have differed -- env -- goes through the
// forker's own envs memo, so both copies name the SAME copied environment.
//
// If a funData field ever becomes mutable, or env stops being memoised,
// this test is what goes red and the row is what has to change.
func TestForkRebuildsFunDataPerHeaderIndistinguishably(t *testing.T) {
	for _, tc := range []struct {
		name  string
		build func(t *testing.T, env *LEnv) *LVal
	}{
		{"lambda", func(t *testing.T, env *LEnv) *LVal {
			t.Helper()
			return setGlobal(t, env, "f", "lambda", QExpr([]*LVal{Symbol("x")}), Symbol("x"))
		}},
		{"builtin", func(t *testing.T, env *LEnv) *LVal {
			t.Helper()
			f := env.GetFunGlobal(Symbol("+"))
			if f.Type != LFun {
				t.Fatalf("(+) is not a function: %v", f)
			}
			if lerr := env.PutGlobal(Symbol("f"), f); lerr.Type == LError {
				t.Fatalf("PutGlobal f: %v", lerr)
			}
			return f
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newForkTestEnv(t)
			f := tc.build(t, env)
			ref := FunRef(Symbol("g"), f)
			if ref.Type != LFun {
				t.Fatalf("FunRef: %v", ref)
			}
			if lerr := env.PutGlobal(Symbol("g"), ref); lerr.Type == LError {
				t.Fatalf("PutGlobal g: %v", lerr)
			}
			if f.funData() != ref.funData() {
				t.Fatal("premise: FunRef did not share the *funData")
			}

			_, get := forkOf(t, env)
			ff, fg := get("f"), get("g")
			a, b := ff.funData(), fg.funData()
			if a == b {
				// Not a failure -- a fork that memoised funData would be
				// strictly better. The row only has to hold when it does not.
				t.Skip("the fork shared the *funData; nothing to measure")
			}
			if a.fid != b.fid || a.pkg != b.pkg {
				t.Errorf("the split copies differ: fid %q/%q, pkg %q/%q", a.fid, b.fid, a.pkg, b.pkg)
			}
			if a.env != b.env {
				t.Errorf("the split copies name different environments (%p, %p); env is supposed to\n"+
					"travel through the forker's envs memo, which is what makes the split unobservable",
					a.env, b.env)
			}
			if reflect.ValueOf(a.builtin).Pointer() != reflect.ValueOf(b.builtin).Pointer() {
				t.Error("the split copies hold different builtins")
			}
			if a.loc != nil || b.loc != nil {
				t.Errorf("fork kept a defining location (%v, %v); it is documented to drop it", a.loc, b.loc)
			}
		})
	}
}
