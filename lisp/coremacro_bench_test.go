// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// BenchmarkCoreMacroExpand measures one expansion of each core Go macro,
// called directly, so a change to how the macros build their forms shows
// in time and allocations without the evaluator around it.
func BenchmarkCoreMacroExpand(b *testing.B) {
	env := NewEnv(nil)
	if lerr := InitializeUserEnv(env); lerr.IsError() {
		b.Fatal(lerr)
	}
	body := Cells{Symbol("+"), Symbol("x"), Int(1)}.SExpr()
	for _, bm := range []struct {
		name string
		fn   LBuiltin
		args func() *LVal
	}{
		{"defun", macroDefun, func() *LVal {
			return Cells{Symbol("f"), Cells{Symbol("x")}.SExpr(), body}.SExpr()
		}},
		{"defconst", macroDefconst, func() *LVal {
			return Cells{Symbol("c"), Int(1), String("doc")}.SExpr()
		}},
		{"curry-function", macroCurryFun, func() *LVal {
			return Cells{Symbol("f"), Int(1), Int(2)}.SExpr()
		}},
		{"get-default", macroGetDefault, func() *LVal {
			return Cells{Symbol("m"), Symbol("k"), Symbol("d")}.SExpr()
		}},
		{"trace", macroTrace, func() *LVal {
			return Cells{body, String("here")}.SExpr()
		}},
	} {
		args := bm.args()
		b.Run(bm.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				if v := bm.fn(env, args); v.IsError() {
					b.Fatal(v)
				}
			}
		})
	}
}
