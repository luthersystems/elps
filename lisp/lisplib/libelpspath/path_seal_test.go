// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestRangeGetDoesNotAliasSealedBacking is the regression test for issue #392,
// the seal-laundering defect elpsvet's elpsseal rule now catches at CI time.
//
// rangePath.Get read the input's LIVE cell backing (toCells), resliced it, and
// wrapped the result in a fresh lisp.SExpr header. A fresh header is unsealed
// by construction, so `(? <program-literal> [a:b])` handed the value domain a
// MUTABLE WINDOW onto a parsed program: substrate's parse cache aliases one
// tree into every warm environment, so a single in-place write through that
// window rewrites the program for all of them — silently, permanently, and
// reachable from pure lisp (the substrate#378 class; see lisp/seal.go).
//
// The kernel's own slicing ops answer this by copying the sealed flag onto the
// new header (builtinCdr, builtinRest, builtinSlice). libelpspath is outside
// package lisp and cannot reach the unexported flag, so Get copies the range
// instead — and only when the input reports IsSealed.
//
// The two subtests are the whole contract: DISJOINT for a sealed input,
// SHARED for an unsealed one. The second matters as much as the first — it
// pins that ordinary values keep the cheap aliasing path and the copy is paid
// only by program literals.
func TestRangeGetDoesNotAliasSealedBacking(t *testing.T) {
	t.Parallel()

	t.Run("sealed input gets disjoint backing", func(t *testing.T) {
		t.Parallel()
		lit := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
		lit.SealAST()
		if !lit.IsSealed() {
			t.Fatal("precondition: SealAST did not seal the literal")
		}

		got, err := Range(0, 2, false).Get(lit)
		if err != nil {
			t.Fatalf("Get: %v", err)
		}
		if len(got.Cells) != 2 {
			t.Fatalf("range length: got %d, want 2", len(got.Cells))
		}

		// The corruption itself: write through the returned header and see
		// whether it lands in the program literal. Before the fix this
		// rewrote lit.Cells[0] and every environment sharing the parse saw
		// 99 where the program said 1.
		got.Cells[0] = lisp.Int(99)
		if lit.Cells[0].Int != 1 {
			t.Fatalf("writing the range result corrupted the sealed literal:"+
				" lit.Cells[0] = %d, want 1 (issue #392)", lit.Cells[0].Int)
		}
		if &got.Cells[0] == &lit.Cells[0] {
			t.Fatal("range result still shares the sealed literal's backing array")
		}
	})

	t.Run("unsealed input keeps the aliasing path", func(t *testing.T) {
		t.Parallel()
		v := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
		if v.IsSealed() {
			t.Fatal("precondition: a hand-built list must not be sealed")
		}

		got, err := Range(0, 2, false).Get(v)
		if err != nil {
			t.Fatalf("Get: %v", err)
		}
		if &got.Cells[0] != &v.Cells[0] {
			t.Fatal("unsealed input was copied: the guard is not conditional on the seal," +
				" so every path range now pays a copy it does not need")
		}
	})
}
