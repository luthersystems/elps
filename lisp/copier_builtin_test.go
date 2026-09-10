// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestCopyBuiltinIsUnchangedByTheCopier: the lisp `copy` builtin is backed
// by the detacher in copy mode (builtinCopy -> deepCopy -> detacher with
// shareOpaque; lisp/copy.go), not by (*LVal).Copy, so the copier fix in
// lisp/copier.go is not lisp-visible through it.  This pins the
// lisp-observable behaviour of `copy` over the shapes the copier changed
// -- a map holding a bytes value and a nested list, and two headers over
// one bytes value -- with expectations that hold identically on 74e4ac8
// (the commit before the copier) and after it.  A `copy` that started
// routing through (*LVal).Copy would need a parity argument, not a Go one;
// this is the test that would notice.
func TestCopyBuiltinIsUnchangedByTheCopier(t *testing.T) {
	t.Parallel()
	env := copierEnv(t)
	copierEval(t, env, `
(set 'm (sorted-map "b" (to-bytes "abc") "l" (list 1 (list 2 3))))
(set 'c (copy m))
(append! (get c "b") 7)
(assoc! c "x" 1)
(set 'b1 (to-bytes "ab"))
(set 'm2 (sorted-map "p" b1 "q" b1))
(set 'c2 (copy m2))
(append! (get c2 "p") 9)
`)
	for _, tc := range []struct{ probe, want string }{
		// The copy's bytes are its own; the source's are untouched.
		{`(list (length (get m "b")) (length (get c "b")))`, `'(3 4)`},
		// A key added to the copy is not on the source.
		{`(list (get m "x") (get c "x"))`, `'(() 1)`},
		// The nested list is equal and private.
		{`(equal? (get m "l") (get c "l"))`, `true`},
		{`(list (nth (get c "l") 1) (nth (get m "l") 1))`, `'('(2 3) '(2 3))`},
		// Two headers over one bytes value are one copied buffer.
		{`(list (length (get c2 "p")) (length (get c2 "q")) (length b1))`, `'(3 3 2)`},
	} {
		if got := copierEval(t, env, tc.probe).String(); got != tc.want {
			t.Errorf("%s = %s, want %s", tc.probe, got, tc.want)
		}
	}
	// The nested list is private to the copy (a distinct header), as the
	// detacher's contract says.
	src := env.GetGlobal(lisp.Symbol("m")).Map()
	cp := env.GetGlobal(lisp.Symbol("c")).Map()
	sl, _ := src.Get(lisp.String("l"))
	cl, _ := cp.Get(lisp.String("l"))
	if sl == cl || sl.Cells[1] == cl.Cells[1] {
		t.Errorf("`copy` shares the nested list with the source")
	}
}
