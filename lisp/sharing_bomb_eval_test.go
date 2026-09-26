// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// sharingBombBaseEnv returns a user environment with a reader and a 1M step
// budget.
func sharingBombBaseEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	lisp.WithMaxSteps(1_000_000)(env)
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

// Sharing bombs (lisp/sharing.go).  (set! x (list x x)) repeated D times
// costs D steps and D two-cell lists and has 2^D paths.  Each walker below
// used to walk it as a tree, inside one builtin step, with no step charge,
// no context poll and no aggregate allocation cap: at D=40 the walk ran for
// hours under --timeout and --max-steps, and where it allocated per path it
// exhausted the host's memory well before that.

const sharingBombDepth = 40

// sharingBombEnv returns a user environment in which x and y are distinct
// D-level sharing chains, Q and QQ are the quote and quasiquote symbols, and
// (embed) is a lisp macro whose expansion carries x as data.
func sharingBombEnv(t *testing.T, depth int) *lisp.LEnv {
	t.Helper()
	env := sharingBombBaseEnv(t)
	setup := fmt.Sprintf(`
(set 'x 1) (set 'y 1)
(dotimes (i %d) (set! x (list x x)) (set! y (list y y)))
(set 'Q (car '(quote)))
(set 'QQ (car '(quasiquote)))
(defmacro embed () (list Q x))
()`, depth)
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	return env
}

// runSharingBomb evaluates src against a D=40 sharing bomb under a 100ms
// deadline and a 1M step budget, and returns its result.  The watchdog ends
// the test binary if the evaluation does not come back, or allocates without
// bound, since neither can be interrupted from outside.
func runSharingBomb(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	env := sharingBombEnv(t, sharingBombDepth)
	var rc *lisp.LVal
	testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
		ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
		defer cancel()
		rc = env.LoadStringContext(ctx, "probe.lisp", src)
	})
	return rc
}

// sharedChainDepth checks that v is a depth-level chain of two-cell lists
// ending in the integer 1, whose two cells are the SAME node at every level
// the walk finished after its memo switched on -- the sharing of the input,
// preserved.  The bottom levels were walked, as a tree, before that, and
// may be unshared: 2^13 containers is past sharedWalkBudget.
func sharedChainDepth(t *testing.T, v *lisp.LVal, depth int) {
	t.Helper()
	for v.Type == lisp.LQuote { // a runtime list evaluates to itself, quoted
		v = v.Cells[0]
	}
	for i := range depth {
		if v.Type != lisp.LSExpr || len(v.Cells) != 2 {
			t.Fatalf("level %d: got %v with %d cells, want a two-cell list", i, v.Type, len(v.Cells))
		}
		if i < depth-13 && v.Cells[0] != v.Cells[1] {
			t.Fatalf("level %d: the two cells are distinct nodes; the output unshared the input", i)
		}
		v = v.Cells[0]
	}
	if v.Type != lisp.LInt || v.Int != 1 {
		t.Fatalf("leaf: got %v, want 1", v)
	}
}

func TestSharingBombMacroExpansion(t *testing.T) {
	rc := runSharingBomb(t, `(embed)`)
	if rc.Type == lisp.LError {
		t.Fatalf("(embed): %v", rc)
	}
	sharedChainDepth(t, rc, sharingBombDepth)
}

func TestSharingBombQuasiquote(t *testing.T) {
	rc := runSharingBomb(t, `(eval (list QQ x))`)
	if rc.Type == lisp.LError {
		t.Fatalf("(eval (list QQ x)): %v", rc)
	}
	sharedChainDepth(t, rc, sharingBombDepth)
}

// A quasiquote template that shares a subtree holding an UNQUOTE must still
// evaluate it once per occurrence, as it always has: the memo covers only
// pure subtrees, and the evaluations are what the step budget charges for.
func TestSharingBombQuasiquoteEvaluatesSharedUnquotesPerOccurrence(t *testing.T) {
	env := sharingBombBaseEnv(t)
	rc := env.LoadString("probe.lisp", `
(set 'n 0)
(set 'u (car '((unquote (progn (set! n (+ n 1)) n)))))
(set 'x (list u 1))
(dotimes (i 14) (set! x (list x x)))
(set 'out (eval (list (car '(quasiquote)) x)))
n`)
	if rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc.Int != 1<<14 {
		t.Fatalf("shared unquote evaluated %d times, want %d (once per occurrence)", rc.Int, 1<<14)
	}
}

// The same bomb with an unquote under it is bounded by the step budget: each
// occurrence is an evaluation, and the budget stops it.  Not a regression
// test -- main stops here too -- but a pin on the semantics: a shared
// unquote is still evaluated, and charged, once per occurrence.
func TestSharingBombQuasiquoteWithUnquoteHitsStepBudget(t *testing.T) {
	env := sharingBombEnv(t, 1)
	lisp.WithMaxSteps(100_000)(env) // enough to show the budget stops it; cheap under -race
	src := `
(set 'u (car '((unquote 1))))
(set 'x (list u 1))
(dotimes (i 40) (set! x (list x x)))
(eval (list QQ x))`
	var rc *lisp.LVal
	testdeadline.Watch("quasiquote over shared unquotes", 20*time.Second, 1<<30, func() {
		rc = env.LoadString("probe.lisp", src)
	})
	if rc.Type != lisp.LError || rc.Str != lisp.CondStepLimitExceeded {
		t.Fatalf("got %v, want the step limit", rc)
	}
}

// runSharingBombStepsOnly loads setup, then evaluates probe under the step
// budget alone -- no deadline -- which is the configuration a sharing bomb
// used to bypass outright.
func runSharingBombStepsOnly(t *testing.T, setup, probe string) *lisp.LVal {
	t.Helper()
	env := sharingBombEnv(t, 1)
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	var rc *lisp.LVal
	testdeadline.Watch(probe, 20*time.Second, 1<<30, func() {
		rc = env.LoadString("probe.lisp", probe)
	})
	return rc
}

// A WIDE shared list holding an unquote: every occurrence evaluates the
// unquote (one step) but also rebuilds all of the list's cells.  Past the
// budget a re-entered impure list is charged its width, so the step budget
// bounds the rebuilding too.
func TestSharingBombQuasiquoteChargesSharedImpureRebuilds(t *testing.T) {
	rc := runSharingBombStepsOnly(t, `
(set 'n (cons (car '((unquote 1))) (make-sequence 0 10000)))
(set 'x n)
(dotimes (i 40) (set! x (list x x)))
()`, `(eval (list QQ x))`)
	if rc.Type != lisp.LError || rc.Str != lisp.CondStepLimitExceeded {
		t.Fatalf("got %v, want the step limit", rc)
	}
}

// A wide PURE list shared along many paths: the budget counts cells, so the
// memo switches on after a few revisits rather than after thousands.
func TestSharingBombWidePureList(t *testing.T) {
	setup := `(set 'w (make-sequence 0 100000)) (dotimes (i 40) (set! w (list w w))) (defmacro embed-w () (list Q w)) ()`
	for _, probe := range []string{`(eval (list QQ w))`, `(embed-w)`} {
		if rc := runSharingBombStepsOnly(t, setup, probe); rc.Type == lisp.LError {
			t.Fatalf("%s: %v", probe, rc)
		}
	}
}

// A template that merely repeats a form -- a macro-writing macro putting one
// argument form into an inner quasiquote three times -- is not a sharing
// bomb, and must cost exactly the steps it costs as a tree, even with an
// unquote in the repeated form and the walk past the budget.  Only
// DUPLICATED rebuild work beyond its own allowance is ever charged.
func TestSharingQuasiquoteRepeatedFormKeepsStepCount(t *testing.T) {
	steps := func(shared bool) int64 {
		env := sharingBombEnv(t, 1)
		setup := `(set 'form (cons (car '((unquote 1))) (make-sequence 0 3000)))
(set 'tpl (list form form form))
(set 'tree (list (copy form) (copy form) (copy form)))
()`
		if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
			t.Fatalf("setup: %v", rc)
		}
		probe := `(eval (list QQ tree))`
		if shared {
			probe = `(eval (list QQ tpl))`
		}
		before := env.Runtime.TotalSteps()
		if rc := env.LoadString("probe.lisp", probe); rc.Type == lisp.LError {
			t.Fatalf("%s: %v", probe, rc)
		}
		return env.Runtime.TotalSteps() - before
	}
	if tree, dag := steps(false), steps(true); tree != dag {
		t.Fatalf("a form repeated three times costs %d steps; as a tree, %d", dag, tree)
	}
}
