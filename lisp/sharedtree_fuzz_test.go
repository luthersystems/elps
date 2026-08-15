// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Fuzzing OWNERSHIP of the parse tree, which no other target in this
// repository covers.
//
// FuzzEval hands every evaluation its own freshly-read source, so each input
// gets a private AST and any write the evaluator makes into that AST is
// invisible: the tree is thrown away immediately afterwards.  The property
// that matters downstream is the opposite one.  A reader result is a value
// callers hold onto -- substrate reads a phylum once and evaluates it under
// more than one environment, the LSP and the linter analyse a tree the parser
// already handed out, and mcpserver's workspace index keeps parsed documents
// alive across requests.  If evaluation writes into the tree it was given,
// the SECOND consumer of that tree sees a program that is no longer the one
// the reader produced.
//
// That failure mode is not hypothetical here.  Four separate defects with
// exactly this shape were found and fixed in this repository: macro expansion
// aliasing the form it expands (elps#396), the reader emitting nodes macro
// expansion writes into (elps#370), sequence views writing through into their
// source (elps#369/#373), and env.Loc aliasing (elps#362/#366).  Each was a
// tree that two consumers shared and one of them mutated.
//
// # The invariant
//
// Evaluating a tree must not change what that tree means.  Stated
// operationally, and in the two independent ways it can break:
//
//   - AGREEMENT.  Evaluating a shared tree must produce what evaluating a
//     private tree produces.  This holds without the race detector, so the
//     nightly sweep tests it at full fuzzing speed: a mutation that corrupts
//     the tree shows up as a diverging result on the second or third
//     evaluation of it.
//
//   - NO CONCURRENT WRITE.  The evaluations run on separate goroutines over
//     one tree, so `go test -race` over the seed corpus (make race) reports
//     any write into a shared node directly, with both stacks, rather than
//     waiting for the corruption to become observable.  This is the shape
//     that found elps#370.
//
// A source that does not parse is skipped -- that is the parser targets'
// business.  A source whose FIRST evaluation errors is still checked: an
// error is a value, and two evaluations of one tree must produce the SAME
// error.
//
// # Budgets
//
// Identical to FuzzEval's, and for the same reason: evaluation is
// Turing-complete, so every run here is bounded by the interpreter's own step,
// tail-iteration, physical-height, nesting and allocation limits plus a
// context deadline.  The per-input cost is a small multiple of FuzzEval's
// because the same source is evaluated sharedRuns+1 times.

// sharedRuns is how many goroutines evaluate the shared tree at once.  Two is
// enough for the race detector to see a conflicting pair, and keeping it small
// keeps the per-input cost near FuzzEval's.
const sharedRuns = 2

// evalTreeOnce evaluates every expression of exprs under a fresh environment
// and returns the rendered results.  exprs may be shared with other
// goroutines; nothing here may write to it.
func evalTreeOnce(exprs []*lisp.LVal) ([]string, error) {
	env, _, rc := newFuzzEnv()
	if rc != nil {
		return nil, errFromLVal(rc)
	}
	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()
	out := make([]string, 0, len(exprs))
	for _, e := range exprs {
		v := env.EvalContext(ctx, e)
		if v == nil {
			return nil, errNilResult
		}
		if lisp.IsInternalPanic(v) {
			return nil, errInternalPanic
		}
		out = append(out, v.String())
	}
	return out, nil
}

type constErr string

func (e constErr) Error() string { return string(e) }

const (
	errNilResult     = constErr("evaluation returned a nil LVal")
	errInternalPanic = constErr("evaluation recovered a Go panic")
)

func errFromLVal(v *lisp.LVal) error { return constErr(v.String()) }

// readTree reads src into a parse tree, or reports that it does not parse.
func readTree(src []byte) ([]*lisp.LVal, bool) {
	exprs, err := parser.NewReader().Read("fuzz", bytes.NewReader(src))
	if err != nil {
		return nil, false
	}
	return exprs, true
}

// sharedTreeProperty is the body of the target, factored out so the corpus
// tests below assert exactly what the fuzzer asserts.
func sharedTreeProperty(t *testing.T, src []byte) {
	t.Helper()

	shared, ok := readTree(src)
	if !ok {
		return
	}
	// Baseline over a PRIVATE tree read from the same bytes.  Read
	// separately rather than deep-copied so the baseline is exactly what a
	// first-and-only consumer of the reader would get.
	private, ok := readTree(src)
	if !ok {
		return
	}

	done := make(chan struct{})
	var want []string
	var wantErr error
	var got [sharedRuns][]string
	var gotErr [sharedRuns]error

	go func() {
		defer close(done)
		want, wantErr = evalTreeOnce(private)
		var wg sync.WaitGroup
		// Ranged over the arrays themselves rather than over sharedRuns: the
		// bound is then the array's own length, which the bounds-check
		// analyser can see (gosec G602 cannot, when the bound is a const).
		for i := range got {
			wg.Add(1)
			go func(i int) {
				defer wg.Done()
				got[i], gotErr[i] = evalTreeOnce(shared)
			}(i)
		}
		wg.Wait()
	}()

	// The evaluations are budgeted, but a budget only stops code that reaches
	// a check; a builtin that loops inside Go is step-blind.  The watchdog is
	// what turns "this terminates" into an assertion.  Denominated in
	// SCHEDULED time so a starved process is not charged to the evaluator.
	select {
	case <-done:
	case <-time.After(watchdogTimeout * (sharedRuns + 1)):
		t.Fatalf("shared-tree evaluation did not terminate\n--- source (%d bytes) ---\n%q", len(src), src)
		return
	}

	if wantErr != nil {
		// The baseline itself could not be evaluated (nil result or a
		// recovered Go panic).  FuzzEval owns that assertion; reporting it
		// here too would duplicate its findings.
		return
	}
	for i := range got {
		if gotErr[i] != nil {
			t.Fatalf("evaluating the SHARED tree failed where the private tree did not: %v"+
				"\n--- source (%d bytes) ---\n%q", gotErr[i], len(src), src)
			return
		}
		if len(got[i]) != len(want) {
			t.Fatalf("shared tree produced %d results, private tree produced %d"+
				"\n--- source (%d bytes) ---\n%q", len(got[i]), len(want), len(src), src)
			return
		}
		for j := range want {
			if got[i][j] != want[j] {
				t.Fatalf("evaluating a SHARED parse tree changed the program's meaning"+
					"\n  expression %d, shared run %d"+
					"\n  private tree: %s"+
					"\n  shared tree:  %s"+
					"\n--- source (%d bytes) ---\n%q",
					j, i, want[j], got[i][j], len(src), src)
				return
			}
		}
	}
}

// FuzzSharedTreeEval asserts that evaluating a parse tree does not change what
// that tree means to the next consumer of it.
func FuzzSharedTreeEval(f *testing.F) {
	for _, src := range fuzzseed.EvalTerminating() {
		f.Add([]byte(src))
	}
	for _, src := range fuzzseed.EvalAdversarial() {
		f.Add([]byte(src))
	}
	// Macro-heavy shapes: expansion is where every aliasing defect found so
	// far has lived, because it is the one part of evaluation that builds new
	// forms out of the caller's own parse-tree nodes.
	for _, src := range sharedTreeSeeds() {
		f.Add([]byte(src))
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		sharedTreeProperty(t, src)
	})
}

// sharedTreeSeeds are programs whose evaluation re-uses caller-supplied
// fragments: macro definitions and calls, quasiquote splicing, and nested
// expansions.  A tree is only corruptible if something writes into it, and
// these are the shapes that write.
func sharedTreeSeeds() []string {
	return []string{
		`(defmacro m (x) (quasiquote (+ 1 (unquote x)))) (m 2) (m (m 3))`,
		`(defmacro twice (e) (quasiquote (list (unquote e) (unquote e)))) (twice (+ 1 2))`,
		`(defmacro ident (x) x) (ident (ident (ident 1)))`,
		`(defmacro splice (xs) (quasiquote (list (unquote-splicing xs)))) (splice '(1 2 3))`,
		`(defmacro q (x) (quote (quote x))) (q 1)`,
		`(defun f (x) (+ x 1)) (f 1) (f 2)`,
		`(set 'v '(1 2 3)) (append v 4) v`,
		`(defmacro when2 (c &rest body) (quasiquote (if (unquote c) (progn (unquote-splicing body)) ())))
		 (when2 true 1 2 3)`,
		`(let ([x '(1 2 3)]) (list (nth x 0) (slice x 0 2) x))`,
		`(sorted-map 'a 1 'b 2)`,
		`(defmacro outer (x) (quasiquote (defmacro inner (y) (quasiquote (list (unquote (unquote x)) (unquote y))))))`,
		`'#^0`,
		`(lambda (x) x)`,
		`(handler-bind ([error (lambda (c &rest r) 'handled)]) (error 'boom))`,
	}
}

// TestSharedTreeSeedsAgree runs the hand-written seeds through the same
// property outside fuzzing, so a regression is caught by `make test` and, with
// the race detector, by `make race`.
func TestSharedTreeSeedsAgree(t *testing.T) {
	t.Parallel()
	for _, src := range sharedTreeSeeds() {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			sharedTreeProperty(t, []byte(src))
		})
	}
}
