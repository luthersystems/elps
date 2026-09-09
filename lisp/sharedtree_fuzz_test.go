// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"fmt"
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
// source (elps#369/#373), and env.loc aliasing (elps#362/#366).  Each was a
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
//
// # Why there is no exception for a result that looks like a budget
//
// A comparison oracle that agrees to ignore some results is the one way this
// target can fail silently: every exception it carries is a class of
// divergence it has promised not to report.  So there is none.  A rendered
// result is compared EXACTLY, always.
//
// This target used to carry one, for the case where the two arms disagreed
// about which runaway-loop backstop stopped the same infinite loop (crasher
// 423b7dd9e421bd27).  Both spellings of it were forgeable by the program
// under test.  The first matched the budget's MESSAGE, which is just text a
// program is free to return or to raise.  The second read PROVENANCE -- the
// unforgeable *lisp.TailIterationLimitError the evaluator stores in the
// error's data cell -- and was forgeable anyway, by CAPTURED-BUDGET REPLAY:
// handler-bind catches every condition but internal-panic (opHandlerBind), so
// a program can catch the tail-iteration error, keep the Go error it carries,
// run arbitrary code that DIVERGES between the two arms, and finally raise a
// new error carrying the captured cell.  Both arms then classify as the same
// budget, and everything the program did in between is hidden.  See
// TestOracleReportsCapturedBudgetReplay.
//
// The exception was never needed for that budget in the first place.  The
// tail-iteration counter -- and max steps, max alloc and physical height with
// it -- is DETERMINISTIC: the same program under the same fresh environment
// and the same limit trips at the same point in both arms and renders the same
// message, so exact comparison already agrees.
// TestOracleTailBudgetArmsCompareEqual pins that.
//
// # The deadline is inconclusiveness, not a classification
//
// The context deadline is the one budget that is not deterministic, because it
// is denominated in WALL CLOCK: the two arms run at different speeds (the
// shared arm has skipped a parse) and on a loaded machine one can be stopped
// by the clock where the other was not.  That is not a fact about the value,
// so no amount of looking at the value can decide it -- which is exactly why
// every value-based attempt was forgeable.
//
// It is decided by HARNESS STATE instead.  Each arm records, per expression,
// whether its OWN context had expired when that expression's evaluation
// returned (`ctx.Err() != nil`), which no program can influence.  If either
// arm's context had expired at expression j, this input is INCONCLUSIVE from j
// onwards: expressions before j are still compared exactly, and j and
// everything after it are not compared at all.  A run the clock interrupted
// carries no information about whether the tree changed meaning, so the
// correct answer is to decline to answer -- not to declare two differing
// results equal.

// sharedRuns is how many goroutines evaluate the shared tree at once.  Two is
// enough for the race detector to see a conflicting pair, and keeping it small
// keeps the per-input cost near FuzzEval's.
const sharedRuns = 2

// treeEval is one evaluation of a whole tree: what each expression rendered
// to, and -- alongside the rendering, never derived from it -- whether this
// arm's own context had already expired when that expression's evaluation
// returned.  The two are kept in step by index: rendered[j] and expired[j]
// describe expression j.
//
// expired is HARNESS state, not a property of the value.  It is the only
// thing the oracle consults besides the rendered text, and it is the only
// thing it can consult that the program under test has no say in: a program
// can return, raise, or replay any value it likes, but it cannot make this
// process's context deadline pass.
type treeEval struct {
	rendered []string
	expired  []bool
}

// evalTreeOnce evaluates every expression of exprs under a fresh environment
// and returns the rendered results together with the per-expression record of
// whether this run's context had expired.
// exprs may be shared with other goroutines; nothing here may write to it.
func evalTreeOnce(exprs []*lisp.LVal) (treeEval, error) {
	env, _, rc := newFuzzEnv()
	if rc != nil {
		return treeEval{}, errFromLVal(rc)
	}
	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()
	out := treeEval{
		rendered: make([]string, 0, len(exprs)),
		expired:  make([]bool, 0, len(exprs)),
	}
	for _, e := range exprs {
		v := env.EvalContext(ctx, e)
		if v == nil {
			return treeEval{}, errNilResult
		}
		if lisp.IsInternalPanic(v) {
			return treeEval{}, errInternalPanic
		}
		// Sampled HERE, where this run's context can still be asked whether
		// it expired.  Once the value has been rendered to a string that fact
		// is gone, and the string itself cannot supply it.
		out.expired = append(out.expired, ctx.Err() != nil)
		out.rendered = append(out.rendered, v.String())
	}
	return out, nil
}

// treeComparison is the verdict of comparing one shared arm against the
// private baseline.  Exactly one of the three states holds:
//
//   - divergedAt >= 0: expression divergedAt rendered differently in the two
//     arms, with both arms' contexts still live.  That is the finding this
//     target exists to report.
//   - inconclusive != "": an arm's context had expired at expression
//     firstExpired, so that expression and every one after it were not
//     compared.  Everything before it did compare equal.
//   - neither: every expression compared equal.
type treeComparison struct {
	inconclusive string
	divergedAt   int
	firstExpired int
}

// compareTreeEvals is the whole comparison oracle, factored out so that the
// tests in sharedtree_oracle_test.go assert exactly what the fuzz target
// asserts.
//
// There is no exception and no classification: renderings are compared with
// ==.  The only thing that stops a comparison is an EXPIRED CONTEXT, which is
// read from the harness's own record and never from the values.  See this
// file's header for why every value-based exception this target has carried
// was forgeable by the program under test.
//
// want and got must describe the same number of expressions; the caller
// checks that and reports a mismatch as a finding of its own.
func compareTreeEvals(want, got treeEval) treeComparison {
	out := treeComparison{divergedAt: -1, firstExpired: -1}
	for j := range want.rendered {
		// Checked BEFORE the comparison for expression j: an evaluation the
		// clock interrupted says nothing about whether the tree changed
		// meaning, and neither does anything evaluated after it in the same
		// environment.
		if want.expired[j] || got.expired[j] {
			which := "the private arm"
			switch {
			case want.expired[j] && got.expired[j]:
				which = "both arms"
			case got.expired[j]:
				which = "the shared arm"
			}
			out.firstExpired = j
			out.inconclusive = fmt.Sprintf(
				"%s ran out of context deadline at expression %d;"+
					" expressions 0..%d compared equal and %d.. were not compared",
				which, j, j-1, j)
			return out
		}
		if got.rendered[j] != want.rendered[j] {
			out.divergedAt = j
			return out
		}
	}
	return out
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

// copyWalkCap bounds the paired walk in copyOwnsItsPositions.  The reader's
// own nesting and size limits keep real inputs far below it; the cap is here
// so a pathological one costs a bounded walk rather than the whole budget, and
// a truncated walk says so rather than reporting a silent pass.
const copyWalkCap = 200000

// copyOwnsItsPositions asserts that LVal.Copy hands back a tree that shares no
// *token.Location with the tree it copied -- the property lisp.TextLoader's
// "each evaluation gets a private tree" rests on (elps#446).
//
// NO pointer is allowed to be shared.  The one exception this check used to
// carry -- nativeSource's process-wide singleton, which LVal.Copy deliberately
// did not separate -- went away with the singleton: issue #362 deleted it, so
// a Go-constructed value records no location at all and there is nothing left
// for two nodes to share.  The check is therefore unconditional now, which is
// strictly stronger and one fewer thing to get wrong.
//
// Iterative rather than recursive: the walk must not be the thing that
// overflows the goroutine stack on a deeply nested input.
func copyOwnsItsPositions(t *testing.T, exprs []*lisp.LVal, src []byte) {
	t.Helper()

	seen := 0
	for _, orig := range exprs {
		stack := [][2]*lisp.LVal{{orig, orig.Copy()}}
		for len(stack) > 0 {
			pair := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a, b := pair[0], pair[1]
			if a == nil || b == nil {
				continue
			}
			seen++
			if seen > copyWalkCap {
				t.Logf("copy-ownership walk truncated at %d nodes; the rest of"+
					" this input was not checked", copyWalkCap)
				return
			}
			// SourceRefForTest, not Source(): the property is pointer
			// IDENTITY, and Source() returns a value copy exactly so that no
			// caller can hold the pointer (#382).  See lisp/export_test.go.
			aLoc := lisp.SourceRefForTest(a)
			if aLoc != nil && aLoc == lisp.SourceRefForTest(b) {
				t.Fatalf("LVal.Copy handed back a node sharing the original's"+
					" *token.Location (elps#446)"+
					"\n  node type: %v"+
					"\n  location:  %v"+
					"\n--- source (%d bytes) ---\n%q",
					a.Type, aLoc, len(src), src)
				return
			}
			// LArray shares its Cells backing and LSortMap shares its value
			// pointers, both deliberately, so a copy of either legitimately
			// reaches the same child nodes.  Descend only where Copy did.
			if a.Type == lisp.LArray || a.Type == lisp.LSortMap {
				continue
			}
			if len(a.Cells) != len(b.Cells) {
				t.Fatalf("LVal.Copy changed a node's arity: %d cells became %d"+
					"\n--- source (%d bytes) ---\n%q",
					len(a.Cells), len(b.Cells), len(src), src)
				return
			}
			for i := range a.Cells {
				stack = append(stack, [2]*lisp.LVal{a.Cells[i], b.Cells[i]})
			}
		}
	}
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
	// The other way a consumer gets a private tree: LVal.Copy, which is what
	// lisp.TextLoader hands every evaluation.  Same ownership question as the
	// rest of this target, one step earlier -- a copy that shares position
	// OBJECTS with the tree it came from is not private, and the retained
	// cache then reports whatever the last writer through any copy left
	// behind (elps#446).  Asserted before evaluation so the walk sees the
	// reader's output and nothing else.  Costs one Copy and one paired walk;
	// no extra evaluation.
	copyOwnsItsPositions(t, shared, src)

	done := make(chan struct{})
	var want treeEval
	var wantErr error
	var got [sharedRuns]treeEval
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
		if len(got[i].rendered) != len(want.rendered) {
			t.Fatalf("shared tree produced %d results, private tree produced %d"+
				"\n--- source (%d bytes) ---\n%q",
				len(got[i].rendered), len(want.rendered), len(src), src)
			return
		}
		cmp := compareTreeEvals(want, got[i])
		if cmp.inconclusive != "" {
			// Not a pass and not a failure: a wall-clock deadline fired, so
			// this ARM carries no verdict from that expression on.  Logged
			// rather than silently dropped, in the style of copyWalkCap's
			// truncated walk, so an input that goes quiet says why.  The
			// remaining arms are still compared: an arm whose own context
			// stayed live can still produce a conclusive finding.
			t.Logf("shared run %d inconclusive: %s", i, cmp.inconclusive)
			continue
		}
		if cmp.divergedAt >= 0 {
			j := cmp.divergedAt
			t.Fatalf("evaluating a SHARED parse tree changed the program's meaning"+
				"\n  expression %d, shared run %d"+
				"\n  private tree: %s"+
				"\n  shared tree:  %s"+
				"\n--- source (%d bytes) ---\n%q",
				j, i, want.rendered[j], got[i].rendered[j], len(src), src)
			return
		}
	}
}

// FuzzSharedTreeEval asserts that evaluating a parse tree does not change what
// that tree means to the next consumer of it.
func FuzzSharedTreeEval(f *testing.F) {
	for _, src := range fuzzseed.EvalTerminating() {
		f.Add([]byte(src))
	}
	for _, src := range fuzzseed.EvalErroring() {
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
