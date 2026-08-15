// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
)

// Fuzzing the EVALUATOR, not the parser.
//
// In substrate a phylum is customer-supplied ELPS executed as Fabric
// chaincode.  The parser targets cover the path from bytes to an AST; this one
// covers what happens when that AST is run.  A panic there is a chaincode
// process crash and a non-terminating evaluation is a wedged transaction, so
// both are the properties worth asserting.
//
// # Why this needs a budgeted harness
//
// `go test -fuzz` has no per-input deadline.  A parser target does not need
// one -- parsing n bytes is bounded by n -- but evaluation is Turing-complete,
// so a target that just called Eval would hang on the first `(defun f () (f))`
// the mutator produced and stay hung until the whole job timed out.  Every
// evaluation here therefore runs under the interpreter's own limits, all of
// which are documented in lisp/runtime.go:
//
//   - WithMaxSteps            bounds evaluation steps.  The ONLY limit that
//                             catches a loop which neither recurses nor
//                             tail-calls (`(dotimes (i 1000000000))`).
//   - WithMaxTailIterations   bounds turns of a tail-recursive loop.  Tail
//                             calls run in constant stack space, so no stack
//                             limit can see them.
//   - WithMaximumPhysicalStackHeight
//                             bounds frames actually on the stack.  This is
//                             the guard against unbounded NON-tail recursion
//                             exhausting the Go goroutine stack, which aborts
//                             the process in a way recover() cannot intercept.
//   - WithMaxAlloc            bounds a single builtin's output size.
//   - a context deadline      the only limit that measures TIME.  A single
//                             step may do arbitrary work inside a builtin, so
//                             none of the counters above bound wall clock.
//
// Note what is deliberately NOT set: MaxHeightLogical.  Its unit is elided
// tail frames, a running total that depends on the SHAPE of a loop body rather
// than on nesting depth or iteration count, so a limit on it fires
// data-dependently on correct constant-space loops.  MaxTailIterations bounds
// the same runaway shape in honest units.
//
// # Why there is also a watchdog
//
// The limits above are checked by the interpreter, so they can only stop code
// that reaches a check.  A builtin that loops inside Go -- exactly defect A1
// below -- is step-blind and context-blind, and no configured budget will
// interrupt it.  The watchdog turns "this terminates" from an assumption into
// an assertion: each evaluation runs on its own goroutine and the test fails
// if it has not returned within watchdogTimeout.
//
// # The load-bearing assertion
//
// env.eval installs a recover() that converts ANY Go panic into an ordinary
// *LVal error.  That is correct for an embedded interpreter and fatal for a
// naive fuzz target: the process survives every defect, so "it did not crash"
// proves nothing whatsoever.  The assertion that actually has teeth is
//
//	lisp.IsInternalPanic(result) == false
//
// which keys off the Go-stack snapshot the recover handler attaches to the
// error, not off the condition name.  Without it, A1 through A3 all come back
// as perfectly ordinary-looking values and the target passes.  See
// TestInternalPanicMarkerIsNotForgeable for the non-forgeability check this
// depends on.

const (
	// Evaluation budget.  Sized to be generous enough that every program in
	// fuzzseed.EvalTerminating completes (asserted by
	// TestEvalTerminatingSeedsComplete -- a budget that stops a correct
	// program is a defect, not a pass) and tight enough that a runaway
	// program is stopped in milliseconds rather than seconds.
	//
	// Measured headroom for the terminating corpus is reported by
	// TestEvalBudgetHeadroom.
	fuzzMaxSteps          = 2_000_000
	fuzzMaxTailIterations = 100_000
	fuzzMaxPhysicalHeight = 2_000
	fuzzMaxAlloc          = 1_000_000
	fuzzMacroDepth        = 100

	// fuzzMaxEvalNesting bounds the evaluator's recursion into itself, which
	// is a different quantity from physical height: a call's arguments are
	// evaluated before its frame is pushed, so nested arguments recurse
	// through the evaluator at height zero and fuzzMaxPhysicalHeight cannot
	// see them (issue #316).  Sized against the physical budget by the
	// measured ~1.5 eval levels per physical frame, with headroom, so the
	// physical bound stays the one that fires on ordinary recursion.
	//
	// Before this existed, a macro that generates nesting at expansion time
	// was stopped here only by the 2s context deadline -- i.e. by wall clock,
	// after hundreds of thousands of Go frames had already been pushed.
	fuzzMaxEvalNesting = 20_000

	// fuzzDeadline is the context deadline every evaluation runs under.  It
	// is the only limit that bounds wall-clock time, and it is checked at
	// each evaluation step.
	fuzzDeadline = 2 * time.Second

	// watchdogTimeout is the outer bound, denominated in SCHEDULED time (see
	// internal/fuzzwatch): wall clock during which this process was not run by the
	// OS is not charged to the evaluator.  It is deliberately an order of
	// magnitude above fuzzDeadline: reaching it means evaluation ignored
	// every budget it was given, which is itself the bug.
	watchdogTimeout = 30 * time.Second
)

// newFuzzEnv builds a fresh, fully-loaded environment under the evaluation
// budget, with debug output captured rather than written to the process's
// streams.
//
// A FRESH environment per input is not negotiable.  Evaluation mutates global
// state -- defun, set, in-package, deftype -- so a shared environment would
// make a crasher depend on every input that ran before it, and a crasher that
// does not reproduce from its own testdata file is worthless.
//
// Runtime.Library is left nil on purpose: that is what makes `load-file`
// return an error instead of reading the filesystem (see LEnv.LoadFile), so
// fuzzer-generated source cannot escape into the test machine's disk.
func newFuzzEnv() (*lisp.LEnv, *bytes.Buffer, *lisp.LVal) {
	stderr := &bytes.Buffer{}
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(stderr),
		lisp.WithMaxSteps(fuzzMaxSteps),
		lisp.WithMaxTailIterations(fuzzMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(fuzzMaxPhysicalHeight),
		lisp.WithMaxEvalNesting(fuzzMaxEvalNesting),
		lisp.WithMaxAlloc(fuzzMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(fuzzMacroDepth),
	)
	if rc.Type == lisp.LError {
		return nil, stderr, rc
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, stderr, rc
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, stderr, rc
	}
	return env, stderr, nil
}

// evalOutcome is the record of one budgeted evaluation.
type evalOutcome struct {
	Result  *lisp.LVal
	Stderr  string
	Steps   int64
	Elapsed time.Duration
}

// locationWatch records, for every node the loader's reader produces, the
// *token.Location that node held and the position that Location described --
// so evaluation can be asked afterwards whether it moved either.
//
// WHAT THIS COVERS, and why it lives in FuzzEval rather than in a target of its
// own (the fuzz sweep's budget has no spare shard-minutes: see
// scripts/fuzz-budget-check.sh).
//
// A parse tree is not private to one evaluation.  LEnv.load evaluates the
// reader's expressions directly without copying; a function body IS the parse
// tree it was defined from, re-entered on every call; and a *Package -- LFun
// bodies included -- is shared by pointer across the per-request environments
// an embedder derives from one registry.  So a write into a parsed node's
// position is not a local mistake, it is retroactive corruption of every error
// message, stack frame and LSP range computed from that node, in every
// evaluation that ever uses it.
//
// The evaluator has exactly one writer of LVal.Source: stampMacroExpansion.
// It is supposed to claim only nodes the MACRO created.  It reached parser
// output twice before -- elps#370, where the reader emitted synthetic locations
// the stamp then rewrote, and elps#431, where the stamp wrote the caller's own
// *token.Location onto every node of the expansion, leaving two trees on one
// mutable object.  Both are fixed; this states the property they broke, over
// arbitrary programs, rather than over the two shapes their regression tests
// pin.
//
// GUARD, NOT A CATCH: this passes on the parent commit.  elps#431 is an
// ownership defect, and its corruption is latent -- no in-tree writer can
// currently reach a Location it does not own.  What this adds is that a future
// one cannot arrive unnoticed.
type locationWatch struct {
	lisp.Reader
	nodes     []*lisp.LVal
	locs      []*token.Location
	positions []token.Location
	truncated bool
}

// locationWatchLimit bounds what one input may make the watch retain.  A
// program is free to call `load-string` in a loop, and an unbounded watch would
// turn that into memory pressure on the fuzz process.  Past the limit the watch
// stops recording and says so, rather than recording a prefix and reporting as
// if it had seen everything.
const locationWatchLimit = 200_000

// Read implements lisp.Reader, recording the tree on the way past.  Every
// exported Load* entry point funnels through Runtime.Reader, so this sees the
// program itself and anything it loads at runtime.
func (w *locationWatch) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	exprs, err := w.Reader.Read(name, r)
	for _, e := range exprs {
		w.record(e, make(map[*lisp.LVal]bool))
	}
	return exprs, err
}

func (w *locationWatch) record(v *lisp.LVal, seen map[*lisp.LVal]bool) {
	// Bounded like every other walk over a value the fuzzer chose: a macro may
	// build a structure that contains itself (elps#390), and the reader is not
	// the only producer feeding this.
	if v == nil || seen[v] {
		return
	}
	if len(w.nodes) >= locationWatchLimit {
		w.truncated = true
		return
	}
	seen[v] = true
	if v.Source != nil {
		w.nodes = append(w.nodes, v)
		w.locs = append(w.locs, v.Source)
		w.positions = append(w.positions, *v.Source)
	}
	for _, c := range v.Cells {
		w.record(c, seen)
	}
}

// verify reports the first node whose position evaluation moved.
func (w *locationWatch) verify(t fatalf, src []byte) {
	t.Helper()
	// A truncated watch still reports honestly on what it did record; it says
	// so, so a failure is never read as "and nothing else moved".
	scope := fmt.Sprintf("%d nodes watched", len(w.nodes))
	if w.truncated {
		scope = fmt.Sprintf("first %d nodes only; the watch was truncated at its limit", len(w.nodes))
	}
	for i, node := range w.nodes {
		switch {
		case node.Source == nil:
			t.Fatalf("evaluation cleared the source location of a parsed %v %q (was %v) [%s]"+
				"\n--- source (%d bytes) ---\n%q",
				node.Type, node.Str, w.positions[i], scope, len(src), src)
			return
		case node.Source != w.locs[i]:
			t.Fatalf("evaluation re-pointed the Source of a parsed %v %q from %v to %v;"+
				" the stamp must claim only nodes the macro created (#370, #431) [%s]"+
				"\n--- source (%d bytes) ---\n%q",
				node.Type, node.Str, w.positions[i], node.Source, scope, len(src), src)
			return
		case *node.Source != w.positions[i]:
			t.Fatalf("evaluation moved the recorded position of a parsed %v %q from %+v to %+v;"+
				" a write through a *token.Location the evaluator does not own (#431) [%s]"+
				"\n--- source (%d bytes) ---\n%q",
				node.Type, node.Str, w.positions[i], *node.Source, scope, len(src), src)
			return
		}
	}
}

// fatalf is the subset of *testing.T and *testing.F the harness needs, so the
// same code serves the fuzz target and the ordinary corpus tests.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...interface{})
	// Skipf is how the harness declines to answer for one input when the
	// process was starved throughout its watchdog window -- see
	// internal/fuzzwatch. Both *testing.T and *testing.F have it.
	Skipf(format string, args ...interface{})
}

// evalBudgeted parses and evaluates src under the budget, on its own
// goroutine, with the watchdog running.  It returns (outcome, true) when the
// source evaluated, and (zero, false) when it did not parse -- a parse error
// is the parser targets' business, not this one's.
//
// The watchdog failure is Fatalf rather than a returned error because there is
// nothing a caller could usefully do with it: the evaluation goroutine is
// still running and cannot be stopped.  Leaking it is acceptable precisely
// because the run has already failed.
func evalBudgeted(t fatalf, src []byte) (evalOutcome, bool) {
	t.Helper()

	// Decide "did this parse?" with the reader itself rather than by pattern-
	// matching the returned error's text.  Both a parse failure and an
	// evaluation failure come back from Load* as an LError, so a message
	// heuristic is the only alternative -- and it is unsound in the direction
	// that matters: a program that evaluates `(load-string "(")` produces an
	// EVALUATION result whose message contains the reader's wording, and a
	// heuristic would silently discard it.  Discarding inputs is how a fuzz
	// target goes quiet.
	//
	// Running it FIRST also means unparsable input -- most of what the mutator
	// produces -- skips the environment construction below, which dominates
	// the per-input cost.
	if _, err := parser.NewReader().Read("fuzz", bytes.NewReader(src)); err != nil {
		return evalOutcome{}, false
	}

	env, stderr, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
		return evalOutcome{}, false
	}

	// Watch the positions of every node the loader is about to evaluate.
	// Installed AFTER newFuzzEnv so the stdlib's own parse trees -- tens of
	// thousands of nodes, evaluated once during setup and never again -- are
	// not recorded per input.  The wrapper adds one walk of the program's tree
	// and no extra evaluation.
	watch := &locationWatch{Reader: env.Runtime.Reader}
	env.Runtime.Reader = watch

	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()

	type done struct {
		result  *lisp.LVal
		steps   int64
		elapsed time.Duration
	}
	ch := make(chan done, 1)
	go func() {
		start := time.Now()
		result := env.LoadStringContext(ctx, "fuzz", string(src))
		ch <- done{result: result, steps: env.Runtime.TotalSteps(), elapsed: time.Since(start)}
	}()

	// SCHEDULED time, not wall clock: see internal/fuzzwatch.  At a measured
	// 0.33ms mean per input this bound is already ~90,000x the work, so
	// widening it further would not buy anything -- the only way a healthy
	// machine reaches it is a genuine hang, and the only other way to reach it
	// is not being given the CPU, which is not the evaluator's fault.
	budget := fuzzwatch.New(watchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case d := <-ch:
			if d.result == nil {
				t.Fatalf("evaluation returned a nil LVal")
				return evalOutcome{}, false
			}
			// Only once the evaluation goroutine has finished: verify walks
			// the same nodes it was writing to, and reading them while it runs
			// is the race the property is about.
			watch.verify(t, src)
			return evalOutcome{
				Result:  d.result,
				Stderr:  stderr.String(),
				Steps:   d.steps,
				Elapsed: d.elapsed,
			}, true
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				// Starved throughout. Nothing can be said about this input,
				// and saying it anyway is how a gate stops meaning anything.
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return evalOutcome{}, false
			default:
				// The evaluation goroutine is unstoppable by construction --
				// if it were interruptible it would have honoured the context
				// deadline fuzzDeadline seconds ago.  Leaking it is the price
				// of reporting the failure at all; the process is about to
				// fail the test regardless.
				t.Fatalf("evaluation did not terminate within %s of SCHEDULED time despite a %s context deadline,"+
					" a %d-step budget and a %d-iteration tail budget (%s)"+
					"\n--- source (%d bytes) ---\n%q",
					budget.Total(), fuzzDeadline, int64(fuzzMaxSteps), fuzzMaxTailIterations,
					report, len(src), src)
				return evalOutcome{}, false
			}
		}
	}
}

// assertNoInternalPanic is the assertion the whole target exists to make.
func assertNoInternalPanic(t fatalf, src []byte, out evalOutcome) {
	t.Helper()
	if lisp.IsInternalPanic(out.Result) {
		t.Fatalf("evaluation recovered a Go panic (a host-code defect, not a lisp error)"+
			"\n--- error ---\n%v\n--- source (%d bytes) ---\n%q",
			out.Result, len(src), src)
	}
}

// FuzzEval fuzzes evaluation of arbitrary source text.
//
// The asserted properties, and only these:
//
//  1. Evaluation terminates (the watchdog).
//  2. It never recovers a Go panic (IsInternalPanic).
//  3. It never returns a nil LVal.
//  4. The result is printable -- every error path in the interpreter renders
//     values, so an unprintable result is as fatal as an unevaluable one.
//
// NOT asserted: that any given input evaluates successfully.  Almost all
// mutator output is nonsense and an LError is the correct answer for it.
func FuzzEval(f *testing.F) {
	for _, src := range fuzzseed.EvalAdversarial() {
		f.Add([]byte(src))
	}
	for _, src := range fuzzseed.EvalTerminating() {
		f.Add([]byte(src))
	}
	for _, src := range fuzzseed.EvalRunaway() {
		f.Add([]byte(src))
	}
	// A handful of the parser corpus's small adversarial inputs, so the
	// mutator also starts from shapes that stress the reader/evaluator
	// boundary.  fuzzseed.All() is NOT used: it carries the repository's real
	// .lisp sources, and an eval seed is executed, so seeding from the test
	// suite would throttle every generation descended from it.
	for _, src := range fuzzseed.Adversarial() {
		if len(src) <= 64 {
			f.Add(src)
		}
	}

	f.Fuzz(func(t *testing.T, src []byte) {
		out, ok := evalBudgeted(t, src)
		if !ok {
			return
		}
		assertNoInternalPanic(t, src, out)
		// String() walks the whole value; a result that cannot be rendered is
		// a defect in its own right.
		_ = out.Result.String()
	})
}

// TestEvalRunawaySeedsAreStopped pins that every program in the runaway corpus
// is stopped by a budget rather than running to completion.  A runaway seed
// that starts returning a value means a limit stopped working -- which is how
// a fuzz target silently degrades into one that only ever runs terminating
// programs.
func TestEvalRunawaySeedsAreStopped(t *testing.T) {
	t.Parallel()
	for name, src := range fuzzseed.EvalRunaway() {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			out, ok := evalBudgeted(t, []byte(src))
			if !ok {
				t.Fatalf("runaway seed %q does not parse; it can never have tested a budget", name)
			}
			assertNoInternalPanic(t, []byte(src), out)
			if out.Result.Type != lisp.LError {
				t.Fatalf("runaway seed %q ran to completion and returned %v"+
					" (%d steps, %s); a budget is not holding",
					name, out.Result, out.Steps, out.Elapsed)
			}
		})
	}
}

// TestEvalTerminatingSeedsComplete is the false-positive guard, and it is the
// half of the split that a previous version of this corpus got wrong.
//
// Both halves are asserted here: the program must FINISH (no watchdog) and it
// must finish WITHOUT an error.  Asserting only termination is what let
// `deep-nested-progn` sit in the terminating corpus while actually returning
// "physical stack height exceeded maximum" -- the split was enforced in one
// direction only, so a budget firing on a correct program was invisible.
func TestEvalTerminatingSeedsComplete(t *testing.T) {
	t.Parallel()
	for name, src := range fuzzseed.EvalTerminating() {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			out, ok := evalBudgeted(t, []byte(src))
			if !ok {
				t.Fatalf("terminating seed %q does not parse", name)
			}
			assertNoInternalPanic(t, []byte(src), out)
			if out.Result.Type == lisp.LError {
				t.Fatalf("terminating seed %q must complete without error, got: %v"+
					" (%d steps, %s) -- either the program is wrong or a budget"+
					" has been tuned into the range of correct programs",
					name, out.Result, out.Steps, out.Elapsed)
			}
		})
	}
}

// TestEvalAdversarialSeedsSurvive runs the unclassified adversarial corpus.
// These carry no expected outcome -- an error is a fine answer for most of
// them -- so the only assertions are the universal ones: termination, no
// recovered Go panic, no nil result.
func TestEvalAdversarialSeedsSurvive(t *testing.T) {
	t.Parallel()
	for _, src := range fuzzseed.EvalAdversarial() {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			out, ok := evalBudgeted(t, []byte(src))
			if !ok {
				return
			}
			assertNoInternalPanic(t, []byte(src), out)
			_ = out.Result.String()
		})
	}
}

// TestInternalPanicMarkerIsNotForgeable pins the property the entire target
// rests on.
//
// Everything FuzzEval reports as a defect is reported because
// IsInternalPanic returned true.  If lisp code could produce a value
// satisfying that predicate, the target would be a noise generator; if a real
// recovered panic did NOT satisfy it, the target would be blind.  Both
// directions are checked here, next to the harness that depends on them,
// rather than only in the lisp package's own tests.
//
// A NAMED handler-bind on internal-panic does intercept a real panic -- that
// is deliberate, so an embedder can implement a recovery policy -- which means
// fuzzer-generated source containing such a handler can legitimately mask a
// defect from this target.  That is a known and accepted gap: the alternative
// is an error no program can handle.
func TestInternalPanicMarkerIsNotForgeable(t *testing.T) {
	t.Parallel()

	forged := `(error 'internal-panic "forged")`
	out, ok := evalBudgeted(t, []byte(forged))
	if !ok {
		t.Fatal("the forged internal-panic program must parse")
	}
	if out.Result.Type != lisp.LError {
		t.Fatalf("expected an error from %s, got %v", forged, out.Result)
	}
	if lisp.IsInternalPanic(out.Result) {
		t.Fatalf("a lisp-forged internal-panic satisfied IsInternalPanic;"+
			" every assertion in this file is now forgeable from fuzzer input: %v",
			out.Result)
	}

	// And the positive direction: a genuine host panic must be detected.  The
	// panic is raised from a builtin installed for this test only, which is
	// the same mechanism a real host-code defect takes.
	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
	}
	env.AddBuiltins(true, elpsutil.Function("fuzz-panic", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			panic("genuine host defect")
		}))
	genuine := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("fuzz-panic")}))
	if !lisp.IsInternalPanic(genuine) {
		t.Fatalf("a genuine recovered panic did not satisfy IsInternalPanic;"+
			" FuzzEval would be blind to every defect it exists to find: %v", genuine)
	}
}

// TestEvalBudgetHeadroom reports what the terminating corpus actually costs
// against the configured budget.  It is a measurement, not a threshold: the
// pass/fail assertion lives in TestEvalTerminatingSeedsComplete.  Run with
// -v to see the numbers when retuning a limit.
func TestEvalBudgetHeadroom(t *testing.T) {
	var worstSteps int64
	var worstName string
	var worstElapsed time.Duration
	for name, src := range fuzzseed.EvalTerminating() {
		out, ok := evalBudgeted(t, []byte(src))
		if !ok {
			continue
		}
		if out.Steps > worstSteps {
			worstSteps, worstName, worstElapsed = out.Steps, name, out.Elapsed
		}
	}
	t.Logf("most expensive terminating seed: %s -- %d steps of %d budgeted (%.1f%%), %s of %s deadline",
		worstName, worstSteps, int64(fuzzMaxSteps),
		100*float64(worstSteps)/float64(fuzzMaxSteps), worstElapsed, fuzzDeadline)
}
