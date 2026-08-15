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

	// fuzzDeadline is the context deadline FUZZED input runs under.  It is
	// the only limit that bounds wall-clock time, and it is checked at each
	// evaluation step.
	//
	// It is deliberately NOT applied to the fixed corpora.  A wall-clock
	// deadline is a THROUGHPUT bound: it keeps one pathological mutation from
	// eating a whole fuzz job.  FuzzEval asserts nothing about whether an
	// input succeeds, so the deadline firing there is an acceptable answer and
	// costs only that input.  The corpus tests are different in kind --
	// TestEvalTerminatingSeedsComplete asserts that a fixed, known-terminating
	// program COMPLETES -- and enforcing a correctness property with a clock
	// makes the assertion's outcome a function of how much CPU the process was
	// given rather than of the program under test.  That is issue #435: on a
	// loaded machine `tail-recursion-bounded` (35ms of CPU, 130k of 2,000,000
	// budgeted steps) failed this assertion, stopping at a DIFFERENT step count
	// on every run, and reported it as "either the program is wrong or a budget
	// has been tuned into the range of correct programs" -- neither of which
	// was true.
	//
	// See evalCorpusBudgeted for what the corpora run under instead.
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
// WHAT THIS COVERS, and why it rides the existing harness rather than being a
// target of its own: it costs one tree walk and no extra evaluation, so
// folding it in buys the property on every input FuzzEval and the fixed
// corpora already run, for no additional shard-minutes.  (When it was written
// scripts/fuzz-budget-check.sh reported zero headroom, which forced the
// choice; elps#457's eighth shard has since restored some, and the reasoning
// above is why it stayed folded in anyway.)
//
// It sits in evalUnderBudget, so it covers BOTH sides of elps#435's split --
// fuzzed input under the wall clock and the fixed corpora without one.
//
// NOT covered by FuzzSharedTreeEval (elps#457), which is the other
// tree-ownership target.  That one asserts AGREEMENT -- the rendered results
// of evaluating a shared tree match a private tree's -- plus whatever `-race`
// sees when two goroutines evaluate one tree.  A Source write is invisible to
// both: positions are not rendered, so agreement still holds, and a
// single-owner write into a borrowed Location is not a data race at all, just
// silent corruption.  The two properties are independent.
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

// evalBudgeted parses and evaluates FUZZED src under the full budget,
// including the wall-clock fuzzDeadline.  See evalCorpusBudgeted for the
// fixed corpora.
func evalBudgeted(t fatalf, src []byte) (evalOutcome, bool) {
	t.Helper()
	return evalUnderBudget(t, src, fuzzDeadline)
}

// evalCorpusBudgeted parses and evaluates a FIXED corpus program under the
// deterministic budgets only -- no wall clock (issue #435).
//
// What still bounds it, and why that is enough:
//
//   - fuzzMaxSteps and friends.  Every corpus program is bounded by one of
//     them, and their units are counts, not seconds, so a machine under load
//     reaches exactly the same verdict as an idle one.
//     TestEvalRunawaySeedsAreStopped runs through here, so it now also pins
//     the property this removal depends on: every runaway seed is stopped by
//     a DETERMINISTIC budget.  If a seed ever needed the clock, that test
//     hangs into the watchdog and says so.
//   - the watchdog.  It is the bound on a builtin that loops inside Go, which
//     is step-blind and context-blind, so it was never the deadline's job in
//     the first place -- see "Why there is also a watchdog" above.
//
// Why not simply denominate the deadline in SCHEDULED time, the way
// watchdogTimeout is: fuzzwatch measures scheduler STALL -- wall clock during
// which the process was not run at all -- which is the right instrument for a
// 30s bound on sub-millisecond work.  It is not an instrument for CPU SHARE.
// Measured on the 4-core sandbox that reproduces #435, with 200 competing
// spinners, evaluation slowed by roughly 50x while fuzzwatch recorded ZERO
// lost time: a heartbeat goroutine that sleeps 100ms and does nothing still
// wakes promptly when the run queue is long, so its ticks stay inside the
// tolerance.  A 2s scheduled-time deadline would have failed here exactly as
// the wall-clock one did.  The fix has to be a budget with no clock in it.
func evalCorpusBudgeted(t fatalf, src []byte) (evalOutcome, bool) {
	t.Helper()
	return evalUnderBudget(t, src, 0)
}

// evalUnderBudget parses and evaluates src under the budget, on its own
// goroutine, with the watchdog running.  A deadline of 0 means no wall-clock
// deadline at all.  It returns (outcome, true) when the source evaluated, and
// (zero, false) when it did not parse -- a parse error is the parser targets'
// business, not this one's.
//
// The watchdog failure is Fatalf rather than a returned error because there is
// nothing a caller could usefully do with it: the evaluation goroutine is
// still running and cannot be stopped.  Leaking it is acceptable precisely
// because the run has already failed.
func evalUnderBudget(t fatalf, src []byte, deadline time.Duration) (evalOutcome, bool) {
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
	//
	// It sits in evalUnderBudget rather than on one side of #435's split, so
	// both the fuzzed path and the clock-free corpus path are covered.  That
	// is sound in the direction #435 cares about: the location check is a
	// CORRECTNESS assertion, and #447's rule is that a correctness assertion
	// must not be decided by a clock -- but a fired deadline can only cut the
	// evaluation short, which means FEWER writes and a weaker check, never a
	// spurious mismatch.  Machine load cannot turn a tree the evaluator left
	// alone into a reported failure.
	watch := &locationWatch{Reader: env.Runtime.Reader}
	env.Runtime.Reader = watch

	ctx, cancel := evalContext(deadline)
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
			// is the race the property is about.  Deliberately not done on the
			// watchdog branch below, where the goroutine is still live -- more
			// so now that the corpus path carries no deadline and reaches that
			// branch on budgets alone (#435).
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
				// if it were interruptible it would have honoured its step
				// budget long ago.  Leaking it is the price of reporting the
				// failure at all; the process is about to fail the test
				// regardless.
				t.Fatalf("evaluation did not terminate within %s of SCHEDULED time despite %s,"+
					" a %d-step budget and a %d-iteration tail budget (%s)"+
					"\n--- source (%d bytes) ---\n%q",
					budget.Total(), describeDeadline(deadline), int64(fuzzMaxSteps), fuzzMaxTailIterations,
					report, len(src), src)
				return evalOutcome{}, false
			}
		}
	}
}

// evalContext builds the context an evaluation runs under.  A deadline of 0
// yields a context that is cancellable but carries no deadline -- the shape
// the fixed corpora need, where the bound must not be a clock (#435).
func evalContext(deadline time.Duration) (context.Context, context.CancelFunc) {
	if deadline <= 0 {
		return context.WithCancel(context.Background())
	}
	return context.WithTimeout(context.Background(), deadline)
}

// describeDeadline renders the deadline for a failure message, so a watchdog
// firing under the corpus budget does not claim a context deadline that is not
// there.
func describeDeadline(deadline time.Duration) string {
	if deadline <= 0 {
		return "no context deadline (the fixed-corpus budget: counts, not clocks)"
	}
	return fmt.Sprintf("a %s context deadline", deadline)
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
//
// It runs under evalCorpusBudgeted, i.e. with NO wall-clock deadline, which
// makes it also the guard for what #435's fix depends on: every runaway seed
// must be stopped by a DETERMINISTIC budget.  Measured at the time of that
// change, each of the fourteen seeds is stopped by a step, tail-iteration,
// physical-height, eval-nesting, macro-depth or allocation limit, and none by
// the clock.  If a seed ever came to need the clock, it would reach the
// watchdog here and fail loudly rather than quietly change what this file
// tests.
func TestEvalRunawaySeedsAreStopped(t *testing.T) {
	t.Parallel()
	for name, src := range fuzzseed.EvalRunaway() {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			out, ok := evalCorpusBudgeted(t, []byte(src))
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
//
// The budgets it runs against are deterministic (evalCorpusBudgeted): the
// verdict for a given seed is a property of the seed and the budgets, not of
// how much CPU this process happened to be given.  Before #435 this ran under
// a 2s wall-clock deadline and could fail a correct seed on a loaded machine,
// which is the same defect in the opposite direction -- a check firing on
// innocent code.
func TestEvalTerminatingSeedsComplete(t *testing.T) {
	t.Parallel()
	for name, src := range fuzzseed.EvalTerminating() {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			out, ok := evalCorpusBudgeted(t, []byte(src))
			if !ok {
				t.Fatalf("terminating seed %q does not parse", name)
			}
			assertNoInternalPanic(t, []byte(src), out)
			if out.Result.Type == lisp.LError {
				t.Fatalf("terminating seed %q must complete without error, got: %v"+
					" (%d steps of %d budgeted, %s) -- the program is wrong or a"+
					" deterministic budget has been tuned into the range of correct"+
					" programs; this assertion no longer depends on wall clock, so"+
					" machine load is not a candidate explanation (#435)",
					name, out.Result, out.Steps, int64(fuzzMaxSteps), out.Elapsed)
			}
		})
	}
}

// TestTerminatingSeedVerdictIsNotAFunctionOfTheClock is the regression test
// for #435, in a form that does not need a loaded machine to run.
//
// The defect was that a correctness assertion -- this known-terminating
// program completes -- was enforced by a wall-clock deadline, so its outcome
// moved with how much CPU the process was given.  On the sandbox that found
// it, six concurrent runs of TestEvalTerminatingSeedsComplete under 200
// competing spinners failed six times, the same fixed seed stopping at eight
// different step counts across independent samples.  That is not reproducible
// in a unit test, so this flips the same variable directly: it starves the
// clock rather than the machine, and asserts the corpus path does not notice.
//
// The first half is what makes the second half mean anything.  If evaluation
// stopped honouring context deadlines, the "immune" assertion below would pass
// for the wrong reason, so the sensitivity is demonstrated rather than assumed.
func TestTerminatingSeedVerdictIsNotAFunctionOfTheClock(t *testing.T) {
	t.Parallel()

	const seed = "tail-recursion-bounded"
	src := []byte(fuzzseed.EvalTerminating()[seed])
	if len(src) == 0 {
		t.Fatalf("seed %q is gone from the terminating corpus; this test pins its verdict", seed)
	}

	// A wall clock that is already spent.  Under one, this seed reports an
	// error -- the same verdict #435 saw against a 2s clock on a machine that
	// was not running us, and for the same reason.
	starved, ok := evalUnderBudget(t, src, time.Nanosecond)
	if !ok {
		t.Fatalf("terminating seed %q does not parse", seed)
	}
	if starved.Result.Type != lisp.LError {
		t.Fatalf("a spent wall clock must stop evaluation of %q, got %v (%d steps)."+
			" If the context deadline no longer bounds evaluation, the assertion below"+
			" passes for the wrong reason and this test is not testing anything",
			seed, starved.Result, starved.Steps)
	}

	// The corpus path has no clock in it, so there is nothing to starve and
	// the seed reaches its real verdict.
	out, ok := evalCorpusBudgeted(t, src)
	if !ok {
		t.Fatalf("terminating seed %q does not parse", seed)
	}
	if out.Result.Type == lisp.LError {
		t.Fatalf("the corpus budget must not depend on wall clock, but %q returned %v"+
			" (%d steps of %d budgeted)", seed, out.Result, out.Steps, int64(fuzzMaxSteps))
	}
}

// TestEvalCorpusHasNoWallClockDeadline pins the shape of #435's fix: the fixed
// corpora are handed a context with no deadline, and fuzzed input is still
// handed one.
//
// It is deliberately narrow.  The behavioural assertion lives in
// TestTerminatingSeedVerdictIsNotAFunctionOfTheClock; this one exists so that
// re-introducing a clock on the corpus path fails immediately and by name,
// rather than years later on somebody's loaded laptop.
func TestEvalCorpusHasNoWallClockDeadline(t *testing.T) {
	t.Parallel()

	corpus, cancel := evalContext(0)
	defer cancel()
	if d, ok := corpus.Deadline(); ok {
		t.Fatalf("the fixed-corpus budget must carry no wall-clock deadline, got one at %s."+
			" A correctness assertion enforced by a clock measures machine load (#435)", d)
	}

	fuzzed, cancel := evalContext(fuzzDeadline)
	defer cancel()
	if _, ok := fuzzed.Deadline(); !ok {
		t.Fatal("fuzzed input must still run under fuzzDeadline: it is the throughput bound" +
			" that stops one pathological mutation from eating a whole fuzz job, and nothing" +
			" in FuzzEval asserts that an input succeeds, so it cannot fail innocent code")
	}
}

// TestEvalAdversarialSeedsSurvive runs the unclassified adversarial corpus.
// These carry no expected outcome -- an error is a fine answer for most of
// them -- so the only assertions are the universal ones: termination, no
// recovered Go panic, no nil result.
//
// This one KEEPS the wall-clock deadline (evalBudgeted).  Nothing here asserts
// that a seed succeeds, so a fired deadline is an acceptable answer and cannot
// turn machine load into a failure; and the deadline bounds what this test
// costs.  That is the same reasoning that leaves it in place for FuzzEval.
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
	out, ok := evalCorpusBudgeted(t, []byte(forged))
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

// stepHeadroomCeiling is the largest fraction of fuzzMaxSteps any terminating
// seed may consume.  Measured worst case is `tail-recursion-bounded` at
// 130,019 steps, 6.5% of the 2,000,000 budgeted; 50% leaves an 8x margin over
// that and still fails long before a seed can reach the budget.
const stepHeadroomCeiling = 0.50

// TestEvalBudgetHeadroom asserts that the terminating corpus stays clear of
// the step budget, and reports by how much.
//
// It is the assertion that "a budget has been tuned into the range of correct
// programs" deserves, in the unit that claim is actually about.  Steps are a
// count: this test reaches the same verdict on a loaded machine and an idle
// one, which is what distinguishes it from the wall-clock deadline the
// terminating-seed assertion used to run into (#435).  Run with -v to see the
// numbers when retuning a limit.
func TestEvalBudgetHeadroom(t *testing.T) {
	t.Parallel()
	var worstSteps int64
	var worstName string
	var worstElapsed time.Duration
	for name, src := range fuzzseed.EvalTerminating() {
		out, ok := evalCorpusBudgeted(t, []byte(src))
		if !ok {
			continue
		}
		if out.Steps > worstSteps {
			worstSteps, worstName, worstElapsed = out.Steps, name, out.Elapsed
		}
	}
	used := float64(worstSteps) / float64(fuzzMaxSteps)
	t.Logf("most expensive terminating seed: %s -- %d steps of %d budgeted (%.1f%%), %s elapsed",
		worstName, worstSteps, int64(fuzzMaxSteps), 100*used, worstElapsed)
	if used > stepHeadroomCeiling {
		t.Fatalf("terminating seed %q used %d of %d budgeted steps (%.1f%%), over the %.0f%% ceiling:"+
			" the corpus and the budget have converged, so a correct program is close to being"+
			" stopped by a limit.  Raise fuzzMaxSteps or shrink the seed -- do not raise the ceiling",
			worstName, worstSteps, int64(fuzzMaxSteps), 100*used, 100*stepHeadroomCeiling)
	}
}
