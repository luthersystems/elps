// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FuzzSharedTreeEval's comparison oracle, tested on its own.
//
// The oracle is the one part of that target that can fail SILENTLY.  A
// missed write into a shared tree shows up as a test that passes, so any
// licence the oracle gives itself to ignore a result is a class of corruption
// it has promised not to report.  This file is where that licence is
// measured.
//
// The oracle has exactly one thing it is allowed to consult besides the
// rendered text, and it is not the value: whether an arm's own context had
// expired.  Everything below is built from results the evaluator really
// produced, under contexts the harness really put in that state -- never from
// a rendered string matched against a phrase, and never from a hand-written
// stand-in for an evaluator verdict.
//
// The two exceptions this target used to carry, and why neither survived:
//
//   - MESSAGE MATCHING.  A substring test for a budget's message reads a
//     program's own `"context deadline exceeded: …"` string, or an error it
//     raised with that text, as a backstop.  See
//     TestOracleComparesAValueNamingABudget.
//
//   - PROVENANCE.  Reading the unforgeable *lisp.TailIterationLimitError out
//     of the error's data cell fixed the message problem and left a bigger
//     one.  handler-bind catches every condition but internal-panic, so a
//     program can CATCH the tail-iteration error, keep the Go error it
//     carries, do arbitrary work that differs between the two arms, and raise
//     a fresh error carrying the captured cell.  Both arms then read as the
//     same budget and everything in between is hidden.  See
//     TestOracleReportsCapturedBudgetReplay, which is the case that
//     retired value-based classification altogether.
//
// The deterministic budgets need no exception at all, which is what
// TestOracleTailBudgetArmsCompareEqual shows: the same program, the same
// fresh environment and the same tail-iteration limit trip at the same turn
// in both arms and render the same text, so exact comparison already agrees.
// Only the wall-clock deadline is non-deterministic, and it is handled as
// INCONCLUSIVENESS rather than as a verdict about the value.
//
// # The second vacuity hazard: an oracle that is never reached
//
// Every case above drives compareTreeEvals directly, from treeEvals this file
// built with its own evalArm helper.  That is deliberate -- it is the only way
// to put an arm's context into a chosen state -- and it leaves a gap of
// exactly the shape this file was written to close.  The REAL harness path is
// sharedTreeProperty -> evalTreeOnce, under the real fuzzDeadline, the real
// newFuzzEnv and real shared arms; nothing here ran it.  So a regression that
// made every real run INCONCLUSIVE from expression 0 -- a mis-set deadline, a
// context created already cancelled, `expired` read from the wrong context or
// pinned to true, a `continue` that skips the comparison -- would leave every
// test in this file passing, FuzzSharedTreeEval finding nothing, and the
// target comparing nothing.  "No failure" is worth as little here as "no
// crash" is under a blanket recover().
//
// TestSharedTreeLiveResultIsConclusive is the direct control: it runs the real
// path on a small terminating program and asserts the verdict came back
// CONCLUSIVE with every expression compared.  Each of the three mutations
// above fails it.

// oracleDeadline is the deadline the live-context cases run under.  Long
// enough that nothing here reaches it, so an expired context in those cases
// would be a real finding rather than a slow machine.
const oracleDeadline = 30 * time.Second

// oracleRun is one arm: the treeEval the fuzz target's own arm would produce,
// plus the evaluator's values themselves, kept so the fixture checks can ask
// what a result really is rather than what it renders as.
type oracleRun struct {
	eval   treeEval
	values []*lisp.LVal
}

func (r oracleRun) last() *lisp.LVal { return r.values[len(r.values)-1] }

func (r oracleRun) lastRendered() string { return r.eval.rendered[len(r.eval.rendered)-1] }

// evalArm evaluates every expression of src under a fresh fuzz environment
// and a context with the given deadline, recording exactly what evalTreeOnce
// records.  cancelAfter, when non-negative, cancels the context once that
// many expressions have been evaluated -- the deterministic way to put an arm
// into the expired state PART WAY through a tree, which a deadline alone can
// only do by racing the evaluator.
func evalArm(t *testing.T, src string, deadline time.Duration, cancelAfter int) oracleRun {
	t.Helper()
	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("building the environment failed: %v", rc)
	}
	exprs, err := parser.NewReader().Read("oracle", strings.NewReader(src))
	if err != nil {
		t.Fatalf("reading %q failed: %v", src, err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), deadline)
	defer cancel()
	run := oracleRun{eval: treeEval{
		rendered: make([]string, 0, len(exprs)),
		expired:  make([]bool, 0, len(exprs)),
	}}
	for i, e := range exprs {
		v := env.EvalContext(ctx, e)
		if v == nil {
			t.Fatalf("evaluating %q returned a nil LVal", src)
		}
		run.values = append(run.values, v)
		run.eval.expired = append(run.eval.expired, ctx.Err() != nil)
		run.eval.rendered = append(run.eval.rendered, v.String())
		if cancelAfter >= 0 && i+1 == cancelAfter {
			cancel()
		}
	}
	return run
}

// liveArm is the common case: a generous deadline and no cancellation.
func liveArm(t *testing.T, src string) oracleRun {
	t.Helper()
	return evalArm(t, src, oracleDeadline, -1)
}

// The programs every case below is built from.  Each is real source the
// evaluator runs; none of them is a stand-in.
const (
	// runawayLoopSrc is the shape crasher 423b7dd9e421bd27 was reduced to: a
	// tail-recursive loop that only the tail-iteration budget stops.
	runawayLoopSrc = `(defun s () (let () (s))) (s)`

	// trivialSrc completes instantly under any live context.
	trivialSrc = `(+ 1 1)`
)

// capturedReplaySrc is the forgery that retired value-based classification.
//
// handler-bind catches the tail-iteration condition (opHandlerBind matches
// everything but internal-panic), the handler keeps the error's data cell --
// which holds the evaluator's own *lisp.TailIterationLimitError, the marker
// no lisp program can construct -- and the program then does work that
// DIFFERS between arms before raising a fresh error carrying that captured
// cell back out as its result.
//
// The payload goes FIRST because ErrorVal.errorMessage renders only
// Cells[0].Native when that cell holds a Go error; with the captured cell
// second, both the divergent work and the captured marker are in the value.
func capturedReplaySrc(payload string) string {
	return `(defun s () (s))
(set 'captured ())
(handler-bind ([condition (lambda (c &rest r) (set! captured (car r)) 'caught)]) (s))
(error 'replay "` + payload + `" captured)`
}

// raisedCancelledSrc is a program raising the RESERVED condition symbol the
// evaluator uses for a blown deadline, under a live context.  Nothing about
// the value distinguishes it from the real thing; the harness's own context
// state does.
func raisedCancelledSrc(payload string) string {
	return `(error '` + lisp.CondContextCancelled +
		` "context cancelled: context deadline exceeded: ` + payload + `")`
}

// valueNamingABudgetSrc is a SUCCESSFUL value whose text is a budget's.
func valueNamingABudgetSrc(payload string) string {
	return `(concat 'string "tail-call iteration limit exceeded: " "` + payload + `")`
}

// carriesTailLimitError reports whether v holds the evaluator's own
// *lisp.TailIterationLimitError in one of its data cells -- the marker the
// retired classifier keyed off.
func carriesTailLimitError(v *lisp.LVal) bool {
	if v == nil || v.Type != lisp.LError {
		return false
	}
	for _, c := range v.Cells {
		if c == nil {
			continue
		}
		err, ok := c.Native.(error)
		if !ok {
			continue
		}
		var tail *lisp.TailIterationLimitError
		if errors.As(err, &tail) {
			return true
		}
	}
	return false
}

// TestOracleFixturesAreWhatTheyClaim checks the fixtures before anything is
// concluded from them.  A fixture that quietly stopped being what it says --
// a retuned limit, a changed condition symbol, a handler-bind that no longer
// catches the tail-iteration condition -- would make every assertion below
// pass vacuously.
func TestOracleFixturesAreWhatTheyClaim(t *testing.T) {
	t.Run("the runaway loop is stopped by the tail budget", func(t *testing.T) {
		run := liveArm(t, runawayLoopSrc)
		if got := run.last(); got.Type != lisp.LError || !carriesTailLimitError(got) {
			t.Fatalf("the runaway loop did not raise a tail-iteration budget error: %v", got)
		}
		if !strings.Contains(run.lastRendered(), "tail-call iteration limit exceeded") {
			t.Fatalf("the runaway loop hit some other limit: %s", run.lastRendered())
		}
		for j, expired := range run.eval.expired {
			if expired {
				t.Fatalf("expression %d ran with an expired context on a %v deadline", j, oracleDeadline)
			}
		}
	})

	t.Run("the captured replay really carries the budget marker", func(t *testing.T) {
		run := liveArm(t, capturedReplaySrc("original"))
		last := run.last()
		if last.Type != lisp.LError {
			t.Fatalf("the replay fixture is not an error: %v", last)
		}
		// The whole hazard: an error the PROGRAM raised, carrying the
		// evaluator's own unforgeable budget marker.
		if !carriesTailLimitError(last) {
			t.Fatalf("the replay fixture no longer carries the tail-iteration"+
				" marker, so it is not the forgery this file is about: %s",
				run.lastRendered())
		}
		if last.Str != "replay" {
			t.Fatalf("the replay fixture raised %q, not its own condition", last.Str)
		}
		if !strings.Contains(run.lastRendered(), "original") {
			t.Fatalf("the replay fixture lost its divergent payload: %s", run.lastRendered())
		}
		for j, expired := range run.eval.expired {
			if expired {
				t.Fatalf("expression %d ran with an expired context on a %v deadline", j, oracleDeadline)
			}
		}
	})

	t.Run("a deadline already in the past expires every expression", func(t *testing.T) {
		run := evalArm(t, trivialSrc, time.Nanosecond, -1)
		if !run.eval.expired[0] {
			t.Fatal("the expired-deadline fixture ran with a live context")
		}
		if last := run.last(); last.Type != lisp.LError || last.Str != lisp.CondContextCancelled {
			t.Fatalf("the expired-deadline fixture produced %v (condition %q)",
				run.last(), run.last().Str)
		}
	})

	t.Run("cancelling after the first expression expires only the rest", func(t *testing.T) {
		run := evalArm(t, trivialSrc+" "+trivialSrc, oracleDeadline, 1)
		if len(run.eval.expired) != 2 {
			t.Fatalf("expected two expressions, got %d", len(run.eval.expired))
		}
		if run.eval.expired[0] {
			t.Fatal("the first expression ran with an already-cancelled context")
		}
		if !run.eval.expired[1] {
			t.Fatal("the second expression ran with a live context after cancellation")
		}
	})

	t.Run("a program can raise the reserved condition itself", func(t *testing.T) {
		run := liveArm(t, raisedCancelledSrc("original"))
		last := run.last()
		if last.Type != lisp.LError || last.Str != lisp.CondContextCancelled {
			t.Fatalf("the raised-condition fixture produced %v (condition %q)", last, last.Str)
		}
		if run.eval.expired[0] {
			t.Fatal("the raised-condition fixture ran with an expired context")
		}
	})

	t.Run("a successful value can name a budget", func(t *testing.T) {
		run := liveArm(t, valueNamingABudgetSrc("original"))
		if last := run.last(); last.Type != lisp.LString {
			t.Fatalf("the value fixture is not a string: %v", last)
		}
		if !strings.Contains(run.lastRendered(), "tail-call iteration limit exceeded") {
			t.Fatalf("the value fixture no longer names a budget: %s", run.lastRendered())
		}
	})
}

// assertDiverged is the oracle reporting a finding at the expected index.
func assertDiverged(t *testing.T, want, got treeEval, at int) {
	t.Helper()
	cmp := compareTreeEvals(want, got)
	if cmp.inconclusive != "" {
		t.Fatalf("the oracle declined to compare a live-context input: %s"+
			"\n  private: %q\n  shared:  %q",
			cmp.inconclusive, want.rendered, got.rendered)
	}
	if cmp.divergedAt != at {
		t.Fatalf("the oracle reported divergedAt=%d, want %d"+
			"\n  private: %q\n  shared:  %q",
			cmp.divergedAt, at, want.rendered, got.rendered)
	}
	// The count the live control reads: everything up to the divergence was
	// compared, and the diverging expression itself was not counted.
	if cmp.compared != at {
		t.Fatalf("the oracle compared %d expressions before diverging at %d",
			cmp.compared, at)
	}
}

// assertAgreed is the oracle finding nothing, with no exception invoked.
func assertAgreed(t *testing.T, want, got treeEval) {
	t.Helper()
	cmp := compareTreeEvals(want, got)
	if cmp.inconclusive != "" {
		t.Fatalf("the oracle declined to compare a live-context input: %s", cmp.inconclusive)
	}
	if cmp.divergedAt >= 0 {
		t.Fatalf("the oracle reported a divergence at expression %d"+
			"\n  private: %s\n  shared:  %s",
			cmp.divergedAt, want.rendered[cmp.divergedAt], got.rendered[cmp.divergedAt])
	}
	// Agreement over zero expressions is not agreement.
	if cmp.compared != len(want.rendered) {
		t.Fatalf("the oracle agreed after comparing %d of %d expressions",
			cmp.compared, len(want.rendered))
	}
}

// TestOracleReportsCapturedBudgetReplay is (a): the forgery that
// broke the provenance classifier, stated as the property that closes it.
//
// Both arms end in an error carrying the evaluator's own unforgeable
// tail-iteration marker, so the retired classifier called them the same
// outcome and collapsed them.  They are not the same outcome: the work the
// program did between catching the budget and replaying it DIFFERS, which is
// exactly the corruption FuzzSharedTreeEval exists to find.  Both contexts
// are live, so nothing here is inconclusive and the divergence must be
// reported.
func TestOracleReportsCapturedBudgetReplay(t *testing.T) {
	private := liveArm(t, capturedReplaySrc("private"))
	shared := liveArm(t, capturedReplaySrc("shared-corrupted"))

	// The premise: both arms really do carry the budget marker, and they
	// really do render differently.  Without both, the case proves nothing.
	if !carriesTailLimitError(private.last()) || !carriesTailLimitError(shared.last()) {
		t.Fatalf("an arm lost the tail-iteration marker, so this is no longer a replay"+
			"\n  private: %s\n  shared:  %s", private.lastRendered(), shared.lastRendered())
	}
	if private.lastRendered() == shared.lastRendered() {
		t.Fatalf("the arms do not diverge, so this case proves nothing: %s",
			private.lastRendered())
	}
	assertDiverged(t, private.eval, shared.eval, len(private.eval.rendered)-1)
}

// TestOracleComparesAValueNamingABudget and
// TestOracleComparesAnErrorNamingABudget are the older blind spot, kept
// because a future oracle that starts reading text again would pass
// everything else in this file.
func TestOracleComparesAValueNamingABudget(t *testing.T) {
	private := liveArm(t, valueNamingABudgetSrc("original"))
	shared := liveArm(t, valueNamingABudgetSrc("corrupted"))
	assertDiverged(t, private.eval, shared.eval, 0)
}

func TestOracleComparesAnErrorNamingABudget(t *testing.T) {
	private := liveArm(t, `(error 'boom "tail-call iteration limit exceeded: original")`)
	shared := liveArm(t, `(error 'boom "tail-call iteration limit exceeded: corrupted")`)
	assertDiverged(t, private.eval, shared.eval, 0)
}

// TestOracleTailBudgetArmsCompareEqual is (b), and the regression guard on crasher
// cfba74b0f2cbb127 and 423b7dd9e421bd27: two arms that both trip the
// tail-iteration budget on the same runaway loop compare EQUAL by exact text,
// with no exception involved.
//
// This is why the deterministic budgets need no collapse.  The counter is
// denominated in TURNS, not in time, and both arms start from a fresh
// environment with the same limit, so they stop at the same turn and render
// the same message however differently the two goroutines were scheduled.
func TestOracleTailBudgetArmsCompareEqual(t *testing.T) {
	private := liveArm(t, runawayLoopSrc)
	shared := liveArm(t, runawayLoopSrc)

	if !carriesTailLimitError(private.last()) || !carriesTailLimitError(shared.last()) {
		t.Fatalf("an arm was not stopped by the tail budget, so this proves nothing"+
			"\n  private: %s\n  shared:  %s", private.lastRendered(), shared.lastRendered())
	}
	assertAgreed(t, private.eval, shared.eval)
}

// TestOracleExpiredContextIsInconclusive is (c): an arm whose context expired makes
// the input inconclusive from that expression on, and the expressions before
// it are still compared exactly.
func TestOracleExpiredContextIsInconclusive(t *testing.T) {
	t.Run("an already-passed deadline is inconclusive from expression 0", func(t *testing.T) {
		private := liveArm(t, runawayLoopSrc)
		shared := evalArm(t, runawayLoopSrc, time.Nanosecond, -1)
		if private.lastRendered() == shared.lastRendered() {
			t.Fatalf("the arms did not diverge, so this case proves nothing: %s",
				private.lastRendered())
		}
		cmp := compareTreeEvals(private.eval, shared.eval)
		if cmp.inconclusive == "" {
			t.Fatalf("an expired arm was compared anyway (divergedAt=%d)", cmp.divergedAt)
		}
		if cmp.firstExpired != 0 {
			t.Fatalf("firstExpired=%d, want 0", cmp.firstExpired)
		}
		if cmp.compared != 0 {
			t.Fatalf("the oracle compared %d expressions of an arm expired from 0", cmp.compared)
		}
		if cmp.divergedAt >= 0 {
			t.Fatalf("an inconclusive comparison also reported a divergence at %d", cmp.divergedAt)
		}
	})

	t.Run("expressions before the expiry are still compared", func(t *testing.T) {
		// The arms differ at expression 0, under two live contexts; the
		// shared arm's context is cancelled only afterwards.  A divergence
		// the deadline had not yet touched must still be reported.
		private := evalArm(t, valueNamingABudgetSrc("private")+" "+trivialSrc, oracleDeadline, -1)
		shared := evalArm(t, valueNamingABudgetSrc("shared-corrupted")+" "+trivialSrc, oracleDeadline, 1)
		if shared.eval.expired[0] || !shared.eval.expired[1] {
			t.Fatalf("the cancellation did not land between the expressions: %v", shared.eval.expired)
		}
		assertDiverged(t, private.eval, shared.eval, 0)
	})

	t.Run("expressions from the expiry on are not compared", func(t *testing.T) {
		// Same first expression in both arms, so the comparison reaches
		// expression 1 -- where the shared arm's context has been cancelled
		// and the two renderings differ because of it.
		private := evalArm(t, trivialSrc+" "+trivialSrc, oracleDeadline, -1)
		shared := evalArm(t, trivialSrc+" "+trivialSrc, oracleDeadline, 1)
		if private.eval.rendered[1] == shared.eval.rendered[1] {
			t.Fatalf("the cancelled arm produced the same result, so this proves nothing: %s",
				shared.eval.rendered[1])
		}
		cmp := compareTreeEvals(private.eval, shared.eval)
		if cmp.inconclusive == "" {
			t.Fatalf("a cancelled arm was compared anyway (divergedAt=%d)", cmp.divergedAt)
		}
		if cmp.firstExpired != 1 {
			t.Fatalf("firstExpired=%d, want 1", cmp.firstExpired)
		}
		if cmp.compared != 1 {
			t.Fatalf("the oracle compared %d expressions, want the one before the expiry",
				cmp.compared)
		}
	})
}

// TestOracleComparesRaisedContextCancelled is (d): a program that raises
// the reserved context-cancelled condition itself, under a live context, is
// an ordinary result.  It is not inconclusive and it is not exempt -- the
// harness's own context state is what decides that, and this program never
// touched it.
func TestOracleComparesRaisedContextCancelled(t *testing.T) {
	private := liveArm(t, raisedCancelledSrc("original"))
	shared := liveArm(t, raisedCancelledSrc("corrupted"))

	for _, run := range []oracleRun{private, shared} {
		if run.eval.expired[0] {
			t.Fatal("an arm's context expired, so this case proves nothing")
		}
		if last := run.last(); last.Str != lisp.CondContextCancelled {
			t.Fatalf("an arm did not raise the reserved condition: %v (%q)", last, last.Str)
		}
	}
	assertDiverged(t, private.eval, shared.eval, 0)

	// And the same program in both arms still agrees: raising the reserved
	// condition is not by itself a divergence either.
	same := liveArm(t, raisedCancelledSrc("original"))
	assertAgreed(t, private.eval, same.eval)
}

// liveConclusiveSrc is the program TestSharedTreeLiveResultIsConclusive runs.
// Several top-level forms, and deliberately mixed in kind: a plain value, a
// definition, a call through that definition, a caught error, and finally an
// UNCAUGHT one.  A conclusive verdict over this tree therefore says the
// harness compared both ordinary values and an error result -- an oracle that
// silently stopped comparing errors would not reach the end of it.
//
// Everything here completes in microseconds, orders of magnitude inside
// fuzzDeadline, so an inconclusive verdict is a harness defect and not a slow
// machine.
const liveConclusiveSrc = `(+ 1 1)
(defun double (x) (* x 2))
(double 21)
(handler-bind ([error (lambda (c &rest r) 'handled)]) (error 'caught "handled here"))
(error 'deliberate "raised on purpose")`

// TestSharedTreeLiveResultIsConclusive is the live control on the REAL fuzz
// harness path: real newFuzzEnv, real fuzzDeadline, real evalTreeOnce, real
// shared arms, driven through sharedTreeProperty exactly as FuzzSharedTreeEval
// drives it.
//
// The assertion is not "it did not fail" -- sharedTreeProperty passes silently
// when it compares nothing -- but that every shared arm came back CONCLUSIVE
// having compared every expression of the tree.  That is the property the
// inconclusiveness rule can destroy without any test noticing, so it is
// asserted directly rather than inferred from a green run.
func TestSharedTreeLiveResultIsConclusive(t *testing.T) {
	t.Parallel()

	// The expected expression count comes from the reader, not from a
	// hand-counted constant that could drift away from the source above.
	exprs, ok := readTree([]byte(liveConclusiveSrc))
	if !ok {
		t.Fatalf("the fixture does not parse:\n%s", liveConclusiveSrc)
	}
	if len(exprs) < 2 {
		t.Fatalf("the fixture parsed to %d expressions; it is meant to have several", len(exprs))
	}

	res := sharedTreeProperty(t, []byte(liveConclusiveSrc))

	if res.skipped != "" {
		t.Fatalf("the live path skipped the fixture: %s", res.skipped)
	}
	if res.exprs != len(exprs) {
		t.Fatalf("the harness saw %d expressions, the reader read %d", res.exprs, len(exprs))
	}
	if len(res.runs) != sharedRuns {
		t.Fatalf("%d shared arms reported a verdict, want %d", len(res.runs), sharedRuns)
	}
	for i, v := range res.runs {
		if !v.conclusive() {
			t.Fatalf("shared run %d was inconclusive on a program that finishes"+
				" in microseconds under a %v deadline: %s"+
				"\n  the real harness path compared nothing from expression %d on",
				i, fuzzDeadline, v.inconclusive, v.compared)
		}
		if v.compared == 0 {
			t.Fatalf("shared run %d compared no expressions at all", i)
		}
		if v.compared != len(exprs) {
			t.Fatalf("shared run %d compared %d of %d expressions", i, v.compared, len(exprs))
		}
	}
	if !res.conclusive() {
		t.Fatal("the result is not conclusive as a whole")
	}

	// And the fixture really did put both kinds of result through the
	// comparison: without this, a tree of five successful values would pass
	// the assertions above just as well.
	final := liveArm(t, liveConclusiveSrc).last()
	if final.Type != lisp.LError {
		t.Fatalf("the fixture no longer ends in an error result: %v", final)
	}
	if final.Str != "deliberate" {
		t.Fatalf("the fixture raised %q, not its own condition", final.Str)
	}
}
