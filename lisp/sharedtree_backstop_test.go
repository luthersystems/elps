// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The backstop collapse must be narrow: it exists so that two arms stopped by
// DIFFERENT budgets on the same runaway loop agree, and for nothing else.  A
// collapse that reached any further would blind FuzzSharedTreeEval to the
// divergences it exists to find, which is a worse outcome than the flake it
// removes (crasher 423b7dd9e421bd27).
//
// "Narrow" is a claim about how a result is CLASSIFIED, so every case below
// is a real result the evaluator produced, classified the way the target
// classifies it -- not a rendered string matched against a phrase.  The
// rendered text is the one thing that cannot decide this: a program is free
// to return, or to raise, whatever a budget's message happens to say.

// backstopDeadline is the deadline the non-deadline cases run under.  Long
// enough that nothing here reaches it, so a context-cancelled verdict in
// those cases would be a real finding rather than a slow machine.
const backstopDeadline = 30 * time.Second

// evalForVerdict evaluates src under a fresh fuzz environment and the given
// deadline, returning the last result together with whether that run's own
// context had expired -- the two inputs classifyBudget takes.
func evalForVerdict(t *testing.T, src string, deadline time.Duration) (*lisp.LVal, bool) {
	t.Helper()
	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("building the environment failed: %v", rc)
	}
	exprs, err := parser.NewReader().Read("backstop", strings.NewReader(src))
	if err != nil {
		t.Fatalf("reading %q failed: %v", src, err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), deadline)
	defer cancel()
	var last *lisp.LVal
	for _, e := range exprs {
		last = env.EvalContext(ctx, e)
		if last == nil {
			t.Fatalf("evaluating %q returned a nil LVal", src)
		}
		if last.Type == lisp.LError {
			break
		}
	}
	return last, ctx.Err() != nil
}

// The four results every case below is built from.  Each is what the
// evaluator actually produces, not a hand-written stand-in.
//
//   - tailBudget:  a tail-recursive loop stopped by MaxTailIterations.
//   - ctxBudget:   an evaluation whose context deadline had already passed.
//   - raisedPhrase: an error the PROGRAM raised, whose message is a budget's.
//   - valuePhrase:  a successful string value that reads like a budget.
//
// The last two are the whole point: they render as a backstop and are not
// one.
func tailBudget(t *testing.T) (*lisp.LVal, bool) {
	t.Helper()
	return evalForVerdict(t, `(defun s () (s)) (s)`, backstopDeadline)
}

func ctxBudget(t *testing.T) (*lisp.LVal, bool) {
	t.Helper()
	// A deadline already in the past: the first limit check the evaluator
	// reaches raises context-cancelled, deterministically and in microseconds.
	return evalForVerdict(t, `(+ 1 1)`, time.Nanosecond)
}

func raisedPhrase(t *testing.T, payload string) (*lisp.LVal, bool) {
	t.Helper()
	return evalForVerdict(t,
		`(error 'boom "context deadline exceeded: `+payload+`")`, backstopDeadline)
}

func valuePhrase(t *testing.T, payload string) (*lisp.LVal, bool) {
	t.Helper()
	return evalForVerdict(t,
		`(concat 'string "context deadline exceeded: " "`+payload+`")`, backstopDeadline)
}

// TestBackstopFixturesAreWhatTheyClaim checks the fixtures before anything is
// concluded from them: a fixture that quietly stopped producing a budget
// error (a retuned limit, a changed condition symbol) would make every
// collapse assertion below pass vacuously.
func TestBackstopFixturesAreWhatTheyClaim(t *testing.T) {
	tail, _ := tailBudget(t)
	if tail.Type != lisp.LError {
		t.Fatalf("the runaway loop did not error: %v", tail)
	}
	if !strings.Contains(tail.String(), "tail-call iteration limit exceeded") {
		t.Fatalf("the runaway loop hit some other limit: %v", tail)
	}
	cancelled, expired := ctxBudget(t)
	if !expired {
		t.Fatal("the expired-deadline fixture ran with a live context")
	}
	if cancelled.Type != lisp.LError || cancelled.Str != lisp.CondContextCancelled {
		t.Fatalf("the expired-deadline fixture produced %v (condition %q)",
			cancelled, cancelled.Str)
	}
	raised, _ := raisedPhrase(t, "original")
	if raised.Type != lisp.LError {
		t.Fatalf("the raised-error fixture is not an error: %v", raised)
	}
	value, _ := valuePhrase(t, "original")
	if value.Type != lisp.LString {
		t.Fatalf("the value fixture is not a string: %v", value)
	}
	// The fixtures earn their keep only if they really do carry the phrase a
	// substring classifier would trip on.
	for _, s := range []string{raised.String(), value.String()} {
		if !strings.Contains(s, "context deadline exceeded") {
			t.Fatalf("fixture no longer contains the backstop phrase: %s", s)
		}
	}
}

// TestClassifyBudgetProvenance pins the classifier itself: what counts as an
// evaluator budget, and what only looks like one.
func TestClassifyBudgetProvenance(t *testing.T) {
	tail, tailExpired := tailBudget(t)
	cancelled, cancelledExpired := ctxBudget(t)
	raised, raisedExpired := raisedPhrase(t, "original")
	value, valueExpired := valuePhrase(t, "original")
	for _, tt := range []struct {
		v          *lisp.LVal
		name       string
		want       budgetKind
		ctxExpired bool
	}{
		{tail, "a tail-iteration budget", budgetTailIterations, tailExpired},
		{cancelled, "an expired context deadline", budgetContextDeadline, cancelledExpired},
		{raised, "an error the program raised, naming a budget", notABudget, raisedExpired},
		{value, "a successful value naming a budget", notABudget, valueExpired},
		// The condition symbol alone is not the marker.  A program can raise
		// the reserved condition; it cannot expire the harness's context, and
		// a run that finished inside its deadline is never admitted.
		{
			lisp.ErrorConditionf(lisp.CondContextCancelled, "context cancelled: %v", context.DeadlineExceeded),
			"the reserved condition on a run whose context is live",
			notABudget,
			false,
		},
		{nil, "a nil result", notABudget, true},
	} {
		t.Run(tt.name, func(t *testing.T) {
			if got := classifyBudget(tt.v, tt.ctxExpired); got != tt.want {
				t.Fatalf("classifyBudget = %v, want %v\n  value: %v", got, tt.want, tt.v)
			}
		})
	}
}

// TestOnlyTwoBackstopsCollapse is the collapse decision itself, over the same
// real results.
func TestOnlyTwoBackstopsCollapse(t *testing.T) {
	tail, tailExpired := tailBudget(t)
	cancelled, cancelledExpired := ctxBudget(t)
	raised, raisedExpired := raisedPhrase(t, "original")
	value, valueExpired := valuePhrase(t, "original")

	iter := classifyBudget(tail, tailExpired)
	deadline := classifyBudget(cancelled, cancelledExpired)
	other := classifyBudget(raised, raisedExpired)
	val := classifyBudget(value, valueExpired)

	for _, tt := range []struct {
		name     string
		a, b     budgetKind
		collapse bool
	}{
		{"the two budgets, either order", iter, deadline, true},
		{"the two budgets, reversed", deadline, iter, true},
		{"the same budget twice", iter, iter, true},
		// The asymmetric cases: one arm stopped by a budget and the other
		// not.  Scheduling does not explain those, so they stay divergences.
		{"a budget against a value", iter, val, false},
		{"a budget against an ordinary error", deadline, other, false},
		{"a value against a budget", val, deadline, false},
		// A program is free to RAISE an error whose text mentions a limit.
		// That is the program's meaning, not a backstop.
		{"a program-raised error naming a limit", other, iter, false},
		{"two ordinary errors", other, val, false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			if got := bothHitAResourceBackstop(tt.a, tt.b); got != tt.collapse {
				t.Fatalf("collapse=%v, want %v\n  a: %v\n  b: %v", got, tt.collapse, tt.a, tt.b)
			}
		})
	}
}

// TestBackstopNegativeControls is the blind spot the substring classifier had,
// stated as the property that closes it: two results that DIFFER must be
// reported as a divergence however much their text reads like a budget.
//
// Each case is a pair the two fuzz arms could genuinely produce -- the same
// expression evaluated over a private tree and over a shared one -- where the
// shared arm's value has been corrupted.  Under the substring classifier all
// three collapsed silently, which is FuzzSharedTreeEval declining to report
// exactly the corruption it exists to find.
func TestBackstopNegativeControls(t *testing.T) {
	for _, tt := range []struct {
		fixture func(*testing.T, string) (*lisp.LVal, bool)
		name    string
	}{
		// (a) two SUCCESSFUL values whose text contains the phrase.
		{valuePhrase, "two successful strings containing the phrase"},
		// (b) two USER-RAISED errors containing the phrase, different payloads.
		{raisedPhrase, "two user-raised errors containing the phrase"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			a, aExpired := tt.fixture(t, "original")
			b, bExpired := tt.fixture(t, "corrupted")
			if a.String() == b.String() {
				t.Fatalf("the control does not diverge: both arms render %s", a.String())
			}
			ka, kb := classifyBudget(a, aExpired), classifyBudget(b, bExpired)
			if bothHitAResourceBackstop(ka, kb) {
				t.Fatalf("collapsed a divergence between two non-budget results"+
					"\n  a: %s  [%v]\n  b: %s  [%v]", a.String(), ka, b.String(), kb)
			}
		})
	}
	// (c) one arm stopped by a budget, the other returning a value that reads
	// like one.  Scheduling explains a budget racing a budget; it does not
	// explain a budget racing a result.
	t.Run("a budget against a success containing the phrase", func(t *testing.T) {
		tail, tailExpired := tailBudget(t)
		value, valueExpired := valuePhrase(t, "corrupted")
		ka, kb := classifyBudget(tail, tailExpired), classifyBudget(value, valueExpired)
		if bothHitAResourceBackstop(ka, kb) {
			t.Fatalf("collapsed a budget against a value"+
				"\n  a: %s  [%v]\n  b: %s  [%v]", tail.String(), ka, value.String(), kb)
		}
	})
}

// TestRunawayLoopStillCollapses is the positive case the collapse exists for,
// and the regression guard on crasher 423b7dd9e421bd27: ONE runaway loop, two
// arms, stopped by different budgets because they ran at different speeds.
// The renderings differ; the verdicts agree; the target must not report it.
func TestRunawayLoopStillCollapses(t *testing.T) {
	// Same program in both arms.  The private arm is given room to reach the
	// tail-iteration limit; the shared arm is charged a deadline that has
	// already passed, which is the slow-arm outcome the crasher exhibited.
	private, privateExpired := evalForVerdict(t, `(defun s () (let () (s))) (s)`, backstopDeadline)
	shared, sharedExpired := evalForVerdict(t, `(defun s () (let () (s))) (s)`, time.Nanosecond)
	if private.String() == shared.String() {
		t.Fatalf("the two arms did not diverge, so this case proves nothing: %s", private.String())
	}
	kp, ks := classifyBudget(private, privateExpired), classifyBudget(shared, sharedExpired)
	if kp != budgetTailIterations {
		t.Fatalf("the private arm was not stopped by the tail budget: %s [%v]", private.String(), kp)
	}
	if ks != budgetContextDeadline {
		t.Fatalf("the shared arm was not stopped by the deadline: %s [%v]", shared.String(), ks)
	}
	if !bothHitAResourceBackstop(kp, ks) {
		t.Fatalf("two budgets on one runaway loop no longer collapse"+
			"\n  private: %s  [%v]\n  shared:  %s  [%v]",
			private.String(), kp, shared.String(), ks)
	}
}
