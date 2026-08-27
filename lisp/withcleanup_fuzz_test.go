// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FuzzWithCleanup is a DIFFERENTIAL target: it evaluates a fuzzer-chosen
// body three ways and asserts the invariants that relate the results.
//
// FuzzEval already runs with-cleanup programs, but its assertions are
// generic -- it terminates, and it did not recover a Go panic.  Neither says
// anything about what the operator is FOR.  The properties below do, and they
// hold for an arbitrary body rather than for the shapes a human thought to
// write down in a table:
//
//	A:  (progn BODY)                                -- the baseline
//	B:  (with-cleanup ((debug-print MARKER)) BODY)  -- benign cleanup
//	C:  (with-cleanup ((error ...)) BODY)           -- signalling cleanup
//
// The body needs no progn in B or C: with-cleanup evaluates its body as an
// implicit progn, so the arms differ from A only by the bracket itself.
//
//  1. Cleanup ALWAYS runs (B).  This is the defining property of the
//     operator, and the one a hand-written table can only spot-check.
//  2. It NEVER catches (B).  If A errored, B errored.  Asserted in one
//     direction only: the converse is legitimately false, because the
//     operator blocks tail-call optimisation and a deep tail loop can
//     therefore exhaust the stack in B where A ran in constant space.
//  3. The internal-panic carve-out survives BOTH cleanups (B and C).  C is
//     the load-bearing one: an error raised by a cleanup form replaces an
//     ordinary outcome but must never mask a recovered host panic.
//  4. When the body did not panic, C reports the CLEANUP's condition -- the
//     "cleanup error wins" rows of the decision table.
//
// WHY THERE IS NO PANICKING BUILTIN HERE, though property 3 would be easier
// to reach with one.  Installing it means lisp.RegisterDefaultBuiltin, which
// mutates the DEFAULT builtin set for the whole test binary -- so the symbol
// would also be in reach of FuzzEval, whose entire assertion is
// IsInternalPanic == false.  A coverage-guided mutator that found it would
// turn elps's primary evaluator oracle into a false-positive generator, and
// the corpus entry would persist in testdata.  Not worth it: the panic rows
// are pinned deterministically in withcleanup_test.go, against a builtin
// installed on ONE environment.  Property 3 is still asserted below, just
// opportunistically -- it costs nothing, and if this target ever does
// discover a genuine host panic it will also prove the carve-out survived it.
//
// The benign cleanup form is debug-print, which writes to Runtime.Stderr and
// cannot fail.  A (set! ...) cleanup would have been observable too, but it
// can error if the body disturbed the binding -- which would surface as a
// violation of the very property being measured.
//
// Cross-arm comparison is only sound because each arm gets a FRESH
// environment (evalBudgeted builds one per call), so a body with side effects
// cannot leak from one arm into the next.
func FuzzWithCleanup(f *testing.F) {
	for _, src := range fuzzseed.EvalTerminating() {
		f.Add(src)
	}
	for _, src := range fuzzseed.EvalErroring() {
		f.Add(src)
	}
	for _, src := range fuzzseed.EvalRunaway() {
		f.Add(src)
	}
	// Bodies aimed at this target's own invariants rather than at the
	// evaluator: the panic path, the cleanup-error interaction, and the
	// tail-position shape that makes property 2 one-directional.
	for _, src := range []string{
		`(error 'internal-panic "forged")`,
		`(error 'boom "x")`,
		`1`,
		`(defun spin (n) (if (<= n 0) 'done (spin (- n 1)))) (spin 200)`,
		`(with-cleanup ((error 'cleanup "y")) (error 'inner "x"))`,
		`(set 'v 0) (with-cleanup ((set! 'v 2)) (set! 'v 1)) v`,
	} {
		f.Add(src)
	}

	f.Fuzz(func(t *testing.T, body string) {
		// A body naming a sentinel could rebind it and turn a genuine
		// invariant violation into a false report; skip rather than reason
		// about it.
		if strings.Contains(body, cleanupMarker) ||
			strings.Contains(body, sentinelCleanupCond) {
			t.Skip("body names a harness sentinel")
		}
		// Long bodies dominate the cost of a coverage-guided run for the
		// same reason evalseed.go is hand-written rather than seeded from
		// the test suite.
		if len(body) > 512 {
			t.Skip("body too large to be a useful generation")
		}
		if !parses(t, body) {
			t.Skip("body does not parse")
		}

		outA, okA := evalBudgeted(t, []byte(fmt.Sprintf("(progn %s)", body)))
		outB, okB := evalBudgeted(t, []byte(fmt.Sprintf(
			"(with-cleanup ((debug-print %q)) %s)", cleanupMarker, body)))
		outC, okC := evalBudgeted(t, []byte(fmt.Sprintf(
			"(with-cleanup ((error '%s \"x\")) %s)", sentinelCleanupCond, body)))
		if !okA || !okB || !okC {
			t.Skip("a wrapped arm does not parse")
		}

		panickedA := lisp.IsInternalPanic(outA.Result)
		erroredA := outA.Result.Type == lisp.LError

		// (1) Cleanup always runs.  The one exception is a resource limit,
		// which cuts the cleanup form's own evaluation exactly as it cut the
		// body's -- the counters are already spent by the time cleanup is
		// reached.
		if !cleanupRan(outB) && !isResourceStop(outB.Result) {
			t.Fatalf("cleanup form did not run and the evaluation was not"+
				" resource-stopped\nbody: %q\nresult: %v", body, outB.Result)
		}

		// (2) It never catches.  One direction only -- see the header.
		if erroredA && outB.Result.Type != lisp.LError {
			t.Fatalf("with-cleanup swallowed an error\nbody: %q\nbare: %v\nwrapped: %v",
				body, outA.Result, outB.Result)
		}

		// (3) The carve-out holds under a benign cleanup and under a
		// signalling one.  The second is what stops a cleanup error from
		// becoming a way to launder a host defect into an ordinary condition.
		if panickedA {
			if !lisp.IsInternalPanic(outB.Result) && !isResourceStop(outB.Result) {
				t.Fatalf("a recovered host panic was lost through a benign"+
					" cleanup form\nbody: %q\nresult: %v", body, outB.Result)
			}
			if !lisp.IsInternalPanic(outC.Result) && !isResourceStop(outC.Result) {
				t.Fatalf("a recovered host panic was MASKED by a signalling"+
					" cleanup form\nbody: %q\nresult: %v", body, outC.Result)
			}
			return
		}

		// (4) The body did not panic, so a signalling cleanup form wins --
		// over a value and over an ordinary error alike.
		if outC.Result.Type != lisp.LError {
			if !isResourceStop(outC.Result) {
				t.Fatalf("a signalling cleanup form did not produce an error"+
					"\nbody: %q\nresult: %v", body, outC.Result)
			}
			return
		}
		if outC.Result.Str != sentinelCleanupCond && !isResourceStop(outC.Result) {
			t.Fatalf("expected the cleanup form's condition %q to win, got %q"+
				"\nbody: %q\nresult: %v",
				sentinelCleanupCond, outC.Result.Str, body, outC.Result)
		}
	})
}

const (
	// sentinelCleanupCond is the condition the signalling cleanup form
	// raises.  It is deliberately not a name any real code uses.
	sentinelCleanupCond = "fuzz-cleanup-ran-and-signalled"
	// cleanupMarker is what the benign cleanup form debug-prints.
	cleanupMarker = "<<fuzz-cleanup-ran>>"
)

// cleanupRan reports whether the benign cleanup form executed, read from the
// captured stderr rather than from lisp state.
func cleanupRan(out evalOutcome) bool {
	return strings.Contains(out.Stderr, cleanupMarker)
}

// isResourceStop reports whether v is an evaluation cut short by a budget
// rather than by the program's own logic.
//
// The stack and tail-iteration limits arrive as wrapped Go errors rather than
// as named conditions, so they are matched on message text.  Being
// over-broad here only skips an assertion; it cannot manufacture a passing
// run out of a failing one, which is the safe direction for a heuristic
// inside an oracle.
func isResourceStop(v *lisp.LVal) bool {
	if v == nil || v.Type != lisp.LError {
		return false
	}
	switch v.Str {
	case lisp.CondStepLimitExceeded, lisp.CondContextCancelled,
		lisp.CondEvalNestingExceeded, lisp.CondSleepLimitExceeded:
		return true
	}
	msg := v.String()
	for _, frag := range []string{
		"stack height exceeded maximum",
		"tail-call iteration limit exceeded",
		"allocation limit",
		"macro expansion depth",
	} {
		if strings.Contains(msg, frag) {
			return true
		}
	}
	return false
}

// parses reports whether src is readable, so the target can decline an input
// that is the parser targets' business rather than this one's.
func parses(t *testing.T, src string) bool {
	t.Helper()
	_, err := parser.NewReader().Read("fuzz", strings.NewReader(src))
	return err == nil
}
