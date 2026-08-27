// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"crypto/sha256"
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
// The benign cleanup form is lisp:debug-print, which writes to Runtime.Stderr
// and cannot fail.  It is QUALIFIED because the body runs first, in the same
// environment: an unqualified (debug-print ...) resolves after the body has
// had a chance to (defun debug-print ...) over it, and seven such bodies were
// demonstrated turning a correct implementation into permanent testdata
// crashers.  Same for lisp:error in arm C.  A (set! ...) cleanup would have been observable too, but it
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
		// The marker is derived from the body, so a body cannot contain it
		// and forge a cleanup that never ran.  A literal sentinel could be
		// reconstructed dynamically -- (concat 'string "<<fc-" "...>>") --
		// which defeated a containment check on a fixed string.
		marker := cleanupMarker(body)
		if strings.Contains(body, marker) ||
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
			"(with-cleanup ((lisp:debug-print %q)) %s)", marker, body)))
		outC, okC := evalBudgeted(t, []byte(fmt.Sprintf(
			"(with-cleanup ((lisp:error '%s \"x\")) %s)", sentinelCleanupCond, body)))
		outD, okD := evalBudgeted(t, []byte(fmt.Sprintf(
			"(with-cleanup ((lisp:debug-print %q) (lisp:debug-print %q)) %s)",
			marker+"1", marker+"2", body)))
		if !okA || !okB || !okC || !okD {
			t.Skip("a wrapped arm does not parse")
		}

		panickedA := lisp.IsInternalPanic(outA.Result)
		erroredA := outA.Result.Type == lisp.LError

		// (1) Cleanup always runs.  The one exception is a resource limit,
		// which cuts the cleanup form's own evaluation exactly as it cut the
		// body's -- the counters are already spent by the time cleanup is
		// reached.
		if !strings.Contains(outB.Stderr, marker) && !isResourceStop(outB.Result) {
			t.Fatalf("cleanup form did not run and the evaluation was not"+
				" resource-stopped\nbody: %q\nresult: %v", body, outB.Result)
		}
		// (1b) EVERY cleanup form runs, in order.  A single-cleanup arm
		// cannot see this: reversing the forms, running only the last, or
		// not abandoning the rest after one signals are all invisible to it.
		i1 := strings.Index(outD.Stderr, marker+"1")
		i2 := strings.Index(outD.Stderr, marker+"2")
		if !isResourceStop(outD.Result) {
			if i1 < 0 || i2 < 0 {
				t.Fatalf("a cleanup form was skipped: first=%v second=%v"+
					"\nbody: %q\nresult: %v", i1 >= 0, i2 >= 0, body, outD.Result)
			}
			if i1 > i2 {
				t.Fatalf("cleanup forms ran out of order\nbody: %q", body)
			}
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
			t.Fatalf("a signalling cleanup form did not produce an error"+
				"\nbody: %q\nresult: %v", body, outC.Result)
		}
		if outC.Result.Str != sentinelCleanupCond && !isResourceStop(outC.Result) {
			t.Fatalf("expected the cleanup form's condition %q to win, got %q"+
				"\nbody: %q\nresult: %v",
				sentinelCleanupCond, outC.Result.Str, body, outC.Result)
		}
	})
}

// sentinelCleanupCond is the condition the signalling cleanup form raises.
// It is deliberately not a name any real code uses.
const sentinelCleanupCond = "fuzz-cleanup-ran-and-signalled"

// cleanupMarker returns the string the benign cleanup forms debug-print for
// this body.  It is DERIVED from the body so that the body cannot contain it,
// which a fixed sentinel could not promise: a containment check on a literal
// is defeated by building the same string dynamically.
func cleanupMarker(body string) string {
	sum := sha256.Sum256([]byte(body))
	return fmt.Sprintf("<<fc-%x>>", sum[:8])
}

// isResourceStop reports whether v is an evaluation cut short by a budget
// rather than by the program's own logic.
//
// The stack, tail-iteration, allocation and macro-depth limits arrive as
// wrapped Go errors rather than as named conditions, so there is no condition
// to switch on: env.Error stamps them all with the generic "error".  They are
// therefore matched on message text -- but ONLY when the condition is that
// generic one.  That qualification is the whole guard.  Without it a body can
// forge an excuse for its own failure:
//
//	(error 'x "stack height exceeded maximum")
//
// renders a message carrying the fragment, and a match on the message alone
// silently excused it.  A defect that skipped cleanup on the error path then
// went unreported for every body that named a limit in its message -- verified
// against a real mutation, not imagined.  A user condition is never the
// generic "error", so requiring it closes that family.
//
// THE RESIDUAL, stated rather than waved away: a body that raises the generic
// condition itself, (error 'error "stack height exceeded maximum"), is still
// excused.  Closing that needs a discriminator the LVal does not carry.
//
// An earlier version of this comment claimed being over-broad "cannot
// manufacture a passing run out of a failing one, which is the safe direction".
// That had the direction backwards: excusing an assertion is EXACTLY how a
// failing run is turned into a passing one.  What over-breadth cannot
// manufacture is a false FAILURE.
func isResourceStop(v *lisp.LVal) bool {
	if v == nil || v.Type != lisp.LError {
		return false
	}
	switch v.Str {
	case lisp.CondStepLimitExceeded, lisp.CondContextCancelled,
		lisp.CondEvalNestingExceeded, lisp.CondSleepLimitExceeded:
		return true
	case genericErrorCondition:
		// Fall through to the message match below.
	default:
		// A named user condition is never a budget stop.
		return false
	}
	msg := v.String()
	for _, frag := range []string{
		"stack height exceeded maximum",
		"tail-call iteration limit exceeded",
		// runtime.go renders "allocation size N exceeds maximum (M)".  An
		// earlier list said "allocation limit", which matches nothing the
		// interpreter emits -- so a genuine WithMaxAlloc stop was NOT
		// recognised while the fragment stayed live as a forgery.
		"allocation size",
		"macro expansion depth",
	} {
		if strings.Contains(msg, frag) {
			return true
		}
	}
	return false
}

// genericErrorCondition is what env.Error stamps on an error built from a Go
// error -- every budget stop that is not one of the named conditions above.
const genericErrorCondition = "error"

// parses reports whether src is readable, so the target can decline an input
// that is the parser targets' business rather than this one's.
func parses(t *testing.T, src string) bool {
	t.Helper()
	_, err := parser.NewReader().Read("fuzz", strings.NewReader(src))
	return err == nil
}
