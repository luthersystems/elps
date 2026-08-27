// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// with-cleanup is the operator that guarantees a cleanup form runs on the
// way out (#554).  Two properties carry the whole feature, and neither is
// visible from a test that only checks a return value:
//
//  1. The cleanup runs on EVERY exit path -- normal return, ordinary error,
//     and a recovered Go panic.  The tests here therefore assert on a side
//     effect the cleanup performs, not on what the form evaluates to.
//
//  2. A recovered Go panic (internal-panic) is never masked by an error
//     raised from a cleanup form.  ignore-errors and handler-bind's catch-all
//     'condition both refuse to swallow one; an operator that could would
//     reopen that hole and blind FuzzEval's assertion.  The check is
//     IsInternalPanic -- the non-forgeable marker -- and NOT the condition
//     name, which is pinned separately below.

// withCleanupTestEnv returns an initialized environment carrying a `host-panic`
// builtin that raises a genuine Go panic.  That is the only way to produce a
// value satisfying lisp.IsInternalPanic: the marker keys off the recovered
// Go-stack snapshot, so no lisp program can forge one.
func withCleanupTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("initialize env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		t.Fatalf("load library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	env.AddBuiltins(true, elpsutil.Function("host-panic", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			panic("genuine host defect")
		}))
	return env
}

// evalWithCleanup evaluates src and returns the result without failing on an
// error -- an error result is the subject of most tests in this file.
func evalWithCleanup(t testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	return env.LoadString("withcleanup_test.lisp", src)
}

// TestWithCleanupDecisionTable walks every row of the table in #554.
//
// `trace` records what actually ran, so a cleanup that was skipped is
// distinguishable from one that ran and whose value was discarded.
func TestWithCleanupDecisionTable(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		// expr is evaluated with `trace` in scope.
		expr string
		// trace is the expected side-effect record after expr runs.
		trace string
		// result classifies what propagated.
		wantErr     bool
		wantErrCond string
		wantPanic   bool
		wantValue   string
	}{{
		name:      "normal/normal: protected value propagates",
		expr:      `(with-cleanup ((append! trace "cleanup")) (append! trace "body") 42)`,
		trace:     `(vector "body" "cleanup")`,
		wantValue: `42`,
	}, {
		name:        "normal/signals: the cleanup error wins",
		expr:        `(with-cleanup ((error 'cleanup-error "boom")) (append! trace "body") 42)`,
		trace:       `(vector "body")`,
		wantErr:     true,
		wantErrCond: "cleanup-error",
	}, {
		name:        "signals/normal: the protected error propagates after cleanup",
		expr:        `(with-cleanup ((append! trace "cleanup")) (error 'body-error "boom"))`,
		trace:       `(vector "cleanup")`,
		wantErr:     true,
		wantErrCond: "body-error",
	}, {
		name:        "signals/signals: the cleanup error replaces the protected error",
		expr:        `(with-cleanup ((error 'cleanup-error "boom")) (error 'body-error "boom"))`,
		trace:       `(vector)`,
		wantErr:     true,
		wantErrCond: "cleanup-error",
	}, {
		name:      "panic/normal: cleanup runs and the panic propagates",
		expr:      `(with-cleanup ((append! trace "cleanup")) (host-panic))`,
		trace:     `(vector "cleanup")`,
		wantErr:   true,
		wantPanic: true,
	}, {
		name:      "panic/signals: the panic is never masked",
		expr:      `(with-cleanup ((error 'cleanup-error "boom")) (host-panic))`,
		trace:     `(vector)`,
		wantErr:   true,
		wantPanic: true,
	}} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := withCleanupTestEnv(t)
			require.NotEqual(t, lisp.LError,
				evalWithCleanup(t, env, `(set 'trace (vector))`).Type)

			got := evalWithCleanup(t, env, tc.expr)

			if tc.wantErr {
				require.Equal(t, lisp.LError, got.Type,
					"expected an error to propagate, got %v", got)
			} else {
				require.NotEqual(t, lisp.LError, got.Type, "unexpected error: %v", got)
				require.Equal(t, tc.wantValue, got.String())
			}
			if tc.wantErrCond != "" {
				require.Equal(t, tc.wantErrCond, got.Str)
			}
			require.Equal(t, tc.wantPanic, lisp.IsInternalPanic(got),
				"IsInternalPanic mismatch on %v", got)

			// The side-effect record is the real assertion: it says which
			// forms ran, which a return value cannot.
			require.Equal(t, tc.trace, evalWithCleanup(t, env, `trace`).String())
		})
	}
}

// TestWithCleanupPanicExceptionIsNotNameBased pins that the carve-out keys
// off IsInternalPanic and not off the condition name.
//
// A lisp program can write (error 'internal-panic "...") but cannot forge the
// recovered Go-stack snapshot the marker requires.  If this operator tested
// the NAME instead, that forgery would let any program make its own errors
// un-maskable -- and, worse, would make the genuine case indistinguishable.
func TestWithCleanupPanicExceptionIsNotNameBased(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	forged := evalWithCleanup(t, env,
		`(with-cleanup ((error 'cleanup-error "boom")) (error 'internal-panic "forged"))`)
	require.Equal(t, lisp.LError, forged.Type)
	require.False(t, lisp.IsInternalPanic(forged),
		"a lisp-forged internal-panic must not satisfy the marker")
	require.Equal(t, "cleanup-error", forged.Str,
		"a forged internal-panic is an ordinary error, so the cleanup error must win")

	genuine := evalWithCleanup(t, env,
		`(with-cleanup ((error 'cleanup-error "boom")) (host-panic))`)
	require.True(t, lisp.IsInternalPanic(genuine),
		"a genuine recovered panic must survive a signalling cleanup form")
}

// TestWithCleanupPropagatesTheOriginalPanicValue pins the implementation
// note from #554: on the panic/signals row the operator must return the
// ORIGINAL *LVal, not rebuild an error from it.  IsInternalPanic requires the
// attached CallStack, so a reconstruction would silently stop satisfying it
// and the host defect would vanish from FuzzEval's view.
func TestWithCleanupPropagatesTheOriginalPanicValue(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	bare := evalWithCleanup(t, env, `(host-panic)`)
	require.True(t, lisp.IsInternalPanic(bare))

	through := evalWithCleanup(t, env,
		`(with-cleanup ((error 'cleanup-error "boom")) (host-panic))`)
	require.True(t, lisp.IsInternalPanic(through))
	require.Equal(t, bare.Str, through.Str,
		"the condition name must survive the cleanup form")
	require.NotNil(t, through.Native,
		"the CallStack IsInternalPanic keys off must survive the cleanup form")
}

// TestWithCleanupRunsCleanupUnderIgnoreErrors is the motivating case from
// luthersystems/substrate#421, reduced.
//
// A bracket sets a flag, the body signals, and something UPSTREAM recovers.
// Written with handler-bind the flag leaks set, and the next caller in the
// same transaction wrongly trips the guard.  with-cleanup is what makes the
// clear unconditional.
func TestWithCleanupRunsCleanupUnderIgnoreErrors(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	res := evalWithCleanup(t, env, `
		(set 'in-step false)
		(defun guarded (body)
		  (if in-step
		    (error 'nested "already in a step")
		    (with-cleanup ((set! 'in-step false))
		      (set! 'in-step true)
		      (funcall body))))
		(ignore-errors (guarded (lambda () (error 'body-failed "boom"))))
		in-step`)
	require.NotEqual(t, lisp.LError, res.Type, "unexpected error: %v", res)
	require.Equal(t, `false`, res.String(),
		"the flag leaked set: a later call in the same transaction would wrongly report nesting")

	// And the guard still works afterwards, which is the property the leak
	// destroys.
	after := evalWithCleanup(t, env, `(guarded (lambda () 'ok))`)
	require.NotEqual(t, lisp.LError, after.Type, "unexpected error: %v", after)
	require.Equal(t, `'ok`, after.String())
}

// TestWithCleanupSurvivesTailPosition pins that a protected form in tail
// position still runs its cleanup at every level of the recursion.
//
// It is NOT a red-proof of the TROBlock line in opWithCleanup: deleting
// that line leaves this test green, as deleting the identical line from
// opIgnoreErrors leaves the whole suite green.  What this does pin is the
// property that matters to a caller -- five nested brackets produce five
// cleanups -- and it would fail if the operator ever handed the protected
// form back to the trampoline as an env.Terminal expression instead of
// evaluating it eagerly, which is the change that WOULD let TRO collapse a
// frame owing cleanup.
func TestWithCleanupSurvivesTailPosition(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	res := evalWithCleanup(t, env, `
		(set 'depth 0)
		(defun countdown (n)
		  (if (<= n 0)
		    'done
		    (with-cleanup ((set! 'depth (+ depth 1))) (countdown (- n 1)))))
		(countdown 5)
		depth`)
	require.NotEqual(t, lisp.LError, res.Type, "unexpected error: %v", res)
	require.Equal(t, `5`, res.String(),
		"a cleanup was skipped: one recursion level did not run its cleanup form")
}

// TestWithCleanupNestsInnermostFirst pins the ordering, and that an error
// crossing several brackets runs every one of them.
func TestWithCleanupNestsInnermostFirst(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	res := evalWithCleanup(t, env, `
		(set 'trace (vector))
		(ignore-errors
		  (with-cleanup ((append! trace "outer"))
		    (with-cleanup ((append! trace "inner")) (error 'boom "x"))))
		trace`)
	require.NotEqual(t, lisp.LError, res.Type, "unexpected error: %v", res)
	require.Equal(t, `(vector "inner" "outer")`, res.String())
}

// TestWithCleanupRunsEveryCleanupForm covers the multi-form body, and that
// a signalling cleanup form abandons the ones after it.
func TestWithCleanupRunsEveryCleanupForm(t *testing.T) {
	t.Parallel()

	t.Run("all forms run", func(t *testing.T) {
		t.Parallel()
		env := withCleanupTestEnv(t)
		res := evalWithCleanup(t, env, `
			(set 'trace (vector))
			(with-cleanup ((append! trace "a") (append! trace "b") (append! trace "c")) 'body)
			trace`)
		require.Equal(t, `(vector "a" "b" "c")`, res.String())
	})

	t.Run("a signalling form abandons the rest", func(t *testing.T) {
		t.Parallel()
		env := withCleanupTestEnv(t)
		res := evalWithCleanup(t, env, `
			(set 'trace (vector))
			(ignore-errors
			  (with-cleanup ((append! trace "a")
			                 (error 'cleanup-error "boom")
			                 (append! trace "c"))
			    'body))
			trace`)
		require.Equal(t, `(vector "a")`, res.String(),
			"cleanup forms after a signalling one must not run")
	})
}

// TestWithCleanupWithNoCleanupForms degenerates to the protected form.
func TestWithCleanupWithNoCleanupForms(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)
	require.Equal(t, `42`, evalWithCleanup(t, env, `(with-cleanup () 42)`).String())
	require.Equal(t, lisp.LError,
		evalWithCleanup(t, env, `(with-cleanup () (error 'boom "x"))`).Type)
}

// TestWithCleanupDoesNotCatch is the property that separates this operator
// from ignore-errors: after the cleanup has run, the error is still live.
func TestWithCleanupDoesNotCatch(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	res := evalWithCleanup(t, env, `
		(set 'trace (vector))
		(handler-bind ([condition (lambda (c &rest _) (list 'caught c))])
		  (with-cleanup ((append! trace "cleanup")) (error 'body-error "boom")))`)
	require.NotEqual(t, lisp.LError, res.Type, "unexpected error: %v", res)
	require.Equal(t, `'('caught 'body-error)`, res.String(),
		"the error must still reach an enclosing handler after cleanup runs")
	require.Equal(t, `(vector "cleanup")`, evalWithCleanup(t, env, `trace`).String())
}

// TestWithCleanupSuite runs the shapes from docs/lang.md through the
// standard expression harness, so the documented examples are executable
// rather than aspirational, and the values render as the REPL renders them.
func TestWithCleanupSuite(t *testing.T) {
	tests := elpstest.TestSuite{
		{"value and ordering", elpstest.TestSequence{
			// The protected form's value propagates; the cleanup's is dropped.
			{`(with-cleanup (2) 1)`, `1`, ""},
			// Cleanup runs after the body, on the normal path.
			// debug-print returns (), so the VALUE here is nil; the
			// ordering assertion is the captured output.
			{`(with-cleanup ((debug-print "cleanup")) (debug-print "body"))`,
				`()`, "\"body\"\n\"cleanup\"\n"},
			// Several cleanup forms are an implicit progn.
			{`(with-cleanup ((debug-print "a") (debug-print "b")) 'v)`,
				`'v`, "\"a\"\n\"b\"\n"},
		}},
		{"does not catch", elpstest.TestSequence{
			// The docs example: cleanup runs, then the error reaches the
			// enclosing handler with its condition intact.
			{`(handler-bind ((condition (lambda (c &rest args) (list 'caught c))))
				(with-cleanup ((debug-print "cleanup ran"))
				              (error 'my-error "data")))`,
				`'('caught 'my-error)`, "\"cleanup ran\"\n"},
		}},
		{"cleanup error wins", elpstest.TestSequence{
			// Row 2 of the table: an ordinary outcome is replaced.
			{`(ignore-errors (with-cleanup ((error 'cleanup-error "boom")) 1))`, `()`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestWithCleanupArity pins the arity contract at RUNTIME.
//
// opWithCleanup indexes args.Cells[0] unguarded, which is safe only
// because Formals declares the protected form as a required argument --
// opIgnoreErrors defends itself with an explicit length check, this and
// opHandlerBind do not.  Without this test a change to Formals would turn
// (with-cleanup) into an index-out-of-range recovered as an
// internal-panic, and the lisp suite would stay green: the only existing
// zero-argument coverage is in lint, which tests the analyzer rather than
// the evaluator.
func TestWithCleanupArity(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)

	got := evalWithCleanup(t, env, `(with-cleanup)`)
	require.Equal(t, lisp.LError, got.Type, "(with-cleanup) must be an arity error")
	require.False(t, lisp.IsInternalPanic(got),
		"the arity check must reject this, not an index-out-of-range recovered"+
			" as a host panic: %v", got)
	require.Contains(t, got.String(), "invalid number of arguments")
}

// TestWithCleanupPanicThenMixedCleanup covers the row the decision table
// leaves implicit: a panicked protected form whose cleanup forms SUCCEED
// before one of them signals.  The successful forms must still run, and the
// panic must still be what propagates.
func TestWithCleanupPanicThenMixedCleanup(t *testing.T) {
	t.Parallel()
	env := withCleanupTestEnv(t)
	require.NotEqual(t, lisp.LError, evalWithCleanup(t, env, `(set 'trace (vector))`).Type)

	got := evalWithCleanup(t, env, `
		(with-cleanup ((append! trace "a")
		                 (error 'cleanup-error "boom")
		                 (append! trace "c"))
		  (host-panic))`)

	require.True(t, lisp.IsInternalPanic(got),
		"the panic must survive a cleanup form that signals after another succeeded")
	require.Equal(t, `(vector "a")`, evalWithCleanup(t, env, `trace`).String(),
		"cleanup forms before the signalling one must run, and those after it must not")
}
