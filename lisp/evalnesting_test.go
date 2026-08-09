// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Evaluation-nesting limit tests (issue #316).
//
// The quantity under test is how deeply LEnv.eval recurses into itself, which
// is what actually consumes Go stack. It is NOT the call-stack height:
// evalSExprCells evaluates a call's arguments before pushing the call's frame,
// so ((identity (identity ... 1))) recurses through the whole evaluator at
// physical height zero. MaxHeightPhysical -- the limit written to stop
// unbounded recursion from aborting the process with a Go stack overflow --
// cannot see that shape at all.
//
// Two things make these tests worth their runtime:
//
//   - nestedCallGo builds the nesting with no parser involvement, which is the
//     embedder-facing half of the gap.
//   - macroNestSrc generates the nesting at macro-expansion time from an
//     integer, which is the ELPS-facing half: it defeats
//     rdparser.DefaultMaxParseDepth, the limit that was incidentally
//     containing this before. Measured on linux/amd64 against the pre-fix
//     tree, `(nest 800000)` -- about 120 bytes of source, every documented
//     limit at its default -- reached `fatal error: stack overflow` and took
//     the process down. That is why the fix is a limit and not a doc note.

// nestedCallGo builds (identity (identity ... 1)) of the given nesting depth
// directly as an LVal tree. No parser, so rdparser.DefaultMaxParseDepth does
// not apply.
func nestedCallGo(depth int) *lisp.LVal {
	v := lisp.Int(1)
	for i := 0; i < depth; i++ {
		v = lisp.SExpr([]*lisp.LVal{lisp.Symbol("identity"), v})
	}
	return v
}

// macroNestSrc returns ELPS source, of constant size regardless of depth, that
// generates depth levels of argument nesting during macro expansion.
func macroNestSrc(depth int) string {
	return fmt.Sprintf(`
(defmacro nest (n)
  (if (<= n 0)
      1
      (quasiquote (identity (nest (unquote (- n 1)))))))
(nest %d)
`, depth)
}

// TestArgumentNestingIsNotBoundedByStackHeight is the ground truth the issue
// reports: argument evaluation recurses through the Go evaluator without
// pushing a single call frame, so no stack-height limit constrains it.
//
// A five-frame physical ceiling -- low enough that three levels of ordinary
// non-tail recursion is refused -- permits 5,000 levels of argument nesting.
func TestArgumentNestingIsNotBoundedByStackHeight(t *testing.T) {
	tiny := []lisp.Config{
		lisp.WithMaximumPhysicalStackHeight(5),
		lisp.WithMaxEvalNesting(-1), // disabled: isolate what stack height sees
	}

	// Non-tail recursion is refused almost immediately under this ceiling.
	env := newLimitTestEnv(t, tiny...)
	require.NotEqual(t, lisp.LError,
		env.LoadString("test", `(defun rec (n) (if (= n 0) 0 (+ 1 (rec (- n 1)))))`).Type)
	res := env.LoadString("test", `(rec 10)`)
	require.Equal(t, lisp.LError, res.Type,
		"10 levels of non-tail recursion must exceed a 5-frame ceiling")
	assert.Contains(t, res.String(), "physical stack height exceeded maximum")

	// The same ceiling permits three orders of magnitude more argument
	// nesting, because argument evaluation pushes no frames.
	env = newLimitTestEnv(t, tiny...)
	res = env.Eval(nestedCallGo(5000))
	require.NotEqual(t, lisp.LError, res.Type,
		"5,000 levels of argument nesting must be invisible to a 5-frame ceiling: %v", res)
	assert.Equal(t, "1", res.String())
	assert.Empty(t, env.Runtime.Stack.Frames)
}

// TestEvalNestingLimitBoundsArgumentNesting pins the fix: the nesting limit
// sees exactly the shape stack height cannot.
func TestEvalNestingLimitBoundsArgumentNesting(t *testing.T) {
	env := newLimitTestEnv(t,
		lisp.WithMaximumPhysicalStackHeight(0), // off: prove nesting is what trips
		lisp.WithMaxEvalNesting(200))
	res := env.Eval(nestedCallGo(5000))
	require.Equal(t, lisp.LError, res.Type, "expected the nesting limit to trip, got %v", res)
	msg := res.String()
	assert.Contains(t, msg, "evaluation nesting depth exceeded maximum")
	assert.Contains(t, msg, "WithMaxEvalNesting",
		"the error must say which knob raises the limit")
	assert.Contains(t, msg, lisp.CondEvalNestingExceeded,
		"the error must carry a distinguishable condition")
	assert.NotContains(t, msg, "stack height exceeded",
		"the message must not read like a call-stack overflow -- it is a different quantity")
}

// TestMacroGeneratedNestingIsBounded is the load-bearing regression test.
//
// The nesting here is generated during macro expansion from an integer
// argument, so the source is a constant ~120 bytes at any depth and
// rdparser.DefaultMaxParseDepth never sees it. Against the pre-fix tree this
// program ran to completion at depth 400,000 and killed the process outright
// at 800,000; the ELPS call stack was empty throughout.
func TestMacroGeneratedNestingIsBounded(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(500))
	res := env.LoadString("test", macroNestSrc(20000))
	require.Equal(t, lisp.LError, res.Type,
		"macro-generated nesting must be bounded, got %v", res)
	assert.Contains(t, res.String(), "evaluation nesting depth exceeded maximum")

	// The same program just under the limit still evaluates: the guard bounds
	// depth, it does not break recursive macros.
	env = newLimitTestEnv(t, lisp.WithMaxEvalNesting(500))
	res = env.LoadString("test", macroNestSrc(100))
	require.NotEqual(t, lisp.LError, res.Type, "100 levels must be permitted: %v", res)
	assert.Equal(t, "1", res.String())
}

// TestDefaultLimitsContainFatalNesting runs, at the DEFAULTS an embedder gets
// from lisp.NewEnv(nil), the exact program that took the pre-fix process down
// with "fatal error: stack overflow". Every other test here configures a small
// limit to stay cheap; this one is deliberately expensive, because a default
// that is merely present but set above the crash point would pass all of them.
//
// Cost: reaching the 100,000-level default consumes ~140MB of goroutine stack
// (measured ~1.4KB per level). That is the price of asserting the default
// rather than the mechanism.
func TestDefaultLimitsContainFatalNesting(t *testing.T) {
	if testing.Short() {
		t.Skip("allocates ~140MB of goroutine stack")
	}
	for _, tt := range []struct {
		name string
		eval func(env *lisp.LEnv) *lisp.LVal
	}{
		{
			// The ELPS-source path: constant-size source, nesting
			// generated at expansion time, parse depth irrelevant.
			name: "macro generated",
			eval: func(env *lisp.LEnv) *lisp.LVal {
				return env.LoadString("test", macroNestSrc(800000))
			},
		},
		{
			// The embedder path: an AST built through the Go API, which
			// never passes the parser at all.
			name: "go api",
			eval: func(env *lisp.LEnv) *lisp.LVal {
				return env.Eval(nestedCallGo(800000))
			},
		},
	} {
		t.Run(tt.name, func(t *testing.T) {
			env := newLimitTestEnv(t) // stock defaults, no overrides
			res := tt.eval(env)
			require.Equal(t, lisp.LError, res.Type,
				"800,000 levels must be refused at the defaults, got %v", res)
			assert.Contains(t, res.String(), "evaluation nesting depth exceeded maximum")
			assert.Empty(t, env.Runtime.Stack.Frames,
				"the ELPS call stack was empty the whole time -- which is exactly why"+
					" MaxHeightPhysical could not see this")
		})
	}
}

// TestEvalNestingLimitBoundary pins the exact off-by-one, so < vs <= cannot
// drift. env.Eval of an n-deep nesting reaches nesting depth n+1: one level
// for the outermost expression plus one per nested argument.
func TestEvalNestingLimitBoundary(t *testing.T) {
	const limit = 100

	for _, tt := range []struct {
		depth   int
		wantErr bool
	}{
		{depth: limit - 2, wantErr: false},
		{depth: limit - 1, wantErr: false},
		{depth: limit, wantErr: true},
	} {
		t.Run(fmt.Sprintf("depth_%d", tt.depth), func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(limit))
			res := env.Eval(nestedCallGo(tt.depth))
			if !tt.wantErr {
				require.NotEqual(t, lisp.LError, res.Type,
					"depth %d must be allowed under a %d-level limit: %v", tt.depth, limit, res)
				return
			}
			require.Equal(t, lisp.LError, res.Type,
				"depth %d must exceed a %d-level limit", tt.depth, limit)
			assert.Contains(t, res.String(),
				fmt.Sprintf("evaluation nesting depth exceeded maximum: %d", limit+1),
				"the error must report the depth that exceeded the limit")
		})
	}
}

// TestEvalNestingDefaults pins the default policy. The guard is on by default
// for the same reason the physical-height guard is: with it off, the failure
// mode is an unrecoverable process abort.
func TestEvalNestingDefaults(t *testing.T) {
	assert.Equal(t, 100000, lisp.DefaultMaxEvalNesting)

	for _, tt := range []struct {
		name string
		rt   *lisp.Runtime
	}{
		{"StandardRuntime", lisp.StandardRuntime()},
		{"NewEnv", lisp.NewEnv(nil).Runtime},
	} {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, 0, tt.rt.MaxEvalNesting,
				"the zero value must mean 'use the default', not 'disabled'")
			assert.Equal(t, lisp.DefaultMaxEvalNesting, tt.rt.MaxEvalNestingDepth())
			assert.Equal(t, 0, tt.rt.EvalNesting())
		})
	}

	// An explicit value wins; a negative value disables the check.
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(1234))
	assert.Equal(t, 1234, env.Runtime.MaxEvalNestingDepth())
	env = newLimitTestEnv(t, lisp.WithMaxEvalNesting(-1))
	assert.Equal(t, 0, env.Runtime.MaxEvalNestingDepth(),
		"a negative limit reports as disabled")
}

// TestEvalNestingLimitCanBeDisabled pins the escape hatch: an embedder that
// has some other bound on expression depth can turn the check off.
func TestEvalNestingLimitCanBeDisabled(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(50))
	require.Equal(t, lisp.LError, env.Eval(nestedCallGo(200)).Type,
		"the limit must trip when configured")

	env = newLimitTestEnv(t, lisp.WithMaxEvalNesting(-1))
	res := env.Eval(nestedCallGo(200))
	require.NotEqual(t, lisp.LError, res.Type, "a negative limit must disable the check: %v", res)
	assert.Equal(t, "1", res.String())
}

// TestEvalNestingIsRecoverable is the point of the whole exercise: what was a
// runtime.throw that no handler-bind and no recover() could intercept is now
// an ordinary condition an ELPS program can catch.
func TestEvalNestingIsRecoverable(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(100))
	res := env.LoadString("test", `
(defmacro nest (n)
  (if (<= n 0)
      1
      (quasiquote (identity (nest (unquote (- n 1)))))))
(handler-bind ([eval-nesting-exceeded (lambda (c &rest args) 'caught)])
  (nest 10000))
`)
	require.NotEqual(t, lisp.LError, res.Type, "the condition must be catchable: %v", res)
	assert.Equal(t, "'caught", res.String())
}

// TestEvalNestingCounterUnwinds pins that the counter is decremented on every
// exit path. A counter that leaks on the error path would degrade a long-lived
// Runtime into one that refuses to evaluate anything -- a far worse failure
// than the one being guarded against.
func TestEvalNestingCounterUnwinds(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(100))

	for _, tt := range []struct {
		name string
		src  string
	}{
		{"success", `(+ 1 (+ 1 (+ 1 1)))`},
		{"lisp error", `(error 'boom "x")`},
		{"caught error", `(ignore-errors (error 'boom "x"))`},
		{"nesting limit", `(defun f (n) (+ 1 (f n))) (f 0)`},
		{"undefined symbol", `(no-such-function 1 2 3)`},
	} {
		t.Run(tt.name, func(t *testing.T) {
			env.LoadString("test", tt.src)
			assert.Equal(t, 0, env.Runtime.EvalNesting(),
				"the nesting counter must return to zero after evaluation")
			assert.Empty(t, env.Runtime.Stack.Frames,
				"the call stack must unwind too")
		})
	}

	// And the runtime is still usable afterwards.
	res := env.LoadString("test", `(+ 1 2)`)
	require.NotEqual(t, lisp.LError, res.Type, "the runtime must survive: %v", res)
	assert.Equal(t, "3", res.String())
}

// TestEvalNestingErrorReportsSource pins the reason the guard is a counter and
// not a stack frame. evalSExprCells pushes nothing while evaluating arguments
// precisely so that an error raised there reports the argument's own location
// and an uncluttered stack; a frame pushed per nesting level would have
// rewritten both. The error must still point at real source.
func TestEvalNestingErrorReportsSource(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxEvalNesting(100))
	res := env.LoadString("nesting.lisp", macroNestSrc(1000))
	require.Equal(t, lisp.LError, res.Type)
	assert.Contains(t, res.String(), "nesting.lisp:",
		"the error must carry the source location of the expression that overflowed")

	// An ordinary error raised while evaluating a nested argument is
	// unaffected by the guard: same location, same short stack.
	env = newLimitTestEnv(t)
	res = env.LoadString("argerr.lisp", "(identity (identity (error 'boom \"deep\")))")
	require.Equal(t, lisp.LError, res.Type)
	msg := res.String()
	assert.Contains(t, msg, "argerr.lisp:1:")
	assert.Contains(t, msg, "boom")
	assert.NotContains(t, strings.ToLower(msg), "nesting depth")
}
