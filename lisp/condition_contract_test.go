// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// This file is the condition-system contract (issue #747, item 5): the
// guarantees of handler-bind, custom conditions carrying data, ignore-errors
// and with-cleanup that macros and embedders may rely on. Each subtest pins
// one clause of the contract documented in docs/lang.md ("The
// condition-system contract"). Changing any expectation here is a language
// change, not a test fix.

// contractEnv returns a user env with a counter and a builtin that panics,
// so a contract can observe side effects and a recovered Go panic.
func contractEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := newSortErrorEnv(t)
	env.AddBuiltins(true, elpsutil.Function("host-panic", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic("host defect") }))
	requireContractOK(t, env, `(set 'log ())
(defun note (x) (set! log (append 'list log x)) x)`)
	return env
}

func requireContractOK(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("contract.lisp", src)
	require.NotEqual(t, lisp.LError, v.Type, "unexpected error: %v", v)
	return v
}

func requireContractValue(t *testing.T, env *lisp.LEnv, src, want string) {
	t.Helper()
	require.Equal(t, want, requireContractOK(t, env, src).String())
}

func requireContractLog(t *testing.T, env *lisp.LEnv, want string) {
	t.Helper()
	requireContractValue(t, env, `log`, want)
}

func TestConditionContractHandlerBind(t *testing.T) {
	t.Parallel()
	t.Run("handler value is the form value", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (c &rest data) 'recovered)))
			   (note 'before) (error 'boom 1) (note 'after))`, `'recovered`)
		// The rest of the body is abandoned once the condition is signalled.
		requireContractLog(t, env, `'('before)`)
	})
	t.Run("no error returns the last body value", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((condition (lambda (&rest _) 'unused))) 1 2 3)`, `3`)
	})
	t.Run("most specific binding wins, whatever the order", func(t *testing.T) {
		env := contractEnv(t)
		// luthersystems/elps#831: the catch-all runs only when nothing more
		// specific matches.  Before #831 the first match in source order ran,
		// so this returned 'catch-all.
		requireContractValue(t, env,
			`(handler-bind ((condition (lambda (&rest _) 'catch-all))
			                (boom (lambda (&rest _) 'specific)))
			   (error 'boom))`, `'specific`)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (&rest _) 'first))
			                (boom (lambda (&rest _) 'second)))
			   (error 'boom))`, `'first`)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (&rest _) 'specific))
			                (condition (lambda (&rest _) 'catch-all)))
			   (error 'boom))`, `'specific`)
	})
	t.Run("non-matching condition propagates to an outer handler", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((other (lambda (&rest _) 'outer)))
			   (handler-bind ((boom (lambda (&rest _) 'inner)))
			     (error 'other)))`, `'outer`)
	})
	t.Run("innermost matching handler runs first", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (&rest _) 'outer)))
			   (handler-bind ((boom (lambda (&rest _) 'inner)))
			     (error 'boom)))`, `'inner`)
	})
	t.Run("handler receives the condition symbol and the evaluated data", func(t *testing.T) {
		env := contractEnv(t)
		// error evaluates its arguments like any function; the handler then
		// receives those values as-is, without evaluating them again.
		requireContractValue(t, env,
			`(handler-bind ((condition (lambda (c &rest data) (list c data))))
			   (error 'boom (+ 1 2) 'unbound-symbol '(not-a-call 1)))`,
			`'('boom '(3 'unbound-symbol '(not-a-call 1)))`)
	})
	t.Run("error raised by a handler escapes its own handler-bind", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((second (lambda (c &rest _) (list 'outer c))))
			   (handler-bind ((first (lambda (&rest _) (error 'second)))
			                  (second (lambda (&rest _) 'sibling-not-reached)))
			     (error 'first)))`, `'('outer 'second)`)
	})
	t.Run("rethrow declines and preserves the original data", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (c &rest data) (list 'outer c data))))
			   (handler-bind ((boom (lambda (c &rest data) (note c) (rethrow))))
			     (error 'boom "detail" 7)))`, `'('outer 'boom '("detail" 7))`)
		requireContractLog(t, env, `'('boom)`)
	})
}

func TestConditionContractCustomConditionData(t *testing.T) {
	t.Parallel()
	t.Run("a structured payload reaches the handler intact", func(t *testing.T) {
		env := contractEnv(t)
		requireContractOK(t, env, `
(defun signal-out-of-range (value limit)
  (error 'out-of-range (sorted-map "value" value "limit" limit)))`)
		requireContractValue(t, env,
			`(handler-bind ((out-of-range (lambda (c payload)
			                                (list c (get payload "value") (get payload "limit")))))
			   (signal-out-of-range 12 10))`, `'('out-of-range 12 10)`)
	})
	t.Run("handler mutation of its copy does not change the error", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((out-of-range (lambda (c payload) (get payload "value"))))
			   (handler-bind ((out-of-range (lambda (c payload)
			                                  (assoc! payload "value" 'changed)
			                                  (rethrow))))
			     (error 'out-of-range (sorted-map "value" 12))))`, `12`)
	})
	t.Run("an uncaught custom condition reaches the host by name", func(t *testing.T) {
		env := contractEnv(t)
		v := env.LoadString("contract.lisp", `(error 'out-of-range "value" 12)`)
		require.Equal(t, lisp.LError, v.Type)
		require.Equal(t, "out-of-range", v.Str)
		require.Equal(t, "out-of-range", (*lisp.ErrorVal)(v).Condition())
		require.Len(t, v.Cells, 2)
		require.Equal(t, "value", v.Cells[0].Str)
		require.Equal(t, 12, v.Cells[1].Int)
		require.False(t, lisp.IsInternalPanic(v))
	})
}

func TestConditionContractIgnoreErrors(t *testing.T) {
	t.Parallel()
	env := contractEnv(t)
	requireContractValue(t, env, `(ignore-errors)`, `()`)
	requireContractValue(t, env, `(ignore-errors 1 2 3)`, `3`)
	requireContractValue(t, env, `(ignore-errors (note 'a) (error 'boom) (note 'b))`, `()`)
	requireContractLog(t, env, `'('a)`)
	// A returned () is indistinguishable from a caught error; that is the
	// contract, which is why handler-bind is preferred.
	requireContractValue(t, env, `(ignore-errors ())`, `()`)

	t.Run("does not suppress internal-panic", func(t *testing.T) {
		env := contractEnv(t)
		v := env.LoadString("contract.lisp", `(ignore-errors (host-panic))`)
		require.Equal(t, lisp.LError, v.Type)
		require.True(t, lisp.IsInternalPanic(v))
	})
	t.Run("a forged internal-panic is an ordinary condition", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env, `(ignore-errors (error 'internal-panic "forged"))`, `()`)
	})
}

func TestConditionContractInternalPanic(t *testing.T) {
	t.Parallel()
	t.Run("the catch-all condition does not match it", func(t *testing.T) {
		env := contractEnv(t)
		v := env.LoadString("contract.lisp",
			`(handler-bind ((condition (lambda (&rest _) 'swallowed))) (host-panic))`)
		require.Equal(t, lisp.LError, v.Type)
		require.True(t, lisp.IsInternalPanic(v))
	})
	t.Run("naming it explicitly intercepts it", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((internal-panic (lambda (c &rest _) c))) (host-panic))`,
			`'internal-panic`)
	})
}

func TestConditionContractWithCleanup(t *testing.T) {
	t.Parallel()
	t.Run("returns the body value and runs cleanup after the body", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(with-cleanup ((note 'cleanup)) (note 'body) 'result)`, `'result`)
		requireContractLog(t, env, `'('body 'cleanup)`)
	})
	t.Run("does not catch and cleans up before the outer handler", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((boom (lambda (c &rest _) (note 'handler) c)))
			   (with-cleanup ((note 'cleanup)) (error 'boom)))`, `'boom`)
		requireContractLog(t, env, `'('cleanup 'handler)`)
	})
	t.Run("nested cleanups run innermost first", func(t *testing.T) {
		env := contractEnv(t)
		env.LoadString("contract.lisp",
			`(with-cleanup ((note 'outer)) (with-cleanup ((note 'inner)) (error 'boom)))`)
		requireContractLog(t, env, `'('inner 'outer)`)
	})
	t.Run("a signalling cleanup replaces an ordinary error", func(t *testing.T) {
		env := contractEnv(t)
		requireContractValue(t, env,
			`(handler-bind ((condition (lambda (c &rest _) c)))
			   (with-cleanup ((error 'cleanup-failed) (note 'skipped)) (error 'body-failed)))`,
			`'cleanup-failed`)
		requireContractLog(t, env, `()`)
	})
	t.Run("never masks an internal-panic from the body", func(t *testing.T) {
		env := contractEnv(t)
		v := env.LoadString("contract.lisp",
			`(handler-bind ((condition (lambda (&rest _) 'swallowed)))
			   (with-cleanup ((note 'cleanup) (error 'cleanup-failed)) (host-panic)))`)
		require.Equal(t, lisp.LError, v.Type)
		require.True(t, lisp.IsInternalPanic(v), "got %v", v)
		requireContractLog(t, env, `'('cleanup)`)
	})
	t.Run("an internal-panic from cleanup wins over the body error", func(t *testing.T) {
		env := contractEnv(t)
		v := env.LoadString("contract.lisp",
			`(with-cleanup ((host-panic)) (error 'body-failed))`)
		require.Equal(t, lisp.LError, v.Type)
		require.True(t, lisp.IsInternalPanic(v), "got %v", v)
	})
}

// TestConditionContractDeterministic pins that handling a condition is
// deterministic: the same program yields the same value and the same step
// count in fresh environments, which embedders that meter steps rely on.
func TestConditionContractDeterministic(t *testing.T) {
	t.Parallel()
	const src = `(handler-bind ((out-of-range (lambda (c payload) (get payload "value"))))
  (with-cleanup ((note 'cleanup))
    (error 'out-of-range (sorted-map "value" 12))))`
	var steps []int64
	for range 3 {
		env := contractEnv(t)
		before := env.Runtime.TotalSteps()
		requireContractValue(t, env, src, `12`)
		steps = append(steps, env.Runtime.TotalSteps()-before)
	}
	require.Equal(t, steps[0], steps[1])
	require.Equal(t, steps[0], steps[2])
}
