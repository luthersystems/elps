// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newCallSemanticsEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	return env
}

func TestFunctionRejectsNonSymbolFormals(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr string
	}{
		{"lambda", `((lambda (123) (set! ran true)) 9)`},
		{"defun required", `(progn (defun bad (123) (set! ran true)) (bad 9))`},
		{"defun optional", `(progn (defun bad (&optional 123) (set! ran true)) (bad))`},
		{"defun rest", `(progn (defun bad (&rest 123) (set! ran true)) (bad 1 2))`},
		{"defun key", `(progn (defun bad (&key 123) (set! ran true)) (bad))`},
		{"defmacro", `(progn (defmacro bad (123) (set! ran true) 42) (bad 9))`},
		{"flet", `(flet ((bad (123) (set! ran true))) (bad 9))`},
		{"labels", `(labels ((bad (123) (set! ran true))) (bad 9))`},
		{"macrolet", `(macrolet ((bad (123) (set! ran true) 42)) (bad 9))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("formals.lisp", `(set 'ran false)`)))
			result := env.LoadString("formals.lisp", tc.expr)
			assert.Equal(t, "false", env.LoadString("formals.lisp", "ran").String(), "invalid formals must prevent the function or macro body from running")
			require.Equal(t, lisp.LError, result.Type)
			assert.Contains(t, result.String(), "non-symbol")
			assert.False(t, lisp.IsInternalPanic(result))
		})
	}
}

func TestFunctionPropagatesConstantBindingErrors(t *testing.T) {
	for _, tc := range []struct {
		name     string
		formals  string
		args     string
		constant string
	}{
		{"required true", "true", "false", "true"},
		{"required false", "false", "true", "false"},
		{"optional supplied", "&optional true", "false", "true"},
		{"optional omitted", "&optional false", "", "false"},
		{"rest supplied", "&rest true", "1 2", "true"},
		{"rest empty", "&rest false", "", "false"},
		{"key supplied", "&key true", ":true false", "true"},
		{"key omitted", "&key false", "", "false"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("formals.lisp", `(set 'ran false)`)))
			src := fmt.Sprintf(`((lambda (%s) (set! ran true) 42) %s)`, tc.formals, tc.args)
			result := env.LoadString("formals.lisp", src)
			assert.Equal(t, "false", env.LoadString("formals.lisp", "ran").String(), "binding errors must abort before the body")
			require.Equal(t, lisp.LError, result.Type)
			assert.Contains(t, result.String(), "cannot rebind constant: "+tc.constant)
			assert.False(t, lisp.IsInternalPanic(result))
		})
	}
}

func TestFunctionRejectsMalformedHostFormals(t *testing.T) {
	for _, formals := range []*lisp.LVal{
		lisp.QExpr([]*lisp.LVal{lisp.Int(123)}),
		lisp.QExpr([]*lisp.LVal{lisp.Symbol(lisp.OptArgSymbol), lisp.Int(123)}),
		lisp.QExpr([]*lisp.LVal{lisp.Symbol(lisp.VarArgSymbol), lisp.Int(123)}),
		lisp.QExpr([]*lisp.LVal{lisp.Symbol(lisp.KeyArgSymbol), lisp.Int(123)}),
	} {
		t.Run(formals.String(), func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			called := false
			env.AddBuiltins(true, elpsutil.Function("invalid-host-formals", formals,
				func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
					called = true
					return lisp.Int(42)
				}))
			result := env.LoadString("formals.lisp", `(invalid-host-formals)`)
			assert.False(t, called, "host registrations must not bypass formal symbol validation")
			require.Equal(t, lisp.LError, result.Type)
			assert.Contains(t, result.String(), "non-symbol")
			assert.False(t, lisp.IsInternalPanic(result))
		})
	}
}

func TestLexicalBindingsRejectKeywords(t *testing.T) {
	for _, expr := range []string{
		`(let ((:k 1)) (set! ran true))`,
		`(let* ((:k 1)) (set! ran true))`,
		`((lambda (:k) (set! ran true)) 1)`,
		`((lambda (&optional :k) (set! ran true)))`,
		`((lambda (&optional :k) (set! ran true)) 1)`,
		`((lambda (&rest :k) (set! ran true)) 1 2)`,
		`((lambda (&rest :k) (set! ran true)))`,
		`((lambda (&key :k) (set! ran true)))`,
		`(dotimes (:k 1) (set! ran true))`,
		`(flet ((:k () 1)) (set! ran true))`,
		`(labels ((:k () 1)) (set! ran true))`,
	} {
		t.Run(expr, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("keyword-bind.lisp", `(set 'ran false)`)))
			result := env.LoadString("keyword-bind.lisp", expr)
			assert.Equal(t, "false", env.LoadString("keyword-bind.lisp", "ran").String())
			require.Equal(t, lisp.LError, result.Type)
			assert.Contains(t, result.String(), "value cannot be assigned to a keyword: :k")
			assert.False(t, lisp.IsInternalPanic(result))
		})
	}
}

func TestThreadEvaluatesInitialValueBeforeSteps(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("thread.lisp", `(set 'events (vector))`)))
			src := fmt.Sprintf(`(%s
				(progn (append! events "initial") 1)
				((progn (append! events "function") +)
				 (progn (append! events "argument") 2))
				((progn (append! events "next-function") +)
				 (progn (append! events "next-argument") 4)))`, thread)
			result := env.LoadString("thread.lisp", src)
			require.Equal(t, "7", result.String())
			assert.Equal(t, `(vector "initial" "function" "argument" "next-function" "next-argument")`, env.LoadString("thread.lisp", "events").String())
		})
	}
}

func TestThreadInitialErrorPreventsStepSideEffects(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("thread.lisp", `(set 'events (vector))`)))
			result := env.LoadString("thread.lisp", fmt.Sprintf(`(%s (error 'abort "initial")
				((progn (append! events "function") list) (append! events "argument")))`, thread))
			assert.Equal(t, "(vector)", env.LoadString("thread.lisp", "events").String())
			require.Equal(t, lisp.LError, result.Type)
			assert.Equal(t, "abort", result.Str)
			require.Len(t, result.Cells, 1)
			assert.Equal(t, "initial", result.Cells[0].Str)
		})
	}
}

func TestThreadStepErrorPreventsFollowingSteps(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			result := env.LoadString("thread.lisp", fmt.Sprintf(`
				(set 'ran false)
				(defun fail (x) (error 'step-error "original"))
				(%s 1 (identity) (fail) ((progn (set! ran true) identity)))`, thread))
			assert.Equal(t, "false", env.LoadString("thread.lisp", "ran").String())
			require.Equal(t, lisp.LError, result.Type)
			assert.Equal(t, "step-error", result.Str)
			require.Len(t, result.Cells, 1)
			assert.Equal(t, "original", result.Cells[0].Str)
		})
	}
}

func TestThreadPreservesReturnedData(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		for _, value := range []string{
			`(car '(unbound-name))`,
			`(car '((progn (set! ran true) 7)))`,
			`'(1 2)`,
			`''(1 2)`,
			`(vector 1 2)`,
		} {
			t.Run(thread+"/"+value, func(t *testing.T) {
				env := newCallSemanticsEnv(t)
				require.NoError(t, lisp.GoError(env.LoadString("thread.lisp", `(set 'ran false)`)))
				baseline := env.LoadString("thread.lisp", value)
				require.NoError(t, lisp.GoError(baseline))
				result := env.LoadString("thread.lisp", fmt.Sprintf(`(%s %s (identity) (identity))`, thread, value))
				assert.Equal(t, "false", env.LoadString("thread.lisp", "ran").String(), "returned list data must never execute as code")
				assert.Equal(t, baseline.Type, result.Type)
				assert.Equal(t, baseline.String(), result.String(), "threading must preserve quote depth and data exactly")
			})
		}
	}
}

func TestThreadPreservesClosureScope(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			result := env.LoadString("thread.lisp", fmt.Sprintf(`
				(let ((x 10))
				  (let ((capture (%s 1 ((lambda (n) (lambda () (+ x n)))))))
				    (set! x 20)
				    (funcall capture)))`, thread))
			assert.Equal(t, "21", result.String(), "thread callbacks must capture the caller's live lexical scope")
		})
	}
}

func TestThreadEmptyAndTailCalls(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			result := env.LoadString("thread.lisp", fmt.Sprintf(`
				(set 'calls 0)
				(%s (progn (set! calls (+ calls 1)) '(1 2)))`, thread))
			assert.Equal(t, "'(1 2)", result.String())
			assert.Equal(t, "1", env.LoadString("thread.lisp", "calls").String())
			// A small stack bound makes loss of final-step tail-call
			// optimization visible without a large or expensive recursion.
			env.Runtime.Stack.MaxHeightPhysical = 40
			result = env.LoadString("thread.lisp", fmt.Sprintf(`
				(defun countdown (n)
				  (if (= n 0) 'done (%s (- n 1) (countdown))))
				(countdown 100)`, thread))
			assert.Equal(t, "'done", result.String())
		})
	}
}

func TestThreadRejectsSpecialFunctions(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		for _, step := range []string{
			`(if (set! ran true) (set! ran true))`,
			`(quote)`,
			`(get-default (sorted-map) (set! ran true))`,
			`(42 (set! ran true))`,
		} {
			t.Run(thread+"/"+step, func(t *testing.T) {
				env := newCallSemanticsEnv(t)
				require.NoError(t, lisp.GoError(env.LoadString("thread.lisp", `(set 'ran false)`)))
				result := env.LoadString("thread.lisp", fmt.Sprintf(`(%s true %s)`, thread, step))
				assert.Equal(t, "false", env.LoadString("thread.lisp", "ran").String(), "invalid steps must be rejected before evaluating their arguments")
				require.Equal(t, lisp.LError, result.Type)
				assert.Contains(t, result.String(), "not a regular function")
				assert.False(t, lisp.IsInternalPanic(result))
			})
		}
	}
}
