// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #657: initializers must capture the scope where they run, rather
// than a map that later gains bindings outside the closure's lexical scope.
func TestLetInitializerClosureScope(t *testing.T) {
	for _, tc := range []struct {
		name, source, want string
	}{
		{"parallel enclosing scope", `(let ((x 1)) (let ((x 2) (f (lambda () x))) (f)))`, "1"},
		{"sequential earlier binding", `(let* ((x 1) (f (lambda () x))) (f))`, "1"},
		{"sequential future binding", `(let ((bar 0)) (let* ((foo (lambda () bar)) (bar 1)) (foo)))`, "0"},
		{"sequential repeated name", `(let* ((x 1) (f (lambda () x)) (x 2)) (list (f) x))`, "'(1 2)"},
		{"sequential live captured binding", `(let* ((x 1) (f (lambda () (set! x (+ x 1)) x)) (x 10)) (list (f) x (f) x))`, "'(2 10 3 10)"},
		{"closure inside vector", `(let* ((x 1) (fs (vector (lambda () x))) (x 2)) ((aref fs 0)))`, "1"},
		{"shadowed function in parallel initializer", `(let ((f (lambda (n) 7))) (let ((f (lambda (n) (if (= n 0) 0 (f (- n 1)))))) (f 1)))`, "7"},
		{"shadowed function in sequential initializer", `(let ((f (lambda (n) 7))) (let* ((f (lambda (n) (if (= n 0) 0 (f (- n 1)))))) (f 1)))`, "7"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			result := env.LoadString("let-scope.lisp", tc.source)
			require.NoError(t, lisp.GoError(result))
			assert.Equal(t, tc.want, result.String())
		})
	}
}

func TestLetInitializersCannotCaptureTheirOwnBinding(t *testing.T) {
	for _, op := range []string{"let", "let*"} {
		t.Run(op, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			// The formerly disabled let* recursion regression, bounded to
			// one recursive call so the broken implementation finishes too.
			source := fmt.Sprintf(`(%s ((f (lambda (x) (if (= 0 x) 0 (f (- x 1)))))) (f 1))`, op)
			result := env.LoadString("let-scope.lisp", source)
			require.Equal(t, lisp.LError, result.Type)
			assert.False(t, lisp.IsInternalPanic(result))
			require.Len(t, result.Cells, 1)
			assert.Equal(t, "unbound symbol: f", result.Cells[0].Str)
		})
	}
}

func TestLetInitializersRetainOrderAndValues(t *testing.T) {
	for _, op := range []string{"let", "let*"} {
		t.Run(op, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			result := env.LoadString("let-scope.lisp", fmt.Sprintf(`
				(let ((events (vector)))
				  (%s ((a (progn (append! events 'a) 1))
				       (b (progn (append! events 'b) 2)))
				    (append! events 'body))
				  events)`, op))
			require.NoError(t, lisp.GoError(result))
			assert.Equal(t, "(vector 'a 'b 'body)", result.String())
			// Binding preserves data, including its quote depth and seal.
			result = env.LoadString("let-scope.lisp", fmt.Sprintf(`(%s ((value ''(1 2))) value)`, op))
			assert.Equal(t, "''(1 2)", result.String())
			result = env.LoadString("let-scope.lisp", fmt.Sprintf(`(%s ((value '(1 2))) (stable-sort > value))`, op))
			require.Equal(t, lisp.LError, result.Type)
			assert.Equal(t, lisp.CondModifyLiteral, result.Str)
		})
	}
}

func TestLetInitializerFailureStopsLaterForms(t *testing.T) {
	for _, op := range []string{"let", "let*"} {
		t.Run(op, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			original := env.ErrorCondition("initializer-failure", lisp.String("exact failure"), lisp.Int(17))
			laterCalls := 0
			env.AddBuiltins(true,
				elpsutil.Function("fail-first", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return original }),
				elpsutil.Function("later", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { laterCalls++; return lisp.Nil() }))
			result := env.LoadString("let-scope.lisp", fmt.Sprintf(`(%s ((a (fail-first)) (b (later))) (later))`, op))
			assert.Same(t, original, result, "initializer errors retain their condition, data, stack and identity")
			assert.Zero(t, laterCalls)
		})
	}
}

func TestLetInitializerCancellationStopsLaterForms(t *testing.T) {
	for _, op := range []string{"let", "let*"} {
		t.Run(op, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			laterCalls := 0
			env.AddBuiltins(true,
				elpsutil.Function("cancel-now", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { cancel(); return lisp.Int(1) }),
				elpsutil.Function("later", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { laterCalls++; return lisp.Nil() }))
			result := env.LoadStringContext(ctx, "let-scope.lisp", fmt.Sprintf(`(%s ((a (cancel-now)) (b (later))) (later))`, op))
			require.Equal(t, lisp.LError, result.Type)
			assert.Equal(t, lisp.CondContextCancelled, result.Str)
			assert.Zero(t, laterCalls)
		})
	}
}
