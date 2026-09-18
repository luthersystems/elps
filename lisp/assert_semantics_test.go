// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #657: assert must evaluate the supplied expression over its original
// values. Copying a constructed expression silently redirects mutations.
func TestAssertEvaluatesOriginalRuntimeData(t *testing.T) {
	for _, tc := range []struct{ name, expr, want string }{
		{"vector", `(let ((v (vector 1))) (eval (quasiquote (assert (append! (unquote v) 2)))) v)`, `(vector 1 2)`},
		{"bytes", `(let ((b (to-bytes "A"))) (eval (quasiquote (assert (append! (unquote b) 66)))) (to-string b))`, `"AB"`},
		{"map", `(let ((m (sorted-map "a" 1))) (eval (quasiquote (assert (assoc! (unquote m) "b" 2)))) m)`, `(sorted-map "a" 1 "b" 2)`},
		{"list", `(let ((xs (list 2 1))) (eval (quasiquote (assert (stable-sort < (unquote xs))))) xs)`, `'(1 2)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := newCallSemanticsEnv(t).LoadString("assert-values.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, tc.want, got.String())
		})
	}
}

func TestAssertPreservesLiteralProtection(t *testing.T) {
	for _, expr := range []string{
		`(assert (stable-sort < '(2 1)))`,
		`(assert (slice 'vector '(1) 0 1))`,
	} {
		t.Run(expr, func(t *testing.T) {
			got := newCallSemanticsEnv(t).LoadString("assert-literal.lisp", expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, lisp.CondModifyLiteral, got.Str, "assert must not turn parsed literals into mutable copies")
		})
	}
}

func TestAssertEvaluatesTestOnceAndMessageOnlyOnFailure(t *testing.T) {
	for _, tc := range []struct {
		name  string
		value *lisp.LVal
		truth bool
	}{
		{"true", lisp.Bool(true), true},
		{"false", lisp.Bool(false), false},
		{"nil", lisp.Nil(), false},
		{"zero is true", lisp.Int(0), true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			tests, messages := 0, 0
			env.AddBuiltins(true,
				elpsutil.Function("test-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					tests++
					return tc.value
				}),
				elpsutil.Function("message-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					messages++
					return lisp.String("detail")
				}))
			got := env.LoadString("assert-once.lisp", `(assert (test-value) "custom {}" (message-value))`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			assert.Equal(t, 1, tests, "the test must execute exactly once")
			if tc.truth {
				assert.True(t, got.IsNil(), "%v", got)
				assert.Zero(t, messages, "successful assertions must not evaluate message arguments")
			} else {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				assert.Equal(t, "error", got.Str)
				assert.Contains(t, got.String(), "custom detail")
				assert.Equal(t, 1, messages)
			}
		})
	}
	got := newCallSemanticsEnv(t).LoadString("assert-default.lisp", `(assert (= 1 2))`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.Contains(t, got.String(), "assertion failure: (= 1 2)")
}

func TestAssertPreservesTestErrorIdentity(t *testing.T) {
	for _, panicError := range []bool{false, true} {
		name := "ordinary condition"
		if panicError {
			name = "recovered host panic"
		}
		t.Run(name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			original := env.LoadString("original.lisp", `(error 'test-failure "original data" 17)`)
			if panicError {
				env.AddBuiltins(true, elpsutil.Function("host-failure", lisp.Formals(),
					func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { panic("assert host panic") }))
				original = env.LoadString("original.lisp", `(host-failure)`)
			}
			require.Equal(t, lisp.LError, original.Type)
			require.Equal(t, panicError, lisp.IsInternalPanic(original))
			stack, message := original.CallStack(), original.String()
			messages := 0
			env.AddBuiltins(true, elpsutil.Function("message-value", lisp.Formals(),
				func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { messages++; return lisp.String("unexpected") }))
			// Host-generated code may hold an error value directly as an
			// unevaluated test; assert must return that same condition.
			got := env.Eval(lisp.SExpr([]*lisp.LVal{
				lisp.Symbol("assert"), original, lisp.String("unused {}"),
				lisp.SExpr([]*lisp.LVal{lisp.Symbol("message-value")}),
			}))
			require.Same(t, original, got)
			assert.Same(t, stack, got.CallStack())
			assert.Equal(t, message, got.String())
			assert.Equal(t, panicError, lisp.IsInternalPanic(got))
			assert.Zero(t, messages, "test errors must bypass assertion message evaluation")
		})
	}
}
