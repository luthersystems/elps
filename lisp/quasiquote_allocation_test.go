// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: quasiquote's returned lists are newly built data containers,
// even when the template is source syntax or comes from a macro expansion.
func TestQuasiquoteAllocationBoundaries(t *testing.T) {
	for _, tc := range []struct{ name, expr, want string }{
		{"empty", `(quasiquote ())`, `'()`},
		{"plain exact", `(quasiquote (1 2))`, `'(1 2)`},
		{"plain over", `(quasiquote (1 2 3))`, ""},
		{"nested exact", `(quasiquote ((1 2) (3 4)))`, `'((1 2) (3 4))`},
		{"nested over", `(quasiquote ((1 2 3)))`, ""},
		{"splice exact", `(quasiquote (1 (unquote-splicing '(2))))`, `'(1 2)`},
		{"splice over", `(let ((xs '(1 2))) (quasiquote ((unquote-splicing xs) (unquote-splicing xs))))`, ""},
		{"empty splices", `(quasiquote ((unquote-splicing '()) (unquote-splicing '()) (unquote-splicing '())))`, `'()`},
		{"template larger than result", `(quasiquote ((unquote-splicing '()) (unquote-splicing '(1 2)) (unquote-splicing '())))`, `'(1 2)`},
		{"quote existing storage", `(quote (1 2 3))`, `'(1 2 3)`},
		{"whole unquote existing storage", `(quasiquote (unquote '(1 2 3)))`, `''(1 2 3)`},
		{"nested unquote existing storage", `(quasiquote ((unquote '(1 2 3))))`, `'('(1 2 3))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			env.Runtime.MaxAlloc = 2
			got := env.LoadString("quasiquote-allocation.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if tc.want == "" {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Contains(t, got.String(), "allocation size")
				require.Contains(t, got.String(), "exceeds maximum (2)")
			} else {
				require.NoError(t, lisp.GoError(got))
				require.Equal(t, tc.want, got.String())
			}
		})
	}
}

func TestQuasiquoteAllocationFromGeneratedMacro(t *testing.T) {
	for _, limit := range []int{2, 3} {
		t.Run(fmt.Sprint(limit), func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			// list builds this macro expansion at runtime; its inner quoted
			// list is the template, rather than a parsed quasiquote form.
			setup := env.LoadString("generated-quasiquote.lisp", `(defmacro generated-quote () (list (car '(quasiquote)) (list 1 2 3)))`)
			require.NoError(t, lisp.GoError(setup))
			env.Runtime.MaxAlloc = limit
			got := env.LoadString("generated-quasiquote.lisp", `(generated-quote)`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if limit == 2 {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Contains(t, got.String(), "allocation size 3 exceeds maximum (2)")
			} else {
				require.NoError(t, lisp.GoError(got))
				require.Equal(t, `''(1 2 3)`, got.String())
			}
		})
	}
}

func TestQuasiquoteAllocationStopsLaterUnquotes(t *testing.T) {
	for _, limit := range []int{2, 4} {
		t.Run(fmt.Sprint(limit), func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			env.Runtime.MaxAlloc = limit
			var events []string
			for _, callback := range []struct {
				name  string
				value *lisp.LVal
			}{
				{"first-value", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})},
				{"second-value", lisp.QExpr([]*lisp.LVal{lisp.Int(3)})},
				{"last-value", lisp.Int(4)},
			} {
				env.AddBuiltins(true, elpsutil.Function(callback.name, lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					events = append(events, callback.name)
					return callback.value
				}))
			}
			got := env.LoadString("quasiquote-limit-order.lisp", `(quasiquote ((unquote-splicing (first-value)) (unquote-splicing (second-value)) (unquote (last-value))))`)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if limit == 2 {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Contains(t, got.String(), "allocation size 3 exceeds maximum (2)")
				require.Equal(t, []string{"first-value", "second-value"}, events)
			} else {
				require.NoError(t, lisp.GoError(got))
				require.Equal(t, `'(1 2 3 4)`, got.String())
				require.Equal(t, []string{"first-value", "second-value", "last-value"}, events)
			}
		})
	}
}

func TestQuasiquoteAllocationPreservesFirstError(t *testing.T) {
	for _, marker := range []string{"unquote", "unquote-splicing"} {
		for _, marked := range []bool{false, true} {
			t.Run(fmt.Sprintf("%s/marked=%t", marker, marked), func(t *testing.T) {
				env := newCallSemanticsEnv(t)
				original := env.LoadString("original-quasiquote-error.lisp", `(error 'first-failure "original data")`)
				if marked {
					env.AddBuiltins(true, elpsutil.Function("host-fault", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
						panic("quasiquote host fault")
					}))
					original = env.LoadString("original-quasiquote-error.lisp", `(host-fault)`)
				}
				require.Equal(t, lisp.LError, original.Type)
				require.Equal(t, marked, lisp.IsInternalPanic(original))
				stack, rendered := original.CallStack(), original.String()
				env.Runtime.MaxAlloc = 2
				var events []string
				env.AddBuiltins(true,
					elpsutil.Function("first-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
						events = append(events, "first")
						return original
					}),
					elpsutil.Function("later-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
						events = append(events, "later")
						return lisp.Int(7)
					}))
				got := env.LoadString("quasiquote-first-error.lisp", fmt.Sprintf(`(quasiquote ((%s (first-value)) (unquote (later-value))))`, marker))
				require.Same(t, original, got)
				require.Same(t, stack, got.CallStack())
				require.Equal(t, rendered, got.String())
				require.Equal(t, marked, lisp.IsInternalPanic(got))
				require.Equal(t, []string{"first"}, events)
			})
		}
	}
}

func TestQuasiquoteAllocationRejectsNonListSpliceBeforeLaterUnquote(t *testing.T) {
	env := newCallSemanticsEnv(t)
	env.Runtime.MaxAlloc = 2
	later := 0
	env.AddBuiltins(true, elpsutil.Function("later-value", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		later++
		return lisp.Int(7)
	}))
	got := env.LoadString("quasiquote-bad-splice.lisp", `(quasiquote ((unquote-splicing 1) (unquote (later-value))))`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.Contains(t, got.String(), "unquote-splicing: cannot splice non-list: int")
	require.Zero(t, later)
}
