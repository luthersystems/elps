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

func newPredicateValuesEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	return env
}

// A quoted outer list already makes its nested forms data. Predicates must
// receive those forms unchanged, without executing them or resolving symbols.
func TestPredicateValuesRemainData(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr string
		want string
	}{
		{"all nested lists", `(all? list? '((1) (2)))`, `true`},
		{"any nested lists", `(any? list? '((1) (2)))`, `true`},
		{"all unbound symbols", `(all? symbol? '(alice bob))`, `true`},
		{"any unbound symbols", `(any? symbol? '(alice bob))`, `true`},
		{"all bound symbol", `(let ((alice 1)) (all? symbol? '(alice)))`, `true`},
		{"any bound symbol", `(let ((alice 1)) (any? symbol? '(alice)))`, `true`},
		{"all quoted symbols", `(all? symbol? '('alice 'bob))`, `true`},
		{"any quoted symbols", `(any? symbol? '('alice 'bob))`, `true`},
		{"sort nested lists", `(map 'list first (stable-sort (lambda (a b) (< (first a) (first b))) (copy '((2) (1)))))`, `'(1 2)`},
		{"sort nested list keys", `(map 'list first (stable-sort < (copy '((2) (1))) first))`, `'(1 2)`},
		{"sort symbols", `(map 'list to-string (stable-sort (lambda (a b) (string< (to-string a) (to-string b))) (copy '(bob alice))))`, `'("alice" "bob")`},
		{"sort symbol keys", `(map 'list to-string (stable-sort string< (copy '(bob alice)) to-string))`, `'("alice" "bob")`},
		{"insert nested lists", `(map 'list first (insert-sorted 'list '((1) (3)) (lambda (a b) (< (first a) (first b))) '(2)))`, `'(1 2 3)`},
		{"insert nested list keys", `(map 'list first (insert-sorted 'vector '((1) (3)) < '(2) first))`, `'(1 2 3)`},
		{"insert symbols", `(map 'list to-string (insert-sorted 'list '(alice carol) (lambda (a b) (string< (to-string a) (to-string b))) 'bob))`, `'("alice" "bob" "carol")`},
		{"insert symbol keys", `(map 'list to-string (insert-sorted 'vector '(alice carol) string< 'bob to-string))`, `'("alice" "bob" "carol")`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := newPredicateValuesEnv(t).LoadString("predicate-values.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
}

func TestPredicateValuesCannotExecuteForms(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr string
	}{
		{"all", `(all? (lambda (x) true) '((set! writes 99)))`},
		{"any", `(any? (lambda (x) true) '((set! writes 99)))`},
		{"sort", `(stable-sort (lambda (a b) false) (copy '((set! writes 99) (set! writes 99))))`},
		{"sort key", `(stable-sort < (copy '((set! writes 99) (set! writes 99))) (lambda (x) 0))`},
		{"insert", `(insert-sorted 'list '((set! writes 99)) (lambda (a b) false) '(set! writes 99))`},
		{"insert key", `(insert-sorted 'list '((set! writes 99)) < '(set! writes 99) (lambda (x) 0))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			require.NotEqual(t, lisp.LError, env.LoadString("predicate-values.lisp", `(set 'writes 0)`).Type)
			got := env.LoadString("predicate-values.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, "0", env.LoadString("predicate-values.lisp", "writes").String(), "the predicate's input is data, even when it looks like an assignment")
		})
	}
}

func TestPredicateValuesPreserveIdentity(t *testing.T) {
	for _, tc := range []struct {
		name  string
		expr  string
		truth bool
	}{
		{"all", `(all? inspect source)`, true},
		{"any", `(any? inspect source)`, false},
		{"sort", `(stable-sort inspect source)`, false},
		{"sort key", `(stable-sort < source inspect)`, false},
		{"sort key results", `(stable-sort inspect source identity)`, false},
		{"insert", `(insert-sorted 'list source inspect item)`, false},
		{"insert key", `(insert-sorted 'vector source < item inspect)`, false},
		{"insert key results", `(insert-sorted 'vector source inspect item identity)`, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			// Both are unquoted values. Evaluation would either resolve the
			// symbol or try calling the integer at the head of the list.
			a := lisp.Symbol("alice")
			b := lisp.SExpr([]*lisp.LVal{lisp.Int(17)})
			env.PutGlobal(lisp.Symbol("source"), lisp.QExpr([]*lisp.LVal{a, b}))
			env.PutGlobal(lisp.Symbol("item"), a)
			seen := make(map[*lisp.LVal]bool)
			env.AddBuiltins(true, elpsutil.Function("inspect", lisp.Formals(lisp.VarArgSymbol, "values"),
				func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
					for _, v := range args.Cells {
						assert.True(t, v == a || v == b, "callback received a copied or evaluated value: %v", v)
						seen[v] = true
					}
					if tc.name == "sort key" || tc.name == "insert key" {
						return lisp.Int(0)
					}
					return lisp.Bool(tc.truth)
				}))
			got := env.LoadString("predicate-values.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.True(t, seen[a], "callback must observe the original symbol")
			assert.True(t, seen[b], "callback must observe the original list")
		})
	}
}

func TestPredicateValuesShortCircuitAndErrors(t *testing.T) {
	for _, tc := range []struct {
		name string
		expr string
		want string
	}{
		{"all empty", `(all? (lambda (x) (error 'unexpected)) ())`, `true`},
		{"any empty", `(any? (lambda (x) (error 'unexpected)) ())`, `false`},
		{"all truthy", `(all? identity '(1 "yes"))`, `true`},
		{"any none", `(any? identity '(false ()))`, `false`},
		{"all stops at false", `(all? (lambda (x) (if (= x 2) (error 'unexpected) false)) (vector 1 2))`, `false`},
		{"all stops at nil", `(all? (lambda (x) (if (= x 2) (error 'unexpected) ())) (vector 1 2))`, `false`},
		{"any retains first truthy", `(any? (lambda (x) (if (= x 1) () (if (= x 2) "matched" (error 'unexpected)))) (vector 1 2 3))`, `"matched"`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := newPredicateValuesEnv(t).LoadString("predicate-values.lisp", tc.expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
	for _, op := range []string{"all?", "any?"} {
		t.Run(op+" error", func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			require.NotEqual(t, lisp.LError, env.LoadString("predicate-values.lisp", `(set 'calls 0)`).Type)
			got := env.LoadString("predicate-values.lisp", fmt.Sprintf(`(%s (lambda (x) (set! calls (+ calls 1)) (error 'predicate-failed "original" x)) '(10 20))`, op))
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type)
			assert.Equal(t, "predicate-failed", got.Str)
			require.Len(t, got.Cells, 2)
			assert.Equal(t, `"original"`, got.Cells[0].String())
			assert.Equal(t, "10", got.Cells[1].String())
			assert.Equal(t, "1", env.LoadString("predicate-values.lisp", "calls").String())
		})
	}
}

func TestPredicateValuesRejectSpecialFunctions(t *testing.T) {
	for _, form := range []string{"quote", "predicate-macro"} {
		for _, expr := range []string{
			`(all? %s ())`, `(all? %s '(1))`,
			`(any? %s ())`, `(any? %s '(1))`,
			`(stable-sort %s (list))`, `(stable-sort %s (list 2 1))`,
			`(stable-sort < (list) %s)`, `(stable-sort < (list 2 1) %s)`,
			`(insert-sorted 'list () %s 1)`, `(insert-sorted 'list '(2) %s 1)`,
			`(insert-sorted 'list () < 1 %s)`, `(insert-sorted 'list '(2) < 1 %s)`,
		} {
			expr := fmt.Sprintf(expr, form)
			t.Run(expr, func(t *testing.T) {
				env := newPredicateValuesEnv(t)
				macro := env.LoadString("predicate-values.lisp", `(defmacro predicate-macro (&rest args) true)`)
				require.NotEqual(t, lisp.LError, macro.Type, "%v", macro)
				got := env.LoadString("predicate-values.lisp", expr)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				require.Equal(t, lisp.LError, got.Type, "a predicate or key must be a regular function: %v", got)
				assert.Contains(t, got.String(), "not a regular function")
			})
		}
	}
}
