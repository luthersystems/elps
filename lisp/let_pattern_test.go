// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// A let or let* binding may give a match pattern in place of its name
// (luthersystems/elps#910).
func TestLetPattern(t *testing.T) {
	for _, tc := range []struct{ src, want string }{
		{`(let ([(list a b) '(1 2)]) (+ a b))`, "3"},
		{`(let* ([(list a b) '(1 2)]) (+ a b))`, "3"},
		{`(let (((list a b) '(1 2))) (list b a))`, "'(2 1)"},
		{`(let ([(list a &rest r) '(1 2 3)]) r)`, "'(2 3)"},
		{`(let ([(list (list a b) c) '((1 2) 3)]) (+ a b c))`, "6"},
		{`(let ([(sorted-map "total" t :n n) (sorted-map "total" 9 :n 1)]) (list t n))`, "'(9 1)"},
		{`(let ([(vector a b) (vector 1 2)]) (+ a b))`, "3"},
		{`(let* ([(vector a &rest r) (vector 1 2 3)] [(vector b c) r]) (list a b c))`, "'(1 2 3)"},
		{`(let ([(list (vector a _) b) (list (vector 1 2) 3)]) (+ a b))`, "4"},
		{`(let ([[s s:string] "hi"]) s)`, `"hi"`},
		{`(let ([(list [a s:int] _) '(1 2)]) a)`, "1"},
		{`(let ([1 1] ["s" "s"] [() ()] ['a 'a] [_ 7]) _)`, "7"},
		// A symbol name is a plain binding, also _.
		{`(let ([_ 5]) _)`, "5"},
		{`(let ([x 1] [(list y) (list 2)]) (+ x y))`, "3"},
		// let evaluates every initializer in the scope around it, so a
		// later initializer does not see a pattern's names.
		{`(set 'a 10) (let ([(list a) '(1)] [b a]) (list a b))`, "'(1 10)"},
		// let* binds in order: an initializer, and a type, see the names
		// before it.
		{`(let* ([(list a) '(1)] [b a]) (list a b))`, "'(1 1)"},
		{`(let* ([ty s:int] [[x ty] 5]) x)`, "5"},
		// A closure made by an earlier initializer does not see a later
		// pattern's names.
		{`(set 'a 10) (let* ([f (lambda () a)] [(list a) '(1)]) (list a (f)))`, "'(1 10)"},
		// The last binding of a name wins in let; let* rebinds.
		{`(let ([(list a) '(1)] [a 2]) a)`, "2"},
		{`(let* ([(list a) '(1)] [(list a) (list (+ a 1))]) a)`, "2"},
		// A value that does not match raises match-error.
		{`(let ([(list a b) '(1)]) a)`, "error: the value does not match the pattern (list a b): got list"},
		{`(let* ([(list a b) 5]) a)`, "error: the value does not match the pattern (list a b): got int"},
		{`(let ([[s s:string] 5]) s)`, "error: the value does not match the pattern '(s s:string): got int"},
		{`(let ([1 2]) 1)`, "error: the value does not match the pattern 1: got int"},
		{`(let ([(vector a b) '(1 2)]) a)`, "error: the value does not match the pattern (vector a b): got list"},
		{`(let* ([(vector a b) (vector 1)]) a)`, "error: the value does not match the pattern (vector a b): got array"},
		{`(handler-bind ((match-error (lambda (c &rest _) c))) (let ([(vector a) '(1)]) a))`, "'match-error"},
		{`(let ([(vector a a) (vector 1 1)]) a)`, "error: pattern binds a twice"},
		{`(let ([(vector a &rest) (vector 1)]) a)`, "error: &rest must come before the last pattern of a vector"},
		{`(handler-bind ((match-error (lambda (c &rest _) c))) (let ([(list a) 5]) a))`, "'match-error"},
		{`(handler-bind ((error (lambda (c &rest _) c))) (let* ([(list a) 5]) a))`, "'match-error"},
		{`(ignore-errors (let ([(list a) 5]) a))`, "()"},
		// Bad patterns.
		{`(let ([(list a a) '(1 1)]) a)`, "error: pattern binds a twice"},
		{`(let ([(list a) :when true '(1)]) a)`, "error: a binding pattern takes no :when guard"},
		{`(let* ([(list a) :when true '(1)]) a)`, "error: a binding pattern takes no :when guard"},
		{`(let ([(list &rest) '(1)]) 1)`, "error: &rest must come before"},
		{`(let ([user:x 1]) 1)`, "1"},
		{`(let ([2.5 2.5] [(quote (1 2)) '(1 2)]) 'ok)`, "'ok"},
		// The initializer runs before the shape is checked, as before for
		// a name Put refused.
		{`(let ([n 0]) (ignore-errors (let ([(list a a) (set! n 1)]) a)) n)`, "1"},
		// Keywords, true and false stay names that cannot be bound.
		{`(let ([:k 1]) 1)`, "error: value cannot be assigned to a keyword"},
		{`(let ([true 1]) 1)`, "error: cannot rebind constant"},
	} {
		t.Run(tc.src, func(t *testing.T) {
			env := newLimitTestEnv(t)
			got := matchOutcome(env, tc.src)
			if strings.HasPrefix(tc.want, "error: ") {
				require.True(t, strings.HasPrefix(got, "error: "), "got %s", got)
				assert.Contains(t, got, strings.TrimPrefix(tc.want, "error: "))
				return
			}
			assert.Equal(t, tc.want, got)
		})
	}
}

// letSteps returns the steps that loading src costs.
func letSteps(t *testing.T, src string) int64 {
	t.Helper()
	env := newLimitTestEnv(t, lisp.WithMaxSteps(1<<40))
	base := env.Runtime.Steps()
	v := env.LoadStringContext(context.Background(), "let.lisp", src)
	require.NoError(t, lisp.GoError(v), "%s", src)
	return env.Runtime.Steps() - base
}

// Step counts are consensus-visible, so they are pinned.  A plain let and
// let* cost what they cost before patterns.  A pattern binding costs what
// match costs for the same pattern: one step for each pattern node tried,
// and nothing more.
func TestLetPatternStepCount(t *testing.T) {
	for _, tc := range []struct {
		src   string
		steps int64
	}{
		// Plain bindings: the counts before #910.
		{`(let ([x 1] [y 2]) (+ x y))`, 8},
		{`(let* ([x 1] [y x]) y)`, 5},
		{`(let ([f (lambda () 1)]) (f))`, 7},
		{`(let* ([f (lambda () 1)] [g f]) (g))`, 8},
		// Patterns.
		{`(let ([(list a b) '(1 2)]) b)`, 7},
		{`(let* ([(list a b) '(1 2)] [c a]) c)`, 8},
		{`(let ([(list a &rest r) '(1 2 3)]) r)`, 7},
		{`(let ([(vector a b) (vector 1 2)]) b)`, 10},
		{`(let* ([(vector a &rest r) (vector 1 2 3)] [c a]) c)`, 12},
	} {
		t.Run(tc.src, func(t *testing.T) {
			assert.Equal(t, tc.steps, letSteps(t, tc.src))
		})
	}
	// A one-binding let costs what the same one-clause match costs.
	for _, tc := range []struct{ let, match string }{
		{`(let ([(list a b) '(1 2)]) b)`, `(match '(1 2) ((list a b) b))`},
		{`(let ([(list a &rest r) '(1 2 3)]) r)`, `(match '(1 2 3) ((list a &rest r) r))`},
		{`(let ([(sorted-map "k" v) (sorted-map "k" 1)]) v)`, `(match (sorted-map "k" 1) ((sorted-map "k" v) v))`},
		{`(let ([[x s:int] 1]) x)`, `(match 1 ([x s:int] x))`},
		{`(let ([(vector a b) (vector 1 2)]) b)`, `(match (vector 1 2) ((vector a b) b))`},
		{`(let ([(vector a &rest r) (vector 1 2 3)]) r)`, `(match (vector 1 2 3) ((vector a &rest r) r))`},
	} {
		t.Run(tc.let, func(t *testing.T) {
			assert.Equal(t, letSteps(t, tc.match), letSteps(t, tc.let))
		})
	}
}

// MatchPatternNames gives lint the shape rules and names of a pattern
// without evaluating it (luthersystems/elps#910).
func TestMatchPatternNames(t *testing.T) {
	read := func(src string) *lisp.LVal {
		t.Helper()
		env := newLimitTestEnv(t)
		vals, err := env.Runtime.Reader.Read("p.lisp", strings.NewReader(src))
		require.NoError(t, err)
		require.Len(t, vals, 1)
		return vals[0]
	}
	names, err := lisp.MatchPatternNames(read(`(list a [b s:int] (sorted-map "k" c :j d) (R :f e) (vector f _ &rest g) _ &rest r)`))
	require.NoError(t, err)
	var got []string
	for _, n := range names {
		got = append(got, n.Str)
	}
	assert.Equal(t, []string{"a", "b", "c", "d", "e", "f", "g", "r"}, got)
	for _, src := range []string{`(f x)`, `(list a a)`, `'(a b c)`, `(list &rest)`, `(R x y)`, `(vector a a)`, `(vector &rest)`} {
		_, err := lisp.MatchPatternNames(read(src))
		assert.Error(t, err, src)
	}
}
