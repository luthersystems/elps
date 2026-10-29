// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// matchOutcome loads src and returns the printed value, or "error: " and
// the error's message.
func matchOutcome(env *lisp.LEnv, src string) string {
	v := env.LoadString("match.lisp", src)
	if v.IsError() {
		return "error: " + lisp.GoError(v).Error()
	}
	return v.String()
}

func TestMatch(t *testing.T) {
	for _, tc := range []struct{ src, want string }{
		{`(match 1)`, "()"},
		{`(match 1 (2 'two))`, "()"},
		{`(match 1 (1 'one) (_ 'other))`, "'one"},
		{`(match 2 (1 'one) (_ 'other))`, "'other"},
		{`(match 2 (x (+ x 1)))`, "3"},
		{`(match 2 (:else 'e))`, "'e"},
		{`(match 2 (1 'one) (:else))`, "()"},
		{`(match 1.5 (1.5 'f))`, "'f"},
		{`(match "a" ("a" 's))`, "'s"},
		{`(match :k (:k 'kw))`, "'kw"},
		{`(match true (false 'f) (true 't))`, "'t"},
		{`(match () (() 'nil))`, "'nil"},
		{`(match () ('() 'nil))`, "'nil"},
		{`(match 'foo ('bar 1) ('foo 2))`, "2"},
		{`(match 'x (x x))`, "'x"},
		{`(match '(1 2) ((quote (1 2)) 'lit))`, "'lit"},
		{`(match '(1 2) ((list 1 b) b))`, "2"},
		{`(match '(1 2 3) ((list a b) 'two) ((list a &rest r) r))`, "'(2 3)"},
		{`(match '(1) ((list a &rest r) r))`, "'()"},
		{`(match '() ((list) 'empty))`, "'empty"},
		{`(match 5 ((list) 'empty) (_ 'no))`, "'no"},
		{`(match '((1 2) 3) ((list (list a b) c) (+ a b c)))`, "6"},
		{`(match (sorted-map "k" 1 :j 2) ((sorted-map "k" a :j b) (+ a b)))`, "3"},
		{`(match (sorted-map "k" 1) ((sorted-map "z" a) a) (_ 'no))`, "'no"},
		{`(match 5 (x :when (> x 9) 'big) (x :when (> x 1) 'mid) (_ 'small))`, "'mid"},
		{`(match 5 (x :when (> x 9) 'big))`, "()"},
		// The guard sees the pattern's names; the body sees the guard's
		// outer scope.
		{`(let ([y 1]) (match 5 (x :when (> x y) (+ x y))))`, "6"},
		// The value is evaluated once.
		{`(let ([n 0]) (match (set! n (+ n 1)) (2 'two) (1 'one)) n)`, "1"},
		{`(match 1 ([x s:int] (+ x 1)))`, "2"},
		{`(match "a" ([x s:int] 'int) ([_ s:string] 'str))`, "'str"},
		{`(match '(1 "a") ((list [a s:int] [b s:string]) (list a b)))`, `'(1 "a")`},
		// Vector patterns (luthersystems/elps#914).
		{`(match (vector 1 2) ((vector a b) (+ a b)))`, "3"},
		{`(match (vector 1 2) ((lisp:vector a b) (+ a b)))`, "3"},
		{`(match (vector) ((vector) 'empty))`, "'empty"},
		{`(match (vector 1 2 3) ((vector a b) 'two) ((vector a &rest r) r))`, "(vector 2 3)"},
		{`(match (vector 1) ((vector a &rest r) r))`, "(vector)"},
		{`(match (vector 1 2 3) ((vector a &rest _) a))`, "1"},
		{`(match (vector 1 2) ((vector 1 b) b))`, "2"},
		{`(match (vector 1 2) ((vector 2 b) b) (_ 'no))`, "'no"},
		{`(match (vector (vector 1 2) 3) ((vector (vector a _) _) a))`, "1"},
		{`(match (list (vector 1 2)) ((list (vector a _)) a))`, "1"},
		{`(match (vector '(1 2)) ((vector (list a b)) (+ a b)))`, "3"},
		{`(match (vector 1 2 3) ((vector a &rest (vector b c)) (+ a b c)))`, "6"},
		{`(match (vector "a" 1) ((vector [s s:string] [n s:int]) (list s n)))`, `'("a" 1)`},
		// The value's type decides: no coercion between lists and vectors.
		{`(match '(1 2) ((vector a b) 'vec) (_ 'no))`, "'no"},
		{`(match (vector 1 2) ((list a b) 'list) (_ 'no))`, "'no"},
		{`(match '() ((vector) 'vec) (_ 'no))`, "'no"},
		{`(match (vector) ((list) 'list) (_ 'no))`, "'no"},
		{`(match "ab" ((vector a b) 'vec) (_ 'no))`, "'no"},
		{`(match 5 ((vector &rest r) r) (_ 'no))`, "'no"},
		// Wrong length.
		{`(match (vector 1 2 3) ((vector a b) 'two) (_ 'no))`, "'no"},
		{`(match (vector 1) ((vector a b) 'two) (_ 'no))`, "'no"},
		{`(match (vector 1) ((vector a b &rest r) r) (_ 'no))`, "'no"},
		// The rest is a new vector: a change to it leaves the value alone.
		{`(let ([v (vector 1 2 3)]) (match v ((vector _ &rest r) (append! r 9) (list v r))))`, "'((vector 1 2 3) (vector 2 3 9))"},
		// Errors.
		{`(match)`, "error: match.lisp:1:1: lisp:match: invalid number of arguments: 0"},
		{`(match 1 (:else 1) (_ 2))`, "error: match: :else must be the last clause"},
		{`(match 1 ((list a a) 1))`, "error: match: pattern binds a twice"},
		{`(match 1 ('(1 2) 1))`, "error: match: bad pattern"},
		{`(match 1 ((list &rest) 1))`, "error: match: &rest must come before the last pattern of a list"},
		{`(match 1 ((vector a a) 1))`, "error: match: pattern binds a twice"},
		{`(match 1 ((vector a &rest a) 1))`, "error: match: pattern binds a twice"},
		{`(match 1 ((vector &rest) 1))`, "error: match: &rest must come before the last pattern of a vector"},
		{`(match 1 ((vector &rest r s) 1))`, "error: match: &rest must come before the last pattern of a vector"},
		{`(match 1 ((vector a &rest b &rest c) 1))`, "error: match: &rest must come before the last pattern of a vector"},
		{`(match 1 ((vector user:x) 1))`, "error: match: bad pattern"},
		{`(match 1 ((sorted-map "k") 1))`, "error: match: (sorted-map \"k\") needs key and pattern pairs"},
		{`(match 1 (x :when))`, "error: match: :when needs a guard"},
		{`(match 1 (x :when (error 'boom "no") 1))`, "error: no"},
		{`(match 1 ([x (error 'boom "t")] 1))`, "error: t"},
		{`(match 5 (user:x 1))`, "error: match: bad pattern"},
		{`(match 5 ([true s:int] 1))`, "error: match: bad pattern"},
		{`(match 5 ([:k s:int] 1))`, "error: match: bad pattern"},
		{`(match 1 (2 1) bad)`, "error: match: a clause is (pattern body...)"},
		// A shape error is found when its clause is reached.
		{`(match 1 (1 'one) ((list a a) 2))`, "'one"},
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

// Without a type library a type pattern is an error; other patterns work.
func TestMatchNoTypeLibrary(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	assert.Equal(t, "2", matchOutcome(env, `(match '(1 2) ((list _ b) b))`))
	assert.Contains(t, matchOutcome(env, `(match 1 ([x int] x))`), "match: a type pattern needs a type library")
	// A let binding pattern's errors carry let's name, not match's
	// (luthersystems/elps#910).
	got := matchOutcome(env, `(let ([[x int] 1]) x)`)
	assert.Contains(t, got, "lisp:let: a type pattern needs a type library")
	assert.NotContains(t, got, "match:")
}

// Each pattern node tried costs one step.  The counts are pinned so a
// change to them is seen (step counts may only go down).
func TestMatchStepCount(t *testing.T) {
	for _, tc := range []struct {
		src   string
		steps int64
	}{
		{`(match 1 (_ 1))`, 5},
		{`(match 1 (2 1) (1 2))`, 6},
		{`(match '(1 2) ((list a b) b))`, 7},
		{`(match '(1 2) ((list 1 3) 1) ((list a &rest r) r))`, 10},
		{`(match (vector 1 2) ((vector a b) b))`, 10},
		{`(match (vector 1 2) ((vector 1 3) 1) ((vector a &rest r) r))`, 13},
		{`(match (vector 1 2) ((vector a &rest _) a))`, 10},
		{`(match '(1 2) ((list a &rest _) a))`, 7},
		{`(match '(1 2) ((vector a b) 1) (_ 2))`, 6},
	} {
		t.Run(tc.src, func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaxSteps(1<<40))
			base := env.Runtime.Steps()
			v := env.LoadStringContext(context.Background(), "match.lisp", tc.src)
			require.NoError(t, lisp.GoError(v))
			assert.Equal(t, tc.steps, env.Runtime.Steps()-base)
		})
	}
}

// A step limit stops a match that walks a long list.
func TestMatchStepLimit(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(50))
	v := env.LoadStringContext(context.Background(), "match.lisp",
		`(match '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
		   ((list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 'all))`)
	require.True(t, v.IsError())
}

// &rest _ binds nothing, so it copies nothing: a match with &rest _ makes
// no more allocations than the same match with a _ for each remaining
// item, which copies nothing (luthersystems/elps#914).  &rest r, which
// copies, makes more.
func TestMatchRestAnyNoAlloc(t *testing.T) {
	allocs := func(t *testing.T, value, src string) float64 {
		t.Helper()
		env := newLimitTestEnv(t)
		require.NoError(t, lisp.GoError(env.LoadString("v.lisp", "(set 'v "+value+")")))
		forms, err := env.Runtime.Reader.Read("m.lisp", strings.NewReader(src))
		require.NoError(t, err)
		require.Len(t, forms, 1)
		form := forms[0]
		require.NoError(t, lisp.GoError(env.Eval(form)))
		return testing.AllocsPerRun(100, func() {
			if v := env.Eval(form); v.IsError() {
				t.Fatal(v)
			}
		})
	}
	for _, tc := range []struct{ value, rest, each, copied string }{
		{`'(1 2 3 4)`, `(match v ((list a &rest _) a))`, `(match v ((list a _ _ _) a))`, `(match v ((list a &rest r) a))`},
		{`(vector 1 2 3 4)`, `(match v ((vector a &rest _) a))`, `(match v ((vector a _ _ _) a))`, `(match v ((vector a &rest r) a))`},
	} {
		t.Run(tc.rest, func(t *testing.T) {
			rest := allocs(t, tc.value, tc.rest)
			assert.LessOrEqual(t, rest, allocs(t, tc.value, tc.each))
			assert.Greater(t, allocs(t, tc.value, tc.copied), rest)
		})
	}
}

// A vector pattern matches only a 1-D array.  A 0-D or 2-D array is not a
// vector, so no vector pattern matches it, &rest included
// (luthersystems/elps#914).
func TestMatchVectorArrayShape(t *testing.T) {
	for name, dims := range map[string]*lisp.LVal{
		"0-D": lisp.QExpr(nil),
		"2-D": lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(2)}),
		"1x1": lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(1)}),
	} {
		t.Run(name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			arr := lisp.Array(dims, nil)
			require.False(t, arr.IsError())
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("arr"), arr)))
			got := matchOutcome(env, `(match arr ((vector) 'empty) ((vector a) 'one) ((vector a b c d) 'four) ((vector &rest r) 'rest) (_ 'no))`)
			assert.Equal(t, "'no", got)
			got = matchOutcome(env, `(let ([(vector &rest r) arr]) r)`)
			assert.Contains(t, got, "the value does not match the pattern (vector &rest r): got array")
		})
	}
}
