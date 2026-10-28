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
		// Errors.
		{`(match)`, "error: match.lisp:1:1: lisp:match: invalid number of arguments: 0"},
		{`(match 1 (:else 1) (_ 2))`, "error: match: :else must be the last clause"},
		{`(match 1 ((list a a) 1))`, "error: match: pattern binds a twice"},
		{`(match 1 ('(1 2) 1))`, "error: match: bad pattern"},
		{`(match 1 ((list &rest) 1))`, "error: match: &rest must come before"},
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
