// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// match's type and record patterns give the same outcome over a Lisp
// record type and the Go type that copies it (luthersystems/elps#905).
func TestMatchParity(t *testing.T) {
	runParity(t, []parityCase{
		{`(match (T :n 1) ([o T] (o 'n)))`, "1"},
		{`(match 5 ([o T] 'rec) (_ 'other))`, "'other"},
		{`(match (T :n 1) ((T :n 1) 'one) (_ 'other))`, "'one"},
		{`(match (T :n 2) ((T :n 1) 'one) ((T :n x) x))`, "2"},
		{`(match (T :n 2) ((T :n x) :when (> x 5) 'big) ((T) 'small))`, "'small"},
		{`(match 7 ((T :n x) x) (_ 'other))`, "'other"},
		{`(match (list (T :n 3)) ((list (T :n x)) x))`, "3"},
		{`(match (T :n 1) ((T :nope x) x))`, "error:unknown-operation"},
		{`(match (T :n 1) ((T n x) x))`, "error:error"},
		{`(match (T :n 1) ([o 5] o))`, "error:bad-arguments"},
	})
}

// A let binding pattern reads a record as match does
// (luthersystems/elps#910).
func TestLetPatternParity(t *testing.T) {
	runParity(t, []parityCase{
		{`(let ([(T :n x) (T :n 1)]) x)`, "1"},
		{`(let* ([[o T] (T :n 2)] [(T :n x) o]) x)`, "2"},
		{`(let ([(T :n x) 5]) x)`, "error:match-error"},
		{`(let ([(T :n 1) (T :n 2)]) 'no)`, "error:match-error"},
	})
}

// A Lisp record field that is not set reads as (), as (o 'f) reads it.
// A record pattern may name an interface.
func TestMatchLisp(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "match.lisp", `
(set 'sized (s:make-interface "sized" (size ())))
(set 'box (s:make-record "box" (s:has-key "n" s:int) (s:may-have-key "tag" s:string)
  (s:method size (o) (o 'n))))`)
	require.Equal(t, "'(5 ())", parityOutcome(env, `(match (box :n 5) ((box :n n :tag tag) (list n tag)))`))
	require.Equal(t, "5", parityOutcome(env, `(match (box :n 5) ([b sized] (b 'size)))`))
	require.Equal(t, "'other", parityOutcome(env, `(match 5 ([b sized] 'sized) (_ 'other))`))
	require.Equal(t, "'sized", parityOutcome(env, `(match (box :n 5) ((sized) 'sized))`))
	// A field pattern on a value that passed a non-record type.
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(match 5 ((s:int :n x) x))`))
}

// An unqualified vector head is always the vector pattern, also in a
// package that binds vector to a record type.  A qualified head matches
// the record (luthersystems/elps#914).
func TestMatchShadowedVector(t *testing.T) {
	env := newSchemaForkEnv(t)
	prelude := `(in-package 'shapes)
(set 'vector (s:make-record "vector" (s:has-key "x" s:int)))
(set 'p (vector :x 1))
`
	for _, tc := range []struct{ src, want string }{
		{`(match p ((vector :x a) a) (_ 'no))`, "'no"},
		{`(match (lisp:vector :x 2) ((vector :x a) a) (_ 'no))`, "2"},
		{`(match p ((shapes:vector :x a) a) (_ 'no))`, "1"},
		{`(match (lisp:vector :x 2) ((shapes:vector :x a) a) (_ 'no))`, "'no"},
		{`(let ([(shapes:vector :x a) p]) a)`, "1"},
	} {
		require.Equal(t, tc.want, parityOutcome(env, prelude+tc.src), tc.src)
	}
}

// A VM a template makes has match's type support.
func TestMatchTemplate(t *testing.T) {
	vm := mustForkEnv(t, newSchemaForkEnv(t))
	require.Equal(t, "2", parityOutcome(vm, `(match 1 ([x s:int] (+ x 1)))`))
}
