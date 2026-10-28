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

// A VM a template makes has match's type support.
func TestMatchTemplate(t *testing.T) {
	vm := mustForkEnv(t, newSchemaForkEnv(t))
	require.Equal(t, "2", parityOutcome(vm, `(match 1 ([x s:int] (+ x 1)))`))
}
