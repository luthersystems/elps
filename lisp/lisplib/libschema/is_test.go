// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// TestIsParity checks s:is? and s:as on a Lisp type and the same Go type
// (#905).
func TestIsParity(t *testing.T) {
	runParity(t, []parityCase{
		{`(s:is? (T :n 1) T)`, "true"},
		{`(s:is? 5 T)`, "false"},
		{`(s:is? (T :n 1) (s:ref T))`, "true"},
		{`(s:is? (sorted-map "n" 1) T)`, "false"},
		{`(s:is? 5 s:int)`, "true"},
		{`(s:is? "5" s:int)`, "false"},
		{`(s:is? (list 1 2) (s:of s:int))`, "true"},
		{`(s:is? (list 1 "2") (s:of s:int))`, "false"},
		{`(s:is? 5 6)`, "error:bad-arguments"},
		{`((s:as (T :n 2) T) 'n)`, "2"},
		{`(s:as 5 T)`, "error:wrong-type"},
		{`(s:as 7 s:int)`, "7"},
		{`(s:is? 1)`, "error:error"},
	})
}

// TestIsLisp checks the parts of s:is? that only Lisp has: an interface,
// a :check that raises its own condition, and gensyms.
func TestIsLisp(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "is.lisp", `
(set 'sized (s:make-interface "sized" (size ())))
(set 'limit 9)
(set 'box (s:make-record "box" (s:has-key "n" s:int)
  :check (lambda (o) (when (> (o 'n) limit) (error 'too-big (o 'n))))
  (s:method size (o) (o 'n))))
(set 'b (box :n 5))`)
	require.Equal(t, "true", parityOutcome(env, `(s:is? b sized)`))
	evalString(t, env, `(set 'positive (s:make-validator "positive" s:int (s:gt 0)))`)
	require.Equal(t, "true", parityOutcome(env, `(s:is? 5 positive)`))
	require.Equal(t, "false", parityOutcome(env, `(s:is? -5 positive)`))
	require.Equal(t, "error:failed-constraint", parityOutcome(env, `(s:as -5 positive)`))
	require.Equal(t, "false", parityOutcome(env, `(s:is? 5 sized)`))
	// A :check that raises its own condition propagates.
	require.Equal(t, "true", parityOutcome(env, `(s:is? b box)`))
	evalString(t, env, `(set 'limit 3)`)
	require.Equal(t, "error:too-big", parityOutcome(env, `(s:is? b box)`))

	// A built-in type name takes no gensym, so later gensym names do not
	// move.
	num := func(v *lisp.LVal) int {
		n, err := strconv.Atoi(strings.TrimLeft(v.Str, "gen"))
		require.NoError(t, err, v.Str)
		return n
	}
	g := evalString(t, env, `(list (gensym) (progn (s:is? 7 s:int) (s:as "a" s:string) (gensym)) (gensym))`)
	a, b, c := num(g.Cells[0]), num(g.Cells[1]), num(g.Cells[2])
	require.Equal(t, c-b, b-a, "a type test took a gensym")
}

// TestIsGo checks libschema.Is and libschema.As.
func TestIsGo(t *testing.T) {
	env := newSchemaForkEnv(t)
	gt := newParityGoType(t)
	obj := evalString(t, env, `5`)
	require.Equal(t, lisp.Bool(false).Str, libschema.Is(env, obj, gt.Value()).Str)
	require.Equal(t, lisp.Bool(true).Str, libschema.Is(env, obj, lisp.String(libschema.Int)).Str)
	require.Same(t, obj, libschema.As(env, obj, lisp.String(libschema.Int)))
	r := libschema.As(env, obj, lisp.String(libschema.String))
	require.True(t, r.IsError())
	require.Equal(t, libschema.WrongType, r.Str)
	r = libschema.Is(env, obj, lisp.Int(3))
	require.True(t, r.IsError())
	require.Equal(t, libschema.BadArgs, r.Str)
}
