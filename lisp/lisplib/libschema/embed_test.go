// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// TestEmbedLisp checks :embed in s:make-interface (#905).
func TestEmbedLisp(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "e.lisp", `
(set 'payer (s:make-interface "payer" (pay (amount))))
(set 'sized (s:make-interface "sized" (size ())))
(set 'both (s:make-interface "both" :embed (payer sized) (note ())))
(set 'both2 (s:make-interface "both2" (pay ([a s:int])) :embed (payer)))
(set 'inv (s:make-record "inv" (s:has-key "t" s:int)
  (s:method pay (o a) a) (s:method size (o) 1) (s:method note (o) "n")))
(set 'part (s:make-record "part" (s:has-key "t" s:int)
  (s:method pay (o a) a) (s:method note (o) "n")))`)
	require.Equal(t, "true", parityOutcome(env, `(s:is? (inv :t 1) both)`))
	require.Equal(t, "false", parityOutcome(env, `(s:is? (part :t 1) both)`), "size comes from sized")
	require.Equal(t, "true", parityOutcome(env, `(s:is? (part :t 1) both2)`), "the same name with the same arity")
	// Embedded methods come first, in :embed order.
	require.Equal(t, `(vector "pay (amount)" "size ()" "note ()")`, parityOutcome(env, `(get (s:describe both) "values")`))

	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-interface "x" :embed (payer) (pay (a b)))`),
		"the same name with another arity")
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-interface "x" :embed (5))`))
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-interface "x" :embed)`))
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-interface "x" :embed payer)`))
}

// TestEmbedGo checks InterfaceSpec.Embed.
func TestEmbedGo(t *testing.T) {
	mk := func(spec libschema.InterfaceSpec) (*libschema.Interface, error) { return libschema.NewInterface(spec) }
	payer, err := mk(libschema.InterfaceSpec{Name: "payer", Methods: []libschema.InterfaceMethod{{Name: "pay", Formals: lisp.Formals("a")}}})
	require.NoError(t, err)
	other, err := mk(libschema.InterfaceSpec{Name: "other", Methods: []libschema.InterfaceMethod{{Name: "pay", Formals: lisp.Formals()}}})
	require.NoError(t, err)
	_, err = mk(libschema.InterfaceSpec{Name: "x", Embed: []*libschema.Interface{payer, payer}})
	require.NoError(t, err, "the same method twice")
	_, err = mk(libschema.InterfaceSpec{Name: "x", Embed: []*libschema.Interface{payer, other}})
	require.ErrorContains(t, err, "pay takes")
	require.ErrorContains(t, err, "payer")
	require.ErrorContains(t, err, "other")
	_, err = mk(libschema.InterfaceSpec{Name: "x", Embed: []*libschema.Interface{nil}})
	require.ErrorContains(t, err, "nil")
}
