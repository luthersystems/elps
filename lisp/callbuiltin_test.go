// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	refGet      = lisp.BuiltinFunc("get")
	refAdd      = lisp.BuiltinFunc("+")
	refAssocMut = lisp.BuiltinFunc("assoc!")
	refFuncall  = lisp.BuiltinFunc("funcall")
	refLoadStr  = lisp.BuiltinFunc("load-string")
)

func TestBuiltinFuncResolves(t *testing.T) {
	assert.Equal(t, "get", refGet.Name())
	assert.Empty(t, lisp.BuiltinRef{}.Name())
	assert.PanicsWithValue(t, "lisp.BuiltinFunc: no default builtin named no-such-builtin", func() {
		lisp.BuiltinFunc("no-such-builtin")
	})
}

// callVia registers (via-go args...) that forwards to ref through CallBuiltin,
// so the Go path and the Lisp path can be compared on the same inputs.
func callVia(env *lisp.LEnv, name string, ref lisp.BuiltinRef) {
	env.AddBuiltins(false, &testBuiltinDef{name: name, formals: lisp.Formals(lisp.VarArgSymbol, "args"),
		fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return env.CallBuiltin(ref, args.Cells...)
		}})
}

// A call through CallBuiltin returns what the Lisp call returns -- value or
// error message -- and charges no step beyond the forwarding call's own.
func TestCallBuiltinMatchesLispCall(t *testing.T) {
	env := newLimitTestEnv(t)
	callVia(env, "go-get", refGet)
	callVia(env, "go-add", refAdd)
	callVia(env, "go-assoc!", refAssocMut)
	callVia(env, "go-funcall", refFuncall)
	callVia(env, "go-load-string", refLoadStr)
	cases := []struct{ lisp, goForm string }{
		{`(get (sorted-map "a" 1) "a")`, `(go-get (sorted-map "a" 1) "a")`},
		{`(get 3 "a")`, `(go-get 3 "a")`},
		{`(get (sorted-map) 1.5)`, `(go-get (sorted-map) 1.5)`},
		{`(+ 1 2 3)`, `(go-add 1 2 3)`},
		{`(+ 1 "x")`, `(go-add 1 "x")`},
		{`(assoc! (sorted-map) "k" 1)`, `(go-assoc! (sorted-map) "k" 1)`},
		{`(assoc! () "k" 1)`, `(go-assoc! () "k" 1)`},
		{`(funcall (lambda (x) (+ x 1)) 41)`, `(go-funcall (lambda (x) (+ x 1)) 41)`},
		{`(load-string "(+ 1 1)" :name "x")`, `(go-load-string "(+ 1 1)" :name "x")`},
	}
	for _, tc := range cases {
		want, wantSteps := stepsOf(t, env, tc.lisp)
		got, gotSteps := stepsOf(t, env, tc.goForm)
		require.Equal(t, want.Type, got.Type, "%s: %v vs %v", tc.goForm, want, got)
		if want.Type == lisp.LError {
			assert.Equal(t, want.Str, got.Str, tc.goForm)
			assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), tc.goForm)
		} else {
			assert.Equal(t, want.String(), got.String(), tc.goForm)
			// The go- form evaluates one extra symbol-free argument list of
			// the same size, so the step counts match exactly.
			assert.Equal(t, wantSteps, gotSteps, tc.goForm)
		}
	}
}

func TestCallBuiltinArityErrors(t *testing.T) {
	env := newLimitTestEnv(t)
	callVia(env, "go-get", refGet)
	callVia(env, "go-load-string", refLoadStr)
	for _, pair := range [][2]string{
		{`(get (sorted-map))`, `(go-get (sorted-map))`},
		{`(load-string "1" :name)`, `(go-load-string "1" :name)`},
	} {
		want, _ := stepsOf(t, env, pair[0])
		got, _ := stepsOf(t, env, pair[1])
		require.Equal(t, lisp.LError, want.Type)
		require.Equal(t, lisp.LError, got.Type)
		assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage())
	}
}

func TestCallBuiltinDoesNotWriteArgs(t *testing.T) {
	env := newLimitTestEnv(t)
	args := []*lisp.LVal{lisp.Int(1), lisp.Int(2)}
	v := env.CallBuiltin(refAdd, args...)
	require.Equal(t, 3, v.Int)
	assert.Equal(t, 1, args[0].Int)
	assert.Equal(t, 2, args[1].Int)
	assert.Equal(t, lisp.LError, env.CallBuiltin(lisp.BuiltinRef{}).Type)
}

func TestCallBuiltinChecksContext(t *testing.T) {
	env := newLimitTestEnv(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	fn := lisp.Fun("f", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		return env.CallBuiltin(refAdd, lisp.Int(1))
	})
	v := env.FunCallContext(ctx, fn, lisp.SExpr(nil))
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, lisp.CondContextCancelled, v.Str)
}
