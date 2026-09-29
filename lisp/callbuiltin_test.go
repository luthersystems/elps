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

var refApply = lisp.BuiltinFunc("apply")

// funcall and apply mark the top stack frame terminal and call FunCall,
// assuming the frame is their own.  Through CallBuiltin the top frame is the
// calling builtin's, so a call in tail position of a recursive Lisp function
// must still come back as a value -- never a tail-recursion marker -- and the
// calling builtin's frame must not be left marked terminal for its later
// calls.
func TestCallBuiltinTailPositionNoMarker(t *testing.T) {
	env := newLimitTestEnv(t)
	check := func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
		if v.Type == lisp.LError || v.Type == lisp.LSymbol || v.Type == lisp.LInt {
			return v
		}
		return env.Errorf("CallBuiltin leaked a %v", v.Type)
	}
	env.AddBuiltins(false,
		&testBuiltinDef{name: "napply", formals: lisp.Formals("f", "xs"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return check(env, env.CallBuiltin(refApply, args.Cells[0], args.Cells[1]))
			}},
		&testBuiltinDef{name: "nfuncall", formals: lisp.Formals("f", "x"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return check(env, env.CallBuiltin(refFuncall, args.Cells[0], args.Cells[1]))
			}},
		&testBuiltinDef{name: "ntwice", formals: lisp.Formals("f", "x"),
			fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				// A plain FunCall after CallBuiltin(funcall ...) must not
				// see a terminal frame funcall left behind.
				if v := check(env, env.CallBuiltin(refFuncall, args.Cells[0], lisp.Int(0))); v.Type == lisp.LError {
					return v
				}
				return check(env, env.FunCall(args.Cells[0], lisp.SExpr([]*lisp.LVal{args.Cells[1]})))
			}},
	)
	for _, tc := range []struct{ src, want string }{
		{`(defun lp (n) (if (> n 0) (napply lp (list (- n 1))) 'done)) (lp 3)`, "'done"},
		{`(defun lq (n) (if (> n 0) (nfuncall lq (- n 1)) 'done)) (lq 3)`, "'done"},
		{`(defun lt (n) (if (> n 0) (ntwice lt (- n 1)) 'done)) (lt 3)`, "'done"},
		{`(defun lr (n) (if (> n 0) (+ 1 (napply lr (list (- n 1)))) 0)) (lr 3)`, "3"},
	} {
		v := env.LoadString("test", tc.src)
		require.NotEqual(t, lisp.LError, v.Type, "%s: %v", tc.src, v)
		assert.Equal(t, tc.want, v.String(), tc.src)
	}
}

// A call whose args match the builtin's required formals exactly costs what
// the hand-written b.Eval(env, QExpr([]*lisp.LVal{m, k})) it replaces did:
// the variadic slice and the list header.  Binding copies nothing more.
// The caller's slice is still never written (TestCallBuiltinDoesNotWriteArgs).
func TestCallBuiltinExactArityAllocs(t *testing.T) {
	env := newLimitTestEnv(t)
	m := env.LoadString("test", `(sorted-map "a" 1)`)
	require.Equal(t, lisp.LSortMap, m.Type)
	k := lisp.String("a")
	allocs := testing.AllocsPerRun(100, func() {
		if v := env.CallBuiltin(refGet, m, k); v.Type != lisp.LInt {
			t.Fatalf("get returned %v", v)
		}
	})
	assert.LessOrEqual(t, allocs, 2.0)
}
