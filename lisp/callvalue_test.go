// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// callFormEvaluator returns a function that evaluates the call form (f 7)
// where f is a variable that holds fn, the evaluator's answer that
// CallValue copies.
func callFormEvaluator(env *lisp.LEnv) func(fn *lisp.LVal) *lisp.LVal {
	var slot *lisp.LVal
	env.AddBuiltins(false, &testBuiltinDef{name: "call-value-fn", formals: lisp.Formals(),
		fn: func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return slot }})
	return func(fn *lisp.LVal) *lisp.LVal {
		slot = fn
		return env.LoadString("call-form", "(let ([f (call-value-fn)]) (f 7))")
	}
}

// TestCallValueMatchesCallForm holds CallValue to the evaluator's call form
// (f 7) for an f of every type: a function gives the same value, and any
// other value the same condition.  The message names the type where the
// evaluator prints the value, so only the condition is compared for those.
// A symbol is refused even when it names a function, as the evaluator
// refuses it.
func TestCallValueMatchesCallForm(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", `(defun two (x) (list x x)) (defun raise (x) (error 'call-failed "no call for" x))`)
	require.False(t, rc.IsError(), "%v", rc)
	fns := oracle.Values()
	fns = append(fns,
		oracle.Value{V: lisp.Symbol("two"), Label: "symbol-fun"},
		oracle.Value{V: env.GetGlobal(lisp.Symbol("two")), Label: "lambda"},
		oracle.Value{V: env.GetGlobal(lisp.Symbol("raise")), Label: "lambda-raises"},
	)
	arg := lisp.Int(7)
	evalCallForm := callFormEvaluator(env)
	for _, fn := range fns {
		if fn.V.Type == lisp.LFun && fn.V.IsSpecialFun() {
			continue // the evaluator expands it; TestCallValueRefusesSpecialFun
		}
		got := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallValue(fn.V, arg)) })
		want := evalCallForm(fn.V)
		if fn.V.Type == lisp.LFun {
			sameResult(t, "call-value/"+fn.Label, got, want)
			continue
		}
		oracle.Same(t, "call-value/"+fn.Label, got, want)
		require.True(t, got.IsError(), fn.Label)
		if fn.V.Type == lisp.LError {
			sameResult(t, "call-value/"+fn.Label, got, want)
			continue
		}
		assert.Equal(t, "first element of expression is not a function: "+fn.V.Type.String(),
			(*lisp.ErrorVal)(got).ErrorMessage(), fn.Label)
	}
}

// TestCallValueRefusesSpecialFun checks that CallValue and CheckCallable
// refuse a macro or special operator with "error", which Go cannot expand.
func TestCallValueRefusesSpecialFun(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, name := range []string{"defun", "if"} {
		fn := env.GetGlobal(lisp.Symbol(name))
		require.Equal(t, lisp.LFun, fn.Type, name)
		got := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallValue(fn, lisp.Int(1))) })
		require.True(t, got.IsError(), name)
		assert.Equal(t, "error", got.Str, name)
		assert.Contains(t, (*lisp.ErrorVal)(got).ErrorMessage(), "first element of expression is not a regular function", name)
	}
}

// TestCheckCallableChecksContextFirst checks that a done context is
// reported before the type of fn, as the evaluator reports it before it
// evaluates the call form, and that CallValue charges no step of its own.
func TestCheckCallableChecksContextFirst(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	env := oracle.NewEnv(t, ctx)
	var got *lisp.LVal
	inEval(t, env, func(env *lisp.LEnv) *lisp.LVal {
		cancel()
		got = lisp.ValueOrError(lisp.Nil(), env.CheckCallable(lisp.Int(3)))
		return lisp.Nil()
	})
	require.True(t, got.IsError())
	assert.Equal(t, lisp.CondContextCancelled, got.Str)

	env = oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", `(defun id (x) x)`)
	require.False(t, rc.IsError(), "%v", rc)
	fn := env.GetGlobal(lisp.Symbol("id"))
	steps := func(f func(env *lisp.LEnv) *lisp.LVal) int64 {
		before := env.Runtime.TotalSteps()
		require.False(t, inEval(t, env, f).IsError())
		return env.Runtime.TotalSteps() - before
	}
	assert.Equal(t,
		steps(func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallFunction(fn, lisp.Int(1))) }),
		steps(func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallValue(fn, lisp.Int(1))) }))
}

// TestMapSeqFuncMatchesMapSeq holds MapSeqFunc to MapSeq for every kind and
// seq, with a Go f that calls the Lisp function MapSeq is given: the same
// value or error, and the same steps.
func TestMapSeqFuncMatchesMapSeq(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", mapSetup)
	require.False(t, rc.IsError(), "%v", rc)
	for _, name := range []string{"twice", "raise"} {
		for _, kind := range seqKinds {
			for i, seq := range oracle.Values() {
				label := "map-seq-func/" + kind.String() + "/" + name + "/" + seq.Label
				var gotSteps, wantSteps int64
				got := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal {
					fn := env.GetGlobal(lisp.Symbol(name))
					before := env.Runtime.TotalSteps()
					defer func() { gotSteps = env.Runtime.TotalSteps() - before }()
					return lisp.ValueOrError(env.MapSeqFunc(kind, oracle.Values()[i].V, func(c *lisp.LVal) (*lisp.LVal, error) {
						return env.CallFunction(fn, c)
					}))
				})
				want := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal {
					fn := env.GetGlobal(lisp.Symbol(name))
					before := env.Runtime.TotalSteps()
					defer func() { wantSteps = env.Runtime.TotalSteps() - before }()
					return lisp.ValueOrError(env.MapSeq(kind, fn, oracle.Values()[i].V))
				})
				sameResult(t, label, got, want)
				assert.Equal(t, wantSteps, gotSteps, label+" steps")
			}
		}
	}
}

// TestMapSeqFuncAllocLimit checks that MapSeqFunc makes map's allocation
// check before it calls f, and skips it for SeqNil, as map does.
func TestMapSeqFuncAllocLimit(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	env.Runtime.MaxAlloc = 2
	seq := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	calls := 0
	f := func(c *lisp.LVal) (*lisp.LVal, error) { calls++; return c, nil }
	got := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.MapSeqFunc(lisp.SeqVector, seq, f)) })
	require.True(t, got.IsError())
	assert.Contains(t, (*lisp.ErrorVal)(got).ErrorMessage(), "allocation size 3 exceeds maximum")
	assert.Zero(t, calls)
	got = inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.MapSeqFunc(lisp.SeqNil, seq, f)) })
	require.False(t, got.IsError(), "%v", got)
	assert.Equal(t, 3, calls)
}
