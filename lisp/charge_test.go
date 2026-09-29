// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// stepsOf evaluates src under a context (so steps are counted) and returns
// the result and the steps the evaluation took.
func stepsOf(t *testing.T, env *lisp.LEnv, src string) (*lisp.LVal, int64) {
	t.Helper()
	v := env.LoadStringContext(context.Background(), "test", src)
	return v, env.Runtime.Steps()
}

// helperBuiltin registers (name n) in the user package, calling f with n.
func helperBuiltin(env *lisp.LEnv, name string, f func(env *lisp.LEnv, n int) *lisp.LVal) {
	env.AddBuiltins(false, &testBuiltinDef{name: name, formals: lisp.Formals("n"),
		fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			if lerr := f(env, args.Cells[0].Int); lerr != nil && lerr.Type == lisp.LError {
				return lerr
			}
			return lisp.Symbol("ok")
		}})
}

type testBuiltinDef struct {
	formals *lisp.LVal
	fn      lisp.LBuiltin
	name    string
}

func (d *testBuiltinDef) Name() string                                    { return d.name }
func (d *testBuiltinDef) Formals() *lisp.LVal                             { return d.formals }
func (d *testBuiltinDef) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return d.fn(env, args) }

func TestChargeHelpersArithmetic(t *testing.T) {
	cases := []struct {
		n                         int
		complete, started, record int64
	}{
		{-5000, 0, 0, 1},
		{-1, 0, 0, 1},
		{0, 0, 0, 1},
		{1, 0, 1, 1},
		{1023, 0, 1, 1},
		{1024, 1, 1, 1},
		{1025, 1, 2, 2},
		{2047, 1, 2, 2},
		{2048, 2, 2, 2},
		{2049, 2, 3, 3},
		{10 << 20, 10 << 10, 10 << 10, 10 << 10},
	}
	env := newLimitTestEnv(t)
	helperBuiltin(env, "nop", func(*lisp.LEnv, int) *lisp.LVal { return nil })
	helperBuiltin(env, "complete", lisp.ChargeCompleteKiB)
	helperBuiltin(env, "started", lisp.ChargeStartedKiB)
	helperBuiltin(env, "record", lisp.ChargeRecord)
	for _, tc := range cases {
		src := func(f string) string { return "(" + f + " " + itoa(tc.n) + ")" }
		_, base := stepsOf(t, env, src("nop"))
		for _, c := range []struct {
			f    string
			want int64
		}{{"complete", tc.complete}, {"started", tc.started}, {"record", tc.record}} {
			v, got := stepsOf(t, env, src(c.f))
			require.Equal(t, lisp.LSymbol, v.Type, "%s: %v", src(c.f), v)
			assert.Equal(t, c.want, got-base, "%s", src(c.f))
		}
	}
}

// The substrate conventions the helpers replace, verbatim (ceil via
// (n+1023)/1024; record adds a floor of one), agree with the helpers on every
// non-negative size.
func TestChargeHelpersMatchLegacyFormulas(t *testing.T) {
	env := newLimitTestEnv(t)
	helperBuiltin(env, "nop", func(*lisp.LEnv, int) *lisp.LVal { return nil })
	helperBuiltin(env, "started", lisp.ChargeStartedKiB)
	helperBuiltin(env, "legacy-started", func(env *lisp.LEnv, n int) *lisp.LVal {
		if v := env.ChargeSteps(int64((n + 1023) / 1024)); v.Type == lisp.LError {
			return v
		}
		return nil
	})
	helperBuiltin(env, "complete", lisp.ChargeCompleteKiB)
	helperBuiltin(env, "legacy-complete", func(env *lisp.LEnv, n int) *lisp.LVal {
		if n < 1024 {
			return nil
		}
		if v := env.ChargeSteps(int64(n >> 10)); v.Type == lisp.LError {
			return v
		}
		return nil
	})
	for n := 0; n < 5000; n += 7 {
		for _, pair := range [][2]string{{"started", "legacy-started"}, {"complete", "legacy-complete"}} {
			_, a := stepsOf(t, env, "("+pair[0]+" "+itoa(n)+")")
			_, b := stepsOf(t, env, "("+pair[1]+" "+itoa(n)+")")
			require.Equal(t, b, a, "%s vs %s at n=%d", pair[0], pair[1], n)
		}
	}
}

func TestStepAndCheckContext(t *testing.T) {
	env := newLimitTestEnv(t)
	helperBuiltin(env, "nop", func(*lisp.LEnv, int) *lisp.LVal { return nil })
	helperBuiltin(env, "steps", func(env *lisp.LEnv, n int) *lisp.LVal {
		for range n {
			if lerr := env.Step(); lerr.Type == lisp.LError {
				return lerr
			}
		}
		return nil
	})
	helperBuiltin(env, "check", func(env *lisp.LEnv, _ int) *lisp.LVal { return env.CheckContext() })
	_, base := stepsOf(t, env, "(nop 0)")
	_, got := stepsOf(t, env, "(steps 5)")
	assert.Equal(t, int64(5), got-base)
	_, got = stepsOf(t, env, "(check 0)")
	assert.Equal(t, base, got, "CheckContext charges nothing")

	// A done context: CheckContext and Step report the evaluator's own
	// condition and message.
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	res := lisp.Fun("check", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		return env.CheckContext()
	})
	v := env.FunCallContext(ctx, res, lisp.SExpr(nil))
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, lisp.CondContextCancelled, v.Str)
	assert.Contains(t, lisp.GoError(v).Error(), "context cancelled: context canceled")

	// Without a context or a limit, nothing is counted and nothing fails.
	plain := newLimitTestEnv(t)
	assert.True(t, plain.Step().IsNil())
	assert.True(t, plain.CheckContext().IsNil())
	assert.True(t, lisp.ChargeStartedKiB(plain, 1<<20).IsNil())
}

func TestChargeHelpersEnforceBudget(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(50))
	helperBuiltin(env, "started", lisp.ChargeStartedKiB)
	v, _ := stepsOf(t, env, "(started 1000000)")
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, lisp.CondStepLimitExceeded, v.Str)
}

func TestChargeCompleteKiBSmallIsFree(t *testing.T) {
	env := newLimitTestEnv(t)
	allocs := testing.AllocsPerRun(100, func() {
		if lisp.ChargeCompleteKiB(env, 100).Type == lisp.LError || env.CheckContext().Type == lisp.LError {
			t.Fatal("unexpected error")
		}
	})
	assert.Zero(t, allocs)
}

func itoa(n int) string {
	return lisp.Int(n).String()
}

// Every #745 helper that only reports success or failure follows one
// convention, the one LEnv.ChargeSteps and package loaders already use:
// lisp.Nil() to continue, or the LError to return.  So `lerr.Type ==
// lisp.LError` is always safe on a result and never dereferences Go nil.
func TestStatusHelpersReturnNilValue(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxSteps(1<<40))
	a := lisp.ReadArgs(env, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	a.Int(0, "first argument")
	for name, v := range map[string]*lisp.LVal{
		"Step":             env.Step(),
		"CheckContext":     env.CheckContext(),
		"ChargeStartedKiB": lisp.ChargeStartedKiB(env, 5000),
		"ChargeRecord":     lisp.ChargeRecord(env, 0),
		"ArgReader.Err":    a.Err(),
		"BindBuiltins":     env.BindBuiltins(lisp.BindOpts{}, constBuiltin("status-helper", 1)),
	} {
		require.NotNil(t, v, name)
		assert.NotEqual(t, lisp.LError, v.Type, name)
		assert.True(t, v.IsNil(), name)
	}
}
