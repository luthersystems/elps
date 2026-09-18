// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

// Issue #657: native macros can return another macro call without evaluating
// any Lisp. macroexpand must charge the repeated work to the enclosing budget.
func TestMacroexpandLimitsNativeLoop(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 8
	env.Runtime.MaxMacroExpansionDepth = 16
	calls := 0
	env.AddMacros(true, &langBuiltin{
		name: "native-loop", formals: Formals(),
		fun: func(*LEnv, *LVal) *LVal {
			calls++
			return SExpr([]*LVal{Symbol("native-loop")})
		},
	})
	got := env.Eval(SExpr([]*LVal{Symbol("macroexpand"), QExpr([]*LVal{Symbol("native-loop")})}))
	requireCondition(t, got, CondStepLimitExceeded)
	require.False(t, IsInternalPanic(got))
	// The call expression, function symbol and quoted argument consume three
	// steps. Five expansion turns fit; the sixth must stop before its callback.
	require.Equal(t, 5, calls)
	require.Equal(t, int64(9), env.Runtime.Steps())
	require.Equal(t, 3, env.Eval(Int(3)).Int, "a fresh evaluation refills its budget")
	require.Equal(t, int64(1), env.Runtime.Steps())
}

func TestMacroexpandLimitsFiniteChainAtBudget(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 7
	calls := 0
	env.AddMacros(true, &langBuiltin{
		name: "native-countdown", formals: Formals("n"),
		fun: func(_ *LEnv, args *LVal) *LVal {
			calls++
			n := args.Cells[0].Int
			if n == 0 {
				return Int(42)
			}
			return SExpr([]*LVal{Symbol("native-countdown"), Int(n - 1)})
		},
	})
	got := env.Eval(SExpr([]*LVal{Symbol("macroexpand"), QExpr([]*LVal{Symbol("native-countdown"), Int(3)})}))
	require.Equal(t, LInt, got.Type, "%v", got)
	require.Equal(t, 42, got.Int)
	require.Equal(t, 4, calls)
	require.Equal(t, int64(7), env.Runtime.Steps(), "three entry steps plus exactly four expansions")
}

func TestMacroexpandLimitsNoopAndOneShotAccounting(t *testing.T) {
	for _, form := range []*LVal{QExpr(nil), QExpr([]*LVal{Int(7)}), QExpr([]*LVal{Symbol("+")})} {
		t.Run(form.String(), func(t *testing.T) {
			env := initSafetyTestEnv(t)
			env.Runtime.maxSteps = 3
			got := env.Eval(SExpr([]*LVal{Symbol("macroexpand"), form}))
			require.Same(t, form, got)
			require.Equal(t, int64(3), env.Runtime.Steps(), "no macro means no additional expansion step")
			got = env.FunCall(env.Get(Symbol("macroexpand")), SExpr([]*LVal{form}))
			require.Same(t, form, got)
			require.Zero(t, env.Runtime.Steps(), "a direct no-op call evaluates nothing")
		})
	}
	t.Run("single expansion APIs", func(t *testing.T) {
		env := initSafetyTestEnv(t)
		env.Runtime.maxSteps = 3
		calls := 0
		env.AddMacros(true, &langBuiltin{
			name: "native-once", formals: Formals(),
			fun: func(*LEnv, *LVal) *LVal {
				calls++
				return SExpr([]*LVal{Symbol("native-once")})
			},
		})
		got := env.Eval(SExpr([]*LVal{Symbol("macroexpand-1"), QExpr([]*LVal{Symbol("native-once")})}))
		require.Equal(t, LSExpr, got.Type, "%v", got)
		require.Equal(t, "'(native-once)", got.String())
		require.Equal(t, 1, calls)
		require.Equal(t, int64(3), env.Runtime.Steps())
		got = env.MacroCall(env.Get(Symbol("native-once")), SExpr(nil))
		require.Equal(t, LMarkMacExpand, got.Type, "%v", got)
		require.Equal(t, 2, calls)
		require.Zero(t, env.Runtime.Steps(), "direct native MacroCall adds no evaluation step")
	})
}

func TestMacroexpandLimitsCancellationAndReuse(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 8
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	calls := 0
	env.AddMacros(true, &langBuiltin{
		name: "native-cancel", formals: Formals(),
		fun: func(*LEnv, *LVal) *LVal {
			calls++
			cancel()
			return SExpr([]*LVal{Symbol("native-cancel")})
		},
	})
	got := env.EvalContext(ctx, SExpr([]*LVal{Symbol("macroexpand"), QExpr([]*LVal{Symbol("native-cancel")})}))
	requireCondition(t, got, CondContextCancelled)
	require.False(t, IsInternalPanic(got))
	require.Equal(t, 1, calls, "cancellation must prevent a second native invocation")
	require.Equal(t, int64(5), env.Runtime.Steps())
	got = env.Eval(SExpr([]*LVal{Symbol("+"), Int(1), Int(2)}))
	require.Equal(t, LInt, got.Type, "%v", got)
	require.Equal(t, 3, got.Int, "cancelled context must not leak into the next evaluation")
}

func TestMacroexpandLimitsPreserveDepthGuard(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.Runtime.maxSteps = 100
	env.Runtime.MaxMacroExpansionDepth = 4
	calls := 0
	env.AddMacros(true, &langBuiltin{
		name: "native-depth", formals: Formals(),
		fun: func(*LEnv, *LVal) *LVal {
			calls++
			return SExpr([]*LVal{Symbol("native-depth")})
		},
	})
	got := env.Eval(SExpr([]*LVal{Symbol("macroexpand"), QExpr([]*LVal{Symbol("native-depth")})}))
	requireCondition(t, got, "error")
	require.False(t, IsInternalPanic(got))
	require.Contains(t, got.String(), "macro expansion depth exceeded (5 expansions)")
	require.Equal(t, 5, calls, "the historical depth+1 stopping point stays unchanged")
	require.Equal(t, int64(8), env.Runtime.Steps())
}
