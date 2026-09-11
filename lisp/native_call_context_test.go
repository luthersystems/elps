// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: a native final argument can cancel the request after the
// expression-entry check. The pending call must not perform its side effect.
func TestNativeCallCancellationBeforeEntry(t *testing.T) {
	for _, code := range []string{
		`(pending (cancel-argument))`,
		`(funcall pending (cancel-argument))`,
		`(apply pending (cancel-arguments))`,
		`(unpack pending (cancel-arguments))`,
		`(new box (cancel-argument))`,
	} {
		t.Run(code, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			arguments, calls := 0, 0
			env.AddBuiltins(true,
				elpsutil.Function("pending", lisp.Formals("value"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
					calls++
					return args.Cells[0]
				}),
				elpsutil.Function("cancel-argument", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					arguments++
					cancel()
					return lisp.Int(9)
				}),
				elpsutil.Function("cancel-arguments", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					arguments++
					cancel()
					return lisp.QExpr([]*lisp.LVal{lisp.Int(9)})
				}))
			setup := env.LoadString("native-setup.lisp", `(set 'box (new lisp:typedef 'user:box pending))`)
			require.NotEqual(t, lisp.LError, setup.Type, "%v", setup)
			got := env.LoadStringContext(ctx, "native-context.lisp", code)
			require.Equal(t, 1, arguments)
			require.Zero(t, calls)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			require.Equal(t, lisp.CondContextCancelled, got.Str)
			// A new uncancelled request may use the same runtime normally.
			next := env.LoadStringContext(context.Background(), "next-request.lisp", `(pending 7)`)
			require.Equal(t, lisp.LInt, next.Type, "%v", next)
			require.Equal(t, 7, next.Int)
			require.Equal(t, 1, calls)
		})
	}
}

func TestNativeCallContextRestoredAfterPanic(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	env.AddBuiltins(true, elpsutil.Function("host-panic", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		cancel()
		panic("test host fault")
	}))
	got := env.LoadStringContext(ctx, "panic-request.lisp", `(host-panic)`)
	require.True(t, lisp.IsInternalPanic(got), "%v", got)
	require.NotEmpty(t, got.CallStack().GoStack)
	// Legacy entry points inherit the active context only during a callback;
	// a completed, recovered call must not leave its cancelled context here.
	next := env.LoadString("next-request.lisp", `(+ 1 2)`)
	require.Equal(t, lisp.LInt, next.Type, "%v", next)
	require.Equal(t, 3, next.Int)
}

func TestNativeFunCallContextRejectsCancelledContext(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	calls := 0
	fun := lisp.FunInPackage(lisp.DefaultUserPackage, "native", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		calls++
		return lisp.Int(9)
	})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	got := env.FunCallContext(ctx, fun, lisp.SExpr(nil))
	require.Zero(t, calls)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.Equal(t, lisp.CondContextCancelled, got.Str)
	got = env.FunCallContext(context.Background(), fun, lisp.SExpr(nil))
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	require.Equal(t, 9, got.Int)
	require.Equal(t, 1, calls)
	require.Zero(t, env.Runtime.Steps(), "the cancellation guard must not charge an evaluation step")
}

func TestNativeCallContextRestoredInDistinctTerminalEnvironment(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	child := lisp.NewEnv(env)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	env.AddBuiltins(true, elpsutil.Function("child-terminal", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		return child.Terminal(lisp.Int(1))
	}))
	got := env.LoadStringContext(ctx, "child-terminal.lisp", `(child-terminal)`)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	require.Equal(t, 1, got.Int)
	cancel()
	next := child.LoadString("next-child-request.lisp", `(+ 1 2)`)
	require.Equal(t, lisp.LInt, next.Type, "%v", next)
	require.Equal(t, 3, next.Int)
}

func TestNativeCallContextRestoredAfterTerminalResult(t *testing.T) {
	for _, tc := range []struct {
		code      string
		wantError bool
	}{{`(if true 1 2)`, false}, {`(progn 1)`, false}, {`(if true (error 'expected) ())`, true}} {
		t.Run(tc.code, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			got := env.LoadStringContext(ctx, "terminal-context.lisp", tc.code)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			if tc.wantError {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Equal(t, "expected", got.Str)
			} else {
				require.Equal(t, lisp.LInt, got.Type)
				require.Equal(t, 1, got.Int)
			}
			cancel()
			next := env.LoadString("next-request.lisp", `(+ 1 2)`)
			require.Equal(t, lisp.LInt, next.Type, "%v", next)
			require.Equal(t, 3, next.Int)
		})
	}
}

func TestNativeCallCancellationBeforeSpecialEntry(t *testing.T) {
	for _, macro := range []bool{false, true} {
		t.Run(map[bool]string{false: "special", true: "macro"}[macro], func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			calls, cancels := 0, 0
			fn := elpsutil.Function("pending-special", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				calls++
				return lisp.Int(9)
			})
			if macro {
				env.AddMacros(true, fn)
			} else {
				env.AddSpecialOps(true, fn)
			}
			env.AddBuiltins(true, elpsutil.Function("cancel-function", lisp.Formals("function"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				cancels++
				cancel()
				return args.Cells[0]
			}))
			got := env.LoadStringContext(ctx, "special-context.lisp", `((cancel-function pending-special))`)
			require.Equal(t, 1, cancels)
			require.Zero(t, calls)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			require.Equal(t, lisp.CondContextCancelled, got.Str)
		})
	}
}
