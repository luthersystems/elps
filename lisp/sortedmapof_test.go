// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var refSortedMap = lisp.BuiltinFunc("sorted-map")

// callInEval runs f inside an evaluation of env, so f sees the evaluation's
// context and step counter.  When cancel is true the context is cancelled
// after the evaluation starts and before f runs.  It returns f's result and
// the steps f used.
func callInEval(t *testing.T, env *lisp.LEnv, cancel bool, f func(env *lisp.LEnv) *lisp.LVal) (*lisp.LVal, int64) {
	t.Helper()
	ctx, stop := context.WithCancel(context.Background())
	defer stop()
	var (
		got   *lisp.LVal
		steps int64
		ran   bool
	)
	fn := lisp.Fun("call-in-eval", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
		ran = true
		if cancel {
			stop()
		}
		before := env.Runtime.Steps()
		got = f(env)
		steps = env.Runtime.Steps() - before
		return lisp.Nil()
	})
	env.FunCallContext(ctx, fn, lisp.SExpr(nil))
	require.True(t, ran, "the evaluation did not call f")
	require.NotNil(t, got)
	return got, steps
}

// SortedMapOf returns what CallBuiltin(sorted-map) returns for every input:
// the same value, or the same error condition and message.  Neither charges a
// step, and neither writes the caller's slice.
func TestSortedMapOfMatchesBuiltin(t *testing.T) {
	cases := []struct {
		name     string
		kv       []*lisp.LVal
		maxAlloc int
		cancel   bool
		wantErr  bool
	}{
		{name: "pairs", kv: []*lisp.LVal{lisp.String("b"), lisp.Int(2), lisp.Int(7), lisp.Int(1), lisp.Symbol("a"), lisp.String("x")}},
		{name: "empty", kv: nil},
		{name: "odd arguments", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.String("b")}, wantErr: true},
		{name: "one argument", kv: []*lisp.LVal{lisp.String("a")}, wantErr: true},
		{name: "allocation cap on insert", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.String("b"), lisp.Int(2), lisp.String("c"), lisp.Int(3)}, maxAlloc: 2, wantErr: true},
		{name: "allocation cap replacement", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.String("b"), lisp.Int(2), lisp.Symbol("a"), lisp.Int(3)}, maxAlloc: 2},
		{name: "cancelled context", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1)}, cancel: true, wantErr: true},
		{name: "cancelled context odd arguments", kv: []*lisp.LVal{lisp.String("a")}, cancel: true, wantErr: true},
		{name: "refused key", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.Float(1.5), lisp.Int(2)}, wantErr: true},
		{name: "refused key at allocation cap", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.Float(1.5), lisp.Int(2)}, maxAlloc: 1, wantErr: true},
		{name: "duplicate keys", kv: []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.Symbol("a"), lisp.Int(2), lisp.Int(1), lisp.Int(3), lisp.Int(1), lisp.Int(4)}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			env.Runtime.MaxAlloc = tc.maxAlloc
			kv := append([]*lisp.LVal(nil), tc.kv...)

			want, wantSteps := callInEval(t, env, tc.cancel, func(env *lisp.LEnv) *lisp.LVal {
				return env.CallBuiltin(refSortedMap, kv...)
			})
			got, gotSteps := callInEval(t, env, tc.cancel, func(env *lisp.LEnv) *lisp.LVal {
				return env.SortedMapOf(kv...)
			})

			assert.Equal(t, int64(0), wantSteps, "CallBuiltin steps")
			assert.Equal(t, int64(0), gotSteps, "SortedMapOf steps")
			require.Equal(t, tc.kv, kv, "the kv slice changed")
			for i := range kv {
				require.Same(t, tc.kv[i], kv[i], "kv[%d] changed", i)
			}

			require.Equal(t, want.Type, got.Type, "%v vs %v", want, got)
			if tc.wantErr {
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				assert.Equal(t, want.Str, got.Str, "error condition")
				assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage())
				if tc.cancel {
					assert.Equal(t, lisp.CondContextCancelled, got.Str)
				}
				return
			}
			require.Equal(t, lisp.LSortMap, got.Type, "%v", got)
			assert.Equal(t, want.String(), got.String())
			assert.Equal(t, want.Len(), got.Len())
		})
	}
}

// The error messages SortedMapOf returns are the sorted-map builtin's own.
func TestSortedMapOfErrorMessages(t *testing.T) {
	env := newLimitTestEnv(t)
	v := env.SortedMapOf(lisp.String("a"), lisp.Int(1), lisp.String("b"))
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, "uneven number of arguments: 3", (*lisp.ErrorVal)(v).ErrorMessage())

	env.Runtime.MaxAlloc = 1
	v = env.SortedMapOf(lisp.String("a"), lisp.Int(1), lisp.String("b"), lisp.Int(2))
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, "allocation size 2 exceeds maximum (1)", (*lisp.ErrorVal)(v).ErrorMessage())
}
