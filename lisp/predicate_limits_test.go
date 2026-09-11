// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestPredicateLimitsStopThreadAfterArgumentCancellation(t *testing.T) {
	for _, thread := range []string{"thread-first", "thread-last"} {
		t.Run(thread, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			arguments, calls := 0, 0
			env.AddBuiltins(true,
				elpsutil.Function("cancel-argument", lisp.Formals(),
					func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
						arguments++
						cancel()
						return lisp.Int(2)
					}),
				elpsutil.Function("pending-call", lisp.Formals("a", "b"),
					func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
						calls++
						return lisp.Int(3)
					}))
			got := env.LoadStringContext(ctx, "predicate-limits.lisp", fmt.Sprintf(`(%s 1 (pending-call (cancel-argument)))`, thread))
			assert.Equal(t, 1, arguments, "the last explicit argument must be evaluated")
			assert.Zero(t, calls, "cancellation while evaluating the last argument must prevent the pending native call")
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, lisp.CondContextCancelled, got.Str)
		})
	}
}

func TestPredicateLimitsObserveCancellationBetweenCallbacks(t *testing.T) {
	for _, tc := range []struct {
		name            string
		expr            string
		cancelKeyAt     int
		wantKeys        int
		wantPredicates  int
		predicateResult bool
	}{
		{"all predicate", `(all? predicate source)`, 0, 0, 1, true},
		{"any predicate", `(any? predicate source)`, 0, 0, 1, false},
		{"sort predicate", `(stable-sort predicate source)`, 0, 0, 1, false},
		{"insert predicate", `(insert-sorted 'vector source predicate 0)`, 0, 0, 1, false},
		{"sort first key", `(stable-sort predicate source key)`, 1, 1, 0, false},
		{"sort second key", `(stable-sort predicate source key)`, 2, 2, 0, false},
		{"sort predicate after keys", `(stable-sort predicate source key)`, 0, 2, 1, false},
		{"insert first key", `(insert-sorted 'vector source predicate 0 key)`, 1, 1, 0, false},
		{"insert second key", `(insert-sorted 'vector source predicate 0 key)`, 2, 2, 0, false},
		{"insert predicate after keys", `(insert-sorted 'vector source predicate 0 key)`, 0, 2, 1, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newPredicateValuesEnv(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			env.PutGlobal(lisp.Symbol("source"), lisp.Vector([]*lisp.LVal{
				lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4),
				lisp.Int(5), lisp.Int(6), lisp.Int(7), lisp.Int(8),
			}))
			keys, predicates := 0, 0
			// Native callbacks deliberately do not evaluate Lisp: any limit
			// check must come from the higher-order operation itself.
			env.AddBuiltins(true,
				elpsutil.Function("predicate", lisp.Formals(lisp.VarArgSymbol, "values"),
					func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
						predicates++
						if tc.cancelKeyAt == 0 {
							cancel()
						}
						return lisp.Bool(tc.predicateResult)
					}),
				elpsutil.Function("key", lisp.Formals("value"),
					func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						keys++
						if keys == tc.cancelKeyAt {
							cancel()
						}
						return args.Cells[0]
					}))

			got := env.LoadStringContext(ctx, "predicate-limits.lisp", tc.expr)
			assert.Equal(t, tc.wantKeys, keys, "no further key may run after cancellation")
			assert.Equal(t, tc.wantPredicates, predicates, "no further predicate may run after cancellation")
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "cancellation must stop callback processing: %v", got)
			assert.Equal(t, lisp.CondContextCancelled, got.Str)
		})
	}
}

func TestPredicateLimitsBoundNativeCallbackSteps(t *testing.T) {
	const (
		budget = 10
		size   = 128
	)
	for _, tc := range []struct {
		name            string
		expr            string
		predicateResult bool
	}{
		{"all", `(all? predicate source)`, true},
		{"any", `(any? predicate source)`, false},
		{"sort", `(stable-sort predicate source)`, false},
		{"sort key", `(stable-sort predicate source key)`, false},
		{"insert", `(insert-sorted 'vector source predicate 0)`, false},
		{"insert key", `(insert-sorted 'vector source predicate 0 key)`, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, limit := range []int64{10000, budget} {
				env := newPredicateValuesEnv(t)
				require.NoError(t, lisp.GoError(lisp.WithMaxSteps(limit)(env)))
				cells := make([]*lisp.LVal, size)
				for i := range cells {
					cells[i] = lisp.Int(i + 1)
				}
				// Host-provided data avoids spending the budget constructing
				// a sequence before the first native callback is reached.
				env.PutGlobal(lisp.Symbol("source"), lisp.Vector(cells))
				calls := 0
				env.AddBuiltins(true,
					elpsutil.Function("predicate", lisp.Formals(lisp.VarArgSymbol, "values"),
						func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
							calls++
							return lisp.Bool(tc.predicateResult)
						}),
					elpsutil.Function("key", lisp.Formals("value"),
						func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
							calls++
							return args.Cells[0]
						}))

				got := env.LoadString("predicate-limits.lisp", tc.expr)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				if limit == budget {
					assert.Positive(t, calls, "the budget must allow the operation to enter its callback loop")
					assert.LessOrEqual(t, calls, budget, "native callbacks must consume the evaluation budget")
					require.Equal(t, lisp.LError, got.Type, "a native callback loop must hit its step limit: %v", got)
					assert.Equal(t, lisp.CondStepLimitExceeded, got.Str)
					continue
				}
				require.NotEqual(t, lisp.LError, got.Type, "the same bounded input must succeed with enough budget: %v", got)
				if tc.name == "all" || tc.name == "any" {
					assert.Equal(t, size, calls, "the control must traverse every element")
					assert.Equal(t, lisp.Bool(tc.predicateResult).String(), got.String())
				} else if tc.name == "insert" || tc.name == "insert key" {
					assert.Equal(t, size+1, got.Len())
					assert.Equal(t, "0", got.ArrayIndex(lisp.Int(size)).String())
				} else {
					assert.Equal(t, size, got.Len())
					assert.Equal(t, "1", got.ArrayIndex(lisp.Int(0)).String())
					assert.Equal(t, "128", got.ArrayIndex(lisp.Int(size-1)).String())
				}
			}
		})
	}
}
