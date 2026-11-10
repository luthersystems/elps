// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// callListProbe registers a Go function under name, as a special operator
// or as a builtin, and marks it as one whose call list is reused.  Only the
// functions of callListDroppers carry that mark outside this test.  The
// function records the length and the capacity of the list it is handed,
// and returns the list with one more cell appended.
func callListProbe(t *testing.T, env *LEnv, name string, operator bool) (*int, *int) {
	t.Helper()
	length, capacity := new(int), new(int)
	def := &langBuiltin{name: name, formals: Formals("a", "b"), fun: func(_ *LEnv, args *LVal) *LVal {
		*length, *capacity = len(args.Cells), cap(args.Cells)
		return QExpr(append(args.Cells, FreshInt(3)))
	}}
	if operator {
		env.AddSpecialOps(true, def)
	} else {
		env.AddBuiltins(true, def)
	}
	fn := env.Get(Symbol(name))
	require.Equal(t, LFun, fn.Type)
	fn.funData().dropsList = true
	require.True(t, reusesCallList(env.Runtime, fn))
	return length, capacity
}

// requireCallListIsClipped calls the probe name twice with two arguments,
// with a call of another function in between that takes the free list.
// The probe's list must have no spare capacity (bindOwned's contract), so
// what the probe appended to it must not sit in the array the next call
// writes its own arguments to.
func requireCallListIsClipped(t *testing.T, env *LEnv, name string, length, capacity *int) {
	t.Helper()
	call := func(a, b int) *LVal {
		return env.Eval(SExpr([]*LVal{Symbol(name), FreshInt(a), FreshInt(b)}))
	}
	for range 2 {
		first := call(1, 2)
		require.Equal(t, "'(1 2 3)", first.String())
		require.Equal(t, 2, *length)
		require.Equal(t, *length, *capacity, "a call list was handed over with spare capacity")

		second := call(7, 8)
		require.Equal(t, "'(7 8 3)", second.String())
		require.Equal(t, "'(1 2 3)", first.String(), "a later call wrote into a list an earlier call returned")
	}
}

// A special operator whose list is reused gets a list with no spare
// capacity, as it does from newSExprCap.
func TestCallListOfAnOperatorHasNoSpareCapacity(t *testing.T) {
	env := initSafetyTestEnv(t)
	length, capacity := callListProbe(t, env, "probe-op", true)
	requireCallListIsClipped(t, env, "probe-op", length, capacity)
}

// get returns the whole backing array, which put files the list by.  The
// evaluator clips the list's cells out of it.
func TestCallListGetReturnsTheBackingArray(t *testing.T) {
	for n, want := range map[int]int{1: 2, 2: 2, 3: 4, 4: 4, 5: 8, 8: 8, 9: 9} {
		var p callLists
		for range 2 {
			v, back := p.get(n)
			require.Equal(t, LSExpr, v.Type)
			require.Empty(t, back)
			require.Equal(t, want, cap(back), "n=%d", n)
			v.Cells = append(back[:0:n], FreshInt(1))
			p.put(v, back)
		}
	}
}
