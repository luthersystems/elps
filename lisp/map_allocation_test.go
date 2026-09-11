// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestMapAllocationBoundaries(t *testing.T) {
	for _, size := range []int{7, 8, 9} {
		for _, tc := range []struct {
			expr string
			want int
		}{
			{`(keys source)`, size},
			{`(assoc source 'added 42)`, size + 1},
			{`(assoc source "key0" 42)`, size},
			{`(dissoc source 'absent)`, size},
			{`(apply sorted-map pairs)`, size},
		} {
			t.Run(fmt.Sprintf("%d/%s", size, tc.expr), func(t *testing.T) {
				env, err := (&elpstest.Runner{}).NewEnv(t)
				require.NoError(t, err)
				m := lisp.SortedMap()
				var pairs []*lisp.LVal
				for i := range size {
					key := lisp.String(fmt.Sprintf("key%d", i))
					value := lisp.Int(i)
					require.NoError(t, lisp.GoError(m.Map().Set(key, value)))
					pairs = append(pairs, key, value)
				}
				env.PutGlobal(lisp.Symbol("source"), m)
				env.PutGlobal(lisp.Symbol("pairs"), lisp.QExpr(pairs))
				env.Runtime.MaxAlloc = 8
				before := m.String()
				got := env.LoadString("map-allocation.lisp", tc.expr)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				require.Equal(t, before, m.String(), "allocating map operations must preserve their input")
				if tc.want > 8 {
					require.Equal(t, lisp.LError, got.Type, "%v", got)
					require.Contains(t, got.String(), "exceeds maximum (8)")
					return
				}
				require.NoError(t, lisp.GoError(got))
				require.Equal(t, tc.want, got.Len())
			})
		}
	}
}

func TestMapAllocationMutationAndDuplicateKeys(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	env.Runtime.MaxAlloc = 1
	for _, tc := range []struct{ expr, want string }{
		{`(sorted-map 'a 1 "a" 2 'a 3)`, `(sorted-map 'a 3)`},
		{`(set 'source (sorted-map 'a 1))`, `(sorted-map 'a 1)`},
		{`(assoc! source "a" 2)`, `(sorted-map 'a 2)`},
		{`(handler-bind ((condition (lambda (&rest _) 'caught))) (assoc! source 'b 3))`, `'caught`},
		{`source`, `(sorted-map 'a 2)`},
		{`(dissoc! source 'a)`, `(sorted-map)`},
		{`(assoc! source 'b 3)`, `(sorted-map 'b 3)`},
	} {
		got := env.LoadString("map-allocation.lisp", tc.expr)
		require.NoError(t, lisp.GoError(got), "%s", tc.expr)
		require.Equal(t, tc.want, got.String(), "%s", tc.expr)
	}
}

type allocationCaseFoldMap struct{ lisp.Map }

func (m allocationCaseFoldMap) Get(key *lisp.LVal) (*lisp.LVal, bool) {
	return m.Map.Get(lisp.String(strings.ToUpper(key.Str)))
}

func (m allocationCaseFoldMap) Set(key, value *lisp.LVal) *lisp.LVal {
	return m.Map.Set(lisp.String(strings.ToUpper(key.Str)), value)
}

func (m allocationCaseFoldMap) Del(key *lisp.LVal) *lisp.LVal {
	return m.Map.Del(lisp.String(strings.ToUpper(key.Str)))
}

func TestMapAllocationAssocUsesCopiedKeyIdentity(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	backing := lisp.SortedMap()
	require.NoError(t, lisp.GoError(backing.Map().Set(lisp.String("A"), lisp.Int(1))))
	source := lisp.SortedMapFromData(lisp.NewMapData(allocationCaseFoldMap{backing.Map()}))
	env.PutGlobal(lisp.Symbol("source"), source)
	env.Runtime.MaxAlloc = 1
	// The source folds case, but assoc copies into a stock case-sensitive
	// map: "a" adds a second key in the result, rather than replacing "A".
	got := env.LoadString("map-allocation.lisp", `(assoc source "a" 42)`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	require.Contains(t, got.String(), "exceeds maximum (1)")
	require.Equal(t, `(sorted-map "A" 1)`, source.String())
	got = env.LoadString("map-allocation.lisp", `(assoc source "A" 42)`)
	require.NoError(t, lisp.GoError(got))
	require.Equal(t, `(sorted-map "A" 42)`, got.String())
}
