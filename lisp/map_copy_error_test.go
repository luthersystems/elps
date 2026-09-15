// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

type failedCopyEntriesMap struct{ lisp.Map }

func (failedCopyEntriesMap) Entries([]*lisp.LVal) *lisp.LVal {
	return lisp.ErrorConditionf("enumeration-failed", "cannot read backing store")
}

func TestMapCopyPropagatesEnumerationErrors(t *testing.T) {
	for _, expr := range []string{`(assoc source "a" 2)`, `(dissoc source "a")`} {
		t.Run(expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			backing := lisp.SortedMap()
			require.NoError(t, lisp.GoError(backing.Map().Set(lisp.String("a"), lisp.Int(1))))
			source := lisp.SortedMapFromData(lisp.NewMapData(failedCopyEntriesMap{backing.Map()}))
			env.PutGlobal(lisp.Symbol("source"), source)
			got := env.LoadString("map-copy-error.lisp", expr)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Contains(t, got.String(), "enumeration-failed")
			require.Contains(t, got.String(), "cannot read backing store")
			require.Equal(t, `(sorted-map "a" 1)`, backing.String())
		})
	}
}
