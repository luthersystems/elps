// Copyright © 2026 The ELPS authors

package oracle_test

import (
	"context"
	"math"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// CancelAt reports cancelled from its n-th Err call on, and never for n 0.
func TestCancelAt(t *testing.T) {
	c := oracle.NewCancelAt(2)
	require.NoError(t, c.Err())
	require.ErrorIs(t, c.Err(), context.Canceled)
	require.ErrorIs(t, c.Err(), context.Canceled)
	assert.Equal(t, 3, c.Checks())
	never := oracle.NewCancelAt(0)
	for range 5 {
		require.NoError(t, never.Err())
	}
}

// Values builds new values on each call, so a test may mutate them.
func TestValuesAreFresh(t *testing.T) {
	a, b := oracle.Values(), oracle.Values()
	require.Len(t, b, len(a))
	labels := map[string]bool{}
	for i := range a {
		assert.Equal(t, a[i].Label, b[i].Label)
		assert.False(t, labels[a[i].Label], "duplicate label %s", a[i].Label)
		labels[a[i].Label] = true
		if !a[i].V.IsNil() && (a[i].V.Type == lisp.LSExpr || a[i].V.Type == lisp.LSortMap || a[i].V.Type == lisp.LArray) {
			assert.NotSame(t, a[i].V, b[i].V, a[i].Label)
		}
	}
}

// Same passes on equal values and on errors of one condition, and fails on
// a different value, a different type or a different condition.
func TestSame(t *testing.T) {
	ok := func(got, want *lisp.LVal) {
		t.Helper()
		ft := &testing.T{}
		oracle.Same(ft, "x", got, want)
		assert.False(t, ft.Failed(), "%v vs %v", got, want)
	}
	bad := func(got, want *lisp.LVal) {
		t.Helper()
		ft := &testing.T{}
		oracle.Same(ft, "x", got, want)
		assert.True(t, ft.Failed(), "%v vs %v", got, want)
	}
	ok(lisp.Int(1), lisp.Int(1))
	ok(lisp.ErrorConditionf("c", "one"), lisp.ErrorConditionf("c", "two"))
	bad(lisp.Int(1), lisp.Int(2))
	bad(lisp.Int(1), lisp.Float(1))
	bad(lisp.Float(0), lisp.Float(math.Copysign(0, -1)))
	ok(lisp.Float(math.NaN()), lisp.Float(math.NaN()))
	bad(lisp.ErrorConditionf("c", "x"), lisp.ErrorConditionf("d", "x"))
	bad(lisp.Int(1), lisp.ErrorConditionf("c", "x"))
}
