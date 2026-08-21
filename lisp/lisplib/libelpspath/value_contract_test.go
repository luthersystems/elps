// Copyright © 2026 Luther Systems, Ltd. All right reserved.

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The ?-family's VALUE contract, pinned so it cannot drift silently
// (issue #399).
//
// The non-mutating ops return a copy that is independent of the SOURCE
// document -- that is #395's contract, held by the alias battery in
// path_alias_test.go.  The supplied NEW VALUE is the other side of the same
// node, and its contract is the opposite: it is stored by reference.  Both
// answers were defensible when #399 was filed; this is the characterization
// test for the one that was chosen, the same treatment AddPackage's sharing
// contract got.  Kept deliberately: copy-on-store would cost an allocation on
// a path substrate runs per transaction, would surprise a caller who passed a
// large structure expecting it to be shared, and would also have to change
// ?set!'s aliasing semantics to stay coherent.  The docstrings for ?set and
// ?set! state the contract; if a future change makes these tests fail, the
// docstrings and the callers relying on sharing (shirocore's blend-paths)
// must change with it.

// TestSetStoresTheSuppliedValueByReference is the issue's own reproduction:
// the value handed to a COPYING ?set is reachable and mutable through the
// result, so a write through the result rewrites the caller's value.
func TestSetStoresTheSuppliedValueByReference(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	doc := lisp.SortedMap()
	doc.MapSet("tag", lisp.String("x"))

	// A vector, as in the issue's reproduction: lists refuse in-place path
	// ops (errMutateList), so a vector is the value the reach is visible on.
	v := lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})

	result := callBuiltin(env, BuiltinQuerySet, doc, lisp.String("k"), v)
	require.NotEqual(t, lisp.LError, result.Type, "%v", result)

	stored := callBuiltin(env, BuiltinQueryGet, result, lisp.String("k"))
	require.NotEqual(t, lisp.LError, stored.Type, "%v", stored)
	assert.Same(t, v, stored,
		"?set stores the supplied value by reference; a copy here is a contract change (#399)")

	// The reach the pointer identity implies, spelled out: an in-place write
	// through the result rewrites the caller's value.
	got := callBuiltin(env, BuiltinQuerySetMutate,
		result, lisp.String("k"), lisp.Int(0), lisp.Int(99))
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	elem := callBuiltin(env, BuiltinQueryGet, v, lisp.Int(0))
	require.NotEqual(t, lisp.LError, elem.Type, "%v", elem)
	assert.Equal(t, 99, elem.Int,
		"the write through the result must reach the caller's value under the by-reference contract (#399)")
}

// TestSetMutateStoresTheSuppliedValueByReference pins the same contract for
// the mutating op, which #399 notes must stay coherent with the copying one.
func TestSetMutateStoresTheSuppliedValueByReference(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	doc := lisp.SortedMap()
	v := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})

	result := callBuiltin(env, BuiltinQuerySetMutate, doc, lisp.String("k"), v)
	require.NotEqual(t, lisp.LError, result.Type, "%v", result)

	stored, ok := doc.Map().Get(lisp.String("k"))
	require.True(t, ok)
	assert.Same(t, v, stored,
		"?set! stores the supplied value by reference (#399)")
}
