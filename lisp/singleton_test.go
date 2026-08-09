// Copyright © 2025 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestIsSingleton(t *testing.T) {
	assert.True(t, isSingleton(Nil()))
	assert.True(t, isSingleton(Bool(true)))
	assert.True(t, isSingleton(Bool(false)))

	// Fresh allocations are not singletons.
	assert.False(t, isSingleton(SExpr(nil)))
	assert.False(t, isSingleton(Symbol("x")))
	assert.False(t, isSingleton(Int(0)))
	assert.False(t, isSingleton(String("")))

	// Nil pointer is not a singleton.
	assert.False(t, isSingleton(nil))
}

func TestAssertNotSingleton_Panics(t *testing.T) {
	assert.Panics(t, func() { assertNotSingleton(Nil()) })
	assert.Panics(t, func() { assertNotSingleton(Bool(true)) })
	assert.Panics(t, func() { assertNotSingleton(Bool(false)) })
}

func TestAssertNotSingleton_AllowsNonSingleton(t *testing.T) {
	assert.NotPanics(t, func() { assertNotSingleton(SExpr(nil)) })
	assert.NotPanics(t, func() { assertNotSingleton(Symbol("x")) })
	assert.NotPanics(t, func() { assertNotSingleton(Int(0)) })
}

func TestSingletonSnapshot_DetectsMutation(t *testing.T) {
	// This test writes to a singleton on purpose. Pause the write
	// watchdog so the deliberate mutation is ordered against its reads
	// and does not show up as a data race under `make race`.
	defer pauseSingletonWatchdog()()

	// Save originals so we can restore after the test — TestMain
	// snapshot will verify singletons are intact at suite end.
	origSource := singletonTrue.Source
	defer func() { singletonTrue.Source = origSource }()

	snap := TakeSingletonSnapshot()
	require.Empty(t, snap.Verify(), "fresh snapshot should match current state")

	// Mutate a singleton (test only — restored in defer).
	singletonTrue.Source = nil
	drift := snap.Verify()
	assert.Equal(t, "Bool(true)", drift, "Verify should report which singleton drifted")

	// Restore.
	singletonTrue.Source = origSource
	assert.Empty(t, snap.Verify(), "after restore Verify should pass")
}
