// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// TestMapLoadCache pins MapLoadCache as a working LoadCache: the first
// Runtime parses and stores, a second Runtime finds the entry and does not
// parse, and after Clear the next load parses again.
func TestMapLoadCache(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := lisp.NewMapLoadCache()

	env, reader := newCacheEnv(t, files, cache)
	loadFixture(t, env)
	require.Equal(t, 1, reader.reads)
	require.Equal(t, 1, cache.Len())

	env, reader = newCacheEnv(t, files, cache)
	loadFixture(t, env)
	require.Zero(t, reader.reads, "a hit must not parse")

	cache.Clear()
	require.Zero(t, cache.Len())
	env, reader = newCacheEnv(t, files, cache)
	loadFixture(t, env)
	require.Equal(t, 1, reader.reads, "a load after Clear must parse again")
}
