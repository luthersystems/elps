// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type callbackIdentityReader struct {
	*countingReader
	identity func() string
}

func (r *callbackIdentityReader) ReaderIdentity() string { return r.identity() }

func TestLoadCacheIdentityReentryIsGuarded(t *testing.T) {
	cache := newTestLoadCache()
	reader := &callbackIdentityReader{countingReader: newCountingReader()}
	env := readerEnv(t, reader, cache)
	identities := 0
	var nested *lisp.LVal
	reader.identity = func() string {
		identities++
		// A missing guard reaches the second identity call. Stop there so
		// the regression fails by count instead of overflowing the Go stack.
		if identities == 1 {
			nested = env.LoadString("identity-warm.lisp", `(+ 1 2)`)
		}
		return "reentrant-reader"
	}
	got := env.LoadString("main.lisp", `(+ 40 2)`)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	assert.Equal(t, 42, got.Int)
	require.NotNil(t, nested)
	require.Equal(t, lisp.LInt, nested.Type, "%v", nested)
	assert.Equal(t, 3, nested.Int)
	assert.Equal(t, 1, identities, "the nested load must bypass identity and cache hooks")
	assert.Equal(t, 2, reader.reads, "both distinct sources must be parsed")
	assert.Equal(t, 1, cache.loads)
	assert.Equal(t, 1, cache.stores, "the nested warm must not enter the cache")

	// A subsequent load must reach identity again and use the cached parse.
	got = env.LoadString("main.lisp", `(+ 40 2)`)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	assert.Equal(t, 42, got.Int)
	assert.Equal(t, 2, identities, "the guard must be reset after the first load")
	assert.Equal(t, 2, reader.reads)
	assert.Equal(t, 1, cache.hits)
}

func TestLoadCacheIdentityPanicFallsBack(t *testing.T) {
	for _, tc := range []struct {
		name string
		load func(*lisp.LEnv) *lisp.LVal
	}{
		{"LoadString", func(env *lisp.LEnv) *lisp.LVal { return env.LoadString("identity.lisp", `(+ 40 2)`) }},
		{"LoadLocationContext", func(env *lisp.LEnv) *lisp.LVal {
			return env.LoadLocationContext(context.Background(), "identity.lisp", "/identity.lisp", strings.NewReader(`(+ 40 2)`))
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			cache := newTestLoadCache()
			reader := &callbackIdentityReader{countingReader: newCountingReader()}
			identities := 0
			reader.identity = func() string { identities++; panic("identity failed") }
			env := readerEnv(t, reader, cache)
			var diagnostic bytes.Buffer
			env.Runtime.Stderr = &diagnostic
			var got *lisp.LVal
			require.NotPanics(t, func() { got = tc.load(env) })
			require.NotNil(t, got)
			require.Equal(t, lisp.LInt, got.Type, "%v", got)
			assert.Equal(t, 42, got.Int)
			assert.Equal(t, 1, identities)
			assert.Equal(t, 1, reader.reads)
			assert.Zero(t, cache.loads, "an unknown reader identity must never query the cache")
			assert.Zero(t, cache.stores)
			assert.Contains(t, diagnostic.String(), "ReaderIdentity")
			assert.Contains(t, diagnostic.String(), "identity failed")

			reader.identity = func() string { identities++; return "recovered-reader" }
			for range 2 {
				got = tc.load(env)
				require.Equal(t, lisp.LInt, got.Type, "%v", got)
				assert.Equal(t, 42, got.Int)
			}
			assert.Equal(t, 3, identities)
			assert.Equal(t, 2, reader.reads, "the final load must hit the cache")
			assert.Equal(t, 2, cache.loads)
			assert.Equal(t, 1, cache.stores)
			assert.Equal(t, 1, cache.hits)
		})
	}
}

func TestLoadCacheIdentityFallbackPreservesParseError(t *testing.T) {
	uncached := readerEnv(t, newCountingReader(), nil)
	want := uncached.LoadString("invalid.lisp", `(`)
	require.Equal(t, lisp.LError, want.Type, "%v", want)
	cache := newTestLoadCache()
	reader := &callbackIdentityReader{countingReader: newCountingReader(), identity: func() string { panic("identity failed") }}
	env := readerEnv(t, reader, cache)
	var got *lisp.LVal
	require.NotPanics(t, func() { got = env.LoadString("invalid.lisp", `(`) })
	require.NotNil(t, got)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.False(t, lisp.IsInternalPanic(got))
	assert.Equal(t, want.String(), got.String(), "identity failure must preserve the ordinary uncached parse error")
	assert.Contains(t, got.String(), "unmatched-syntax")
	assert.Equal(t, 1, reader.reads)
	assert.Zero(t, cache.loads)
	assert.Zero(t, cache.stores)
}

func TestLoadCacheNilSkipsReaderIdentity(t *testing.T) {
	reader := &callbackIdentityReader{countingReader: newCountingReader(), identity: func() string { panic("no cache must mean no identity hook") }}
	env := readerEnv(t, reader, nil)
	got := env.LoadString("uncached.lisp", `(+ 40 2)`)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	assert.Equal(t, 42, got.Int)
	assert.Equal(t, 1, reader.reads)
}

type cachePanicWriter struct{ writes int }

func (w *cachePanicWriter) Write([]byte) (int, error) {
	w.writes++
	panic("diagnostic writer failed")
}

func TestLoadCachePanicDiagnosticCannotEscape(t *testing.T) {
	for _, hook := range []string{"ReaderIdentity", "Load", "Store"} {
		t.Run(hook, func(t *testing.T) {
			cache := &panickingCache{entries: make(map[string]*lisp.CachedSource), panicLoad: hook == "Load", panicStore: hook == "Store"}
			reader := &callbackIdentityReader{countingReader: newCountingReader(), identity: func() string {
				if hook == "ReaderIdentity" {
					panic("identity failed")
				}
				return "diagnostic-reader"
			}}
			env := readerEnv(t, reader, cache)
			writer := &cachePanicWriter{}
			env.Runtime.Stderr = writer
			var got *lisp.LVal
			require.NotPanics(t, func() { got = env.LoadString("diagnostic.lisp", `(+ 40 2)`) })
			require.NotNil(t, got)
			require.Equal(t, lisp.LInt, got.Type, "%v", got)
			assert.Equal(t, 42, got.Int)
			assert.Equal(t, 1, writer.writes, "the diagnostic must be attempted exactly once")
			assert.Equal(t, 1, reader.reads)
		})
	}
}
