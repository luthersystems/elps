// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"io"
	"testing"
)

type ownershipIdentityReader struct{ identity func() string }

func (r ownershipIdentityReader) ReaderIdentity() string { return r.identity() }
func (ownershipIdentityReader) Read(string, io.Reader) ([]*LVal, error) {
	return []*LVal{Int(42)}, nil
}

type ownershipHookCache struct {
	load  func()
	store func()
}

func (c ownershipHookCache) Load(string) (*CachedSource, bool) {
	c.load()
	return nil, false
}
func (c ownershipHookCache) Store(string, *CachedSource) { c.store() }

type ownershipDiagnosticWriter struct{ write func() }

func (w ownershipDiagnosticWriter) Write(p []byte) (int, error) {
	w.write()
	return len(p), nil
}

// Optional cache failure recovery must not hide an actual cross-runtime
// violation raised by checked-mode host code (issue #657).
func TestLoadCacheHooksPreserveOwnershipFailure(t *testing.T) {
	for _, hook := range []string{"ReaderIdentity", "Load", "Store", "Stderr"} {
		t.Run(hook, func(t *testing.T) {
			owner := newOwnershipTestEnv()
			env := newOwnershipTestEnv()
			shared := Int(7)
			if got := owner.Put(Symbol("owned"), shared); got.Type == LError {
				t.Fatal(got)
			}
			violate := func() { env.Put(Symbol("borrowed"), shared) }
			cache := ownershipHookCache{load: func() {}, store: func() {}}
			reader := ownershipIdentityReader{identity: func() string { return "ownership-reader" }}
			switch hook {
			case "ReaderIdentity":
				reader.identity = func() string { violate(); return "ownership-reader" }
			case "Load":
				cache.load = violate
			case "Store":
				cache.store = violate
			case "Stderr":
				cache.load = func() { panic("cache hook failed") }
				env.Runtime.Stderr = ownershipDiagnosticWriter{write: violate}
			}
			env.Runtime.Reader = reader
			env.Runtime.LoadCache = cache
			expectOwnershipPanic(t, func() { env.LoadString("ownership.lisp", "42") })
			if env.Runtime.loadCacheActive {
				t.Error("ownership failure left the cache reentry guard active")
			}
		})
	}
}
