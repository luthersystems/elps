// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"io"
	"strings"
	"testing"
)

// Checked-mode red proof for the sealed load cache (issue #368).
//
// The hook's licence to alias a cached parse across environments is the
// ownership checker's allowlist entry 2: a SEALED node may be reached by
// more than one Runtime, because immutability rather than confinement is
// what protects it (lisp/ownership_check_elpscheck.go).  No new admission
// class was added for this hook — a cache entry is sealed throughout by
// construction, so it is already inside that exemption.
//
// A guard that has never been shown to fire is not known to work, so this
// file drives the cache path from both sides:
//
//   - GREEN: a legally-minted entry, whose expressions newCachedSource
//     sealed at admission, is served to two Runtimes and must NOT trip the
//     checker.  If it did, an embedder running the supported topology could
//     not use checked mode at all — which is the exact contradiction entry
//     2 exists to resolve.
//   - RED: an entry carrying an UNSEALED node — which no production path
//     can build, hence CachedSourceForTest — served to the same two
//     Runtimes must still panic.  The exemption is "sealed", not "came out
//     of a cache".
//
// The red half is what makes the green half meaningful: without it, a
// checker that had been silently disabled on this path would look identical.

// fixedLoadCache answers every probe with one entry, stored under the key
// the test derived, so a load hits deterministically without a first
// environment having to populate anything.
type fixedLoadCache struct {
	entry *CachedSource
}

func (c *fixedLoadCache) Load(key string) (*CachedSource, bool) {
	if c.entry == nil || c.entry.key != key {
		return nil, false
	}
	return c.entry, true
}

func (c *fixedLoadCache) Store(string, *CachedSource) {}

// loadCacheSharedExprs is the shape both halves share: a top-level
// expression whose evaluation is trivial and whose nodes the eval path
// therefore certainly touches.
func loadCacheSharedExprs() []*LVal {
	// A quoted list, built the way the ownership tests build one: quoting by
	// the flag rather than by a `quote` head keeps the expression's
	// evaluation free of symbol resolution, so what the checker sees is the
	// shared nodes and nothing else.
	v := SExpr([]*LVal{Int(1), Int(2), Int(3)})
	v.quoted = true
	return []*LVal{v}
}

// loadCacheEnvWith returns a root LEnv with its own private Runtime, the
// given cache installed, and a reader that must never be reached (the cache
// answers every probe, so a parse here means the entry was rejected).
func loadCacheEnvWith(t *testing.T, cache LoadCache) *LEnv {
	t.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("could not initialize the environment: %v", rc)
	}
	// Installed AFTER initialization: what is under test is the cached
	// load, not whatever the boot sequence parses.
	env.Runtime.LoadCache = cache
	env.Runtime.Reader = refusingReader{t}
	return env
}

type refusingReader struct{ t *testing.T }

func (r refusingReader) Read(string, io.Reader) ([]*LVal, error) {
	r.t.Fatal("the cache entry was rejected and the load fell back to parsing")
	return nil, nil
}

// ReadLocation exists so LoadLocation takes the LocationReader path rather
// than falling back to Load — the fallback re-keys the lookup under a
// different (name, loc) pair and would turn every hit into a miss.
func (r refusingReader) ReadLocation(string, string, io.Reader) ([]*LVal, error) {
	r.t.Fatal("the cache entry was rejected and the load fell back to parsing")
	return nil, nil
}

// TestLoadCacheSealedEntryIsExemptFromOwnership is the green half: a legally
// admitted entry, served to two Runtimes, must not trip the checker.
func TestLoadCacheSealedEntryIsExemptFromOwnership(t *testing.T) {
	const name, loc, src = "shared.lisp", "shared.lisp", "'(1 2 3)"
	// The envs below install refusingReader and reach the funnel through
	// LoadLocation (byLoc == true); the key must be derived under the same
	// reader identity and method or every hit turns into a miss.
	key := loadCacheKey(name, loc, readerIdentity(refusingReader{}), true, []byte(src))
	entry, err := newCachedSource(key, name, loc, loadCacheSharedExprs())
	if err != nil {
		t.Fatalf("admission refused an ordinary parse: %v", err)
	}
	for _, expr := range entry.prog.exprs {
		if !expr.sealed {
			t.Fatal("newCachedSource admitted an unsealed expression; the whole exemption rests on this")
		}
	}
	cache := &fixedLoadCache{entry: entry}

	for i := range 2 {
		env := loadCacheEnvWith(t, cache)
		if v := env.LoadLocation(name, loc, strings.NewReader(src)); v.Type == LError {
			t.Fatalf("load %d of the shared sealed entry failed: %v", i, v)
		}
	}
}

// TestLoadCacheUnsealedEntryTripsOwnership is the red half: the same
// topology with an unsealed node in the entry must panic under -tags
// elpscheck, naming both runtimes.
func TestLoadCacheUnsealedEntryTripsOwnership(t *testing.T) {
	const name, loc, src = "smuggled.lisp", "smuggled.lisp", "'(1 2 3)"
	key := loadCacheKey(name, loc, readerIdentity(refusingReader{}), true, []byte(src))
	// CachedSourceForTest deliberately skips the admission walk, so the
	// entry carries exactly the unsealed, mutable tree newCachedSource
	// would have copied and sealed.
	entry := CachedSourceForTest(key, name, loc, loadCacheSharedExprs())
	for _, expr := range entry.prog.exprs {
		if expr.sealed {
			t.Fatal("the smuggled entry is sealed; this red proof would be vacuous")
		}
	}
	cache := &fixedLoadCache{entry: entry}

	envA := loadCacheEnvWith(t, cache)
	if v := envA.LoadLocation(name, loc, strings.NewReader(src)); v.Type == LError {
		t.Fatalf("the first load of the smuggled entry failed before the checker could speak: %v", v)
	}
	envB := loadCacheEnvWith(t, cache)
	expectOwnershipPanic(t, func() {
		envB.LoadLocation(name, loc, strings.NewReader(src))
	})
}

// TestLoadCacheLaunderedClosureTripsOwnership is the checked-build red proof
// for finding 1: the ownership exemption must key off the node TYPE, not the
// seal FLAG alone.
//
// The entry carries a closure — an LFun that captured a mutable *LEnv — with
// the seal flag laundered onto it (a non-sealable node no admission would
// produce, so CachedSourceForTest is used to bypass the walk).  Served to two
// Runtimes, it is the exact cross-runtime sharing the checker exists to stop:
// the same closure, evaluated in runtime A then runtime B, is one mutable
// value reached by two runtimes (the reviewer's ticking-counter repro).
//
// Before the fix, checkOwnership exempted anything with v.sealed set, so the
// laundered closure slipped past and both runtimes shared it silently.  After
// the fix the exemption is (v.sealed && sealableNodeType(v.Type)); an LFun is
// not sealable, so the closure is checked and runtime B's load panics.
func TestLoadCacheLaunderedClosureTripsOwnership(t *testing.T) {
	const name, loc, src = "laundered.lisp", "laundered.lisp", "(a closure)"
	key := loadCacheKey(name, loc, readerIdentity(refusingReader{}), true, []byte(src))

	fn := mintClosure(t)
	forceSealAll(fn) // launder: seal flag on the non-sealable closure node
	if !fn.sealed || sealableNodeType(fn.Type) {
		t.Fatal("the laundered node must be a sealed non-sealable type, or this proof is vacuous")
	}
	// The top-level expression IS the closure, so evaluating the cached program
	// routes the closure through env.eval, where checkOwnership sees it.
	entry := CachedSourceForTest(key, name, loc, []*LVal{fn})
	cache := &fixedLoadCache{entry: entry}

	envA := loadCacheEnvWith(t, cache)
	if v := envA.LoadLocation(name, loc, strings.NewReader(src)); v.Type == LError {
		t.Fatalf("the first load of the laundered entry failed before the checker could speak: %v", v)
	}
	envB := loadCacheEnvWith(t, cache)
	expectOwnershipPanic(t, func() {
		envB.LoadLocation(name, loc, strings.NewReader(src))
	})
}
