// Copyright © 2026 The ELPS authors

// Tests for the sealed per-file load cache hook (issue #368).
//
// The hook's whole claim is that elps can hand a cached parse to an
// environment BY REFERENCE where an embedder could only reach safety by
// deep-copying.  Three things have to hold for that claim, and each has its
// own test below:
//
//  1. The cache is actually consulted, and actually aliases — a "cache" that
//     silently reparses, or silently copies, would pass every correctness
//     test in this file while buying nothing.
//  2. A cached load produces exactly what a fresh parse produces, in every
//     environment, including the environment's whole binding state.
//  3. Nothing an environment does to the shared tree is visible to another
//     one: a lisp-level write raises modify-literal-error, and the sealed
//     bytes are identical afterwards.
//
// The nil-cache path gets its own test too.  "Byte-identical to today when
// no cache is installed" is the compatibility promise the hook makes to
// every existing embedder, and the way to break it silently is to interpose
// a buffer on the stream even when there is nothing to cache.

package lisp_test

import (
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"
	"testing/fstest"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// testLoadCache is the smallest useful lisp.LoadCache: a map, a mutex, and
// counters.  The counters are what make the cache tests non-vacuous — every
// assertion below about "a hit" or "one parse" is read off them.
type testLoadCache struct {
	entries map[string]*lisp.CachedSource
	mu      sync.Mutex
	loads   int
	hits    int
	stores  int
}

func newTestLoadCache() *testLoadCache {
	return &testLoadCache{entries: make(map[string]*lisp.CachedSource)}
}

func (c *testLoadCache) Load(key string) (*lisp.CachedSource, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.loads++
	src, ok := c.entries[key]
	if ok {
		c.hits++
	}
	return src, ok
}

func (c *testLoadCache) Store(key string, src *lisp.CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.stores++
	c.entries[key] = src
}

func (c *testLoadCache) only(t *testing.T) *lisp.CachedSource {
	t.Helper()
	c.mu.Lock()
	defer c.mu.Unlock()
	require.Len(t, c.entries, 1, "expected exactly one cache entry")
	for _, src := range c.entries {
		return src
	}
	return nil
}

// countingReader wraps the real parser and counts parses.  A cache that
// works is a cache whose second load does not reach this.
type countingReader struct {
	inner   readLocationReader
	streams []io.Reader // the io.Reader each call received, in order
	reads   int
}

type readLocationReader interface {
	lisp.Reader
	lisp.LocationReader
}

func newCountingReader() *countingReader {
	return &countingReader{inner: parser.NewReader().(readLocationReader)}
}

func (r *countingReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	r.reads++
	r.streams = append(r.streams, in)
	return r.inner.Read(name, in)
}

func (r *countingReader) ReadLocation(name, loc string, in io.Reader) ([]*lisp.LVal, error) {
	r.reads++
	r.streams = append(r.streams, in)
	return r.inner.ReadLocation(name, loc, in)
}

// newCacheEnv builds an environment over an in-memory source tree, with the
// given cache installed (nil for the no-hook path).
func newCacheEnv(t *testing.T, files fstest.MapFS, cache lisp.LoadCache) (*lisp.LEnv, *countingReader) {
	t.Helper()
	reader := newCountingReader()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.FSLibrary{FS: files}
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("could not initialize the environment: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("could not enter the user package: %v", rc)
	}
	return env, reader
}

const cacheFixtureName = "fixture.lisp"

// cacheFixture is deliberately ordinary source: a defun, a literal a
// function hands back on every call, and a macro that splices its argument
// into its expansion (the shape that put the caller's parse nodes in
// stampMacroExpansion's path in elps#370).
const cacheFixture = `
(defun answer () 42)
(defun literal () '(3 1 2))
(defmacro ident (x) x)
(set 'loaded 'yes)
(answer)`

func cacheFixtureFS() fstest.MapFS {
	return fstest.MapFS{
		cacheFixtureName: &fstest.MapFile{Data: []byte(cacheFixture)},
	}
}

func loadFixture(t *testing.T, env *lisp.LEnv) *lisp.LVal {
	t.Helper()
	v := env.LoadFile(cacheFixtureName)
	require.NotEqual(t, lisp.LError, v.Type, "loading the fixture failed: %v", v)
	return v
}

// TestLoadCacheMissThenHit is the base case: the first environment parses
// and stores, the second finds the entry and does not parse at all.
func TestLoadCacheMissThenHit(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	envA, readerA := newCacheEnv(t, files, cache)
	require.Equal(t, 42, loadFixture(t, envA).Int)
	assert.Equal(t, 1, readerA.reads, "the first load must parse exactly once")
	assert.Equal(t, 1, cache.stores, "the first load must store exactly one entry")
	assert.Equal(t, 1, cache.loads, "the first load must probe the cache exactly once")
	assert.Equal(t, 0, cache.hits, "the first load cannot hit")

	envB, readerB := newCacheEnv(t, files, cache)
	require.Equal(t, 42, loadFixture(t, envB).Int)
	assert.Equal(t, 0, readerB.reads, "the second load must not reach the parser")
	assert.Equal(t, 1, cache.stores, "the second load must not store again")
	assert.Equal(t, 1, cache.hits, "the second load must hit")

	entry := cache.only(t)
	assert.Equal(t, cacheFixtureName, entry.Name())
	assert.Equal(t, cacheFixtureName, entry.Location())
	assert.NotEmpty(t, entry.Key())
	assert.Equal(t, 5, entry.Len(), "the fixture has five top-level expressions")
	assert.NotZero(t, entry.Fingerprint())
	assert.Contains(t, entry.String(), "cached-source")
}

// TestLoadCacheServesTheSameNodes is the ALIAS proof, and it is the test
// that makes this whole hook worth having.  Everything else in this file
// would still pass if a hit deep-copied the cached tree; only pointer
// identity separates "elps hands out the alias" from "elps copies like the
// embedder had to".
func TestLoadCacheServesTheSameNodes(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	envA, _ := newCacheEnv(t, files, cache)
	loadFixture(t, envA)
	first := lisp.CachedSourceExprs(cache.only(t))
	require.NotEmpty(t, first)

	envB, _ := newCacheEnv(t, files, cache)
	loadFixture(t, envB)
	second := lisp.CachedSourceExprs(cache.only(t))

	require.Len(t, second, len(first))
	for i := range first {
		assert.Samef(t, first[i], second[i],
			"expression %d was copied for the second load; the cache must serve the same node", i)
		assert.Truef(t, first[i].IsSealed(),
			"expression %d is not sealed; aliasing it across environments would be illegal", i)
	}
}

// TestLoadCacheNilPathUnchanged is the compatibility promise.  With no cache
// installed the reader must receive the CALLER'S OWN io.Reader — not a
// buffer elps read the stream into — because interposing one is the way this
// hook would silently change every existing embedder's load path (and its
// allocation profile) while every other test still passed.
func TestLoadCacheNilPathUnchanged(t *testing.T) {
	t.Parallel()
	env, reader := newCacheEnv(t, cacheFixtureFS(), nil)

	stream := strings.NewReader(cacheFixture)
	v := env.Load(cacheFixtureName, stream)
	require.NotEqual(t, lisp.LError, v.Type, "load failed: %v", v)

	require.Equal(t, 1, reader.reads)
	assert.Same(t, io.Reader(stream), reader.streams[0],
		"with no cache installed the reader must get the caller's stream, unwrapped")
}

// TestLoadCacheMatchesFreshParses is assertion 2: two environments sharing
// one cache entry must end up in exactly the state two environments that
// each parsed for themselves end up in — every package, every symbol, every
// bound value.
func TestLoadCacheMatchesFreshParses(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	var cached, fresh []string
	for range 2 {
		env, _ := newCacheEnv(t, files, cache)
		loadFixture(t, env)
		cached = append(cached, envStateDump(t, env))
	}
	for range 2 {
		env, _ := newCacheEnv(t, files, nil)
		loadFixture(t, env)
		fresh = append(fresh, envStateDump(t, env))
	}

	assert.Equal(t, cached[0], cached[1], "two cached loads must agree with each other")
	assert.Equal(t, fresh[0], fresh[1], "two fresh loads must agree with each other")
	assert.Equal(t, fresh[0], cached[0], "a cached load must match a fresh parse exactly")

	// And the values a program can actually observe.
	for _, cache := range []lisp.LoadCache{cache, nil} {
		env, _ := newCacheEnv(t, files, cache)
		loadFixture(t, env)
		assert.Equal(t, "42", evalString(t, env, "(answer)"))
		assert.Equal(t, "'(3 1 2)", evalString(t, env, "(literal)"))
		assert.Equal(t, "7", evalString(t, env, "(ident 7)"))
		assert.Equal(t, "'yes", evalString(t, env, "loaded"))
	}
}

// TestLoadCacheWriteThroughSharedTreeIsRefused is red-proof (b): a lisp-level
// in-place write through the shared literal must raise
// modify-literal-error, and the OTHER environment sharing that literal must
// see nothing — neither a different answer nor a different byte.
func TestLoadCacheWriteThroughSharedTreeIsRefused(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	envA, _ := newCacheEnv(t, files, cache)
	loadFixture(t, envA)
	entry := cache.only(t)
	exprs := lisp.CachedSourceExprs(entry)
	before := lisp.SealedASTFingerprint(exprs)
	require.Equal(t, entry.Fingerprint(), before, "the entry's admission fingerprint must describe the tree it holds")

	envB, readerB := newCacheEnv(t, files, cache)
	loadFixture(t, envB)
	require.Equal(t, 0, readerB.reads, "envB must be reading the cached tree, or this proves nothing")

	// The write: stable-sort's documented in-place effect, applied to the
	// literal a cached function hands back.
	res := envA.LoadString("write", "(stable-sort < (literal))")
	require.Equal(t, lisp.LError, res.Type, "writing through a shared program literal must fail")
	assert.Contains(t, res.String(), "cannot modify a program literal",
		"the refusal must be the modify-literal-error condition")
	assert.Equal(t, "modify-literal-error", conditionType(t, res))

	// envB's view: unchanged answer, unchanged bytes.
	assert.Equal(t, "'(3 1 2)", evalString(t, envB, "(literal)"),
		"the other environment must not observe the attempted write")
	assert.Equal(t, before, lisp.SealedASTFingerprint(exprs),
		"the shared sealed tree changed; a guard failed and one environment rewrote every other one's program")

	// Anti-vacuity for the refusal: the sanctioned remedy still works, in
	// both environments, and leaves the shared tree alone.
	assert.Equal(t, "'(1 2 3)", evalString(t, envA, "(stable-sort < (copy (literal)))"))
	assert.Equal(t, "'(3 1 2)", evalString(t, envB, "(literal)"))
	assert.Equal(t, before, lisp.SealedASTFingerprint(exprs))
}

// TestLoadCacheDebuggerAliasIntact is the debugger ruling, checked rather
// than assumed.
//
// Attaching a debugger changes two things that matter here: tail-call
// optimisation is disabled globally, and macroCall builds a
// macroExpansionContext which stampMacroExpansion turns into per-node
// metadata on every node it claims.  Those are WRITES, and a macro receives
// its arguments unevaluated, so the caller's parse nodes are spliced
// straight into the expansion — which under this hook are the CACHED nodes
// another environment is also evaluating.
//
// The reason no private copy is needed is one line in stampGuarded: it
// returns at the first sealed node, whole subtree skipped.  This test drives
// a debugger-attached environment through repeated macro expansion over the
// cached tree and proves the other environment's aliased view is
// byte-identical afterwards.  If that guard is ever removed, this fails.
func TestLoadCacheDebuggerAliasIntact(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	// Env A: debugger attached before anything is loaded, so every macro
	// expansion it performs carries an expansion context.
	envA, _ := newCacheEnv(t, files, cache)
	envA.Runtime.Debugger = dormantDebugger{}
	loadFixture(t, envA)

	entry := cache.only(t)
	exprs := lisp.CachedSourceExprs(entry)
	before := lisp.SealedASTFingerprint(exprs)

	// Env B: no debugger, same cached tree, byte-for-byte.
	envB, readerB := newCacheEnv(t, files, cache)
	loadFixture(t, envB)
	require.Equal(t, 0, readerB.reads, "envB must be reading the cached tree, or this proves nothing")

	// Exercise the expansion path hard, over nodes that came out of the
	// cache: the macro is defined in the cached source and its argument is
	// a literal from the cached source.
	for range 8 {
		res := envA.LoadString("expand", "(ident (literal))")
		require.NotEqual(t, lisp.LError, res.Type, "macro expansion under a debugger failed: %v", res)
		assert.Equal(t, "'(3 1 2)", res.String())
	}

	assert.Equal(t, before, lisp.SealedASTFingerprint(exprs),
		"a debugger-attached environment wrote macro-expansion metadata into the SHARED cached tree")
	assert.Equal(t, "'(3 1 2)", evalString(t, envB, "(literal)"))
	assert.Equal(t, "42", evalString(t, envB, "(answer)"))
}

// TestLoadCacheMismatchedKeyIsNotTrusted pins the defensive fall-back: a
// cache that hands back an entry it did not store under this key degrades to
// "no cache", not to "runs the wrong file".
func TestLoadCacheMismatchedKeyIsNotTrusted(t *testing.T) {
	t.Parallel()
	files := fstest.MapFS{
		"a.lisp": &fstest.MapFile{Data: []byte(`(set 'which 'a) 'a`)},
		"b.lisp": &fstest.MapFile{Data: []byte(`(set 'which 'b) 'b`)},
	}

	// Prime a cache with b.lisp's parse, then answer EVERY probe with it.
	primed := newTestLoadCache()
	envPrime, _ := newCacheEnv(t, files, primed)
	require.NotEqual(t, lisp.LError, envPrime.LoadFile("b.lisp").Type)
	wrong := primed.only(t)

	env, reader := newCacheEnv(t, files, &alwaysCache{entry: wrong})
	v := env.LoadFile("a.lisp")
	require.NotEqual(t, lisp.LError, v.Type, "load failed: %v", v)
	assert.Equal(t, 1, reader.reads, "a rejected entry must fall back to a real parse")
	assert.Equal(t, "'a", v.String(), "the wrong-key entry must not have been served")
	assert.Equal(t, "'a", evalString(t, env, "which"))
}

// alwaysCache answers every Load with one entry, whatever the key.  It is
// the misbehaving embedder cache the key check exists for.
type alwaysCache struct {
	entry *lisp.CachedSource
}

func (c *alwaysCache) Load(string) (*lisp.CachedSource, bool) { return c.entry, true }
func (c *alwaysCache) Store(string, *lisp.CachedSource)       {}

// TestLoadCacheRefusesUncacheableParse pins the admission's refusal path.  A
// Reader that returns a reference type produces a parse that no seal can
// protect, so it must not enter a process-wide cache — and the load must
// still work, exactly as it does with no cache at all.
func TestLoadCacheRefusesUncacheableParse(t *testing.T) {
	t.Parallel()
	cache := newTestLoadCache()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = bytesReader{}
	env.Runtime.LoadCache = cache
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)

	for range 3 {
		v := env.Load("ref.lisp", strings.NewReader("ignored"))
		require.NotEqual(t, lisp.LError, v.Type, "load failed: %v", v)
		assert.Equal(t, lisp.LBytes, v.Type)
	}
	assert.Equal(t, 0, cache.stores, "an unsealable parse must never be stored")
	assert.Equal(t, 3, cache.loads, "the cache is still probed; it simply never hits")
	assert.Equal(t, 0, cache.hits)
}

// bytesReader returns a reference type, which checkLoaderExpr rejects.  No
// real parser can produce one; the Reader interface cannot promise that.
type bytesReader struct{}

func (bytesReader) Read(string, io.Reader) ([]*lisp.LVal, error) {
	return []*lisp.LVal{lisp.Bytes([]byte("payload"))}, nil
}

// TestLoadCacheKeySeparatesIdentityFromContent pins the deviation from
// substrate's content-only key: two files with identical bytes get distinct
// entries, because the cached tree carries the first file's parse locations
// and serving it for the second would misattribute every error raised from
// it.
func TestLoadCacheKeySeparatesIdentityFromContent(t *testing.T) {
	t.Parallel()
	const same = `(defun boom () (error 'boom "no"))`
	files := fstest.MapFS{
		"one.lisp": &fstest.MapFile{Data: []byte(same)},
		"two.lisp": &fstest.MapFile{Data: []byte(same)},
	}
	cache := newTestLoadCache()

	env, reader := newCacheEnv(t, files, cache)
	require.NotEqual(t, lisp.LError, env.LoadFile("one.lisp").Type)
	require.NotEqual(t, lisp.LError, env.LoadFile("two.lisp").Type)
	assert.Equal(t, 2, reader.reads, "identical content under two names must not share an entry")
	assert.Equal(t, 2, cache.stores)

	// The key derivation itself: content alone does not determine the key,
	// and the length prefixes stop one tuple's concatenation colliding with
	// another's.
	src := []byte(same)
	const rid = "go:reader"
	assert.NotEqual(t,
		lisp.LoadCacheKeyForTest("one.lisp", "one.lisp", rid, false, src),
		lisp.LoadCacheKeyForTest("two.lisp", "two.lisp", rid, false, src))
	assert.NotEqual(t,
		lisp.LoadCacheKeyForTest("ab", "c", rid, false, src),
		lisp.LoadCacheKeyForTest("a", "bc", rid, false, src))
	assert.Equal(t,
		lisp.LoadCacheKeyForTest("one.lisp", "one.lisp", rid, false, src),
		lisp.LoadCacheKeyForTest("one.lisp", "one.lisp", rid, false, []byte(same)))

	// The two new key components — reader identity and the Read/ReadLocation
	// method — each separate an otherwise-identical tuple.
	assert.NotEqual(t,
		lisp.LoadCacheKeyForTest("one.lisp", "one.lisp", "go:readerA", false, src),
		lisp.LoadCacheKeyForTest("one.lisp", "one.lisp", "go:readerB", false, src),
		"a different reader identity must give a different key")
	assert.NotEqual(t,
		lisp.LoadCacheKeyForTest("one.lisp", "", rid, false, src),
		lisp.LoadCacheKeyForTest("one.lisp", "", rid, true, src),
		"Read and ReadLocation must not collide on the same (name, \"\", src) tuple")
}

// TestLoadCacheLoadFileFromLisp drives the hook through the funnel it was
// filed for: the `load-file` builtin, called from lisp source.
func TestLoadCacheLoadFileFromLisp(t *testing.T) {
	t.Parallel()
	files := cacheFixtureFS()
	cache := newTestLoadCache()

	envA, readerA := newCacheEnv(t, files, cache)
	res := envA.LoadString("driver", fmt.Sprintf("(load-file %q)", cacheFixtureName))
	require.NotEqual(t, lisp.LError, res.Type, "(load-file ...) failed: %v", res)
	// Two parses: the driver string itself and the file it loads.
	assert.Equal(t, 2, readerA.reads)
	assert.Equal(t, 2, cache.stores)

	envB, readerB := newCacheEnv(t, files, cache)
	res = envB.LoadString("driver", fmt.Sprintf("(load-file %q)", cacheFixtureName))
	require.NotEqual(t, lisp.LError, res.Type, "(load-file ...) failed: %v", res)
	assert.Equal(t, 0, readerB.reads, "both the driver and the loaded file must hit")
	assert.Equal(t, "42", evalString(t, envB, "(answer)"))
}

// --- finding 2: the key must bind the producing reader, not just the bytes ---

// sealedValue returns a sealed integer literal — the smallest observable a
// stub reader can hand back, so a wrong-program serve shows up as the wrong
// number.
func sealedValue(n int) *lisp.LVal {
	v := lisp.Int(n)
	v.SealAST()
	return v
}

// intReaderA and intReaderB are DISTINCT reader types that parse any bytes into
// distinct programs (A -> 1, B -> 2).  They are distinct Go types on purpose:
// the cache binds an entry to its reader by identity, and two readers that
// parse the same bytes differently must be told apart.  (Instances of ONE type
// are treated as interchangeable, which is what lets many Runtimes each hold
// their own reader of the same type and still share cache entries — the
// motivating warm-cache topology.  A reader that multiplexes parse behaviour
// behind one Go type distinguishes itself with lisp.ReaderIdentity; see
// TestLoadCacheReaderIdentityInterfaceSeparates.)
type intReaderA struct{ reads int }

func (r *intReaderA) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	r.reads++
	return []*lisp.LVal{sealedValue(1)}, nil
}
func (r *intReaderA) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

type intReaderB struct{ reads int }

func (r *intReaderB) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	r.reads++
	return []*lisp.LVal{sealedValue(2)}, nil
}
func (r *intReaderB) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

func readerEnv(t *testing.T, reader lisp.Reader, cache lisp.LoadCache) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestLoadCacheDifferentReadersDoNotServeEachOther is finding 2, repro 1: two
// Runtimes with different Readers sharing one cache must not serve each other's
// parses.  Before the fix the key ignored the reader, so environment B (which
// parses to 2) hit environment A's entry and ran A's program (1).
func TestLoadCacheDifferentReadersDoNotServeEachOther(t *testing.T) {
	t.Parallel()
	const src = "identical bytes"
	cache := newTestLoadCache()

	ra := &intReaderA{}
	envA := readerEnv(t, ra, cache)
	va := envA.Load("shared.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, va.Type, "load A failed: %v", va)
	assert.Equal(t, 1, va.Int)

	rb := &intReaderB{}
	envB := readerEnv(t, rb, cache)
	vb := envB.Load("shared.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, vb.Type, "load B failed: %v", vb)
	assert.Equal(t, 2, vb.Int, "reader B was served reader A's cached parse")
	assert.Equal(t, 1, rb.reads, "reader B must reparse rather than serve the wrong entry")
}

// TestLoadCacheSwappedReaderReparses is finding 2, repro 2: swapping
// Runtime.Reader between two loads of the same bytes must not re-serve the
// stale parse produced by the previous reader.
func TestLoadCacheSwappedReaderReparses(t *testing.T) {
	t.Parallel()
	const src = "identical bytes"
	cache := newTestLoadCache()

	env := readerEnv(t, &intReaderA{}, cache)
	v1 := env.Load("f.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, v1.Type)
	assert.Equal(t, 1, v1.Int)

	env.Runtime.Reader = &intReaderB{}
	v2 := env.Load("f.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, v2.Type)
	assert.Equal(t, 2, v2.Int, "the swapped reader was served the previous reader's stale parse")
}

// methodReader parses the same bytes differently through Read (10) and
// ReadLocation (20), so a Load / LoadLocation collision is observable.
type methodReader struct{}

func (methodReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return []*lisp.LVal{sealedValue(10)}, nil
}
func (methodReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return []*lisp.LVal{sealedValue(20)}, nil
}

// TestLoadCacheReadVsReadLocationDoNotCollide is finding 2, repro 3: elps's own
// Load (Read) and LoadLocation (ReadLocation) reach the funnel with loc == ""
// for the same file, and before the fix collided on the same (name, "", src)
// key even though ReadLocation parses differently.
func TestLoadCacheReadVsReadLocationDoNotCollide(t *testing.T) {
	t.Parallel()
	const src = "identical bytes"
	cache := newTestLoadCache()
	env := readerEnv(t, methodReader{}, cache)

	vRead := env.Load("f.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, vRead.Type)
	assert.Equal(t, 10, vRead.Int)

	vLoc := env.LoadLocation("f.lisp", "", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, vLoc.Type)
	assert.Equal(t, 20, vLoc.Int, "LoadLocation (ReadLocation) was served Load's (Read) cached parse")
}

// identReader multiplexes two parse behaviours behind ONE Go type, selected by
// a field, and states which through lisp.ReaderIdentity.  It shows the optional
// hook closes the same-type / different-config gap the type-only default
// leaves open.
type identReader struct {
	tag string
	val int
}

func (r identReader) ReaderIdentity() string { return r.tag }
func (r identReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return []*lisp.LVal{sealedValue(r.val)}, nil
}
func (r identReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// TestLoadCacheReaderIdentityInterfaceSeparates shows the optional escape hatch
// for readers that share a Go type but parse differently.
func TestLoadCacheReaderIdentityInterfaceSeparates(t *testing.T) {
	t.Parallel()
	const src = "identical bytes"
	cache := newTestLoadCache()

	envA := readerEnv(t, identReader{tag: "cfg-a", val: 7}, cache)
	va := envA.Load("f.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, va.Type)
	assert.Equal(t, 7, va.Int)

	envB := readerEnv(t, identReader{tag: "cfg-b", val: 8}, cache)
	vb := envB.Load("f.lisp", strings.NewReader(src))
	require.NotEqual(t, lisp.LError, vb.Type)
	assert.Equal(t, 8, vb.Int, "same-type readers with distinct ReaderIdentity must not share entries")

	// And two readers with the SAME identity DO share (the sharing the topology
	// depends on): the second must hit without reparsing.
	same := newTestLoadCache()
	envC := readerEnv(t, identReader{tag: "cfg-a", val: 7}, same)
	require.Equal(t, 7, envC.Load("f.lisp", strings.NewReader(src)).Int)
	envD := readerEnv(t, identReader{tag: "cfg-a", val: 99}, same)
	// envD claims identity cfg-a, so it is served envC's entry (7), not its own 99.
	assert.Equal(t, 7, envD.Load("f.lisp", strings.NewReader(src)).Int,
		"readers with equal ReaderIdentity are interchangeable producers")
}

// --- finding 3: admission walks must be bounded on the load-file path ---

// graphReader hands back a caller-built tree (a cycle or an interned DAG) as a
// single top-level expression, with a cache installed so the load runs through
// the admission walk newProgram performs.
type graphReader struct{ tree *lisp.LVal }

func (r graphReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	_, _ = io.ReadAll(in)
	return []*lisp.LVal{r.tree}, nil
}
func (r graphReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	return r.Read("", in)
}

// TestLoadCacheCyclicReaderOutputIsBounded is finding 3, cyclic case: a Reader
// returning a cyclic tree must be handled in bounded time — refused with an
// error — rather than recursing until the Go stack overflows and the process
// dies.  This runs only because a cache is installed (the admission walk is on
// the load path); before the fix the walk had no cycle guard.
func TestLoadCacheCyclicReaderOutputIsBounded(t *testing.T) {
	t.Parallel()
	cyc := lisp.SExpr(nil)
	cyc.Cells = []*lisp.LVal{cyc} // a -> a

	env := readerEnv(t, graphReader{tree: cyc}, newTestLoadCache())

	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.Load("cyclic.lisp", strings.NewReader("x")) }()
	select {
	case v := <-done:
		require.Equal(t, lisp.LError, v.Type, "a cyclic reader tree must be refused, not admitted")
		assert.Contains(t, v.String(), "finite strict tree")
	case <-time.After(20 * time.Second):
		t.Fatal("loading a cyclic reader tree did not terminate; the admission walk is unbounded")
	}
}

// TestLoadCacheInternedSubtreeIsBounded is finding 3, interned-subtree case: a
// Reader returning a DAG (a subtree reachable by exponentially many paths) must
// be handled in bounded time.  Before the fix the memo-less walk re-descended
// the shared subtree once per path — measured at 5.2s at sharing-depth 28 and
// climbing.  This DAG is depth 40 (~10^12 paths); a memo-less walk would not
// finish, so completing at all is the proof.
func TestLoadCacheInternedSubtreeIsBounded(t *testing.T) {
	t.Parallel()
	// leaf; each level points twice at the next, so paths double per level.
	node := sealedValue(0)
	for range 40 {
		parent := lisp.SExpr(nil)
		parent.Cells = []*lisp.LVal{node, node}
		node = parent
	}

	env := readerEnv(t, graphReader{tree: node}, newTestLoadCache())

	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.Load("dag.lisp", strings.NewReader("x")) }()
	select {
	case v := <-done:
		require.Equal(t, lisp.LError, v.Type, "an interned-subtree reader tree must be refused")
		assert.Contains(t, v.String(), "finite strict tree")
	case <-time.After(20 * time.Second):
		t.Fatal("loading an interned-subtree reader tree did not terminate in bounded time")
	}
}

// --- finding 7: a re-entrant cache must not kill the process ---

// warmingCache re-enters the load path from inside Load, unconditionally, as a
// cache that warms itself would.  Loading "warm" calls Load again, which warms
// "warm" again: without the in-flight guard this recurses until the Go stack
// overflows.  With the guard the re-entered load bypasses the cache, so Load is
// invoked exactly once and the warm is a single ordinary parse.  The high
// safety cap only exists so a regression can never wedge the whole suite; the
// guard, not the cap, is what makes the green path terminate at warmed == 1.
type warmingCache struct {
	env    *lisp.LEnv
	warmed int
}

func (c *warmingCache) Load(key string) (*lisp.CachedSource, bool) {
	if c.warmed < 100000 {
		c.warmed++
		// Re-enter the load path while servicing a Load.
		c.env.LoadString("warm", "(+ 1 2)")
	}
	return nil, false
}
func (c *warmingCache) Store(string, *lisp.CachedSource) {}

// TestLoadCacheReentrantLoadIsGuarded is finding 7: a Load that re-enters the
// load path (a warming cache) must be treated as a miss rather than recursing
// into the cache forever.  It must terminate and produce the right answer.
func TestLoadCacheReentrantLoadIsGuarded(t *testing.T) {
	t.Parallel()
	cache := &warmingCache{}
	env := readerEnv(t, parser.NewReader(), cache)
	cache.env = env

	done := make(chan *lisp.LVal, 1)
	go func() { done <- env.LoadString("main", "(+ 40 2)") }()
	select {
	case v := <-done:
		require.NotEqual(t, lisp.LError, v.Type, "re-entrant load failed: %v", v)
		assert.Equal(t, 42, v.Int)
		assert.Equal(t, 1, cache.warmed,
			"the guard must collapse re-entry to a single warm; more means it recursed, "+
				"the safety cap (not the guard) stopped it")
	case <-time.After(20 * time.Second):
		t.Fatal("a re-entrant cache load did not terminate; the re-entrancy guard is missing")
	}
}

// --- helpers ---

func evalString(t *testing.T, env *lisp.LEnv, expr string) string {
	t.Helper()
	v := env.LoadString("expr", expr)
	require.NotEqual(t, lisp.LError, v.Type, "evaluating %s failed: %v", expr, v)
	return v.String()
}

// conditionType reports the condition name of an error LVal, so a test can
// assert on the condition rather than on prose.
func conditionType(t *testing.T, v *lisp.LVal) string {
	t.Helper()
	require.Equal(t, lisp.LError, v.Type)
	var err *lisp.ErrorVal
	require.ErrorAs(t, lisp.GoError(v), &err)
	return err.Condition()
}

// envStateDump renders every binding an environment holds, through the
// sorted accessors so Go's randomised map order cannot leak into the
// comparison.
func envStateDump(t *testing.T, env *lisp.LEnv) string {
	t.Helper()
	var sb strings.Builder
	for _, pkgName := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(pkgName)
		if pkg == nil {
			continue
		}
		fmt.Fprintf(&sb, "pkg %s\n", pkgName)
		for _, sym := range pkg.SymbolNames() {
			v, _ := pkg.Symbol(sym)
			fmt.Fprintf(&sb, "  %s = %s\n", sym, valueDump(v))
		}
	}
	fmt.Fprintf(&sb, "current package %s\n", env.Runtime.Package.Name)
	return sb.String()
}

// valueDump renders a bound value stably.  A function is reduced to its type
// and formals: the stdlib's function bindings are identical in every
// environment and rendering their bodies would make the dump enormous
// without making it more discriminating.
func valueDump(v *lisp.LVal) string {
	if v == nil {
		return "<nil>"
	}
	if v.Type == lisp.LFun {
		return fmt.Sprintf("fun/%v", v.FunType)
	}
	return v.String()
}

// syntheticReader returns a hand-built sealed tree whose macro argument
// carries NO parse location.
//
// It exists because the ordinary parser cannot produce the node that makes
// the debugger question sharp.  stampMacroExpansion writes only where
// `v.source == nil || v.source.Pos < 0`, and rdparser gives every node it
// emits a real location — that was the elps#370 fix, and it is why an
// ordinary cached parse is out of the stamp's reach whether or not the
// sealed skip exists.  The LoadCache admits whatever a Reader returns,
// though, and the Reader interface makes no promise about locations, so a
// location-less node in a cached tree is a shape the hook has to be correct
// for.  With one, the sealed skip in stampGuarded is the ONLY thing between
// a debugger-attached environment and a write into every other
// environment's program.
type syntheticReader struct {
	reads int
}

func (r *syntheticReader) exprs() []*lisp.LVal {
	arg := lisp.Quote(lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}))
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol("ident"), arg})
	// The CALL node needs a real location and the ARGUMENT must not have
	// one.  macroCall passes env.loc as the stamp's call site and
	// stampGuarded returns immediately on a nil call site, so a tree with
	// no locations anywhere never reaches the stamp at all — the guard
	// under test would then be unexercised and this test would be green for
	// the wrong reason.  SetSource is applied before SealAST because it is
	// (correctly) a no-op on a sealed node.
	call.SetSource(&token.Location{File: "syn.lisp", Path: "syn.lisp", Pos: 1, Line: 1, Col: 1})
	call.SealAST()
	return []*lisp.LVal{call}
}

func (r *syntheticReader) Read(_ string, in io.Reader) ([]*lisp.LVal, error) {
	if _, err := io.ReadAll(in); err != nil {
		return nil, err
	}
	r.reads++
	return r.exprs(), nil
}

func (r *syntheticReader) ReadLocation(_, _ string, in io.Reader) ([]*lisp.LVal, error) {
	if _, err := io.ReadAll(in); err != nil {
		return nil, err
	}
	r.reads++
	return r.exprs(), nil
}

// TestLoadCacheDebuggerDoesNotStampSharedNodes is the sensitive half of the
// debugger ruling.
//
// TestLoadCacheDebuggerAliasIntact proves the end-to-end property over an
// ordinary parse; this one removes the parser's own protection from the
// picture so that the sealed skip is the only guard left, and shows the
// shared tree still survives a debugger-attached expansion over it.
//
// Sam's ruling was that debug mode MAY pay a performance penalty — a private
// copy of a cached tree whenever a debugger is attached.  It does not have
// to: this is the test that says why.
func TestLoadCacheDebuggerDoesNotStampSharedNodes(t *testing.T) {
	t.Parallel()
	const name, loc, payload = "syn.lisp", "syn.lisp", "(ignored by the synthetic reader)"
	cache := newTestLoadCache()

	// A warm environment populates the cache WITHOUT expanding anything:
	// `ident` is unbound here, so the load errors before macroCall runs and
	// the entry is stored pristine.  (readCached stores before env.load
	// evaluates, which is what makes this possible.)
	warm := lisp.NewEnv(nil)
	warm.Runtime.Reader = &syntheticReader{}
	warm.Runtime.LoadCache = cache
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(warm).Type)
	require.Equal(t, lisp.LError, warm.LoadLocation(name, loc, strings.NewReader(payload)).Type,
		"the warm load was expected to fail on the unbound macro name")

	entry := cache.only(t)
	exprs := lisp.CachedSourceExprs(entry)
	before := lisp.SealedASTFingerprint(exprs)

	// Anti-vacuity: the argument really is sealed and really has no usable
	// location, so the stamp's guard condition is satisfied and only the
	// seal stands between it and this node.
	require.Len(t, exprs, 1)
	require.Len(t, exprs[0].Cells, 2)
	arg := exprs[0].Cells[1]
	require.True(t, arg.IsSealed(), "the cached argument must be sealed")
	if src, ok := arg.Source(); ok {
		require.Negative(t, src.Pos, "the cached argument must have no real parse location")
	}

	// Now an environment with the macro defined and a debugger attached
	// hits that entry and expands over it, repeatedly.
	for _, withDebugger := range []bool{true, false} {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
		require.NotEqual(t, lisp.LError, env.LoadString("prelude", "(defmacro ident (x) x)").Type)
		if withDebugger {
			env.Runtime.Debugger = dormantDebugger{}
		}
		env.Runtime.LoadCache = cache
		env.Runtime.Reader = &syntheticReader{}

		for range 4 {
			v := env.LoadLocation(name, loc, strings.NewReader(payload))
			require.NotEqual(t, lisp.LError, v.Type, "expanding the cached macro call failed: %v", v)
			assert.Equal(t, "'(1 2 3)", v.String())
		}
		assert.Equalf(t, before, lisp.SealedASTFingerprint(exprs),
			"macro expansion (debugger=%v) wrote into the SHARED cached tree", withDebugger)
	}

	assert.Equal(t, 1, cache.stores, "the cached entry must have been parsed exactly once")
}
