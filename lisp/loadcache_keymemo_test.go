// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Repeated loads of byte-identical source reuse the digest instead of
// re-hashing (luthersystems/substrate#548).
func TestLoadCacheKeyMemoAvoidsRehash(t *testing.T) {
	resetLoadCacheKeyMemo()
	src := []byte(strings.Repeat("(set 'x 1)\n", 1000))
	before := loadCacheKeyDigests.Load()
	k1 := memoLoadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: true, src: append([]byte(nil), src...)})
	k2 := memoLoadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: true, src: append([]byte(nil), src...)})
	assert.Equal(t, int64(1), loadCacheKeyDigests.Load()-before, "second load must not re-hash")
	assert.Equal(t, k1, k2)
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: true, src: src}), k1)
}

// Same identity, same length, different content: never the same key.  The
// memo compares full bytes, so a caller mutating its slice cannot poison it.
func TestLoadCacheKeyMemoSameLengthDifferentContent(t *testing.T) {
	resetLoadCacheKeyMemo()
	a := []byte("(set 'x 1)")
	b := []byte("(set 'x 2)")
	require.Len(t, b, len(a))
	ka := memoLoadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: false, src: a})
	a[len(a)-2] = '2' // caller mutates its slice after the load
	kb := memoLoadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: false, src: b})
	assert.NotEqual(t, ka, kb)
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: false, src: b}), kb)
	// Only the last byte differs: still a distinct, correct key.
	c := bytes.Clone(b)
	c[0] = '['
	assert.Equal(t, loadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: false, src: c}),
		memoLoadCacheKey("f.lisp", "f.lisp", loadKeySource{readerID: "rid", byLoc: false, src: c}))
	// Every identity component still separates entries.
	for _, tc := range []struct {
		name, loc, rid string
		byLoc          bool
	}{{"g", "f.lisp", "rid", false}, {"f.lisp", "g", "rid", false}, {"f.lisp", "f.lisp", "r2", false}, {"f.lisp", "f.lisp", "rid", true}} {
		assert.Equal(t, loadCacheKey(tc.name, tc.loc, loadKeySource{readerID: tc.rid, byLoc: tc.byLoc, src: b}), memoLoadCacheKey(tc.name, tc.loc, loadKeySource{readerID: tc.rid, byLoc: tc.byLoc, src: b}))
	}
}

func BenchmarkLoadCacheKeyRepeat(b *testing.B) {
	src := []byte(strings.Repeat("(defun f (x) (+ x 1))\n", 20000)) // ~430 KB
	resetLoadCacheKeyMemo()
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for range b.N {
		memoLoadCacheKey("bench.lisp", "bench.lisp", loadKeySource{readerID: "rid", byLoc: true, src: src})
	}
}

func BenchmarkLoadCacheKeyNoMemo(b *testing.B) {
	src := []byte(strings.Repeat("(defun f (x) (+ x 1))\n", 20000))
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for range b.N {
		loadCacheKey("bench.lisp", "bench.lisp", loadKeySource{readerID: "rid", byLoc: true, src: src})
	}
}

// Through the funnel: a second environment loading the same bytes hits the
// cache without re-hashing, and a same-length edit under the same name is a
// miss that reparses (never the stale program).
func TestReadCachedReusesDigestAcrossEnvs(t *testing.T) {
	resetLoadCacheKeyMemo()
	cache := &mapLoadCache{}
	var parsed []string
	parse := func(r io.Reader) ([]*LVal, error) {
		b, err := io.ReadAll(r)
		if err != nil {
			return nil, err
		}
		parsed = append(parsed, string(b))
		e := SExpr([]*LVal{Symbol("quote"), String(string(b))})
		e.SealAST()
		return []*LVal{e}, nil
	}
	load := func(src string) []*LVal {
		env := NewEnv(nil)
		env.Runtime.LoadCache = cache
		exprs, err := env.readCached("f.lisp", "f.lisp", cachedRead{byLoc: true, r: strings.NewReader(src)}, parse)
		require.NoError(t, err)
		return exprs
	}
	before := loadCacheKeyDigests.Load()
	a1 := load("(set 'x 1)")
	a2 := load("(set 'x 1)")
	assert.Equal(t, int64(1), loadCacheKeyDigests.Load()-before)
	assert.Same(t, a1[0], a2[0])
	b := load("(set 'x 2)")
	assert.Equal(t, int64(2), loadCacheKeyDigests.Load()-before)
	assert.Equal(t, "(set 'x 2)", b[0].Cells[1].Str)
	assert.Equal(t, []string{"(set 'x 1)", "(set 'x 2)"}, parsed)
}

// Many empty sources under distinct names are still bounded: every entry is
// charged its overhead, so the memo resets instead of growing without limit.
func TestLoadCacheKeyMemoBoundsEntryCount(t *testing.T) {
	resetLoadCacheKeyMemo()
	t.Cleanup(resetLoadCacheKeyMemo)
	// A long reader identity keeps the entry count (and test time) small;
	// the identity strings are charged like the source copy.
	rid := strings.Repeat("r", 4096)
	maxEntries := loadCacheKeyMemoMaxBytes / (len(rid) + loadCacheKeyMemoEntryOverhead)
	for i := range maxEntries + 10 {
		name := fmt.Sprintf("f%d.lisp", i)
		memoLoadCacheKey(name, name, loadKeySource{readerID: rid, byLoc: true, src: nil})
		if i%1024 != 0 && i < maxEntries {
			continue
		}
		entries, charged := loadCacheKeyMemoUsage()
		require.LessOrEqual(t, charged, loadCacheKeyMemoMaxBytes)
		require.LessOrEqual(t, entries, maxEntries)
	}
	// Re-storing one identity replaces, never double-charges, its entry.
	resetLoadCacheKeyMemo()
	memoLoadCacheKey("a", "a", loadKeySource{readerID: "rid", byLoc: true, src: []byte("x")})
	_, c1 := loadCacheKeyMemoUsage()
	memoLoadCacheKey("a", "a", loadKeySource{readerID: "rid", byLoc: true, src: []byte("y")})
	e2, c2 := loadCacheKeyMemoUsage()
	assert.Equal(t, 1, e2)
	assert.Equal(t, c1, c2)
}

// Parallel runtimes hitting the memo concurrently (run under -race): every
// returned key equals the un-memoised key for its input.
func TestLoadCacheKeyMemoConcurrent(t *testing.T) {
	resetLoadCacheKeyMemo()
	t.Cleanup(resetLoadCacheKeyMemo)
	var wg sync.WaitGroup
	for g := range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := range 200 {
				src := []byte(fmt.Sprintf("(set 'x %d)", (g+i)%3))
				name := fmt.Sprintf("f%d.lisp", i%2)
				want := loadCacheKey(name, name, loadKeySource{readerID: "rid", byLoc: false, src: src})
				if got := memoLoadCacheKey(name, name, loadKeySource{readerID: "rid", byLoc: false, src: src}); got != want {
					t.Errorf("key mismatch for %q", src)
					return
				}
			}
		}()
	}
	wg.Wait()
}

// Sources over loadCacheKeyMemoMaxSource are hashed every time and never
// retained.
func TestLoadCacheKeyMemoSkipsOversizedSource(t *testing.T) {
	resetLoadCacheKeyMemo()
	t.Cleanup(resetLoadCacheKeyMemo)
	big := make([]byte, loadCacheKeyMemoMaxSource+1)
	before := loadCacheKeyDigests.Load()
	k1 := memoLoadCacheKey("big", "big", loadKeySource{readerID: "rid", byLoc: true, src: big})
	k2 := memoLoadCacheKey("big", "big", loadKeySource{readerID: "rid", byLoc: true, src: big})
	assert.Equal(t, k1, k2)
	assert.Equal(t, int64(2), loadCacheKeyDigests.Load()-before, "oversized source must not be memoised")
	entries, charged := loadCacheKeyMemoUsage()
	assert.Equal(t, 0, entries)
	assert.Equal(t, 0, charged)
	// Exactly at the limit is memoised.
	atLimit := make([]byte, loadCacheKeyMemoMaxSource)
	memoLoadCacheKey("edge", "edge", loadKeySource{readerID: "rid", byLoc: true, src: atLimit})
	entries, _ = loadCacheKeyMemoUsage()
	assert.Equal(t, 1, entries)
}

// Retained source bytes never exceed loadCacheKeyMemoMaxBytes: an insert
// that would pass it clears the memo first.
func TestLoadCacheKeyMemoTotalCapClears(t *testing.T) {
	resetLoadCacheKeyMemo()
	t.Cleanup(resetLoadCacheKeyMemo)
	src := make([]byte, loadCacheKeyMemoMaxSource)
	perEntry := loadCacheKeyMemoMaxBytes / loadCacheKeyMemoMaxSource // entries that fit, minus overhead
	for i := range perEntry + 2 {
		name := fmt.Sprintf("s%d", i)
		memoLoadCacheKey(name, name, loadKeySource{readerID: "rid", byLoc: true, src: src})
		entries, charged := loadCacheKeyMemoUsage()
		require.LessOrEqual(t, charged, loadCacheKeyMemoMaxBytes)
		require.GreaterOrEqual(t, entries, 1)
		require.Less(t, entries, perEntry+1)
	}
}
